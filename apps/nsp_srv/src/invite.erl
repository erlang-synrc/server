-module(invite).

-compile(export_all).

-include_lib("nsm_db/include/invite.hrl").
-include_lib("nsm_db/include/user.hrl").
-include_lib("nitrogen_core/include/wf.hrl").
-include("common.hrl").
-include("setup.hrl").

main() ->
    InviteCode = wf:path_info(),
    invite_cookie(InviteCode),
    wf:redirect(lists:concat([?_U("/login/register/invite/"), InviteCode])).


%%
%% Utility functions
%%

convert_data(Data) ->
    Sorted = lists:sort(
        fun(#invite_code{create_date = CD1},
            #invite_code{create_date = CD2}) ->
                CD1 < CD2
            end, Data),
    Fun =
        fun(#invite_code{code = Code,
                         create_date = CData0,
                         recipient = Rec,
                         created_user = CUser}) ->

                Url = site_utils:create_url_invite(Code),
                Mail =
                    case Rec of
                        undefined ->
                            " - ";
                        A ->
                            A
                    end,
                CData = utils:now_to_seconds(CData0),
                ETime = ?INVITE_CODE_EXPIRED,
                ExpireTime = calendar:gregorian_seconds_to_datetime(CData+ETime),
                NowTime  = utils:now_to_seconds(now()),
                Expired = NowTime-(CData+ETime),
            ?PRINT({Expired, NowTime, CData, ETime, NowTime - CData}),

                UsedBy =
                    case CUser of
                        undefined ->
                            case Expired > 0  of
                                true -> ?_T("This invitiation code is expired. Please get new one.");
                                _    -> " - "
                            end;
                        User -> User
                    end,
                [Url, Mail, utils:date_to_text(ExpireTime), UsedBy]
        end,
    lists:map(Fun, Sorted).

%% API
-spec invite_cookie(string()) -> ok.
invite_cookie(InviteCode) ->
    wf:cookie("invite_code", InviteCode, "/", 100*24*3600), % 100 days
    ok.

%% API
-spec get_invite() -> {ok, #invite_code{}} | error | undefined.
%% @doc Returns invite stored by code stored in cookie or passed as parameter
%% if cookie isn't set.
get_invite() ->
    InviteCode =
    case wf:q(invite) of
        undefined ->
            wf:cookie("invite_code");
        Invite ->
            Invite
    end,
    ?PRINT(InviteCode),
    case InviteCode of
	undefined ->
            undefined;
	_ ->
	    case nsm_invite:check_code(InviteCode) of
		{ok, InviteRec} -> {ok, InviteRec};
		error -> error
	    end
    end.


send_invite(Email) ->
    case nsm_invite:send_invite_email(_User = #user{}, Email, _Username = ?_T("User"),  _OptionalText = "") of
        {ok, _Code} ->
           {ok, sent};
        {error, wrong_username} ->
            {error, wrong_username};
        {error, wrong_email} ->
            {error, wromg_email}
    end.
