-module(invite).

-export([generate_code/1,
         generate_code/2,
         check_code/1,
         use_code/2,
         get_user_code/1,
         get_code_per_created_user/1,
         get_all_code/0,
         send_invite_email/4,
         send_invite_email/5
	]).

-include_lib("nsm_db/include/user.hrl").
-include_lib("nsm_db/include/invite.hrl").
-include_lib("alog/include/alog.hrl").

-include("config.hrl").

%% use rpc call to web node to get gettext support
-define(TXT2(Key, Lang), rpc:call(?WEBSERVER_NODE, gettext, key2str, [Key, Lang])).

generate_code(User) ->
    generate_code(User, undefined).

generate_code(#user{username = User}, Mail) ->
    generate_code(User, Mail);
generate_code(User, Mail) when is_list(User);
                               User == undefined ->
    Code = code(),
    Rec = #invite_code{code = Code,
                       create_date = erlang:now(),
                       recipient = Mail,
                       issuer = User},
    %nsm_db:put(Rec),
    %nsx_util_notification:notify(["system", "put"], Rec),
    nsx_util_notification:notify(["invite", "user", User, "add_invite_to_issuer"], {Rec}),
    {ok, Code}.

check_code(Code) ->
    case nsm_db:get(invite_code, Code) of
        {ok, Invite} ->
            case Invite of
                Invite when Invite#invite_code.created_user =/= undefined ->
                    error;
                _ ->
                    {ok, Invite}
            end;
        {error, _} ->
            error
    end.

use_code(Code, #user{username = UN}) ->
    use_code(Code, UN);
use_code(Code, User) when is_list(User) ->
    case nsm_db:get(invite_code, Code) of
        {ok, Invite} ->
            Result = nsm_db:put(Invite#invite_code{created_user = User}),
            %% add to tree
            Parent = Invite#invite_code.issuer,
            Parent /= undefined andalso
                nsm_db:put_into_invitation_tree(Parent, User, Code),
            ?INFO("Put code: parent ~p, user: ~p, Code: ~p", [Parent, User, Code]),
            Result;
        {error, _} ->
            error
    end.

get_user_code(#user{username = UN}) ->
    get_user_code(UN);
get_user_code(User) when is_list(User) ->
    nsm_db:invite_code_by_issuer(User).

get_code_per_created_user(#user{username = UN}) ->
    get_code_per_created_user(UN);
get_code_per_created_user(User) ->
    nsm_db:invite_code_by_user(User).

get_all_code() ->
    nsm_db:all(invite_code).

code() ->
    <<A:(16*8), _/binary>> = crypto:rand_bytes(16),
    lists:sublist(lists:flatten(io_lib:format("~25.36.0b", [A])), 14, 9).


send_invite_email(User, Email, UserName, Text) ->
    %% FIXME: hardcoded tr language
    send_invite_email(User, Email, UserName, Text, "tr").

-spec send_invite_email(record(user), string(), string(), string(), string()) -> {ok, string()} | {error, atom()}.
send_invite_email(User, Email, UserName, Text, Lang) ->
    case rpc:call(?WEBSERVER_NODE,validator_is_email,validate,["", Email]) of
	true ->
	    case length(UserName)>0 of
		true ->
		    Code = invite:generate_code(User, Email),
		    ?INFO("Code: ~p",[Code]),
		    {ok, InviteCode} = Code,
		    Url = rpc:call(?WEBSERVER_NODE, site_utils, create_url_invite, [InviteCode]),
		    ?INFO("Invite url: ~p, InviteCode: ~p",[Url,InviteCode]),
		    FromUser = case User#user.username of
				   S when is_list(S) -> S;
				   undefined -> ?TXT2("'kakaranet robot'", Lang)
			       end,
		    {Subject, Content} = rpc:call(?WEBSERVER_NODE,site_utils,invite_message,[FromUser, Url, Text, UserName, Lang]),
                    nsx_util_notification:notify_email(Subject, Content, Email),
		    {ok, InviteCode};
		_ ->
		    {error, wrong_username}
	    end;
	false ->
	    {error, wrong_email}
    end.


