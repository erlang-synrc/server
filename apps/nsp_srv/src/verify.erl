-module(verify).
-compile(export_all).
-include_lib("nsm_db/include/config.hrl").
-include_lib("nsm_db/include/user.hrl").
-include("gettext.hrl").
-include("setup.hrl").

main() ->
    VerificationCode = wf:q(code),
    wf:redirect(lists:concat(["/login/verify/code/", VerificationCode])).

-spec verify_account(string()) -> {error, atom()} | {ok, atom()}.
verify_account(Code) ->
    User = nsm_db:user_by_verification_code(Code),
    case User of
        {error, _NotFound} ->
            {error, bad_verification_code};
        {ok, User0} ->
            UpdateUser = User0#user{status = ok},
            nsm_db:put(UpdateUser),
            %nsx_msg:notify(["system", "put"], UpdateUser),
            wf:session(user_info, UpdateUser),
            {ok, account_activated}
    end.

-spec create_url(string()) -> iolist().
create_url(Code) ->
    lists:concat([?_U("/login/verify"), "/code/", Code]).
