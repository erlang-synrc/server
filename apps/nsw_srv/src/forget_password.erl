-module(forget_password).

-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("nsm_db/include/user.hrl").
-include_lib("nsm_db/include/config.hrl").

-include("gettext.hrl").
-include("setup.hrl").

main() ->
    Request = wf_context:request_bridge(),
    Ip = Request:peer_ip(),
    case rpc:call(?APPSERVER_NODE,nsm_acl,check_access,[{ip, Ip}, {feature, forget_password}]) of
        deny ->
            wf:redirect(?_U("/access-denied"));
        _ ->
            case wf:q(token) of
                undefined ->
                    wf:redirect(?_U("/access-denied"));
                Token ->
                    Url = lists:concat(["/index/forget/token/", wf:url_encode(Token)]),
                    wf:redirect(Url)
            end
    end.
