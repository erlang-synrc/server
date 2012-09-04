-module (element_show_lightbox).
-compile (export_all).
-include_lib ("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-include("gettext.hrl").


reflect() -> record_info(fields, show_lightbox).


render_element(#show_lightbox{action=Action0}) ->
    Action = case Action0 /= undefined of
                 true -> Action0;
                 false -> wf:to_atom(wf:q(action))
             end,
    case Action of
        register ->
            register();
        "facebook-register" ->
            facebook_register();
        "login" ->
            login();
        "facebook-login" -> [];
        _ -> []
            %% facebook_login()

    end.
    %% case webutils:show_splash() of
    %%     false -> "";
    %%     true -> [webutils:splash_lightbox()]
    %% end,



register() ->
    %% case invite:get_invite_code() of
    %%     {ok, _Invite} ->
    wf:update(lightbox_content, #register{}),
    wf:wire(#show{target=lightbox}),
    [].
    %%     _error_or_undefined ->
    %%         event({error, ?_T("Invalid invitation code")})
    %% end.

facebook_register() ->
    case invite:get_invite() of
        {ok, _Invite} ->
            register();
        _error_or_undefined ->
            event({error, ?_T("Invalid invitation code")})
    end.

login() ->
    #show{target=login_lightbox}.



event(Other) ->
    webutils:event(Other).

    %% case wf:q(facebook) of
    %%     "true" ->
    %%         case wf:q(code) /= undefined of
    %%             true ->
    %%                 facebook_login();
    %%             false ->
    %%                 main_notauthorized()
    %%         end;
    %%     _ ->
    %%         case wf:q(verify) /= undefined of
    %%             true ->
    %%                 verification_account();
    %%             false ->
    %%                case wf:q(forget) of
    %%                    "true" ->
    %%                        case wf:q(token) of
    %%                            undefined -> wf:redirect("./access_denied");
    %%                            Token -> forget_password(Token)
    %%                        end;
    %%                    _ ->
    %%                        main_notauthorized()
    %%                end
    %%         end
    %% end.

    %% case wf:q(register) of
    %%     undefined ->
    %%         case wf:q(login) of
    %%             "facebook" ->
    %%                 event(show_register);
    %%              _ ->
    %%                 ok
    %%         end;
    %%     "true" ->
    %%         case invite:get_invite_code() of
    %%             {ok, _Invite} ->
    %%                 event(show_register);
    %%             _error_or_undefined ->
    %%                 event({error, ?_T("Invalid invitation code")})
    %%         end
    %% end,
    %% case wf:q(show_register) of
    %%     "true" ->
    %%         event(show_register);
    %%     _ ->
    %%         ok
    %% end,
    %% case wf:q(show_login) of
    %%     "true" ->
    %%         event(show_login);
    %%     "facebook" ->
    %%         event(show_login_with_facebook);
    %%     _ ->
    %%         ok
    %% end,
    %% Page.
