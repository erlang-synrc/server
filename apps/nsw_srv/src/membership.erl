%% -*- mode: nitrogen -*-
-module (membership).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").

main() -> #template { file=code:priv_dir(nsw_srv)++"e/templates/bare.html" }.

title() -> "Membership".

body() ->
    #container_12 { body=[
        #grid_3 { alpha=true, suffix=6, class="blue logo", body=logo()  },
        #grid_3 { omega=true, class="account", body=account()  },

        #grid_12 { alpha=true, omega=true, class="blue member", style="min-height: 660px", body=member() }
    ]}.


logo() ->
    [
     "LOGO"
    ].

account() ->
    [
     "Account"
    ].

member() ->
    [
     "Membership step"
    ].

