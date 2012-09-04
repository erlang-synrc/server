%% -*- mode: nitrogen -*-
-module (mainmember).
-compile(export_all).

-include_lib("nitrogen_core/include/wf.hrl").

main() -> #template { file=code:priv_dir(nsw_srv)++"/templates/bare.html" }.

title() -> "Game Page".

body() ->
    #container_12 {
      body=[
            #grid_3 { alpha=true, suffix=6, class="blue logo", body=logo()  },
            #grid_3 { omega=true, class="account", body=account()  },

            #grid_clear{},

            #grid_3 { body=[
                            #panel { class="green tournaments", body=tournaments() },
                            #panel { class="blue gifts", body=gifts() }
                           ], alpha=true},
            #grid_9 { body=[
                            #panel { class="blue rss_container", body=games() },
                            #panel { class="blue gamearea", body=big() }
                            ], omega=true}
           ]}.


logo() ->
    [
     "LOGO"
    ].

account() ->
    [
     "Account"
    ].

gifts() ->
    [
     "Gifts"
    ].

tournaments() ->
    [
     "Tournaments"
    ].

big() ->
    [
     #panel { class="silver search",  body=["Search"] },
     #panel { class="silver", style="margin: 10px;",  body=["Post"] },
     #panel { class="silver area",  body=["AREA?"] }
    ].

games() ->
    [
     "Games<br>",
     #panel { class="orange game", body=["Okey"] },
     #panel { class="orange game", body=["Tavla"] },
     #panel { class="orange game", body=["King"] },
     #panel { class="orange game", body=["Batak"] },
     #panel { class="orange game", body=["Quizmatic"] }
    ].
