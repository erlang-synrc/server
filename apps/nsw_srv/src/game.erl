%% -*- mode: nitrogen -*-
-module (game).
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
                            #panel { class="blue gamearea", body=gamearea() },
                            #panel { class="blue rss_container", body=rss_container() }
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

gamearea() ->
    [
     wf:q(game)
    ].

rss_container() ->
    [
     #panel { class="silver rss", body=rss() }
    ].

rss() ->
    [
     "Feeds"
    ].

