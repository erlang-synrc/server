%% -*- mode: nitrogen -*-
-module (info_matchmaker).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("gettext.hrl").
-include("loger.hrl").
-include("setup.hrl").

main() ->
    webutils:js_for_main_authorized_game_stats_menu(),
    #template { file=code:priv_dir(nsp_srv)++"/templates/bare.html" }.

title() -> webutils:title(?MODULE).

body() ->
    #template{file=code:priv_dir(nsp_srv)++"/templates/info_page.html"}.

content() -> 
    [
        info_gifts:menu(),
        case site_utils:detect_language() of
            "en" -> en();
            "tr" -> tr()
        end
    ].

tr() ->
    #template { file=code:priv_dir(nsp_srv)++"/templates/info_matchmaker_tr.html" }.

en() ->
    #template { file=code:priv_dir(nsp_srv)++"/templates/info_matchmaker_en.html" }.

event(Any)->
    webutils:event(Any).
