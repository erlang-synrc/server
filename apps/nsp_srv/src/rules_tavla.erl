-module(rules_tavla).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("gettext.hrl").
-include("setup.hrl").

main() ->
    webutils:js_for_main_authorized_game_stats_menu(),
    #template { file=code:priv_dir(nsp_srv)++"/templates/bare.html" }.

title() -> webutils:title(?MODULE).
body() -> #template{file=code:priv_dir(nsp_srv)++"/templates/info_page.html"}.
content() -> [ rules_okey:menu(), case site_utils:detect_language() of "en" -> tr(); "tr" -> tr() end ].

tr() -> [ #panel{class="info-page-header-panel", body=[
            #image{image="/images/rules/rules_tavla_bg.png", class="info-page-bg"} ]} ].

event(Any)-> webutils:event(Any).
