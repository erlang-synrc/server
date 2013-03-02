-module(rules_tavla).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("gettext.hrl").
-include("setup.hrl").

main() ->
    #template { file=code:priv_dir(nsp_srv)++"/templates/bare.html" }.

title() -> webutils:title(?MODULE).
body() -> [
  "<section id=\"main\">",
    case site_utils:detect_language() of "en" -> tr(); "tr" -> tr() end,
  "</section>"
  ].

tr() -> [ #panel{class="info-page-header-panel", body=[
            #image{image="/images/rules/rules_tavla_bg.png", class="info-page-bg"} ]} ].

event(Any)-> webutils:event(Any).
