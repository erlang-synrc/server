%% -*- mode: nitrogen -*-
-module (rules_okey).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("common.hrl").
-include("setup.hrl").

main() -> #template{file=code:priv_dir(nsp_srv)++"/templates/base.html"}.

title() -> webutils:title(?MODULE).

body() ->
  Request = wf_context:request_bridge(),
  BasePath = Request:path(),
  [
    #panel{class="page-content", body=webutils:quick_nav()},
    #panel{class="page-content page-canvas", style="position:relative;", body=[ % heritage of absolute positioning
      case uri_translator:translate(BasePath) of
       "/rules-okey" ->
          [#image{image=?STATIC_ADDRESS++"/images/rules/top_plask_1.png", style="margin-top:-15px; margin-left:-24px;"},
          #label{body="OKEY<br>"++?_T("RULES"), class="rules-page-menu-elements rules-page-okey-label"}];
       "/rules-tavla" ->
          #image{image=?STATIC_ADDRESS++"/images/rules/top_plask_2.png", style="margin-top:-15px; margin-left:-24px;"};
        _ ->
          [#image{image=?STATIC_ADDRESS++"/images/rules/top_plask.png", style="margin-top:-15px; margin-left:-24px;"},
          #link{body="OKEY<br>"++?_T("RULES"), url=?_U("/rules-okey"), class="rules-page-menu-elements rules-page-okey-link"}]
      end,
      #link{body="TAVLA<br>"++?_T("RULES"), class="rules-page-menu-elements rules-page-tavla-link", postback=not_implemented},
      #link{body="KING<br>"++?_T("RULES"),  class="rules-page-menu-elements rules-page-king-link", postback=not_implemented},
      #link{body="BATAK<br>"++?_T("RULES"), class="rules-page-menu-elements rules-page-batak-link", postback=not_implemented},
      #link{body="SORBİ<br>"++?_T("RULES"), class="rules-page-menu-elements rules-page-sorbi-link", postback=not_implemented},

      #template{file=code:priv_dir(nsp_srv)++"/templates/rules_okey_tr.html"}
    ]}
  ].

event(not_implemented) ->
    wf:wire(#alert{text=?_T("This feature is not yet available in beta.") ++ " " ++ ?_T("You will see this part very soon.")});

event(Any)->
  webutils:event(Any).

api_event(Name, Tag, Args) ->
  webutils:api_event(Name, Tag, Args).
