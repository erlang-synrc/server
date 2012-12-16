%% -*- mode: nitrogen -*-
-module (rules_okey).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("gettext.hrl").
-include("setup.hrl").

main() ->
    webutils:js_for_main_authorized_game_stats_menu(),
    #template { file=code:priv_dir(nsw_srv)++"/templates/bare.html" }.

title() -> webutils:title(?MODULE).

body() ->
    #template{file=code:priv_dir(nsw_srv)++"/templates/info_page.html"}.

menu() ->
    Request = wf_context:request_bridge(),
    BasePath = Request:path(),
    En = uri_translator:translate(BasePath),
    case site_utils:detect_language() of
        "en" -> menu_en(En);
        "tr" -> menu_tr(En)
    end.

menu_tr(En) ->
    [
        case En of 
           "/rules-okey" ->
                #image{image="/images/rules/top_plask_1.png", style="margin-top:-15px; margin-left:-24px;"};
           "/rules-tavla" ->
                #image{image="/images/rules/top_plask_2.png", style="margin-top:-15px; margin-left:-24px;"};
            _ ->
                #image{image="/images/rules/top_plask.png", style="margin-top:-15px; margin-left:-24px;"}
        end,
        case En of
           "/rules-okey" ->
                 #label{body="OKEY<br>KURALLARI", class="rules-page-menu-elements rules-page-okey-label"};
           _ ->  #link{body="OKEY<br>KURALLARI", url=?_U("/rules-okey"), class="rules-page-menu-elements rules-page-okey-link"}
        end,
        #link{body="TAVLA<br>KURALLARI", class="rules-page-menu-elements rules-page-tavla-link", postback=not_implemented},
        #link{body="KING<br>KURALLARI", class="rules-page-menu-elements rules-page-king-link", postback=not_implemented},
        #link{body="BATAK<br>KURALLARI", class="rules-page-menu-elements rules-page-batak-link", postback=not_implemented},
        #link{body="SORBİ<br>KURALLARI", class="rules-page-menu-elements rules-page-sorbi-link", postback=not_implemented}
    ].

menu_en(En) ->
    [
        case En of 
           "/rules-okey" ->
                #image{image="/images/rules/top_plask_1.png", style="margin-top:-15px; margin-left:-24px;"};
           "/rules-tavla" ->
                #image{image="/images/rules/top_plask_2.png", style="margin-top:-15px; margin-left:-24px;"};
            _ ->
                #image{image="/images/rules/top_plask.png", style="margin-top:-15px; margin-left:-24px;"}
        end,
        case En of
           "/rules-okey" ->
                 #label{body="OKEY<br>RULES", class="rules-page-menu-elements rules-page-okey-label"};
           _ ->  #link{body="OKEY<br>RULES", url=?_U("/rules-okey"), class="rules-page-menu-elements rules-page-okey-link"}
        end,
        #link{body="TAVLA<br>RULES", class="rules-page-menu-elements rules-page-tavla-link", style="letter-spacing:-1px; margin-left:-3px;", postback=not_implemented},
        #link{body="KING<br>RULES", class="rules-page-menu-elements rules-page-king-link", postback=not_implemented},
        #link{body="BATAK<br>RULES", class="rules-page-menu-elements rules-page-batak-link", postback=not_implemented},
        #link{body="SORBI<br>RULES", class="rules-page-menu-elements rules-page-sorbi-link", postback=not_implemented}
    ].

content() ->   
    [
        menu(),
        case site_utils:detect_language() of
            "en" -> tr();
            "tr" -> tr()
        end
    ].


tr() -> 
    #template { file=code:priv_dir(nsw_srv)++"/templates/rules_okey_tr.html" }.

event(not_implemented) ->
    wf:wire(#alert{text=?_T("This feature is not yet available in beta.") ++ " " ++ ?_T("You will see this part very soon.")});

event(Any)->
    webutils:event(Any).

api_event(Name, Tag, Args) ->
  webutils:api_event(Name, Tag, Args).
