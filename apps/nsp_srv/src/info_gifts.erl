%% -*- mode: nitrogen -*-
-module (info_gifts).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("gettext.hrl").
-include("setup.hrl").

main() ->
    #template { file=code:priv_dir(nsp_srv)++"/templates/bare.html" }.

title() -> webutils:title(?MODULE).

body() ->
    #template{file=code:priv_dir(nsp_srv)++"/templates/info_page.html"}.

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
           "/info-gifts" ->
                #image{image="/images/more_info/top_plask_1.png", style="margin-top:-15px; margin-left:-24px;"};
            "/info-tournaments" ->
                #image{image="/images/more_info/top_plask_2.png", style="margin-top:-15px; margin-left:-24px;"};
            "/info-social" ->
                #image{image="/images/more_info/top_plask_3.png", style="margin-top:-15px; margin-left:-24px;"}; 
            "/info-matchmaker" ->
                #image{image="/images/more_info/top_plask_4.png", style="margin-top:-15px; margin-left:-24px;"};  
             "/info-why" ->
                #image{image="/images/more_info/top_plask_5.png", style="margin-top:-15px; margin-left:-24px;"};   
            _ ->
                #image{image="/images/more_info/top_plask.png", style="margin-top:-15px; margin-left:-24px;"}
        end,
        case En of
           "/info-gifts" ->
                 #label{text=("HEDİYELER"), class="info-page-menu-elements info-page-gifts-label"};
           _ ->  #link{text=("HEDİYELER"), url=?_U("/info-gifts"), class="info-page-menu-elements info-page-gifts-link"}
        end,
        case En of 
             "/info-tournaments" ->
                  #label{text=("TURNUVALAR"), class="info-page-menu-elements info-page-tournaments-label"};
             _ -> #link{text=("TURNUVALAR"), url=?_U("/info-tournaments"), class="info-page-menu-elements info-page-tournaments-link"}
        end,
        case En of
             "/info-social" -> 
                  #label{text=("SOSYALLEŞMEK"), class="info-page-menu-elements info-page-social-label"};
             _ -> #link{text=("SOSYALLEŞMEK"), url=?_U("/info-social"), class="info-page-menu-elements info-page-social-link"}
        end,
        case En of
             "/info-matchmaker" -> 
                   #label{text=("KENDİ DÜNYAN"), class="info-page-menu-elements info-page-matchmaker-label"};
             _ ->  #link{text=("KENDİ DÜNYAN"), url=?_U("/info-matchmaker"), class="info-page-menu-elements info-page-matchmaker-link"}
        end,
        case En of
             "/info-why" -> 
                   #label{text=("ÜYELIK PAKETİ"), class="info-page-menu-elements info-page-membership-label"};
             _ ->  #link{text=("ÜYELIK PAKETİ"), url=?_U("/info-why"), class="info-page-menu-elements info-page-membership-link"}
        end,
        #link{text=("ÜYE OL!"), url=?_U("/login/register"), class="info-page-menu-elements info-page-join-link"}
    ].

menu_en(En) ->
    [
        case En of 
           "/info-gifts" ->
                #image{image="/images/more_info/top_plask_1.png", style="margin-top:-15px; margin-left:-24px;"};
            "/info-tournaments" ->
                #image{image="/images/more_info/top_plask_2.png", style="margin-top:-15px; margin-left:-24px;"};
            "/info-social" ->
                #image{image="/images/more_info/top_plask_3.png", style="margin-top:-15px; margin-left:-24px;"}; 
            "/info-matchmaker" ->
                #image{image="/images/more_info/top_plask_4.png", style="margin-top:-15px; margin-left:-24px;"};  
             "/info-why" ->
                #image{image="/images/more_info/top_plask_5.png", style="margin-top:-15px; margin-left:-24px;"};   
            _ ->
                #image{image="/images/more_info/top_plask.png", style="margin-top:-15px; margin-left:-24px;"}
        end,
        case En of
           "/info-gifts" ->
                 #label{text=("GIFTS"), class="info-page-menu-elements info-page-gifts-label"};
           _ ->  #link{text=("GIFTS"), url=?_U("/info-gifts"), class="info-page-menu-elements info-page-gifts-link"}
        end,
        case En of 
             "/info-tournaments" ->
                  #label{text=("TOURNAMENTS"), class="info-page-menu-elements info-page-tournaments-label", style="letter-spacing:-1px; margin-left:-3px;"};
             _ -> #link{text=("TOURNAMENTS"), url=?_U("/info-tournaments"), class="info-page-menu-elements info-page-tournaments-link", style="letter-spacing:-1px; margin-left:-3px;"}
        end,
        case En of
             "/info-social" -> 
                  #label{text=("BE SOCIAL!"), class="info-page-menu-elements info-page-social-label"};
             _ -> #link{text=("BE SOCIAL!"), url=?_U("/info-social"), class="info-page-menu-elements info-page-social-link"}
        end,
        case En of
             "/info-matchmaker" -> 
                   #label{text=("MATCHMAKER"), class="info-page-menu-elements info-page-matchmaker-label"};
             _ ->  #link{text=("MATCHMAKER"), url=?_U("/info-matchmaker"), class="info-page-menu-elements info-page-matchmaker-link"}
        end,
        case En of
             "/info-why" -> 
                   #label{text=("MEMBERSHIP"), class="info-page-menu-elements info-page-membership-label"};
             _ ->  #link{text=("MEMBERSHIP"), url=?_U("/info-why"), class="info-page-menu-elements info-page-membership-link"}
        end,
        #link{text=("SIGNUP!"), url=?_U("/login/register"), class="info-page-menu-elements info-page-join-link"}
    ].

content() ->   
    [
        menu(),
        case site_utils:detect_language() of
            "en" -> en();
            "tr" -> tr()
        end
    ].


tr() ->
    #template { file=code:priv_dir(nsp_srv)++"/templates/info_gifts_tr.html" }.

en() ->
    #template { file=code:priv_dir(nsp_srv)++"/templates/info_gifts_en.html" }.

api_event(Name, Tag, Args)->
  webutils:api_event(Name, Tag, Args).

event(Any)->
    webutils:event(Any).

