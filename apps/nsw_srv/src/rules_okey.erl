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
        case En of 
            "/rules-tavla" ->
                  #label{body="TAVLA<br>KURALLARI", class="rules-page-menu-elements rules-page-tavla-label"};
             _ -> #link{body="TAVLA<br>KURALLARI", url=?_U("/rules-tavla"), class="rules-page-menu-elements rules-page-tavla-link"}
        end,
        #link{body="KING<br>KURALLARI", url=?_U(""), class="rules-page-menu-elements rules-page-king-link"},
        #link{body="BATAK<br>KURALLARI", url=?_U(""), class="rules-page-menu-elements rules-page-batak-link"},
        #link{body="SORBİ<br>KURALLARI", url=?_U(""), class="rules-page-menu-elements rules-page-sorbi-link"},
        #link{text=?_T("ÜYE OL!"), url=?_U("/login/register"), class="rules-page-menu-elements rules-page-join-link"}
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
        case En of 
            "/rules-tavla" ->
                  #label{body="TAVLA<br>RULES", class="rules-page-menu-elements rules-page-tournaments-label", style="letter-spacing:-1px; margin-left:-3px;"};
             _ -> #link{body="TAVLA<br>RULES", url=?_U("/rules-tavla"), class="rules-page-menu-elements rules-page-tavla-link", style="letter-spacing:-1px; margin-left:-3px;"}
        end,
        #link{body="KING<br>RULES", url=?_U(""), class="rules-page-menu-elements rules-page-king-link"},
        #link{body="BATAK<br>RULES", url=?_U(""), class="rules-page-menu-elements rules-page-batak-link"},
        #link{body="SORBI<br>RULES", url=?_U(""), class="rules-page-menu-elements rules-page-sorbi-link"},
        #link{text=?_T("SIGNUP!"), url=?_U("/login/register"), class="rules-page-menu-elements rules-page-join-link"}
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
    [
        #panel{class="info-page-header-panel", body=[
            #image{image="/images/rules/rules_okey_bg.png", class="info-page-bg"}
        ]}
%            #panel{style="top:90px; left:230px; font-size:50px;",body=?_T("Üye Olduğunuz An")},
%            #panel{style="top:145px; left:225px; font-size:70px;",body=?_T("Hediyenizi Alın!")},
%            #panel{style="top:225px; left:230px; font-size:30px; color:#FC6404;",body=?_T("Artık karşılıksız verme devri bitti...")}
%       ]},
%        #panel{class="info-page-content-panel", body=[
%            #panel{style="top:385px; left:400px; font-size:18px;",body=?_T("Bundan sonra oynamaya ayırdığınız")},
%            #panel{style="top:410px; left:430px; font-size:25px; color:#FC6404;",body=?_T("zaman ve yaptığınız harcamalar")},
%            #panel{style="top:444px; left:460px; font-size:18px;",body=?_T("size <b>hediye</b> olarak geri dönüyor. ")}
%        ]},
%        #panel{class="info-page-content-panel", body=[
%            #panel{style="top:627px; left:331px; font-size:18px;",body=?_T("Üyelerimiz, hiçbir şarta bağlı")},
%            #panel{style="top:651px; left:237px; font-size:18px;",body=?_T("olmadan hediye puanları kazanırlar.")},
%            #panel{style="top:674px; left:51px; font-size:25px; color:#FC6404;",body=?_T("Kazanmak yada kaybetmek fark etmez.")},
%            #panel{style="top:705px; left:144px; font-size:18px;",body=?_T("Oyun oynayan her oyuncu hediye puanı kazanır.")},
%            #panel{style="top:730px; left:72px; font-size:18px;",body=?_T("Kaybeden de kazanır, kazanan daha da çok kazanır.")}
%        ]},
%        #panel{class="info-page-content-panel", body=[
%            #panel{style="top:842px; left:420px; font-size:18px;",body=?_T("Hesabında biriken puanlar ile")},
%            #panel{style="top:867px; left:439px; font-size:20px;",body=?_T("hediye kataloğundan <b>istediği</b> hediyeyi alabilir.")}
%        ]},
%        #panel{class="info-page-content-panel", body=[
%            #panel{style="top:1052px; left:209px; font-size:18px;",body=?_T("İsterse, puanlı üyelik paketi satın alarak")},
%            #panel{style="top:1078px; left:186px; font-size:20px;",body=?_T("daha çok <b>puan</b> kazanır.")}
%        ]},
%       #panel{class="info-page-content-panel", body=[
%            #panel{style="top:1287px; left:444px; font-size:18px;",body=?_T("Sanal alemden satın alabileceğiniz")},
%            #panel{style="top:1311px; left:468px; font-size:20px;",body=?_T("<b>çok geniş bir ürün yelpazesi</b> sizi bekliyor…")}
%        ]}
    ].


event(Any)->
    webutils:event(Any).

