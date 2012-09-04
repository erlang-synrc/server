%% -*- mode: nitrogen -*-
-module (info_gifts).
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
                 #label{text=?_T("HEDİYELER"), class="info-page-menu-elements info-page-gifts-label"};
           _ ->  #link{text=?_T("HEDİYELER"), url=?_U("/info-gifts"), class="info-page-menu-elements info-page-gifts-link"}
        end,
        case En of 
             "/info-tournaments" ->
                  #label{text=?_T("TURNUVALAR"), class="info-page-menu-elements info-page-tournaments-label"};
             _ -> #link{text=?_T("TURNUVALAR"), url=?_U("/info-tournaments"), class="info-page-menu-elements info-page-tournaments-link"}
        end,
        case En of
             "/info-social" -> 
                  #label{text=?_T("SOSYALLEŞMEK"), class="info-page-menu-elements info-page-social-label"};
             _ -> #link{text=?_T("SOSYALLEŞMEK"), url=?_U("/info-social"), class="info-page-menu-elements info-page-social-link"}
        end,
        case En of
             "/info-matchmaker" -> 
                   #label{text=?_T("KENDİ DÜNYAN"), class="info-page-menu-elements info-page-matchmaker-label"};
             _ ->  #link{text=?_T("KENDİ DÜNYAN"), url=?_U("/info-matchmaker"), class="info-page-menu-elements info-page-matchmaker-link"}
        end,
        case En of
             "/info-why" -> 
                   #label{text=?_T("ÜYELIK PAKETİ"), class="info-page-menu-elements info-page-membership-label"};
             _ ->  #link{text=?_T("ÜYELIK PAKETİ"), url=?_U("/info-why"), class="info-page-menu-elements info-page-membership-link"}
        end,
        #link{text=?_T("ÜYE OL!"), url=?_U("/login/register"), class="info-page-menu-elements info-page-join-link"}
    ].

content() ->    % PUBLIC BETA will have to redo this with css    
    [
        menu(),
        #panel{class="info-page-header-panel", body=[
            #image{image="/images/more_info/info_gifts_bg.png", class="info-page-bg"},
            #panel{style="top:90px; left:230px; font-size:50px;",body=?_T("Üye Olduğunuz An")},
            #panel{style="top:145px; left:225px; font-size:70px;",body=?_T("Hediyenizi Alın!")},
            #panel{style="top:225px; left:230px; font-size:30px; color:#FC6404;",body=?_T("Artık karşılıksız verme devri bitti...")}
        ]},
        #panel{class="info-page-content-panel", body=[
            #panel{style="top:385px; left:400px; font-size:18px;",body=?_T("Bundan sonra oynamaya ayırdığınız")},
            #panel{style="top:410px; left:430px; font-size:25px; color:#FC6404;",body=?_T("zaman ve yaptığınız harcamalar")},
            #panel{style="top:444px; left:460px; font-size:18px;",body=?_T("size <b>hediye</b> olarak geri dönüyor. ")}
        ]},
        #panel{class="info-page-content-panel", body=[
            #panel{style="top:627px; left:331px; font-size:18px;",body=?_T("Üyelerimiz, hiçbir şarta bağlı")},
            #panel{style="top:651px; left:237px; font-size:18px;",body=?_T("olmadan hediye puanları kazanırlar.")},
            #panel{style="top:674px; left:51px; font-size:25px; color:#FC6404;",body=?_T("Kazanmak yada kaybetmek fark etmez.")},
            #panel{style="top:705px; left:144px; font-size:18px;",body=?_T("Oyun oynayan her oyuncu hediye puanı kazanır.")},
            #panel{style="top:730px; left:72px; font-size:18px;",body=?_T("Kaybeden de kazanır, kazanan daha da çok kazanır.")}
        ]},
        #panel{class="info-page-content-panel", body=[
            #panel{style="top:842px; left:420px; font-size:18px;",body=?_T("Hesabında biriken puanlar ile")},
            #panel{style="top:867px; left:439px; font-size:20px;",body=?_T("hediye kataloğundan <b>istediği</b> hediyeyi alabilir.")}
        ]},
        #panel{class="info-page-content-panel", body=[
            #panel{style="top:1052px; left:209px; font-size:18px;",body=?_T("İsterse, puanlı üyelik paketi satın alarak")},
            #panel{style="top:1078px; left:186px; font-size:20px;",body=?_T("daha çok <b>puan</b> kazanır.")}
        ]},
        #panel{class="info-page-content-panel", body=[
            #panel{style="top:1287px; left:444px; font-size:18px;",body=?_T("Sanal alemden satın alabileceğiniz")},
            #panel{style="top:1311px; left:468px; font-size:20px;",body=?_T("<b>çok geniş bir ürün yelpazesi</b> sizi bekliyor…")}
        ]}
    ].


event(Any)->
    webutils:event(Any).

