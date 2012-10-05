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
                 #label{text=?_T("GIFTS"), class="info-page-menu-elements info-page-gifts-label"};
           _ ->  #link{text=?_T("GIFTS"), url=?_U("/info-gifts"), class="info-page-menu-elements info-page-gifts-link"}
        end,
        case En of 
             "/info-tournaments" ->
                  #label{text=?_T("TOURNAMENTS"), class="info-page-menu-elements info-page-tournaments-label", style="letter-spacing:-1px; margin-left:-3px;"};
             _ -> #link{text=?_T("TOURNAMENTS"), url=?_U("/info-tournaments"), class="info-page-menu-elements info-page-tournaments-link", style="letter-spacing:-1px; margin-left:-3px;"}
        end,
        case En of
             "/info-social" -> 
                  #label{text=?_T("BE SOCIAL!"), class="info-page-menu-elements info-page-social-label"};
             _ -> #link{text=?_T("BE SOCIAL!"), url=?_U("/info-social"), class="info-page-menu-elements info-page-social-link"}
        end,
        case En of
             "/info-matchmaker" -> 
                   #label{text=?_T("MATCHMAKER"), class="info-page-menu-elements info-page-matchmaker-label"};
             _ ->  #link{text=?_T("MATCHMAKER"), url=?_U("/info-matchmaker"), class="info-page-menu-elements info-page-matchmaker-link"}
        end,
        case En of
             "/info-why" -> 
                   #label{text=?_T("MEMBERSHIP"), class="info-page-menu-elements info-page-membership-label"};
             _ ->  #link{text=?_T("MEMBERSHIP"), url=?_U("/info-why"), class="info-page-menu-elements info-page-membership-link"}
        end,
        #link{text=?_T("SIGNUP!"), url=?_U("/login/register"), class="info-page-menu-elements info-page-join-link"}
    ].

content() ->   
    [
        menu(),
        case site_utils:detect_language() of
            "en" -> en();
            "tr" -> tr()
        end
    ].


tr() -> % PUBLIC BETA will have to redo this with css    
    [
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


en() ->
    [
        #panel{class="info-page-header-panel", body=[
            #image{image="/images/more_info/info_gifts_bg.png", class="info-page-bg"},
            #panel{style="top:90px; left:230px; font-size:50px;",body=?_T("Claim your prize right at")},
            #panel{style="top:145px; left:232px; font-size:48px;",body=?_T("the moment you register!")},
            #panel{style="top:225px; left:230px; font-size:30px; color:#FC6404;",body=?_T("No more depositing with no results.")}
        ]},
        #panel{class="info-page-content-panel", body=[
            #panel{style="top:375px; left:400px; font-size:25px;",body=?_T("Your time spent and purchases will")},
            #panel{style="top:410px; left:440px; font-size:25px; color:#FC6404;",body=?_T("come back to you as prizes!")}
        ]},
        #panel{class="info-page-content-panel", body=[
            #panel{style="top:635px; left:112px; font-size:20px; 
                            transform:rotate(1.5deg);
                            -ms-transform:rotate(1.5deg);
                            -moz-transform:rotate(1.5deg);
                            -webkit-transform:rotate(1.5deg);
                            -o-transform:rotate(1.5deg);",
            body=?_T("No matter if you win or lose, you will EARN the")},
            #panel{style="top:668px; left:130px; font-size:25px; color:#FC6404;
                            transform:rotate(1.5deg);
                            -ms-transform:rotate(1.5deg);
                            -moz-transform:rotate(1.5deg);
                            -webkit-transform:rotate(1.5deg);
                            -o-transform:rotate(1.5deg);",body=?_T("points to claim the prize you want.")},
            #panel{style="top:715px; left:51px; font-size:17px;
                            transform:rotate(1.5deg);
                            -ms-transform:rotate(1.5deg);
                            -moz-transform:rotate(1.5deg);
                            -webkit-transform:rotate(1.5deg);
                            -o-transform:rotate(1.5deg);",body=?_T("You still earn points when you lose a game, but earn much more if you win one.")}
        ]},
        #panel{class="info-page-content-panel", body=[
            #panel{style="top:852px; left:460px; font-size:24px;",body=?_T("You can claim the prize you desire")},
            #panel{style="top:885px; left:484px; font-size:24px;",body=?_T("with the points in your account")}
        ]},
        #panel{class="info-page-content-panel", body=[
            #panel{style="top:1052px; left:82px; font-size:21px;",body=?_T("If you wish you can purchase a membership with")},
            #panel{style="top:1082px; left:120px; font-size:21px;",body=?_T("prize points and achieve your goal faster!")}
        ]},
        #panel{class="info-page-content-panel", body=[
            #panel{style="top:1287px; left:444px; font-size:18px;",body=?_T("Hundreds of prizes in our catalogue waiting to be claimed")},
            #panel{style="top:1320px; left:522px; font-size:24px;",body=?_T("with the points you have won!")}
        ]}
    ].


event(Any)->
    webutils:event(Any).

