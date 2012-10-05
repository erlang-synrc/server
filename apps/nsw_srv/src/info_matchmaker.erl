%% -*- mode: nitrogen -*-
-module (info_matchmaker).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("gettext.hrl").
-include("loger.hrl").
-include("setup.hrl").

main() ->
    webutils:js_for_main_authorized_game_stats_menu(),
    #template { file=code:priv_dir(nsw_srv)++"/templates/bare.html" }.

title() -> webutils:title(?MODULE).

body() ->
    #template{file=code:priv_dir(nsw_srv)++"/templates/info_page.html"}.

content() -> 
    [
        info_gifts:menu(),
        case site_utils:detect_language() of
            "en" -> en();
            "tr" -> tr()
        end
    ].

tr() ->
    [
        #panel{class="info-page-header-panel", body=[
            #image{image="/images/more_info/info_matchmaker_bg.png", class="info-page-bg"},
            #panel{style="left:302px; top:135px; font-size:50px;",body=?_T("Nasıl İstiyorsan Öyle!")}            
        ]},
        #panel{class="info-page-content-panel", body=[
            #panel{style="left:302px; top:194px; font-size:20px; font-weight:bold; color:#FC6404;",body=?_T("Bilmediğin, tanımadığın oyunculara mecbur değilsin artık.")}
        ]},
        #panel{class="info-page-content-panel", body=[
            #panel{style="left:468px; top:285px; font-size:16px;",body=?_T("Herkese ulaşabileceğin gibi,")},
            #panel{style="left:181px; top:305px; font-size:16px; color:#FC6404;",body=?_T("oynamak istediğin rakiplerin özelliklerini kendin belirleyebilirsin.")},
            #panel{style="left:391px; top:336px; font-size:18px;",body=?_T("<b>İster</b> sadece <b>arkadaşların!</b>")},
            #panel{style="left:287px; top:357px; font-size:18px;",body=?_T("<b>İster</b> sadece <b>ilgi duyduğun gruplar!</b>")},
            #panel{style="left:72px; top:384px; font-size:18px;",body=?_T("<b>İster</b> sadece <b>belirlediğin tek bir kritere uygun oyuncular!</b>")},
            #panel{style="left:368px; top:417px; font-size:28px; color:#FC6404;",body=?_T("Sana kalmış")}
        ]},
        #panel{class="info-page-content-panel", body=[
            #panel{style="left:277px; top:578px; font-size:16px;",body=?_T("Seçimlerin <b>sadece</b> oyuncular ile sınırlı da değil,")},
            #panel{style="left:314px; top:599px; font-size:16px;",body=?_T("<b>Oyun kriterlerini</b> de sen belirliyorsun. ")},
            #panel{style="left:367px; top:624px; font-size:20px;",body=?_T("İster <b>hızlı</b>, ister <b>yavaş</b>. ")},
            #panel{style="left:423px; top:649px; font-size:20px;",body=?_T("İster <b>az puanlı</b> ister <b>çok puanlı</b>. ")},
            #panel{style="left:470px; top:671px; font-size:20px;",body=?_T("İster <b>standart oyun</b>, ister <b>gelişmiş oyun</b>. ")}
        ]},
        #panel{class="info-page-content-panel", body=[
            #panel{style="left:282px; top:846px; font-size:16px;",body=?_T("Oyunlar ise her yerde oynayabildiklerinden değil.")},
            #panel{style="left:256px; top:864px; font-size:16px;",body=?_T("Çünkü tüm oyunların <b>kaynak kodları bize ait</b>.")},
            #panel{style="left:109px; top:894px; font-size:22px; color:#FC6404;",body=?_T("Bu oyunlar ve içerikler başka yerde yok!")}
        ]},
        #panel{class="info-page-content-panel", body=[
            #panel{style="left:318px; top:1305px; font-size:23px;",body=?_T("Eğlence ve heyecan seni bekliyor!")},
            #panel{style="left:399px; top:1340px; font-size:40px; color:#FC6404;",body=?_T("KEŞFET!")}
        ]}
    ].

en() ->
    [
        #panel{class="info-page-header-panel", body=[
            #image{image="/images/more_info/info_matchmaker_bg.png", class="info-page-bg"},
            #panel{style="left:302px; top:120px; font-size:40px;",body=?_T("It is however you want it to be.")}            
        ]},
        #panel{class="info-page-content-panel", body=[
            #panel{style="left:302px; top:184px; font-size:20px; font-weight:bold; color:#FC6404;",body=?_T("You no longer have to play with players that you don't know.")}
        ]},
        #panel{class="info-page-content-panel", body=[
            #panel{style="left:150px; top:285px; font-size:18px;",body=?_T("You can define the type of players that you wish to play with.")},
            #panel{style="left:274px; top:313px; font-size:18px;",body=?_T("<b>Either</b> just <b>your friends</b>,")},
            #panel{style="left:215px; top:340px; font-size:18px;",body=?_T("<b>or</b> just <b>the groups you are interested in</b>.")},
            #panel{style="left:163px; top:377px; font-size:18px;",body=?_T("<b>Or</b> just the players <b>who match the criteria you define!</b>")},
            #panel{style="left:283px; top:415px; font-size:28px; color:#FC6404;",body=?_T("UP TO YOU!")}
        ]},
        #panel{class="info-page-content-panel", body=[
            #panel{style="left:435px; top:578px; font-size:18px;",body=?_T("Your <b>choices</b> are not limited with the players!")},
            #panel{style="left:482px; top:602px; font-size:18px;",body=?_T("You define <b>the rules of the game</b>!")},
            #panel{style="left:480px; top:628px; font-size:20px;",body=?_T("Either a <b>fast</b> gameplay or <b>slow</b>…")},
            #panel{style="left:402px; top:658px; font-size:20px;",body=?_T("You define for <b>how much points</b> you want to play.")},
            #panel{style="left:420px; top:687px; font-size:20px;",body=?_T("Either play a <b>standart game</b>… or <b>customize it</b>!")}
        ]},
        #panel{class="info-page-content-panel", body=[
            #panel{style="left:100px; top:809px; font-size:18px;",body=?_T("What you have here, it is not like any other game you play.")},
            #panel{style="left:248px; top:836px; font-size:18px;",body=?_T("in a random web site!")},
            #panel{style="left:304px; top:866px; font-size:22px;",body=?_T("Why?")},
            #panel{style="left:150px; top:900px; font-size:18px;",body=?_T("The source codes for every game belongs to us.")},
            #panel{style="left:90px; top:927px; font-size:18px; color:#FC6404;",body=?_T("You can not find the same games and contents anywhere else!")}
        ]},
        #panel{class="info-page-content-panel", body=[
            #panel{style="left:330px; top:1303px; font-size:23px;
                            transform:rotate(-1deg);
                            -ms-transform:rotate(-1deg);
                            -moz-transform:rotate(-1deg);
                            -webkit-transform:rotate(-1deg);
                            -o-transform:rotate(-1deg);",body=?_T("Enjoy the thrill of our games")},
            #panel{style="left:356px; top:1335px; font-size:23px;
                            transform:rotate(-1deg);
                            -ms-transform:rotate(-1deg);
                            -moz-transform:rotate(-1deg);
                            -webkit-transform:rotate(-1deg);
                            -o-transform:rotate(-1deg);",body=?_T("At the tip of your finger.")},
            #panel{style="left:362px; top:1370px; font-size:40px; color:#FC6404;
                            transform:rotate(-1deg);
                            -ms-transform:rotate(-1deg);
                            -moz-transform:rotate(-1deg);
                            -webkit-transform:rotate(-1deg);
                            -o-transform:rotate(-1deg);",body=?_T("DISCOVER!")}
        ]}
    ].

event(Any)->
    webutils:event(Any).
