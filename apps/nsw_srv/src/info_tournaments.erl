%% -*- mode: nitrogen -*-
-module (info_tournaments).
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
            #image{image="/images/more_info/info_tournaments_bg.png", class="info-page-bg"},
            #panel{style="left:242px; top:103px; font-size:50px;",body=("Oyun,")},
            #panel{style="left:242px; top:150px; font-size:70px;",body=("Yarışma ve Ödül…")}
        ]},
        #panel{class="info-page-content-panel", body=[
            #panel{style="left:250px; top:229px; font-size:20px;",body=("Oyun keyfi ve rekabet <b>tavan</b> yapacak.")},
            #panel{style="left:250px; top:252px; font-size:20px;",body=("Dandik oyun puanları ile <b>iyi oyuncu</b> havalarına <b>SON</b>!")}
        ]},
        #panel{class="info-page-content-panel", body=[
            #panel{style="left:281px; top:361px; font-size:18px;",body=("Oyuncunun iyisi turnuvada belli olur. ")},
            #panel{style="left:167px; top:390px; font-size:30px;",body=("“<b>Er Meydanı Turnuva</b>”")}
        ]},
        #panel{class="info-page-content-panel", body=[
            #panel{style="left:345px; top:644px; font-size:18px;",body=("Turnuva kazan, Ödül kazan…")},
            #panel{style="left:386px; top:667px; font-size:32px;",body=("<b>Ödül ne? mi?</b>")},
            #panel{style="left:426px; top:708px; font-size:32px;",body=("Sana kalmış..! ")},
            #panel{style="left:314px; top:748px; font-size:18px;",body=("Oyuncular turnuvaları kendileri yaratıp, <b>ödülü</b> kendileri belirler… ")}
        ]},
        #panel{class="info-page-content-panel", body=[
            #panel{style="left:274px; top:878px; font-size:18px;",body=("İster ödülünü beğendiğin bir turnuvaya katıl,")},
            #panel{style="left:104px; top:900px; font-size:18px;",body=("ister katalogdan <b>istediğin ödülü seç</b> ve <b>kendi turnuvanı</b> yarat.")}
        ]},
        #panel{class="info-page-content-panel", body=[
            #panel{style="left:290px; top:1018px; font-size:22px;",body=("<b>Turnuvanı Yarat!</b>")},
            #panel{style="left:344px; top:1040px; font-size:24px;",body=("Duyur!")},
            #panel{style="left:359px; top:1063px; font-size:26px;",body=("<b>Tanıt!</b>")},
            #panel{style="left:369px; top:1091px; font-size:22px;",body=("Oyuncunu topla!")},
            #panel{style="left:398px; top:1117px; font-size:24px;",body=("<b>Yarış!</b>")},
            #panel{style="left:436px; top:1145px; font-size:24px;",body=("Kazan!")}
        ]},
        #panel{class="info-page-content-panel", body=[
            #panel{style="left:373px; top:1386px; font-size:20px;",body=("Ödülü al. Yürü git. Yenisine bak.")},
            #panel{style="left:475px; top:1411px; font-size:20px;",body=("<b>Bu kadar...</b>")}
        ]}
    ].

en() ->
    [
        #panel{class="info-page-header-panel", body=[
            #image{image="/images/more_info/info_tournaments_bg.png", class="info-page-bg"},
            #panel{style="left:242px; top:95px; font-size:57px;",body=("Games,")},
            #panel{style="left:244px; top:150px; font-size:50px;",body=("Tourneys and Prizes!")}
        ]},
        #panel{class="info-page-content-panel", body=[
            #panel{style="left:250px; top:222px; font-size:20px;",body=("A top level of competition will be awaiting for you!")},
            #panel{style="left:250px; top:252px; font-size:20px;",body=("Prove your skills among many competitors!")}
        ]},
        #panel{class="info-page-content-panel", body=[
            #panel{style="left:180px; top:381px; font-size:23px;",body=("So. You say you have skills?")},
            #panel{style="left:190px; top:415px; font-size:30px;",body=("<b>Join us in the Arena</b>")}
        ]},
        #panel{class="info-page-content-panel", body=[
            #panel{style="left:440px; top:624px; font-size:22px;",body=("Win the tourney, claim your prize")},
            #panel{style="left:476px; top:652px; font-size:32px;",body=("<b>What's the prize?</b>")},
            #panel{style="left:478px; top:693px; font-size:32px;",body=("Well… Up to you!")},
            #panel{style="left:392px; top:740px; font-size:18px;",body=("Players can create their own tournaments and set the")},
            #panel{style="left:536px; top:768px; font-size:18px;",body=("prize they desire.")}
        ]},
        #panel{class="info-page-content-panel", body=[
            #panel{style="left:230px; top:860px; font-size:18px;",body=("Either join the tournament, or select the")},
            #panel{style="left:233px; top:885px; font-size:18px;",body=("prize you want from out catalogue and")},
            #panel{style="left:318px; top:910px; font-size:18px;",body=("create your own!")}
        ]},
        #panel{class="info-page-content-panel", body=[
            #panel{style="left:504px; top:1040px; font-size:22px;",body=("<b>Create your tournament!</b>")},
            #panel{style="left:554px; top:1069px; font-size:24px;",body=("Announce it!")},
            #panel{style="left:552px; top:1103px; font-size:26px;",body=("<b>Advertise it!</b>")},
            #panel{style="left:506px; top:1138px; font-size:22px;",body=("Gather the competitors!")},
            #panel{style="left:590px; top:1166px; font-size:24px;",body=("<b>Play!</b>")},
            #panel{style="left:592px; top:1197px; font-size:24px;",body=("Win!")}
        ]},
        #panel{class="info-page-content-panel", body=[
            #panel{style="left:370px; top:1386px; font-size:20px;",body=("Enjoy your prize, look for a new tourney…")},
            #panel{style="left:490px; top:1414px; font-size:20px;",body=("<b>That's all!</b>")}
        ]}
    ].

event(Any)->
    webutils:event(Any).
