%% -*- mode: nitrogen -*-
-module (info_social).
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
            #image{image="/images/more_info/info_social_bg.png", class="info-page-bg"},
            #panel{style="left:284px; top:104px; font-size:75px;",body=?_T("SOSYALLEŞMEK")},
            #panel{style="left:284px; top:192px; font-size:22px; color:#FC6404;",body=?_T("Burası sadece bir oyun ortamı değil.")},
            #panel{style="left:284px; top:218px; font-size:22px; color:#FC6404;",body=?_T("İstediğin her şeyi paylaşabileceğin bir platform.")}
        ]},
        #panel{class="info-page-content-panel", body=[
            #panel{style="left:410px; top:354px; font-size:16px;",body=?_T("<b>“Nasıl görünmek istiyorsan öyle ol! ”</b> ya da")},
            #panel{style="left:423px; top:372px; font-size:16px; color:#FC6404;",body=?_T("“Nasılsan öyle görün”!")},
            #panel{style="left:448px; top:392px; font-size:16px;",body=?_T("Kendini ifade etmek için ne istiyorsan yap!")},
            #panel{style="left:437px; top:421px; font-size:18px; color:#FC6404;",body=?_T("Resimlerin")},
            #panel{style="left:512px; top:439px; font-size:26px; color:#FC6404;",body=?_T("Müziğin")},
            #panel{style="left:470px; top:466px; font-size:22px; color:#FC6404;",body=?_T("Felsefen")},
            #panel{style="left:542px; top:489px; font-size:16px;",body=?_T("için, dışın…")}
        ]},
        #panel{class="info-page-content-panel", body=[
            #panel{style="left:207px; top:607px; font-size:16px;",body=?_T("Bunları kiminle paylaşmak istiyorsan sen belirle.")},
            #panel{style="left:322px; top:629px; font-size:24px; color:#FC6404;",body=?_T("Kendi dünyanı kur!")}
        ]},
        #panel{class="info-page-content-panel", body=[
            #panel{style="left:452px; top:819px; font-size:16px;",body=?_T("Sohbet et, tartış, takıl, görüşlerini ilet, yada izle...")},
            #panel{style="left:477px; top:837px; font-size:16px; color:#FC6404;",body=?_T("Yorum yap, beğen, beğenil yada beğenilmeyip eleştiril...")},
            #panel{style="left:453px; top:854px; font-size:16px;",body=?_T("Nasıl istersen öyle!")},
            #panel{style="left:479px; top:874px; font-size:16px; color:#FC6404;",body=?_T("Ne verirsen onu alacağını unutma!")},
            #panel{style="left:511px; top:916px; font-size:16px;",body=?_T("<b>Eski</b> arkadaşların – <b>Yeni</b> arkadaşların")},
            #panel{style="left:560px; top:933px; font-size:20px;",body=?_T("<b>Farklı</b> gruplar, <b>Farklı</b> insanlar…")}
        ]},
        #panel{class="info-page-content-panel", body=[
            #panel{style="left:541px; top:1096px; font-size:20px;",body=?_T("İfade Özgürlüğü!")},
            #panel{style="left:403px; top:1124px; font-size:16px; color:#FC6404;",body=?_T("Yasaları zorlarsan birileri engeller elbet...")}
        ]},
        #panel{class="info-page-content-panel", body=[
            #panel{style="left:345px; top:1291px; font-size:18px;",body=?_T("Oyun aleminde bulduğuna sıkışmak zorunda değilsin artık…")},
            #panel{style="left:373px; top:1314px; font-size:24px; color:#FC6404;",body=?_T("Kendi dünyanı kendin yarat…")}
        ]}
    ].

en() ->
    [
        #panel{class="info-page-header-panel", body=[
            #image{image="/images/more_info/info_social_bg.png", class="info-page-bg"},
            #panel{style="left:284px; top:104px; font-size:75px;",body=?_T("Socialize")},
            #panel{style="left:285px; top:192px; font-size:20px; color:#FC6404;",body=?_T("This is not like any other place where you can just play a game.")},
            #panel{style="left:286px; top:218px; font-size:20px; color:#FC6404;",body=?_T("It's a platform where you can share anything you like!")}
        ]},
        #panel{class="info-page-content-panel", body=[
            #panel{style="left:543px; top:354px; font-size:16px;",body=?_T("Appear as you are or be as you appear.")},
            #panel{style="left:520px; top:374px; font-size:16px;",body=?_T("Whatever you need to describe yourself, do so…")},
            #panel{style="left:631px; top:396px; font-size:16px; color:#FC6404;",body=?_T("Your music,")},
            #panel{style="left:631px; top:417px; font-size:18px; color:#FC6404;",body=?_T("your ideas,")},
            #panel{style="left:604px; top:436px; font-size:26px; color:#FC6404;",body=?_T("your photos!")},
            #panel{style="left:602px; top:466px; font-size:22px;",body=?_T("Inside and out!")},
            #panel{style="left:592px; top:493px; font-size:16px; color:#FC6404;",body=?_T("You are welcome here!")}
        ]},
        #panel{class="info-page-content-panel", body=[
            #panel{style="left:80px; top:607px; font-size:18px;",body=?_T("Create your world and only share with the people YOU want…")},
            #panel{style="left:186px; top:636px; font-size:24px; color:#FC6404;",body=?_T("After all it's your world!")}
        ]},
        #panel{class="info-page-content-panel", body=[
            #panel{style="left:452px; top:790px; font-size:18px;",body=?_T("Conversate, debate, hang out, comment or just spectate…")},
            #panel{style="left:567px; top:814px; font-size:18px; color:#FC6404;",body=?_T("Like, be liked or be disliked.")},
            #panel{style="left:566px; top:837px; font-size:18px;",body=?_T("It goes however you want it.")},
            #panel{style="left:610px; top:862px; font-size:20px; color:#FC6404;",body=?_T("Keep in mind!")},
            #panel{style="left:536px; top:888px; font-size:18px;",body=?_T("What <b>goes</b> around – <b>comes</b> around!")},
            #panel{style="left:541px; top:933px; font-size:20px;",body=?_T("Your old mates, new buddies…")},
            #panel{style="left:534px; top:958px; font-size:20px;",body=?_T("<b>Different</b> groups, <b>different</b> people.")}
        ]},
        #panel{class="info-page-content-panel", body=[
            #panel{style="left:300px; top:1066px; font-size:22px;",body=?_T("Freedom of speech!")},
            #panel{style="left:180px; top:1112px; font-size:20px; color:#FC6404;",body=?_T("Don't break the rules though, you will be prevented")},
            #panel{style="left:355px; top:1140px; font-size:20px; color:#FC6404;",body=?_T("if you do.")}
        ]},
        #panel{class="info-page-content-panel", body=[
            #panel{style="left:345px; top:1260px; font-size:20px;",body=?_T("You are not restricted to what you find in the game enviroment")},
            #panel{style="left:568px; top:1286px; font-size:20px;",body=?_T("anymore…")},
            #panel{style="left:470px; top:1324px; font-size:24px; color:#FC6404;",body=?_T("Create YOUR OWN world.")}
        ]}
    ].

event(Any)->
    webutils:event(Any).
