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


event(Any)->
    webutils:event(Any).
