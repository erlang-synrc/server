%% -*- mode: nitrogen -*-
-module(rules_tavla).
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
        rules_okey:menu(),
        case site_utils:detect_language() of
            "en" -> tr();
            "tr" -> tr()
        end
    ].

tr() ->
    [
        #panel{class="info-page-header-panel", body=[
            #image{image="/images/rules/rules_tavla_bg.png", class="info-page-bg"}
        ]}
%            #panel{style="left:242px; top:103px; font-size:50px;",body=?_T("Oyun,")},
%            #panel{style="left:242px; top:150px; font-size:70px;",body=?_T("Yarışma ve Ödül…")}
%        ]},
%        #panel{class="info-page-content-panel", body=[
%            #panel{style="left:250px; top:229px; font-size:20px;",body=?_T("Oyun keyfi ve rekabet <b>tavan</b> yapacak.")},
%            #panel{style="left:250px; top:252px; font-size:20px;",body=?_T("Dandik oyun puanları ile <b>iyi oyuncu</b> havalarına <b>SON</b>!")}
%        ]},
%        #panel{class="info-page-content-panel", body=[
%            #panel{style="left:281px; top:361px; font-size:18px;",body=?_T("Oyuncunun iyisi turnuvada belli olur. ")},
%            #panel{style="left:167px; top:390px; font-size:30px;",body=?_T("“<b>Er Meydanı Turnuva</b>”")}
%        ]},
%        #panel{class="info-page-content-panel", body=[
%            #panel{style="left:345px; top:644px; font-size:18px;",body=?_T("Turnuva kazan, Ödül kazan…")},
%            #panel{style="left:386px; top:667px; font-size:32px;",body=?_T("<b>Ödül ne? mi?</b>")},
%            #panel{style="left:426px; top:708px; font-size:32px;",body=?_T("Sana kalmış..! ")},
%            #panel{style="left:314px; top:748px; font-size:18px;",body=?_T("Oyuncular turnuvaları kendileri yaratıp, <b>ödülü</b> kendileri belirler… ")}
%        ]},
%        #panel{class="info-page-content-panel", body=[
%            #panel{style="left:274px; top:878px; font-size:18px;",body=?_T("İster ödülünü beğendiğin bir turnuvaya katıl,")},
%            #panel{style="left:104px; top:900px; font-size:18px;",body=?_T("ister katalogdan <b>istediğin ödülü seç</b> ve <b>kendi turnuvanı</b> yarat.")}
%        ]},
%        #panel{class="info-page-content-panel", body=[
%            #panel{style="left:290px; top:1018px; font-size:22px;",body=?_T("<b>Turnuvanı Yarat!</b>")},
%            #panel{style="left:344px; top:1040px; font-size:24px;",body=?_T("Duyur!")},
%            #panel{style="left:359px; top:1063px; font-size:26px;",body=?_T("<b>Tanıt!</b>")},
%            #panel{style="left:369px; top:1091px; font-size:22px;",body=?_T("Oyuncunu topla!")},
%            #panel{style="left:398px; top:1117px; font-size:24px;",body=?_T("<b>Yarış!</b>")},
%            #panel{style="left:436px; top:1145px; font-size:24px;",body=?_T("Kazan!")}
%        ]},
%        #panel{class="info-page-content-panel", body=[
%            #panel{style="left:373px; top:1386px; font-size:20px;",body=?_T("Ödülü al. Yürü git. Yenisine bak.")},
%            #panel{style="left:475px; top:1411px; font-size:20px;",body=?_T("<b>Bu kadar...</b>")}
%        ]}
    ].

event(Any)->
    webutils:event(Any).
