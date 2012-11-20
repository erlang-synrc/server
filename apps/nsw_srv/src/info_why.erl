%% -*- mode: nitrogen -*-
-module (info_why).
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
            #image{image="/images/more_info/info_membership_bg.png", class="info-page-bg"},
            #panel{style="left:285px; top:120px; font-size:40px;",body=("Neden Üyelik Paketi")},
            #panel{style="left:285px; top:158px; font-size:60px;",body=("Satın Almalıyım?")},
            #panel{style="left:285px; top:222px; font-size:30px; color:#FC6404;",body=("Niçin Oyun Oynuyorsun?")}
        ]},
        #panel{class="info-page-content-panel", body=[
            #panel{style="left:223px; top:302px; font-size:17px;",body=("<b>• Eğlenerek</b> vakit geçirmek istiyorum,")},
            #panel{style="left:238px; top:324px; font-size:17px;",body=("<b>• </b>Oynayarak <b>yarışmaktan</b> keyif alıyorum,")},
            #panel{style="left:250px; top:343px; font-size:17px;",body=("<b>• Gerçekten oynamaktan</b> zevk alıyorum,")},

            #panel{style="left:269px; top:364px; font-size:17px;",body=("<b>• </b>Yeni arkadaşlıklar kurup, <b>kafama göre</b> takılmak istiyorum,")},
            #panel{style="left:286px; top:386px; font-size:17px;",body=("<b>• </b>Kendimi <b>istediğim</b> gibi ifade edecek ortam arıyorum,")},
            #panel{style="left:304px; top:406px; font-size:17px;",body=("<b>• Muhabbet etmek</b> istiyorum,")},

            #panel{style="left:331px; top:425px; font-size:17px; text-indent:-11px;",body="<nobr>"++("<b>• </b>Harcadığım zamanın ve uğraşın bir şekilde bana <b>geri dönmesini</b>")++"</nobr><br />"++("istiyorum,")},
            #panel{style="left:346px; top:467px; font-size:17px; text-indent:-11px;",body="<nobr>"++("<b>• </b>Sanal oyun ortamında <b>mouse-parmaktan</b> ibaret sanal oyuncu")++"</nobr><br />"++("olmaktan sıkıldım.")},
            #panel{style="left:365px; top:507px; font-size:17px; text-indent:-11px;",body="<nobr>"++("<b>• </b>Oyunlarda ulaşmak için çaba gösterebileceğim <b>farklı hedefler</b>")++"</nobr><br />"++("olsun istiyorum,")},

            #panel{style="left:291px; top:575px; font-size:40px; color:#FC6404;",body=("Daha başka şeyler arıyorum!")}
        ]},
        #panel{class="info-page-content-panel", body=[
            #panel{style="left:91px; top:667px; font-size:17px; text-indent:-11px;",body="<nobr>"++("<b>• </b>Sanal ortamın <b>görünmez duvarları</b> arkasına sığınıp, önüne gelene")++"</nobr><br />"++("bulaşanlardan ve <b>yarım kalan</b> oyunlardan sıkıldım,")},
            #panel{style="left:97px; top:707px; font-size:17px;",body=("<b>• </b>Oyun için harcadığım vaktin <b>boşa</b> gitmesini istemiyorum,")},
            #panel{style="left:111px; top:727px; font-size:17px;",body=("<b>• </b>Saygısız saldırganlar karşısında <b>çaresiz</b> kalmak istemiyorum,")},

            #panel{style="left:130px; top:748px; font-size:17px;",body=("<b>• </b>Hoşlaşmadıklarım ile bir daha <b>karşılaşmak</b> istemiyorum,")},
            #panel{style="left:155px; top:769px; font-size:17px; text-indent:-11px;",body="<nobr>"++("<b>• </b>Avatar yükleme, direk mesaj, nick seçimi, arkadaş listesi, tüm")++"</nobr><br /><nobr>"++("odalara girebilme ayrıcalığı, ne işe yaradığı kendinden menkul")++"</nobr><br /><nobr>"++("fazla puan kazanma gibi her yerde ancak verebildikleri <b>“paran")++"</nobr><br /><nobr>"++("ile caka satmak”</b> babından özelliklere “bilmem ne üyeliği”")++"</nobr><br />"++("adı altında haybeden para ödemek istemiyorum,")},
            #panel{style="left:172px; top:872px; font-size:17px; text-indent:-11px;",body="<nobr>"++("<b>• </b>Benim <b>rahatım</b> ve <b>beklentilerim</b> için çaba gösterildiğini")++"</nobr><br />"++("görmek istiyorum,")},

            #panel{style="left:189px; top:912px; font-size:17px; text-indent:-11px;",body="<nobr>"++("<b>• </b>Kısacası beni “<b>Yolunacak tavuk gibi</b>” görmelerinden")++"</nobr><br />"++("hoşlanmıyorum,")},

            #panel{style="left:337px; top:1021px; font-size:50px; color:#FC6404;",body=("Diyorsan…")},
            #panel{style="left:310px; top:1077px; font-size:36px;",body=("KAKARANET üyesi olmalısın!")}
        ]},
        #panel{class="info-page-content-panel", body=[
            #panel{style="left:179px; top:1207px; font-size:17px; text-indent:-11px;",body="<nobr>"++("<b>• Her şeyi</b>, oyuncunun kendini <b>rahat</b> hissetmesi, bulunduğu")++"</nobr><br /><nobr>"++("ortamdan <b>keyif alarak</b> kendine yeni arkadaşlar, uğraşlar")++"</nobr><br /><nobr>"++("bulup, <b>zevkle</b> vakit geçirebileceği ortamı yaratabilmek için")++"</nobr><br />"++("tasarladık.")},
            #panel{style="left:131px; top:1290px; font-size:17px;",body=("<b>• </b>Henüz ülkemizde bu alanda kullanılmayan <b>teknolojileri</b> kullandık.")},
            #panel{style="left:81px; top:1313px; font-size:17px; text-indent:-11px;",body="<nobr>"++("<b>• </b>İçerideki her özelliği <b>kullanıcının ihtiyaçlarına</b> göre ve arayıp ta")++"</nobr><br /><nobr>"++("<b>bulamadıklarını</b> düşünerek sadece kakaranet’e <b>özel</b> uygulamalar")++"</nobr><br />"++("hazırladık.")}
        ]},
        #panel{class="info-page-content-panel", body=[
            #panel{style="left:235px; top:1432px; font-size:17px;",body=("<b>Çok kafa yorduk</b>, <b>çok uğraştık</b> ve bu çabamız <b>artarak</b> sürecek.")},
            #panel{style="left:282px; top:1453px; font-size:17px;",body=("Siz <b>Arkadaş – Oyundaş</b> üyelerimizin desteği ile...")},
            #panel{style="left:327px; top:1681px; font-size:72px; color:#FC6404;",body=("İyi keyifler.")}
        ]}
    ].

en() ->
    [
        #panel{class="info-page-header-panel", body=[
            #image{image="/images/more_info/info_membership_bg.png", class="info-page-bg"},
            #panel{style="left:285px; top:120px; font-size:40px;",body=("Hmm, but why should I purchase")},
            #panel{style="left:285px; top:158px; font-size:60px;",body=("a membership pack?")},
            #panel{style="left:285px; top:222px; font-size:30px; color:#FC6404;",body=("What are you playing the game for?")}
        ]},
        #panel{class="info-page-content-panel", body=[
            #panel{style="left:436px; top:300px; font-size:18px;",body=("I want to <b>have fun</b> and spend some time.")},
            #panel{style="left:460px; top:324px; font-size:18px;",body=("I enjoy playing against <b>competition</b>.")},
            #panel{style="left:498px; top:348px; font-size:18px;",body=("I <b>REALLY ENJOY</b> playing.")},

            #panel{style="left:446px; top:372px; font-size:18px;",body=("I want to hang out and <b>meet new people</b>.")},
            #panel{style="left:380px; top:396px; font-size:18px;",body=("I need a new enviroment where I can better <b>express myself</b>.")},
            #panel{style="left:520px; top:420px; font-size:18px;",body=("I want to <b>conversate</b>.")},

            #panel{style="left:408px; top:444px; font-size:18px; text-indent:-11px;",body=("I want my time and efforts to <b>have a return</b> in some way.")},
            #panel{style="left:421px; top:468px; font-size:18px; text-indent:-11px;",body=("I'm bored of just clicking around when playing games.")},
            #panel{style="left:480px; top:492px; font-size:18px; text-indent:-11px;",body=("I want to <b>explore</b> new achievments.")},

            #panel{style="left:388px; top:555px; font-size:40px; color:#FC6404;",body=("I'm looking for much more!")}
        ]},
        #panel{class="info-page-content-panel", body=[
            #panel{style="left:100px; top:688px; font-size:18px;",body=("I'm tired of people who hide behind the invisible walls of the internet")},
            #panel{style="left:236px; top:712px; font-size:18px;",body=("and mess with anyone as they like.")},
            #panel{style="left:254px; top:736px; font-size:18px;",body=("I don't want unfinished games.")},

            #panel{style="left:176px; top:760px; font-size:18px;",body=("I don't want to be harrased and do nothing about it.")},
            #panel{style="left:92px; top:784px; font-size:18px;",body=("I don't want to pay for features like <b>uploading my own profile</b> or entering")},
            #panel{style="left:170px; top:808px; font-size:18px;",body=("<b>all available rooms</b> or even to <b>choose my own name</b>…")},

            #panel{style="left:140px; top:832px; font-size:18px;",body=("I want to see that I get a <b>response for what I expect</b> and also.")},
            #panel{style="left:316px; top:856px; font-size:18px;",body=("<b>for my comfort</b>.")},
            #panel{style="left:252px; top:880px; font-size:18px;",body=("I'm not a chicken to be plucked!")},

            #panel{style="left:348px; top:985px; font-size:50px; color:#FC6404;
                            transform:rotate(5deg);
                            -ms-transform:rotate(5deg);
                            -moz-transform:rotate(5deg);
                            -webkit-transform:rotate(5deg);
                            -o-transform:rotate(5deg);",body=("if that's what you think,")},
            #panel{style="left:310px; top:1041px; font-size:36px;
                            transform:rotate(5deg);
                            -ms-transform:rotate(5deg);
                            -moz-transform:rotate(5deg);
                            -webkit-transform:rotate(5deg);
                            -o-transform:rotate(5deg);",body=("you should register to KAKARANET!")}
        ]},
        #panel{class="info-page-content-panel", body=[
            #panel{style="left:100px; top:1212px; font-size:18px; text-align:center;",body=("We have designed everything so our <b>players can enjoy</b> their envi-<br>roment, <b>make new friends</b> and basically <b>have a nice time</b>.")},
            #panel{style="left:97px; top:1290px; font-size:18px; text-align:center;",body=("We have implemented technologies, <b>not yet existing in our country</b>.")},
            #panel{style="left:100px; top:1313px; font-size:18px; text-align:center;",body=("Kakaranet users will benefit from the <b>customized user preferences</b><br>that <b>do not exist anywhere else</b>.")}
        ]},
        #panel{class="info-page-content-panel", body=[
            #panel{style="left:155px; top:1432px; font-size:18px; text-align:center;",body=("<b>We worked hard</b>, and we are still <b>working really hard</b>.<br>With <b>your help</b>, our efforts will increasingly continue to <b>improve this community</b>!")},
            #panel{style="left:180px; top:1681px; font-size:64px; color:#FC6404;",body=("Welcome, and enjoy!")}
        ]}
    ].


event(Any)->
    webutils:event(Any).
