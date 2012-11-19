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
        #link{text="ÜYE OL!", url=?_U("/login/register"), class="rules-page-menu-elements rules-page-join-link"}
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
        #link{text="SIGNUP!", url=?_U("/login/register"), class="rules-page-menu-elements rules-page-join-link"}
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
        ]},
        #panel{class="info-page-content-panel", body=[
            #panel{style="top:150px; left:400px; font-size:28px; color:#FC6404;",body="OYUN SETİ"},
            #panel{style="top:190px; left:355px; font-size:18px;",body="Okey oyunu 2-4 kişi ile oynanır."},
            #panel{style="top:220px; left:400px; font-size:18px;",body="Herbir set <span style='color:#ff5e21'>1’ den 13’ e kadar</span> numaralı olmak üzere;"},
            #panel{style="top:250px; left:400px; font-size:18px;",body="<span style='color:#fe0002'>kırmızı</span>, <span style='color:#000'>siyah</span>, <span style='color:#00b1eb'>mavi</span> ve <span style='color:#2eb77f'>sarı</span> renklerde 4 renk seti vardır."},
            #panel{style="top:280px; left:445px; font-size:18px;",body="2 tane “Sahte Okey” ile birlikte toplam 106 taş vardır."}
        ]},
        #panel{class="info-page-content-panel", body=[
            #panel{style="left:180px; top:450px; font-size:28px; color:#FC6404;",body="OYUNA BAŞLAMA"},
            #panel{style="left:150px; top:490px; font-size:18px;",body="Taşlar karıştırılıp her bir oyuncuya 14 taş verilir."},
            #panel{style="left:100px; top:520px; font-size:18px;",body="İlk oynayacak oyuncuya fazladan 1 taş verilir."}
        ]},
        #panel{class="info-page-content-panel", body=[
            #panel{style="left:410px; top:645px; font-size:28px; color:#FC6404;",body="OYUN DÖNGÜSÜ"},
            #panel{style="left:420px; top:680px; font-size:18px;",body="Oyun saatin tersi yönünde soldan sağa doğru(Shoarma) oynanır."},
            #panel{style="left:413px; top:705px; font-size:18px;",body="Sırası gelen oyuncu soldaki oyuncunun attığı taşı yerden alabilir"},
            #panel{style="left:443px; top:730px; font-size:18px;",body="yada ortadaki desteden taş çekebilir."},
            #panel{style="left:397px; top:755px; font-size:18px;",body="Daha sonra ıstakasındaki istediği herhangibir taşı sağındaki taş"},
            #panel{style="left:800px; top:780px; font-size:18px;",body="atma yerine atar."}
        ]},
        #panel{class="info-page-content-panel", body=[
            #panel{style="left:330px; top:845px; font-size:28px; color:#FC6404;",body="AMAÇ"},
            #panel{style="left:125px; top:885px; font-size:18px;",body="Oyunu bitirerek açma puanını kazanmak, böylece oyun sonunda"},
            #panel{style="left:200px; top:915px; font-size:18px;",body="en yüksek puana ulaşarak oyunu kazanmaktır."}
        ]},
        #panel{class="info-page-content-panel", body=[
            #panel{style="left:390px; top:1020px; font-size:28px; color:#FC6404;",body="AÇAR TAŞLAR"},
            #panel{style="left:370px; top:1060px; font-size:17px;",body="aynı renkten sıralı hale gelmesi, (örn. Yeşil 1, 2, 3, 4, sarı 7, 8, 9, <br>kırmızı 10, 11, 12 gibi) 1 sayılı taş seride 13 ten sonrada kullanılabilir <br><span style='margin-left:-5px;'>(</span>11, 12, 13, 1 gibi)"},
            #panel{style="left:370px; top:1125px; font-size:17px;",body="aynı sayıların farklı renklerden bir seri uluşturması <br><span style='margin-left:-5px;'>(</span>sarı- yeşil-kırmızı 2'li gibi)"},
            #panel{style="left:370px; top:1170px; font-size:17px;",body="eldeki taşların tamamının (14 tane) açarlı gruplar oluşturması <br>oyun bitme şartıdır."},
            #panel{style="left:380px; top:1216px; font-size:17px;",body="Bu gruplar 4 - 4  - 3 - 3 lü olabileceği gibi 5 - 3 - 3 - 3 lü de veya 14 taşı açar hale gelen 5 - 5 - 4 te olabilir. Seri taşlar arasında boşluk olmamalıdır."},
            #panel{style="left:393px; top:1262px; font-size:17px;",body="Eldeki taşların çekilen ile beraber 14 tanesinin açar konuma gelmesi <br>ile oyuncu 15. taşını (ıskarta) masa ortasındaki taş çekme yerine <br>sürükleyip bırakarak oyunu bitirir."}
        ]},
        #panel{class="info-page-content-panel", body=[
            #panel{style="left:200px; top:1380px; font-size:28px; color:#FC6404;",body="ÇİFT AÇMAK"},
            #panel{style="left:80px; top:1420px; font-size:18px;",body="Aynı renk ve sayıdan olmak üzere 7 çift taş da açar seri oluşturur."},
            #panel{style="left:130px; top:1445px; font-size:18px;",body="(sahte okey çifti de dahildir). Buna “ÇİFT AÇMAK” denir."},
            #panel{style="left:50px; top:1470px; font-size:18px;",body="Çift olan taşlar yanyana dururken farklı çiftleri en az bir boşluk bırakarak dizmek gerekir."},
            #panel{style="left:150px; top:1495px; font-size:18px;",body="15. taş çekilen taşların üzerine konarak oyun bitti bilgisi verilir."}
        ]},
        #panel{class="info-page-content-panel", body=[
            #panel{style="left:430px; top:1610px; font-size:28px; color:#FC6404;",body="EL BİTİRME"},
            #panel{style="left:340px; top:1650px; font-size:18px;",body="14 taşın tümü sıralı yada renk serileri şeklinde serilerin arasında "},
            #panel{style="left:370px; top:1675px; font-size:18px;",body="bir boşluk bırakarak dizildikten sonra 15. "},
            #panel{style="left:430px; top:1700px; font-size:18px;",body="Taş ortadaki taş destesi etrafındaki işaretlenmiş alana "},
            #panel{style="left:610px; top:1725px; font-size:18px;",body="bırakılarak el bitirilir."}
        ]},
        #panel{class="info-page-content-panel", body=[
            #panel{style="left:200px; top:1770px; font-size:28px; color:#FC6404;",body="GÖSTERME TAŞI"},
            #panel{style="left:100px; top:1810px; font-size:18px;",body="Taşlar dağıtıldıktan sonra sağ alt köşede “gösterMe” bölümüne açılan taş."},
            #panel{style="left:105px; top:1835px; font-size:18px;",body="Elinde göstermesi olan gösterme taşının üzerini çift tıklayarak oyundaki"},
            #panel{style="left:120px; top:1860px; font-size:18px;",body="(varsa) gösterme puanını kazanır. "},
            #panel{style="left:200px; top:1885px; font-size:18px;",body="Gösterme ilk el taş çekilmeden yapılmalıdır."},
            #panel{style="left:220px; top:1910px; font-size:18px;",body="Taş çekildikten sonra gösterme yapılamaz."}
        ]},
        #panel{class="info-page-content-panel", body=[
            #panel{style="left:480px; top:1975px; font-size:28px; color:#FC6404;",body="OKEY TAŞI"},
            #panel{style="left:330px; top:2015px; font-size:18px;",body="Göstermenin bir sayı yukarısı (aynı renk olmak şartı ile) okey taşıdır. Bu taş joker niteliğinde olup elde eksik olan istenilen renkteki sayı yerine kullanılır."},
            #panel{style="left:380px; top:2063px; font-size:18px;",body="Her el iki adet taş okey taşı yerine geçer. (okey sayılan taşın yerine kullanılmak üzere 2 tane sahte okey bulunur)."}
        ]},
        #panel{class="info-page-content-panel", body=[
            #panel{style="left:200px; top:2200px; font-size:28px; color:#FC6404;",body="SAHTE OKEY TAŞI"},
            #panel{style="left:110px; top:2240px; font-size:18px;",body="“Sahte okey “ taşının sabit bir rengi ve sayısı yoktur."},
            #panel{style="left:150px; top:2265px; font-size:18px;",body="Her el “Okey taşı’” nın rengi ve sayısı yerine kullanılır."}
        ]},
        #panel{class="info-page-content-panel", body=[
            #panel{style="left:430px; top:2440px; font-size:28px; color:#FC6404;",body="OYUN ÇEŞİTLERİ"},

            #panel{style="left:320px; top:2501px; font-size:22px; color:#FC6404;",body="Standart >>"},
            #panel{style="left:550px; top:2504px; font-size:18px;",body="Gösterme"},
            #panel{style="left:750px; top:2504px; font-size:18px;",body="1 Puan"},
            #panel{style="left:550px; top:2529px; font-size:18px;",body="Açma"},
            #panel{style="left:750px; top:2529px; font-size:18px;",body="3 Puan"},
            #panel{style="left:550px; top:2554px; font-size:18px;",body="Okey atma"},
            #panel{style="left:750px; top:2554px; font-size:18px;",body="6 Puan"},
            #panel{style="left:550px; top:2579px; font-size:18px;",body="Çift açma"},
            #panel{style="left:750px; top:2579px; font-size:18px;",body="6 Puan"},
            #panel{style="left:550px; top:2604px; font-size:18px;",body="Çiftten okey atma"},
            #panel{style="left:750px; top:2604px; font-size:18px;",body="12 Puan"}
        ]},
        #panel{class="info-page-content-panel", body=[
            #panel{style="left:300px; top:2662px; font-size:22px; color:#FC6404;",body="Tek Çift >>"},
            #panel{style="left:450px; top:2665px; width:400px; text-align:right; font-size:18px;",body="Gösterme nin tek yada çift olmasına göre puanların belirlendiği oyun şeklidir<span style='margin-right:-4px;'>.</span>"},
            #panel{style="left:450px; top:2720px; width:400px; text-align:right; font-size:18px;",body="Tek sayılı gösterme olduğunda standart<br> oyun puanları geçerli olur<span style='margin-right:-4px;'>.</span>"},
            #panel{style="left:450px; top:2775px; width:400px; text-align:right; font-size:18px;",body="Çift sayılı gösterme olduğunda standart oyun puanlarının 2 katı kabul edilir<span style='margin-right:-4px;'>.</span>"},
            #panel{style="left:100px; top:150px; font-size:18px;",body=""}
        ]},
        #panel{class="info-page-content-panel", body=[
            #panel{style="left:220px; top:2862px; font-size:22px; color:#FC6404;",body="Renkli >>"},
            #panel{style="left:350px; top:2865px; font-size:18px;",body="Gösterme taşının rengine göre puanlamanın yapıldığı oyundur."},
            #panel{style="left:320px; top:2890px; font-size:18px;",body="Gösterme SARI-MAVİ tek sayı  olursa  STANDART OYUN PUANLARI."},
            #panel{style="left:300px; top:2915px; font-size:18px;",body="Gösterme SARI-MAVİ çift sayı ve KIRMIZI - SİYAH tek sayı olursa <br>2 katı puan hesaplanır."},
            #panel{style="left:310px; top:2962px; font-size:18px;",body="Gösterme KIRMIZI - SİYAH çift  sayı olursa 4 katı puan hesaplanır."}
        ]},
        #panel{class="info-page-content-panel", body=[
            #panel{style="left:190px; top:3022px; font-size:22px; color:#FC6404;",body="Çanak Puanı >>"},
            #panel{style="left:380px; top:3025px; font-size:18px;",body="Tek-çift ve renkli oyunlar “çanak” lıdır."},
            #panel{style="left:420px; top:3050px; font-size:18px;",body="“Çanak”, oyun içi bonus puanların toplandığı yerdir."},
            #panel{style="left:400px; top:3075px; font-size:18px;",body="Açılışta oyun puanı sistem tarafından çanağa yazılır."},
            #panel{style="left:450px; top:3100px; font-size:18px;",body="Oyun içinde yapılmayan gösterme puanları çanağa eklenir."},
            #panel{style="left:400px; top:3125px; font-size:18px;",body="Okey atan, çift açan, 8 taş yapan, renk açanlar açma puanlarına"},
            #panel{style="left:440px; top:3150px; font-size:18px;",body="ilave olarak çanak içindeki birikmiş puanı da kazanırlar."},
            #panel{style="left:400px; top:3175px; font-size:18px;",body="Boşalan çanağa , sistem tarafından oyun puanı yeniden eklenir."}
        ]},
        #panel{class="info-page-content-panel", body=[
            #panel{style="left:350px; top:3252px; font-size:22px; color:#FC6404;",body="10’dan Düşmeli >>"},
            #panel{style="left:580px; top:3255px; font-size:18px;",body="Oyuna her oyuncu 10 puan ile başlar."},

            #panel{style="left:450px; top:3300px; font-size:18px;",body="Gösterme"},
            #panel{style="left:580px; top:3300px; width:20px; text-align:right; font-size:18px;",body="1"},
            #panel{style="left:450px; top:3325px; font-size:18px;",body="Açma"},
            #panel{style="left:580px; top:3325px; width:20px; text-align:right; font-size:18px;",body="2"},
            #panel{style="left:450px; top:3350px; font-size:18px;",body="Okey atma"},
            #panel{style="left:580px; top:3350px; width:20px; text-align:right; font-size:18px;",body="4"},
            #panel{style="left:450px; top:3375px; font-size:18px;",body="Çift açma"},
            #panel{style="left:580px; top:3375px; width:20px; text-align:right; font-size:18px;",body="4"},

            #panel{style="left:640px; top:3337px; font-size:18px;",body="olmak üzere puan düşümü yapılır."},

            #panel{style="left:480px; top:3420px; font-size:18px;",body="Puanını il sıfırlayan oyunu kazanır."}
        ]},
        #panel{class="info-page-content-panel", body=[
            #panel{style="left:100px; top:3630px; font-size:28px; color:#FC6404;",body="FARKLI OYUN BİTİRME ÇEŞİTLERİ"},
            #panel{style="left:100px; top:3670px; font-size:18px;",body="Ayrıca Tek-çift ve Renkli oyunlar için geçerli aşağıda açıklanan"},
            #panel{style="left:100px; top:3695px; font-size:18px;",body="özel açma şekilleri de bulunur ve ikramiyeli puan hesabı yapılır."}
        ]},
        #panel{class="info-page-content-panel", body=[
            #panel{style="left:75px; top:3762px; font-size:22px; color:#FC6404;",body="Renk Açmak >>"},
            #panel{style="left:260px; top:3765px; font-size:18px;",body="Açmanın eldeki taşların tamamının "},
            #panel{style="left:130px; top:3790px; font-size:18px;",body="aynı renkten taşlarla yapılması esasına dayanır. Normal oyundaki "},
            #panel{style="left:180px; top:3815px; font-size:18px;",body="gibi okeyler eksik taşların yerine kullanılabilir."},
            #panel{style="left:220px; top:3840px; font-size:18px;",body="Oyuna esas puanlamanın 8 katı açma puanı olarak uygulanır."},
            #panel{style="left:250px; top:3865px; font-size:18px;",body="Bu durumda okey atılırsa 16 katı uygulanır."}
        ]},
        #panel{class="info-page-content-panel", body=[
            #panel{style="left:125px; top:3932px; font-size:22px; color:#FC6404;",body="Renkten Çift Açmak >>"},
            #panel{style="left:380px; top:3935px; font-size:18px;",body="Bütün çift taşların aynı renkten olması hali. "},
            #panel{style="left:350px; top:3960px; font-size:18px;",body="Renk açmanın iki katı puan uygulanır."},
            #panel{style="left:420px; top:3985px; font-size:18px;",body="Okey taşları kullanılabilir."}
        ]},
        #panel{class="info-page-content-panel", body=[
            #panel{style="left:50px; top:4052px; font-size:22px; color:#FC6404;",body="Sekiz Taş >>"},
            #panel{style="left:210px; top:4055px; font-size:18px;",body="Oyun esnasında, oyun taşları içinde herhangi bir taşın"},
            #panel{style="left:160px; top:4080px; font-size:18px;",body="8 tanesini birden ıstakada toplamak demektir. Örn. 8 tane 4'lü gibi."},
            #panel{style="left:220px; top:4105px; font-size:18px;",body="8 taşı toplayan oyuncu ekranındaki “8 Taş” düğmesini tıkladığında "},
            #panel{style="left:240px; top:4130px; font-size:18px;",body="eli açıp açmamasına bakılmaksızın oyun puanının 4 katı puanı ve"},
            #panel{style="left:320px; top:4155px; font-size:18px;",body="çanakta biriken puanı ödül olarak kazanır."}
        ]},
        #panel{class="info-page-content-panel", body=[
            #panel{style="left:244px; top:4262px; font-size:22px; color:#fff;",body="Oyun ve puanlama seçenekleri masa açılışında set edilir. "}
        ]},
        #panel{class="info-page-content-panel", style="height:100px;", body=[
            #panel{style="left:394px; top:4400px; font-size:28px; color:#FC6404;",body="Puanlama Tablosu"},
            #panel{style="top:4450px; font-size:18px;",body=[matchmaker_rules:okey_puan_table_tr()]}
        ]}
    ].


event(Any)->
    webutils:event(Any).

