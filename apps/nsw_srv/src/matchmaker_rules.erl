-module(matchmaker_rules).
-compile(export_all).

-include_lib("nitrogen_core/include/wf.hrl").
-include("gettext.hrl").

okey_rules() ->
    case site_utils:detect_language() of    % I'd move this somewhere separate in a while, but right now I don't want to mess anything so...
        "tr" ->
            [
                #link{text=?_T("Hide"), class="matchmaker_game_rules", style="float:right;", postback=hide_rules},
                #br{},
                #panel{class=holder, style="margin-left:30px; width:800px; font-size:14px;", body=[
                    #h1{text="OKEY OYUNLARI HAKKINDA", style="font-size:26px; margin-bottom:14px;"},
                    #h1{text="OYUNU OYNAMA", style="font-size:20px; margin-bottom:10px;"},
         
                    #table { rows=[
                        #tablerow { cells=[
                            #tablecell { style="vertical-align:top; text-align:right; font-weight:bold; width:150px; padding:5px;", text="Gösterme taşı:"},
                            #tablecell { style="padding:5px; text-align:left; ", body="Taşlar dağıtıldıktan sonra sağ alt köşede \"&#8239;gösterme&#8239;\" bölümüne açılan taş. "
                                            ++ "Elinde göstermesi olan gösterme taşının üzerini çift tıklayarak oyundaki (&#8239;varsa&#8239;) gösterme puanını kazanır. "
                                            ++ "Gösterme ilk el taş çekilmeden yapılmalıdır. Taş çekildikten sonra gösterme yapılamaz." }
                        ]},
                        #tablerow { cells=[
                            #tablecell { style="vertical-align:top; text-align:right; font-weight:bold; width:150px; padding:5px;", text="Okey taşı:"},
                            #tablecell { style="padding:5px; text-align:left; ", body="Göstermenin bir sayı yukarısı (&#8239;aynı renk olmak şartı ile&#8239;) okey taşıdır. Bu taş joker niteliğinde olup elde eksik olan istenilen renkteki sayı yerine kullanılır. "
                                            ++ "her el iki adet taş okey taşı yerine geçer. (&#8239;okey sayılan taşın yerine kullanılmak üzere 2 tane sahte okey bulunur&#8239;)."}
                        ]},
                        #tablerow { cells=[
                            #tablecell { style="vertical-align:top; text-align:right; font-weight:bold; width:150px; padding:5px;", text="Açar taşlar:"},
                            #tablecell { style="padding:5px; text-align:left; ", body="aynı renkten sıralı hale gelmesi, (&#8239;örn. Yeşil 1, 2, 3, 4, sarı 7, 8, 9, kırmızı 10, 11, 12 gibi&#8239;) 1sayılı taş seride 13 ten sonrada kullanılabilir (&#8239;11,12,13,1 gibi&#8239;) "
                                            ++ "aynı sayıların farklı renklerden bir seri uluşturması (&#8239;sarı- yeşil-kırmızı 2'li gibi&#8239;) "
                                            ++ "eldeki taşların tamamının (&#8239;14 tane&#8239;) açarlı gruplar oluşturması oyun bitme şartıdır. "
                                            ++ "Bu gruplar 4 &ndash; 4 &ndash; 3 &ndash; 3 lü olabileceği gibi 5 &ndash; 3 &ndash; 3 &ndash; 3 lü de veya 14 taşı açar hale gelen 5 &ndash; 5 &ndash; 4 te olabilir. Seri taşlar arasında boşluk olmamalıdır. "
                                            ++ "sürükleyip bırakarak oyunu bitirir."}
                        ]},
                        #tablerow { cells=[
                            #tablecell { style="vertical-align:top; text-align:right; font-weight:bold; width:150px; padding:5px;", text="Çift açmak:"},
                            #tablecell { style="padding:5px; margin-top:5px; text-align:left; ", body="Aynı renk ve sayıdan olmak üzere 7 çift taş da açar seri oluşturur. (&#8239;sahte okey çifti de dahildir&#8239;). Buna \" ÇİFT AÇMAK \" denir. "
                                            ++ "Çift olan taşlar yanyana dururken farklı çiftleri en az bir boşluk bırakarak dizmek gerekir. "
                                            ++ "15. taş çekilen taşların üzerine konarak oyun bitti bilgisi verilir."}
                        ]},
                        #tablerow { cells=[
                            #tablecell { style="vertical-align:top; text-align:right; font-weight:bold; width:150px; padding:5px;", text="Okey atmak:"},
                            #tablecell { style="padding:5px; text-align:left; ", body="Taşını açan yere oyun bitme taşı olarak \"okey\"i atarsa, oyundaki açma puanını değil okey atma puanını alır. "}
                        ]}
                    ]},

                    #h1{text="OYUN ÇEŞİTLERİ", style="font-size:20px;"},

                    #table { rows=[
                        #tablerow { cells=[
                            #tablecell { style="vertical-align:top; text-align:right; font-weight:bold; width:150px; padding:5px;", text="Standart:"},
                            #tablecell { style="padding:5px; text-align:left; ", body=[
                                #panel{ class="affiliates-box", style="font-size:12px; margin-left:0px; margin-top:-8px; margin-bottom:-2px; padding-top:6px; padding-bottom:3px;", body=[
                                    #table { rows=[
                                        #tablerow { cells=[
                                            #tablecell { style="padding:3px;", text="Gösterme"},
                                            #tablecell { style="padding:3px; text-align:center;", text="1"},
                                            #tablecell { style="padding:3px;", text="Puan"}
                                        ]},
                                        #tablerow { cells=[
                                            #tablecell { style="padding:3px;", text="Açma"},
                                            #tablecell { style="padding:3px; text-align:center;", text="3"},
                                            #tablecell { style="padding:3px;", text="Puan"}
                                        ]},
                                        #tablerow { cells=[
                                            #tablecell { style="padding:3px;", text="Okey atma"},
                                            #tablecell { style="padding:3px; text-align:center;", text="6"},
                                            #tablecell { style="padding:3px;", text="Puan"}
                                        ]},
                                        #tablerow { cells=[
                                            #tablecell { style="padding:3px;", text="Çift açma"},
                                            #tablecell { style="padding:3px; text-align:center;", text="6"},
                                            #tablecell { style="padding:3px;", text="Puan"}
                                        ]},
                                        #tablerow { cells=[
                                            #tablecell { style="padding:3px;", text="Çiftte okey atma"},
                                            #tablecell { style="padding:3px; text-align:center;", body="&nbsp;12&nbsp;"},
                                            #tablecell { style="padding:3px;", text="Puan"}
                                        ]}
                                    ]}
                                ]}
                            ]}
                        ]},
                        #tablerow { cells=[
                            #tablecell { style="vertical-align:top; text-align:right; font-weight:bold; width:150px; padding:5px;", text="Tek-çift:"},
                            #tablecell { style="padding:5px; text-align:left; ", body="Gösterme nin tek yada çift olmasına göre puanların belirlendiği oyun şeklidir. "
                                        ++ "Tek sayılı gösterme olduğunda standart oyun puanları geçerli olur. "
                                        ++ "Çift sayılı gösterme olduğunda standart oyun puanlarının 2 katı kabul edilir."}
                        ]},
                        #tablerow { cells=[
                            #tablecell { style="vertical-align:top; text-align:right; font-weight:bold; width:150px; padding:5px;", text="Renkli:"},
                            #tablecell { style="padding:5px; text-align:left; ", body="Gösterme taşının rengine göre puanlamanın yapıldığı oyundur. "
                                        ++ "Gösterme SARI-MAVİ tek sayı olursa STANDART OYUN PUANLARI. "
                                        ++ "Gösterme SARI-MAVİ çift sayı ve KIRMIZI-SİYAH tek sayı olursa 2 katı puan hesaplanır. "
                                        ++ "Gösterme KIRMIZI-SİYAH çift sayı olursa 4 katı puan hesaplanır."}
                        ]},
                        #tablerow { cells=[
                            #tablecell { style="vertical-align:top; text-align:right; font-weight:bold; width:150px; padding:5px;", text="Çanak puanı:"},
                            #tablecell { style="padding:5px; text-align:left; ", body="Tek-çift ve renkli oyunlar \"çanak\" lıdır. "
                                        ++ "\"Çanak\", oyun içi bonus puanların toplandığı yerdir. Açılışta oyun puanı sistem tarafından çanağa yazılır. "
                                        ++ "Oyun içinde yapılmayan gösterme puanları çanağa eklenir. Okey atan, çift açan, 8 taş yapan, renk açanlar açma puanlarına ilave olarak. "
                                        ++ "Çanak içindeki birikmiş puanı da kazanırlar. "
                                        ++ "Boşalan çanağa, sistem tarafından oyun puanı yeniden eklenir. Böylece çanak hiçbir el için boş kalmamış olur."}
                        ]},                  
                        #tablerow { cells=[
                            #tablecell { style="vertical-align:top; text-align:right; font-weight:bold; width:150px; padding:5px; padding-top:10px;", text="10' dan düşmeli oyun:"},
                            #tablecell { style="padding:5px; padding-top:12px; text-align:left; ", body=[
                                #panel{ class="affiliates-box", style="font-size:12px; margin-left:0px; margin-top:-8px; padding-top:3px; padding-bottom:3px;", body=[
                                    #table { rows=[
                                        #tablerow { cells=[
                                            #tableheader { style="padding:3px;", text="Oyuna her oyuncu 10 puan ile başlar"},
                                            #tableheader { style="padding:3px;", text=""}
                                        ]},
                                        #tablerow { cells=[
                                            #tablecell { style="padding:3px;", text="Gösterme"},
                                            #tablecell { style="padding:3px;", text="1"}
                                        ]},
                                        #tablerow { cells=[
                                            #tablecell { style="padding:3px;", text="Açma"},
                                            #tablecell { style="padding:3px;", text="2"}
                                        ]},
                                        #tablerow { cells=[
                                            #tablecell { style="padding:3px;", text="Okey atma"},
                                            #tablecell { style="padding:3px;", text="4"}
                                        ]},
                                        #tablerow { cells=[
                                            #tablecell { style="padding:3px;", text="Çift açma"},
                                            #tablecell { style="padding:3px;", text="4"}
                                        ]},
                                        #tablerow { cells=[
                                            #tablecell { style="padding:3px;", text=""},
                                            #tablecell { style="padding:3px;", body=["olmak üzere puan düşümü yapılır. ", #br{}, "Puanını ilk önce bitiren oyuncu oyunu kazanır."]}
                                        ]}                                         
                                    ]}
                                ]}
                            ]}
                        ]}
                    ]},

                    

                    #h1{text="FARKLI OYUN BİTİRME ÇEŞİTLERİ", style="font-size:20px; margin-bottom:4px;"},

                    #panel{style="padding:10px 30px;", 
                            text="Ayrıca Tek-çift ve Renkli oyunlar için geçerli aşağıda açıklanan 
                                özel açma şekilleri de bulunur ve ikramiyeli puan hesabı yapılır."},

                    #table { rows=[
                        #tablerow { cells=[
                            #tablecell { style="vertical-align:top; text-align:right; font-weight:bold; width:150px; padding:5px;", text="Renk açmak:"},
                            #tablecell { style="padding:5px; text-align:left; ", body="Açmanın eldeki taşların tamamının aynı renkten taşlarla yapılması esasına dayanır. Normal oyundaki gibi okeyler eksik taşların yerine kullanılabilir. "
                                        ++ "Oyuna esas puanlamanın 8 katı açma puanı olarak uygulanır. Bu durumda okey atılırsa 16 katı uygulanır."}
                        ]},
                        #tablerow { cells=[
                            #tablecell { style="vertical-align:top; text-align:right; font-weight:bold; width:150px; padding:5px;", text="Renkten çift açmak:"},
                            #tablecell { style="padding:5px; text-align:left;", body="Bütün çift taşların aynı renkten olması hali. Renk açmanın iki katı puan uygulanır. Okey taşları kullanılabilir. "}
                        ]},
                        #tablerow { cells=[
                            #tablecell { style="vertical-align:top; text-align:right; font-weight:bold; width:150px; padding:5px;", text="Sekiz taş:"},
                            #tablecell { style="padding:5px; text-align:left; ", body="Oyun esnasında, oyun taşları içinde herhangi bir taşın 8 tanesini birden ıstakada toplamak demektir. "
                                        ++ "Örn. 8 tane 4'lü gibi. "
                                        ++ "8 taşı toplayan oyuncu ekranındaki \"8 Taş\" düğmesini tıkladığında eli açıp açmamasına bakılmaksızın oyun puanının 4 katı puanı ve "
                                        ++ "çanakta biriken puanı ödül olarak kazanır. "}
                        ]}
                    ]},

                    #panel{style="padding:10px 30px;", text="Oyun ve puanlama seçenekleri masa açılışında set edilir. Masa açılışında herhangi bir oyun seçilmez ise standart oyun yüklenir."},

                    okey_puan_table_tr()

                ]},
                #link{text=?_T("Hide"), class="matchmaker_game_rules", style="float:right", postback=hide_rules},
                #br{},
                #br{},
                #br{},
                #br{}
            ];



        "en" ->
            [
                #link{text=?_T("Hide"), class="matchmaker_game_rules", style="float:right;", postback=hide_rules},
                #br{},
                #panel{class=holder, style="margin-left:30px; width:800px; font-size:14px;", body=[
                    #h1{text="OKEY GAME RULES", style="font-size:26px; margin-bottom:14px;"},
                    #h1{text="GAMEPLAY", style="font-size:20px; margin-bottom:10px;"},
         
                    #table { rows=[
                        #tablerow { cells=[
                            #tablecell { style="vertical-align:top; text-align:right; font-weight:bold; width:150px; padding:5px;", text="Indicator:"},
                            #tablecell { style="padding:5px; text-align:left; ", body="After the tiles are dealt, on the right bottom corner you can see the
                                Indicator tile that will be placed in the highlighted zone. A player who has that
                                same tile, can double click it to earn the dedicated points. This must be done
                                before the first tile has been drawn, and not after." }
                        ]},
                        #tablerow { cells=[
                            #tablecell { style="vertical-align:top; text-align:right; font-weight:bold; width:150px; padding:5px;", text="Joker:"},
                            #tablecell { style="padding:5px; text-align:left; ", body="The tile containing the next number after the Indicator written on it. This
                                tile is a wild card that can be used to replace any other tile as the player desires.
                                (There are 2 fake joker tiles to replace these tiles that will be used as joker tiles)."}
                        ]},
                        #tablerow { cells=[
                            #tablecell { style="vertical-align:top; text-align:right; font-weight:bold; width:150px; padding:5px;", text="Sets & opening a hand:"},
                            #tablecell { style="padding:5px; text-align:left; ", body="Consecutive numbers of the same color ( 7-8-9 All
                                green, or 2-3-4 all yellow etc...)
                                Same numbers from different colors ( 7-7-7 Green-
                                Yellow-Red etc...)
                                Tile 1 can be used after tile 13 when using
                                consecutive numbers.
                                It is necessary that all 14 tiles are used in sets when
                                opening a hand.
                                Possible set variations can be in groups of 4-4-3-3
                                or 5-3-3-3 or 5-5-4 which all make 14.
                                There must not be any gaps between the
                                consecutive numbers (3-4-6 is not acceptable)."}
                        ]},
                        #tablerow { cells=[
                            #tablecell { style="vertical-align:top; text-align:right; font-weight:bold; width:150px; padding:5px;", text="Going double:"},
                            #tablecell { style="padding:5px; margin-top:5px; text-align:left; ", body="7 doubles can also open a hand. These doubles must be made of a
                                set of two tiles that have the same color
                                and number. ( 7- 7 both
                                red or 4-4 both green etc....) This is called going double. It is required to arrange
                                the tiles 2 by 2 with a gap in between them. When the 15th tile is drawn and
                                placed upon the pile, you can declare that you have completed the hand."}
                        ]},
                        #tablerow { cells=[
                            #tablecell { style="vertical-align:top; text-align:right; font-weight:bold; width:150px; padding:5px;", text="Throw the Joker:"},
                            #tablecell { style="padding:5px; text-align:left; ", body="When a player discards the joker tile upon opening a hand,
                                then the player receives the ‘Throw the Joker’ point instead of the hand point."}
                        ]}
                    ]},

                    #h1{text="TYPE OF GAMES", style="font-size:20px;"},

                    #table { rows=[
                        #tablerow { cells=[
                            #tablecell { style="vertical-align:top; text-align:right; font-weight:bold; width:150px; padding:5px;", text="Standart:"},
                            #tablecell { style="padding:5px; text-align:left; ", body=[
                                #panel{ class="affiliates-box", style="font-size:12px; margin-left:0px; margin-top:-8px; margin-bottom:-2px; padding-top:6px; padding-bottom:3px;", body=[
                                    #table { rows=[
                                        #tablerow { cells=[
                                            #tablecell { style="padding:3px;", text="Indicator"},
                                            #tablecell { style="padding:3px; text-align:center;", text="1"},
                                            #tablecell { style="padding:3px;", text="point"}
                                        ]},
                                        #tablerow { cells=[
                                            #tablecell { style="padding:3px;", text="Hand"},
                                            #tablecell { style="padding:3px; text-align:center;", text="3"},
                                            #tablecell { style="padding:3px;", text="points"}
                                        ]},
                                        #tablerow { cells=[
                                            #tablecell { style="padding:3px;", text="Joker"},
                                            #tablecell { style="padding:3px; text-align:center;", text="6"},
                                            #tablecell { style="padding:3px;", text="points"}
                                        ]},
                                        #tablerow { cells=[
                                            #tablecell { style="padding:3px;", text="Going double"},
                                            #tablecell { style="padding:3px; text-align:center;", text="6"},
                                            #tablecell { style="padding:3px;", text="points"}
                                        ]},
                                        #tablerow { cells=[
                                            #tablecell { style="padding:3px;", text="Going double and joker"},
                                            #tablecell { style="padding:3px; text-align:center;", body="&nbsp;12&nbsp;"},
                                            #tablecell { style="padding:3px;", text="points"}
                                        ]}
                                    ]}
                                ]}
                            ]}
                        ]},
                        #tablerow { cells=[
                            #tablecell { style="vertical-align:top; text-align:right; font-weight:bold; width:150px; padding:5px;", text="Odd-even:"},
                            #tablecell { style="padding:5px; text-align:left; ", body="The game type where the game point is determined relative to
                                the indicator being even or odd. The game points remain the same when the
                                indicator is an odd number. But when the indicator is an even number the game
                                points are doubled."}
                        ]},
                        #tablerow { cells=[
                            #tablecell { style="vertical-align:top; text-align:right; font-weight:bold; width:150px; padding:5px;", text="Colorful:"},
                            #tablecell { style="padding:5px; text-align:left; ", body="The game type where the game point is determined relative to the
                                color of the indicator. The game points remain the same when the indicator is
                                a yellow/blue odd number. The game points are doubled when the indicator
                                is a yellow/blue even number or black/red odd number. The game points are
                                quadrupled when the indicator is a black/red even number."}
                        ]},
                        #tablerow { cells=[
                            #tablecell { style="vertical-align:top; text-align:right; font-weight:bold; width:150px; padding:5px;", text="Pot Points:"},
                            #tablecell { style="padding:5px; text-align:left; ", body="Odd-Even and Colorful games have Pot Points. The Pot, is where the
                                in-game bonus points are collected. The game points are collected within the
                                pot during the game including the indicator point. When a player throws a joker,
                                goes double, do an eighter or a color hand, in addition to those points, the player
                                gets the points in the pot.
                                When the pot is emptied, the system adds the game point before a new hand is
                                started so the pot never stays empty."}
                        ]},                  
                        #tablerow { cells=[
                            #tablecell { style="vertical-align:top; text-align:right; font-weight:bold; width:150px; padding:5px; padding-top:10px;", text="Countdown from 10:"},
                            #tablecell { style="padding:5px; padding-top:12px; text-align:left; ", body=[
                                #panel{ class="affiliates-box", style="font-size:12px; margin-left:0px; margin-top:-8px; padding-top:3px; padding-bottom:3px;", body=[
                                    #table { rows=[
                                        #tablerow { cells=[
                                            #tableheader { style="padding:3px;", text="Every player starts with 10 points"},
                                            #tableheader { style="padding:3px;", text=""}
                                        ]},
                                        #tablerow { cells=[
                                            #tablecell { style="padding:3px;", text="Indicator"},
                                            #tablecell { style="padding:3px;", text="1"}
                                        ]},
                                        #tablerow { cells=[
                                            #tablecell { style="padding:3px;", text="Hand"},
                                            #tablecell { style="padding:3px;", text="2"}
                                        ]},
                                        #tablerow { cells=[
                                            #tablecell { style="padding:3px;", text="Joker"},
                                            #tablecell { style="padding:3px;", text="4"}
                                        ]},
                                        #tablerow { cells=[
                                            #tablecell { style="padding:3px;", text="Going Double"},
                                            #tablecell { style="padding:3px;", text="4"}
                                        ]},
                                        #tablerow { cells=[
                                            #tablecell { style="padding:3px;", text=""},
                                            #tablecell { style="padding:3px;", body=["points are deducted in this manner ", #br{}, "and the first to finish their points wins."]}
                                        ]}                                         
                                    ]}
                                ]}
                            ]}
                        ]}
                    ]},

                    

                    #h1{text="Different ways to finish a game", style="font-size:20px; margin-bottom:4px;"},

                    #panel{style="padding:10px 30px;", 
                            text="In addition you can find the specific detailed point system for even-odd and
                                colorful games below."},

                    #table { rows=[
                        #tablerow { cells=[
                            #tablecell { style="vertical-align:top; text-align:right; font-weight:bold; width:150px; padding:5px;", text="Colorful hand:"},
                            #tablecell { style="padding:5px; text-align:left; ", body="This type of hand must only consist of tiles of the same color. As in
                                usual, jokers may be used to replace any missing tiles. When a player opens this
                                hand, the hand points are multiplied by 8. If a player opens this hand and throws
                                a joker, then the game points are multiplied by 16."}
                        ]},
                        #tablerow { cells=[
                            #tablecell { style="vertical-align:top; text-align:right; font-weight:bold; width:150px; padding:5px;", text="Going double with color:"},
                            #tablecell { style="padding:5px; text-align:left;", body="This type of hand must only consist of doubles of the
                                same color. The game points are double of a colorful hand."}
                        ]},
                        #tablerow { cells=[
                            #tablecell { style="vertical-align:top; text-align:right; font-weight:bold; width:150px; padding:5px;", text="Eight Tiles:"},
                            #tablecell { style="padding:5px; text-align:left; ", body="If a player collects all 8 of a particular number in their board and
                                click the Eight Tiles button, then regardless to whether the player opened a hand
                                or not, they win the game and receive quadruple game points in addition to the
                                points in the pot."}
                        ]}
                    ]},

                    okey_puan_table_en()
                ]},
                #link{text=?_T("Hide"), class="matchmaker_game_rules", style="float:right", postback=hide_rules},
                #br{},
                #br{},
                #br{},
                #br{}
            ]
    end.

okey_puan_table_tr() ->
    #table { rows=[
        #tablerow { cells=[
            #tableheader { style="padding:5px;", text="" },
            #tableheader { style="padding:5px;", text="" },
            #tableheader { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="Tek-çift", colspan=2 },
            #tableheader { style="padding:5px; background-color:#ddbbff; text-align:center;", text="Renkli", colspan=2 },
            #tableheader { style="padding:5px;", text="" },
            #tableheader { style="padding:5px;", text="" }
        ]},
        #tablerow { cells=[
            #tableheader { style="padding:5px;", text="" },
            #tableheader { style="padding:5px; background-color:#ddddff; text-align:center;", text="standart" },
            #tableheader { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="tek" },
            #tableheader { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="çift" },
            #tableheader { style="padding:5px; background-color:#ddbbff; text-align:center;", body=["sarı-mavi ÇİFT", #br{}, "siyah-kırmızı TEK"] },
            #tableheader { style="padding:5px; background-color:#ddbbff; text-align:center;", text="Siyah kırmızı ÇİFT" },
            #tableheader { style="padding:5px; background-color:#ddffdd; text-align:center;", text="10'dan düşmeli" },
            #tableheader { style="padding:5px; text-align:center;", text="Çanakta biriken" }
        ]},
        #tablerow { cells=[
            #tableheader { style="padding:5px;", text="Gösterme" },
            #tablecell { style="padding:5px; background-color:#ddddff; text-align:center;", text="1" },
            #tablecell { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="1" },
            #tablecell { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="2" },
            #tablecell { style="padding:5px; background-color:#ddbbff; text-align:center;", text="2" },
            #tablecell { style="padding:5px; background-color:#ddbbff; text-align:center;", text="4" },
            #tablecell { style="padding:5px; background-color:#ddffdd; text-align:center;", text="1" },
            #tablecell { style="padding:5px; text-align:center;", body="&mdash;" }
        ]},
        #tablerow { cells=[
            #tableheader { style="padding:5px;", text="Açma" },
            #tablecell { style="padding:5px; background-color:#ddddff; text-align:center;", text="3" },
            #tablecell { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="3" },
            #tablecell { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="6" },
            #tablecell { style="padding:5px; background-color:#ddbbff; text-align:center;", text="6" },
            #tablecell { style="padding:5px; background-color:#ddbbff; text-align:center;", text="12" },
            #tablecell { style="padding:5px; background-color:#ddffdd; text-align:center;", text="2" },
            #tablecell { style="padding:5px; text-align:center;", body="&mdash;" }
        ]},
        #tablerow { cells=[
            #tableheader { style="padding:5px;", text="Okey atma" },
            #tablecell { style="padding:5px; background-color:#ddddff; text-align:center;", text="6" },
            #tablecell { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="6" },
            #tablecell { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="12" },
            #tablecell { style="padding:5px; background-color:#ddbbff; text-align:center;", text="12" },
            #tablecell { style="padding:5px; background-color:#ddbbff; text-align:center;", text="24" },
            #tablecell { style="padding:5px; background-color:#ddffdd; text-align:center;", text="4" },
            #tablecell { style="padding:5px; text-align:center;", body="açana" }
        ]},
        #tablerow { cells=[
            #tableheader { style="padding:5px;", text="Taş çifti" },
            #tablecell { style="padding:5px; background-color:#ddddff; text-align:center;", text="6" },
            #tablecell { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="6" },
            #tablecell { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="12" },
            #tablecell { style="padding:5px; background-color:#ddbbff; text-align:center;", text="12" },
            #tablecell { style="padding:5px; background-color:#ddbbff; text-align:center;", text="24" },
            #tablecell { style="padding:5px; background-color:#ddffdd; text-align:center;", text="4" },
            #tablecell { style="padding:5px; text-align:center;", body="açana" }
        ]},
        #tablerow { cells=[
            #tableheader { style="padding:5px;", text="8 taş" },
            #tablecell { style="padding:5px; background-color:#ddddff; text-align:center;", text="yok" },
            #tablecell { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="12" },
            #tablecell { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="24" },
            #tablecell { style="padding:5px; background-color:#ddbbff; text-align:center;", text="24" },
            #tablecell { style="padding:5px; background-color:#ddbbff; text-align:center;", text="48" },
            #tablecell { style="padding:5px; background-color:#ddffdd; text-align:center;", text="yok" },
            #tablecell { style="padding:5px; text-align:center;", body="yapana" }
        ]},
        #tablerow { cells=[
            #tableheader { style="padding:5px;", text="Renk açma" },
            #tablecell { style="padding:5px; background-color:#ddddff; text-align:center;", text="yok" },
            #tablecell { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="24" },
            #tablecell { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="48" },
            #tablecell { style="padding:5px; background-color:#ddbbff; text-align:center;", text="48" },
            #tablecell { style="padding:5px; background-color:#ddbbff; text-align:center;", text="96" },
            #tablecell { style="padding:5px; background-color:#ddffdd; text-align:center;", text="yok" },
            #tablecell { style="padding:5px; text-align:center;", body="açana" }
        ]},
        #tablerow { cells=[
            #tableheader { style="padding:5px;", text="Renkten Taş çifti" },
            #tablecell { style="padding:5px; background-color:#ddddff; text-align:center;", text="yok" },
            #tablecell { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="48" },
            #tablecell { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="96" },
            #tablecell { style="padding:5px; background-color:#ddbbff; text-align:center;", text="96" },
            #tablecell { style="padding:5px; background-color:#ddbbff; text-align:center;", text="192" },
            #tablecell { style="padding:5px; background-color:#ddffdd; text-align:center;", text="yok" },
            #tablecell { style="padding:5px; text-align:center;", body="açana" }
        ]},
        #tablerow { cells=[
            #tableheader { style="padding:5px;", text="Renkten okey atma" },
            #tablecell { style="padding:5px; background-color:#ddddff; text-align:center;", text="yok" },
            #tablecell { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="48" },
            #tablecell { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="96" },
            #tablecell { style="padding:5px; background-color:#ddbbff; text-align:center;", text="96" },
            #tablecell { style="padding:5px; background-color:#ddbbff; text-align:center;", text="192" },
            #tablecell { style="padding:5px; background-color:#ddffdd; text-align:center;", text="yok" },
            #tablecell { style="padding:5px; text-align:center;", body="açana" }
        ]},
        #tablerow { cells=[
            #tableheader { style="padding:5px;", text="Renkten Taş çifti okey atma" },
            #tablecell { style="padding:5px; background-color:#ddddff; text-align:center;", text="yok" },
            #tablecell { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="96" },
            #tablecell { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="192" },
            #tablecell { style="padding:5px; background-color:#ddbbff; text-align:center;", text="192" },
            #tablecell { style="padding:5px; background-color:#ddbbff; text-align:center;", text="384" },
            #tablecell { style="padding:5px; background-color:#ddffdd; text-align:center;", text="yok" },
            #tablecell { style="padding:5px; text-align:center;", body="açana" }
        ]},
        #tablerow { cells=[
            #tableheader { style="padding:5px;", text="Yanlış açan" },
            #tablecell { style="padding:5px; background-color:#ddddff; text-align:center;", text="-9" },
            #tablecell { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="-9" },
            #tablecell { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="-18" },
            #tablecell { style="padding:5px; background-color:#ddbbff; text-align:center;", text="-18" },
            #tablecell { style="padding:5px; background-color:#ddbbff; text-align:center;", text="-36" },
            #tablecell { style="padding:5px; background-color:#ddffdd; text-align:center;", text="-2" },
            #tablecell { style="padding:5px; text-align:center;", body="&mdash;" }
        ]},
        #tablerow { cells=[
            #tableheader { style="padding:5px;", text="Yanlış açışta diğerleri" },
            #tablecell { style="padding:5px; background-color:#ddddff; text-align:center;", text="3" },
            #tablecell { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="3" },
            #tablecell { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="6" },
            #tablecell { style="padding:5px; background-color:#ddbbff; text-align:center;", text="6" },
            #tablecell { style="padding:5px; background-color:#ddbbff; text-align:center;", text="12" },
            #tablecell { style="padding:5px; background-color:#ddffdd; text-align:center;", text="yok" },
            #tablecell { style="padding:5px; text-align:center;", body="&mdash;" }
        ]},
        #tablerow { cells=[
            #tableheader { style="padding:5px;", text="Çanak boşalınca çanağa eklenen" },
            #tablecell { style="padding:5px; background-color:#ddddff; text-align:center;", text="yok" },
            #tablecell { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="4" },
            #tablecell { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="8" },
            #tablecell { style="padding:5px; background-color:#ddbbff; text-align:center;", text="8" },
            #tablecell { style="padding:5px; background-color:#ddbbff; text-align:center;", text="16" },
            #tablecell { style="padding:5px; background-color:#ddffdd; text-align:center;", text="yok" },
            #tablecell { style="padding:5px; text-align:center;", body="&mdash;" }
        ]}
    ]}.


okey_puan_table_en() ->
    #table { rows=[
        #tablerow { cells=[
            #tableheader { style="padding:5px;", text="" },
            #tableheader { style="padding:5px;", text="" },
            #tableheader { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="Odd-even", colspan=2 },
            #tableheader { style="padding:5px; background-color:#ddbbff; text-align:center;", text="Colorful", colspan=2 },
            #tableheader { style="padding:5px;", text="" },
            #tableheader { style="padding:5px;", text="" }
        ]},
        #tablerow { cells=[
            #tableheader { style="padding:5px;", text="" },
            #tableheader { style="padding:5px; background-color:#ddddff; text-align:center;", text="standart" },
            #tableheader { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="odd" },
            #tableheader { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="even" },
            #tableheader { style="padding:5px; background-color:#ddbbff; text-align:center;", body=["yellow-and-blue EVEN", #br{}, "black-and-red ODD"] },
            #tableheader { style="padding:5px; background-color:#ddbbff; text-align:center;", text="Black and red EVEN" },
            #tableheader { style="padding:5px; background-color:#ddffdd; text-align:center;", text="Countdown from 10" },
            #tableheader { style="padding:5px; text-align:center;", text="Pot points" }
        ]},
        #tablerow { cells=[
            #tableheader { style="padding:5px;", text="Indicator" },
            #tablecell { style="padding:5px; background-color:#ddddff; text-align:center;", text="1" },
            #tablecell { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="1" },
            #tablecell { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="2" },
            #tablecell { style="padding:5px; background-color:#ddbbff; text-align:center;", text="2" },
            #tablecell { style="padding:5px; background-color:#ddbbff; text-align:center;", text="4" },
            #tablecell { style="padding:5px; background-color:#ddffdd; text-align:center;", text="1" },
            #tablecell { style="padding:5px; text-align:center;", body="&mdash;" }
        ]},
        #tablerow { cells=[
            #tableheader { style="padding:5px;", text="Hand" },
            #tablecell { style="padding:5px; background-color:#ddddff; text-align:center;", text="3" },
            #tablecell { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="3" },
            #tablecell { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="6" },
            #tablecell { style="padding:5px; background-color:#ddbbff; text-align:center;", text="6" },
            #tablecell { style="padding:5px; background-color:#ddbbff; text-align:center;", text="12" },
            #tablecell { style="padding:5px; background-color:#ddffdd; text-align:center;", text="2" },
            #tablecell { style="padding:5px; text-align:center;", body="&mdash;" }
        ]},
        #tablerow { cells=[
            #tableheader { style="padding:5px;", text="Throwing a joker" },
            #tablecell { style="padding:5px; background-color:#ddddff; text-align:center;", text="6" },
            #tablecell { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="6" },
            #tablecell { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="12" },
            #tablecell { style="padding:5px; background-color:#ddbbff; text-align:center;", text="12" },
            #tablecell { style="padding:5px; background-color:#ddbbff; text-align:center;", text="24" },
            #tablecell { style="padding:5px; background-color:#ddffdd; text-align:center;", text="4" },
            #tablecell { style="padding:5px; text-align:center;", body="opened" }
        ]},
        #tablerow { cells=[
            #tableheader { style="padding:5px;", text="Going double" },
            #tablecell { style="padding:5px; background-color:#ddddff; text-align:center;", text="6" },
            #tablecell { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="6" },
            #tablecell { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="12" },
            #tablecell { style="padding:5px; background-color:#ddbbff; text-align:center;", text="12" },
            #tablecell { style="padding:5px; background-color:#ddbbff; text-align:center;", text="24" },
            #tablecell { style="padding:5px; background-color:#ddffdd; text-align:center;", text="4" },
            #tablecell { style="padding:5px; text-align:center;", body="opened" }
        ]},
        #tablerow { cells=[
            #tableheader { style="padding:5px;", text="Eight tiles" },
            #tablecell { style="padding:5px; background-color:#ddddff; text-align:center;", text="no" },
            #tablecell { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="12" },
            #tablecell { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="24" },
            #tablecell { style="padding:5px; background-color:#ddbbff; text-align:center;", text="24" },
            #tablecell { style="padding:5px; background-color:#ddbbff; text-align:center;", text="48" },
            #tablecell { style="padding:5px; background-color:#ddffdd; text-align:center;", text="no" },
            #tablecell { style="padding:5px; text-align:center;", body="yours" }
        ]},
        #tablerow { cells=[
            #tableheader { style="padding:5px;", text="Colorful hand" },
            #tablecell { style="padding:5px; background-color:#ddddff; text-align:center;", text="no" },
            #tablecell { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="24" },
            #tablecell { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="48" },
            #tablecell { style="padding:5px; background-color:#ddbbff; text-align:center;", text="48" },
            #tablecell { style="padding:5px; background-color:#ddbbff; text-align:center;", text="96" },
            #tablecell { style="padding:5px; background-color:#ddffdd; text-align:center;", text="no" },
            #tablecell { style="padding:5px; text-align:center;", body="opened" }
        ]},
        #tablerow { cells=[
            #tableheader { style="padding:5px;", text="Going double with color" },
            #tablecell { style="padding:5px; background-color:#ddddff; text-align:center;", text="no" },
            #tablecell { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="48" },
            #tablecell { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="96" },
            #tablecell { style="padding:5px; background-color:#ddbbff; text-align:center;", text="96" },
            #tablecell { style="padding:5px; background-color:#ddbbff; text-align:center;", text="192" },
            #tablecell { style="padding:5px; background-color:#ddffdd; text-align:center;", text="no" },
            #tablecell { style="padding:5px; text-align:center;", body="opened" }
        ]},
        #tablerow { cells=[
            #tableheader { style="padding:5px;", text="Going double with joker" },
            #tablecell { style="padding:5px; background-color:#ddddff; text-align:center;", text="no" },
            #tablecell { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="48" },
            #tablecell { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="96" },
            #tablecell { style="padding:5px; background-color:#ddbbff; text-align:center;", text="96" },
            #tablecell { style="padding:5px; background-color:#ddbbff; text-align:center;", text="192" },
            #tablecell { style="padding:5px; background-color:#ddffdd; text-align:center;", text="no" },
            #tablecell { style="padding:5px; text-align:center;", body="opened" }
        ]},
        #tablerow { cells=[
            #tableheader { style="padding:5px;", text="Going double with color + joker" },
            #tablecell { style="padding:5px; background-color:#ddddff; text-align:center;", text="no" },
            #tablecell { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="96" },
            #tablecell { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="192" },
            #tablecell { style="padding:5px; background-color:#ddbbff; text-align:center;", text="192" },
            #tablecell { style="padding:5px; background-color:#ddbbff; text-align:center;", text="384" },
            #tablecell { style="padding:5px; background-color:#ddffdd; text-align:center;", text="no" },
            #tablecell { style="padding:5px; text-align:center;", body="opened" }
        ]},
        #tablerow { cells=[
            #tableheader { style="padding:5px;", text="False opening" },
            #tablecell { style="padding:5px; background-color:#ddddff; text-align:center;", text="-9" },
            #tablecell { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="-9" },
            #tablecell { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="-18" },
            #tablecell { style="padding:5px; background-color:#ddbbff; text-align:center;", text="-18" },
            #tablecell { style="padding:5px; background-color:#ddbbff; text-align:center;", text="-36" },
            #tablecell { style="padding:5px; background-color:#ddffdd; text-align:center;", text="-2" },
            #tablecell { style="padding:5px; text-align:center;", body="&mdash;" }
        ]},
        #tablerow { cells=[
            #tableheader { style="padding:5px;", text="Other players during false opening" },
            #tablecell { style="padding:5px; background-color:#ddddff; text-align:center;", text="3" },
            #tablecell { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="3" },
            #tablecell { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="6" },
            #tablecell { style="padding:5px; background-color:#ddbbff; text-align:center;", text="6" },
            #tablecell { style="padding:5px; background-color:#ddbbff; text-align:center;", text="12" },
            #tablecell { style="padding:5px; background-color:#ddffdd; text-align:center;", text="no" },
            #tablecell { style="padding:5px; text-align:center;", body="&mdash;" }
        ]},
        #tablerow { cells=[
            #tableheader { style="padding:5px;", text="Addition to the pot when the pot is empty" },
            #tablecell { style="padding:5px; background-color:#ddddff; text-align:center;", text="no" },
            #tablecell { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="4" },
            #tablecell { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="8" },
            #tablecell { style="padding:5px; background-color:#ddbbff; text-align:center;", text="8" },
            #tablecell { style="padding:5px; background-color:#ddbbff; text-align:center;", text="16" },
            #tablecell { style="padding:5px; background-color:#ddffdd; text-align:center;", text="no" },
            #tablecell { style="padding:5px; text-align:center;", body="&mdash;" }
        ]}
    ]}.
