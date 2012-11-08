%% -*- mode: nitrogen -*-
-module (tournament_lobby).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("gettext.hrl").
-include("setup.hrl").

main() ->
    webutils:js_for_main_authorized_game_stats_menu(),
    webutils:add_to_head({raw,              % this goes to styles.css. Still here for convenience of editing
    "
        <style media='screen' type='text/css'>
            .tourlobby_title {
                width:250px; 
                height:43px; 
                background: url(images/tournament/lobby/top_plask.png);
                position:absolute; 
                left:21px; 
                top:-7px; 
                text-align:center;
            }

            .tourlobby_title_label {
                font-size:18px; 
                color:#fff; 
                font-weight:bold; 
                line-height:42px;
            }

            .tourlobby_left_top_block {
                width:212px; height:386px; background-color:#565656; 
                position:absolute; left:29px; top:75px;
                -moz-border-radius:3px;
                -webkit-border-radius:3px;
                border-radius:3px;
            }

            .tourlobby_left_top_block_label {
                font-size:14px; color:#fff; 
                font-weight:bold; 
                line-height:32px;
            }

            .tourlobby_orange_button {
                display:block; width:190px; height:41px; background: url(/images/tournament/lobby/btn_orange_normal.png);
                font-size:16px; color:#363638; line-height:42px;
            }

            .tourlobby_orange_button:hover {
                display:block; width:190px; height:41px; background: url(/images/tournament/lobby/btn_orange_hover.png);
                font-size:16px; color:#363638; line-height:42px;
                text-decoration:none;
            }

            .tourlobby_orange_button:active {
                display:block; width:190px; height:41px; background: url(/images/tournament/lobby/btn_orange_pressed.png);
                font-size:16px; color:#363638; line-height:42px;
            }

            .tourlobby_red_button {
                display:block; width:190px; height:41px; background: url(/images/tournament/lobby/btn_red_normal.png);
                font-size:16px; color:#121212; line-height:42px;
            }

            .tourlobby_red_button:hover {
                display:block; width:190px; height:41px; background: url(/images/tournament/lobby/btn_red_hover.png);
                font-size:16px; color:#121212; line-height:42px;
                text-decoration:none;
            }

            .tourlobby_red_button:active {
                display:block; width:190px; height:41px; background: url(/images/tournament/lobby/btn_red_pressed.png);
                font-size:16px; color:#121212; line-height:42px;
            }

            .tourlobby_yellow_button {
                display:block; width:190px; height:41px; background: url(/images/tournament/lobby/btn_yellow_normal.png);
                font-size:16px; color:#363638; line-height:42px;
            }

            .tourlobby_yellow_button:hover {
                display:block; width:190px; height:41px; background: url(/images/tournament/lobby/btn_yellow_hover.png);
                font-size:16px; color:#363638; line-height:42px;
                text-decoration:none;
            }

            .tourlobby_yellow_button:active {
                display:block; width:190px; height:41px; background: url(/images/tournament/lobby/btn_yellow_pressed.png);
                font-size:16px; color:#363638; line-height:42px;
            }
            
            .tourlobby_left_bottom_block {
                width:212px; height:102px; background-color:#565656;
                position:absolute; left:29px; top:462px;
                -moz-border-radius:3px;
                -webkit-border-radius:3px;
                border-radius:3px;
            }

            .tourlobby_left_bottom_block_title {
                font-size:18px; color:#fff; font-weight:bold; line-height:27px; padding-left:17px;
            }

            .tourlobby_left_bottom_block_label {
                font-size:14px; color:#fff; line-height:22px; padding-left:17px;
            }

            .tourlobby_orange_plask {
                width:213px; height:61px; background: url(/images/tournament/lobby/orange_plask.png);
                position:absolute; left:275px; top:75px; text-align:center; padding-top:9px;
            }

            .tourlobby_sky_plask {
                width:213px; height:61px; background: url(/images/tournament/lobby/sky_plask.png);
                position:absolute; left:501px; top:75px; text-align:center; padding-top:9px;
            }

            .tourlobby_blue_plask {
                width:213px; height:61px; background: url(/images/tournament/lobby/blue_plask.png);
                position:absolute; left:727px; top:75px; text-align:center; padding-top:9px;
            }

            .tourlobby_every_plask_title {
                font-size:12px; color:#fff; font-weight:bold; line-height:16px;
            }

            .tourlobby_every_plask_label {
                color:#fff; font: 36px 'Gotham Rounded Bold','Trebuchet MS'; margin-top:-5px;
            }

            .tourlobby_prizes {
                width:663px; height:175px; background-color:#e4e8e9;
                position:absolute; left:275px; top:174px;
                -moz-border-radius:2px;
                -webkit-border-radius:2px;
                border-radius:2px;
            }

            .tourlobby_prize_1 {
                width:175px; height:155px; background-color:#fff;
                position:relative; left:50px; top:9px;
                border:1px solid #a5a6a8;
                -moz-border-radius:1px;
                -webkit-border-radius:1px;
                border-radius:1px;
            }

            .tourlobby_prize_2 {
                width:175px; height:155px; background-color:#fff;
                position:relative; left:243px; top:-148px;
                border:1px solid #a5a6a8;
                -moz-border-radius:1px;
                -webkit-border-radius:1px;
                border-radius:1px;
            }

            .tourlobby_prize_3 {
                width:175px; height:155px; background-color:#fff;
                position:relative; left:436px; top:-305px;
                border:1px solid #a5a6a8;
                -moz-border-radius:1px;
                -webkit-border-radius:1px;
                border-radius:1px;
            }

            .tourlobby_chat {
                width:663px; height:191px; background-color:#fff;
                position:absolute; left:275px; top:372px;
                -moz-border-radius:2px;
                -webkit-border-radius:2px;
                border-radius:2px;
                border:1px solid #cdcdcd;
            }

            .tourlobby_chat_title {
                font-size:12px; color:#000; line-height:26px; padding-left:10px;
            }

            .tourlobby_prize_star_1 {
                width:55px; height:51px; background: url(/images/tournament/lobby/star_1.png);
                position:relative; left:-19px; top:-175px; text-align:center;
            }

            .tourlobby_prize_star_2 {
                width:55px; height:51px; background: url(/images/tournament/lobby/star_2.png);
                position:relative; left:-19px; top:-175px; text-align:center;
            }

            .tourlobby_prize_star_3 {
                width:55px; height:51px; background: url(/images/tournament/lobby/star_3.png);
                position:relative; left:-19px; top:-175px; text-align:center;
            }

            .tourlobby_prize_star_text {
                color:#000; font: 26px 'Gotham Rounded Bold','Trebuchet MS'; line-height:55px;
            }

            .tourlobby_chat_window {
                width:600px; height:100px; background-color:#fff;
                position:absolute; left:10px; top:36px;
                font-size:14px; line-height:20px;
            }

            .tourlobby_chat_panel {
                width:663px; height:28px; background-color:#c7dbf4;
                position:absolute; left:0px; top:0px;
            }

            .tourlobby_chat_textarea {
                width:557px; height:24px; background-color:#fff;
                padding:5px;
                position:absolute; left:10px; top:145px;
                -moz-border-radius:2px;
                -webkit-border-radius:2px;
                border-radius:2px;
                border:1px solid #cdcdcd;
                font-size:16px;
                line-height:20px;
                resize:none;
            }

            .tourlobby_chat_button {
                position:absolute; left:577px; top:145px;
                display:block; width:74px; height:36px; background: url(/images/tournament/lobby/btn_chat_normal.png);
                font-size:16px; color:#121212; line-height:35px;
                text-align:center;
            }

            .tourlobby_chat_button:hover {
                position:absolute; left:577px; top:145px;
                display:block; width:74px; height:36px; background: url(/images/tournament/lobby/btn_chat_hover.png);
                font-size:16px; color:#121212; line-height:35px;
                text-align:center;
                text-decoration:none;
            }

            .tourlobby_chat_button:active {
                position:absolute; left:577px; top:145px;
                display:block; width:74px; height:36px; background: url(/images/tournament/lobby/btn_chat_pressed.png);
                font-size:16px; color:#121212; line-height:35px;
                text-align:center;
            }

            .tourlobby_table_panel {
                width:958px; height:580px;
                position:absolute; left:0px; top:610px;
            }

            .tourlobby_table {
                width:958px; font-size:16px; background-color:#e4e8e9;
            }

            .tourlobby_table_arrow {
                text-align:center; height:47px; vertical-align:middle;
            }

            .tourlobby_table_head {
                background-color:#a7b7b7; border-bottom:2px solid 8c8d8f; height:40px;
                color:#fff;
                font: 18px 'Gotham Rounded Bold','Trebuchet MS'; line-height:55px;
                text-shadow:0 1px 1px #353535;
            }
        </style>
        <script>
            new Image('/images/tournament/lobby/btn_orange_hover.png');
            new Image('/images/tournament/lobby/btn_orange_pressed.png');
            new Image('/images/tournament/lobby/btn_red_hover.png');
            new Image('/images/tournament/lobby/btn_red_pressed.png');
            new Image('/images/tournament/lobby/btn_yellow_hover.png');
            new Image('/images/tournament/lobby/btn_yellow_pressed.png');
            new Image('/images/tournament/lobby/btn_chat_hover.png');
            new Image('/images/tournament/lobby/btn_chat_pressed.png');
        </script>
    "}),
    #template { file=code:priv_dir(nsw_srv)++"/templates/bare.html" }.

title() -> webutils:title(?MODULE).

body() ->
    #template{file=code:priv_dir(nsw_srv)++"/templates/info_page.html"}.


content() ->
    [  
        #panel{class="tourlobby_title", body=[
                #label{class="tourlobby_title_label", body="TURNUVA LOBY"}
            ]
        },

        #panel{id="inside", style="height:1188px;", body=[

            % left top block
            #panel{class="tourlobby_left_top_block", body=[
                    "<center>",
                    #label{class="tourlobby_left_top_block_label", body="TAVLA TURNUVASI"},
                    #br{},
                    #image{image="/images/tournament/lobby/tour_avatar.png"},
                    #br{},
                    #br{},
                    #link{postback=orange_button, class="tourlobby_orange_button", text="TURNUVAYA KATIL"},
                    #br{},
                    #link{postback=red_button, class="tourlobby_red_button", text="TURNUVADAN AYRIL"},
                    #br{},
                    #link{postback=yellow_button, class="tourlobby_yellow_button", text="TURNUVADAN GİT"},
                    "</center>"
                ]
            },

            %left bottom block
            #panel{class="tourlobby_left_bottom_block", body=[
                    #br{},
                    #label{class="tourlobby_left_bottom_block_title", body="Turnuva Bilgileri"},
                    #br{},
                    #label{class="tourlobby_left_bottom_block_label", body="Oyun Türü: Tavla"},
                    #br{},
                    #label{class="tourlobby_left_bottom_block_label", body="Kota: 800"}
                ]
             },
        
            %center - three panels with numbers
            #panel{class="tourlobby_orange_plask", body=[
                    #label{class="tourlobby_every_plask_title", body="KATILIMCI SAYISI"},
                    #br{},
                    #label{class="tourlobby_every_plask_label", body="512"}
                ]
            },

            #panel{class="tourlobby_sky_plask", body=[
                    #label{class="tourlobby_every_plask_title", body="BAŞLAMA TARİHİ"},
                    #br{},
                    #label{class="tourlobby_every_plask_label", body="10.02.2012"}
                ]
            },

            #panel{class="tourlobby_blue_plask", body=[
                    #label{class="tourlobby_every_plask_title", body="KATILIMCI SAYISI"},
                    #br{},
                    #label{class="tourlobby_every_plask_label", body="73:49:11"}
                ]
            },

            %prizes
            #panel{class="tourlobby_prizes", body=[
                    #panel{class="tourlobby_prize_1", body=[
                            "<center>",
                            #image{style="width:120px; height:130px;", image="http://www.enilginc.com/images/products/00/08/45/845_buyuk.jpg"},
                            #br{},
                            #label{style="font-size:12px; color:#000;", body="Zamanlay"},
                            "</center>",
                            #panel{class="tourlobby_prize_star_1", body=
                                #label{class="tourlobby_prize_star_text", body="1"}
                            }
                        ]
                    },
                    #panel{class="tourlobby_prize_2", body=[
                            "<center>",
                            #image{style="width:120px; height:130px;", image="http://www.enilginc.com/images/products/00/02/12/212_buyuk.jpg"},
                            #br{},
                            #label{style="font-size:12px; color:#000;", body="Dart Vader Saat"},
                            "</center>",
                            #panel{class="tourlobby_prize_star_2", body=
                                #label{class="tourlobby_prize_star_text", body="2"}
                            }
                        ]
                    },
                    #panel{class="tourlobby_prize_3", body=[
                            "<center>",
                            #image{style="width:120px; height:130px;", image="http://www.enilginc.com/images/products/00/07/31/731_buyuk.jpg"},
                            #br{},
                            #label{style="font-size:12px; color:#000;", body="Kamera Kalemt"},
                            "</center>",
                            #panel{class="tourlobby_prize_star_3", body=
                                #label{class="tourlobby_prize_star_text", body="3"}
                            }
                        ]
                    }
                ]
             },


            %chat
            #panel{class="tourlobby_chat", body=[
                    #panel{class="tourlobby_chat_panel", body=[
                            #label{class="tourlobby_chat_title", body="CHAT"}
                        ]
                    },
                    %chat window
                    #panel{class="tourlobby_chat_window", body=[
                            "<b>Someone:</b> vsem chmoke v etom chate",
                            #br{},
                            "<b>Another:</b> English please.",
                            #br{},
                            "<b>Someone:</b> nyet!",
                            #br{},
                            "<b>Another:</b> Ok then."
                        ]
                    },
                    #textarea{id=chat_input, class="tourlobby_chat_textarea"},
                    #link{postback=chat_input_pressed, class="tourlobby_chat_button", text="Gönder"}
                ]
            },

            %players table
            #panel{class="tourlobby_table_panel", body=[
                    #table{class="tourlobby_table", rows=[
                        #tablerow{class="tourlobby_table_head", cells=[
                            #tableheader{style="padding-left:16px;", text="KULLANICHI"},
                            #tableheader{style="text-align:center;", text="TOPLAM PUAN"},
                            #tableheader{style="text-align:center;", text="YETENEK PUANI"},
                            #tableheader{style="text-align:center;", text="DURUM"}
                        ]},
                        #tablerow{cells=[
                            #tablecell{colspan=4, class="tourlobby_table_arrow", body=[
                                #link{postback=arrow_up, body=#image{image="/images/tournament/lobby/arrow_up.png"}}
                            ]}
                        ]},
                        [[
                            #tablerow{style="background-color:#888c8d; height:1px;", cells=[#tablecell{colspan=4, body=[]}]},

                            case I of
                                1 ->
                                    user_table_row("demo1", 1942, 524, red);
                                2 ->
                                    user_table_row("sustel", 3241, 836, green);
                                3 ->
                                    user_table_row("ahmettez", 3123, 867, green);
                                4 ->
                                    user_table_row("maxim", 1645, 234, red);
                                _ ->
                                    user_table_row("alice", 1345, 456, green)
                            end
                        ] || I <- lists:seq(1,5)],

                        #tablerow{style="background-color:#888c8d; height:1px;", cells=[#tablecell{colspan=4, body=[]}]},

                        #tablerow{cells=[
                            #tablecell{colspan=4, class="tourlobby_table_arrow",  body=[
                                #link{postback=arrow_down, body=#image{image="/images/tournament/lobby/arrow_down.png"}}
                            ]}
                        ]}
                    ]}
                ]
            },
            ""
        ]}
    ].


user_table_row(UId, P1, P2, Color) ->
    RealName = nsm_users:user_realname(UId),
    Avatar = avatar:get_avatar_by_username(UId, small),
    URL = site_utils:user_link(UId),

    #tablerow{cells=[
        #tablecell{body=[
            #singlerow{cells=[
                #tablecell{style="padding: 10px 16px 10px 16px;", body=#image{image=Avatar} },
                #tablecell{body=[
                    #link{style="font-weight:bold;", url=URL, text=UId},
                    " &mdash; ",
                    RealName
                ]}
            ]}
        ]},
        #tablecell{style="text-align:center;", body=[
            integer_to_list(P1)
        ]},
        #tablecell{style="text-align:center;", body=[
            integer_to_list(P2)
        ]},
        #tablecell{style="text-align:center;", body=[
            case Color of
                red ->
                    #image{image="/images/tournament/lobby/red_bullet.png"};
                _ ->
                    #image{image="/images/tournament/lobby/green_bullet.png"}
            end
        ]}
    ]}.


event(Any)->
    webutils:event(Any).

