-module (tournament_lobby).
-author('Alexander Kalenuk <akalenuk@synrc.com>').
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("nsm_db/include/common.hrl").
-include_lib("nsm_db/include/user.hrl").
-include_lib("nsm_db/include/accounts.hrl").
-include_lib("nsm_db/include/tournaments.hrl").
-include_lib("nsm_db/include/table.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include("setup.hrl").
-include("common.hrl").


%% time to wait after heartbeat request to update userlist
-define(SLEEP, 5000). 
-define(MAX_CHAT_LENGTH, 1024). % 1024 bytes
-define(COMET_POOL, tournament_lobby).
-define(MAX_USERS_TO_SWITH_MODE, 32).

main() ->
    case wf:user() /= undefined of
        true  -> main_authorized();
        false -> wf:redirect_to_login(?_U("/login"))
    end.

main_authorized() ->
    TournamentId = wf:q(id),
%    UserInfo = webutils:user_info(),
    wf:state(tournament_id, TournamentId),

    %% user became ready automatically
%    nsx_util_notification:notify_tournament_user_ready(TournamentId, UserInfo),

    TournamentInfo = nsm_tournaments:get(TournamentId),
    wf:state(tournament, TournamentInfo),

    start_comet(),

    webutils:js_for_main_authorized_game_stats_menu(),
    webutils:add_to_head({raw,              % this goes to styles.css. Still here for convenience of editing
    "
        <style media='screen' type='text/css'>
            .tourlobby_title {
                width:250px; 
                height:43px; 
                background: url(/images/tournament/lobby/top_plask.png);
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

            .tourlobby_orange_button_disabled {
                display:block; width:190px; height:41px; background: url(/images/tournament/lobby/btn_orange_disabled.png);
                font-size:16px; color:#363638; line-height:42px;
            }

            .tourlobby_orange_button_disabled:hover {
                text-decoration:none;
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

            .tourlobby_red_button_disabled {
                display:block; width:190px; height:41px; background: url(/images/tournament/lobby/btn_red_disabled.png);
                font-size:16px; color:#121212; line-height:42px;
            }

            .tourlobby_red_button_disabled:hover {
                text-decoration:none;
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

            .tourlobby_yellow_button_disabled {
                display:block; width:190px; height:41px; background: url(/images/tournament/lobby/btn_yellow_disabled.png);
                font-size:16px; color:#363638; line-height:42px;
                text-decoration:none;
            }

            .tourlobby_yellow_button_disabled:hover {
                text-decoration:none;
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
                width:640px; height:100px; background-color:#fff;
                position:absolute; left:10px; top:36px;
                font-size:14px; line-height:20px;
                overflow:auto;
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
                width:958px;
                margin-left:-24px;
                margin-top:575px;
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
                font:18px 'Gotham Rounded Bold','Trebuchet MS'; line-height:55px;
                text-shadow:0 1px 1px #353535;
            }

            .tourlobby_view_mode_link {
                float:right; 
                text-decoration:none;
                margin:10px;
                font-size:16px;
            }
            .tourlobby_view_mode_link:hover {
                text-decoration:none;
            }
            .tourlobby_view_mode_link:active {
                text-decoration:none;
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
    {ok, User} = nsm_users:get_user(wf:user()), % to check if admin  

    Id = list_to_integer(wf:q("id")),
    {ok, T} = nsm_db:get(tournament, Id),
    Title = T#tournament.name,
    Game = case T#tournament.game_type of
        game_okey -> "OKEY";
        game_tavla -> "TAVLA";
        game_batak -> "BATAK";
        _ -> "WTF"
    end,
    Date = integer_to_list(element(3, T#tournament.start_date)) ++ "." ++ 
           integer_to_list(element(2, T#tournament.start_date)) ++ "." ++ 
           integer_to_list(element(1, T#tournament.start_date)),

    wf:state(tour_start_time, T#tournament.start_time),
    Timer = case date() == T#tournament.start_date of
        false ->
            DDays = calendar:date_to_gregorian_days(T#tournament.start_date) - calendar:date_to_gregorian_days(date()),
            case DDays of
                1 -> "1 " ++ ?_T("day");
                N -> 
                    case N>0 of
                        true -> integer_to_list(N) ++ " " ++ ?_T("days");
                        false -> ?_T("FINISHED")
                    end
            end;
        true ->
            wf:wire(#event{type=timer, delay=1000, postback=change_timer}),
            get_timer_for_now()
    end,
    Time = integer_to_list(element(1, T#tournament.start_time)) ++ ":" ++ 
           integer_to_list(element(2, T#tournament.start_time)) ++
           case element(2, T#tournament.start_time) of 
                0 -> "0";
                _ -> ""
           end,
    NPlayers = T#tournament.players_count,
    Quota = T#tournament.quota,
    Prizes = case is_list(T#tournament.awards) of
        true ->
            GOs = [nsm_gifts_db:get_gift(A) || A <- T#tournament.awards],
            [case GO of
                {error, notfound} -> {"?", "/images/tournament/new_tournament/question.png"};
                {ok, {Gift, _}} -> {gifts:decode_letters(Gift#gift.gift_name), Gift#gift.image_small_url}
            end || GO <- GOs];
        false ->
            [{"?", "/images/tournament/new_tournament/question.png"},{"?", "/images/tournament/new_tournament/question.png"},{"?", "/images/tournament/new_tournament/question.png"}]
    end,
    {PN1, PI1} = hd(Prizes),
    {PN2, PI2} = hd(tl(Prizes)),
    {PN3, PI3} = hd(tl(tl(Prizes))),

    TourId = get_tournament(Id),
    wf:session(TourId,TourId),

    wf:state(tour_long_id, TourId),

    case nsm_tournaments:chat_history(Id) of
        H when is_list(H) ->
            add_chat_history(H);
        _ ->
            ok
    end,

    % is user joined already
    JoinedList = [P#play_record.who || P <- nsm_tournaments:joined_users(Id)],
    UserJoined = lists:member(wf:user(), JoinedList),

    [  
        #panel{class="tourlobby_title", body=[
            #label{class="tourlobby_title_label", body="TURNUVA LOBY"}
        ]},


        % left top block
        #panel{class="tourlobby_left_top_block", body=[
                "<center>",
                #label{class="tourlobby_left_top_block_label", body=Title},
                #br{},
                #image{image="/images/tournament/lobby/tour_avatar.png"},
                #br{},
                #br{},
                case UserJoined of
                    true ->
                        #panel{id=join_button, class="tourlobby_orange_button_disabled", text="TURNUVAYA KATIL"};
                    false ->
                        #link{id=join_button, class="tourlobby_orange_button", text="TURNUVAYA KATIL", postback=join_tournament}
                end,
                #br{},
                #panel{id=leave_button, class="tourlobby_red_button_disabled", text="TURNUVADAN AYRIL"},
                #br{},
                case TourId of
                    "" ->
                        #panel{id=attach_button, class="tourlobby_yellow_button_disabled", text=?_T("TAKE MY SEAT")};
                    _ ->
                        #link{id=attach_button, class="tourlobby_yellow_button", text=?_T("TAKE MY SEAT"), postback=attach}
                end,
                #br{},
                case T#tournament.creator == wf:user() of
                     true ->
                        case TourId of
                            "" ->
%                                case nsm_acl:check_access(User, {feature, admin}) of
                                    allow -> #link{id=start_button, text=?_T("MANUAL START"), postback={start_tour, Id, NPlayers}};
%                                    _ -> ""
%                                end;
                            _ -> ""
                        end;
                    _ -> ""
                end,
                "</center>"
            ]
        },

        %left bottom block
        #panel{class="tourlobby_left_bottom_block", body=[
                #br{},
                #label{class="tourlobby_left_bottom_block_title", body="Turnuva Bilgileri"},
                #br{},
                #label{class="tourlobby_left_bottom_block_label", body="Oyun Türü: " ++ Game},
                #br{},
                #label{class="tourlobby_left_bottom_block_label", body="Kota: " ++ integer_to_list(Quota)}
            ]
         },
    
        %center - three panels with numbers
        #panel{class="tourlobby_orange_plask", body=[
                #label{class="tourlobby_every_plask_title", body="KATILIMCI SAYISI"},
                #br{},
                #label{class="tourlobby_every_plask_label", body=integer_to_list(NPlayers)}
            ]
        },

        #panel{class="tourlobby_sky_plask", body=[
                #label{class="tourlobby_every_plask_title", body="BAŞLAMA TARİHİ"},
                #br{},
                #link{body=#label{class="tourlobby_every_plask_label", body=Date}, style="text-decoration:none", title=Time}
            ]
        },

        #panel{class="tourlobby_blue_plask", body=[
                #label{class="tourlobby_every_plask_title", body="KALAN ZAMAN"},
                #br{},
                #label{id=lobby_timer, class="tourlobby_every_plask_label", body=Timer}
            ]
        },

        %prizes
        #panel{class="tourlobby_prizes", body=[
                #panel{class="tourlobby_prize_1", body=[
                        "<center>",
                        #image{style="width:120px; height:130px;", image=PI1},
                        #br{},
                        #label{style="font-size:12px; color:#000;", body=PN1},
                        "</center>",
                        #panel{class="tourlobby_prize_star_1", body=
                            #label{class="tourlobby_prize_star_text", body="1"}
                        }
                    ]
                },
                #panel{class="tourlobby_prize_2", body=[
                        "<center>",
                        #image{style="width:120px; height:130px;", image=PI2},
                        #br{},
                        #label{style="font-size:12px; color:#000;", body=PN2},
                        "</center>",
                        #panel{class="tourlobby_prize_star_2", body=
                            #label{class="tourlobby_prize_star_text", body="2"}
                        }
                    ]
                },
                #panel{class="tourlobby_prize_3", body=[
                        "<center>",
                        #image{style="width:120px; height:130px;", image=PI3},
                        #br{},
                        #label{style="font-size:12px; color:#000;", body=PN3},
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
                #panel{id=chat_history, class="tourlobby_chat_window", body=[
                    ]
                },
                #textbox{id=message_text_box, class="tourlobby_chat_textarea", postback=chat},
                #link{id=chat_send_button, class="tourlobby_chat_button", text="Gönder", postback=chat}
            ]

        },

        %players table
        #panel{id=players_table, class="tourlobby_table_panel", body=[
            user_table(get_tour_user_list())
        ]},
        ""
    ].


user_table(Users) ->
    case wf:session(tourlobby_view_mode) of
        short ->
            [#panel{style="font-size:16px; line-height:24px; margin-left:25px; margin-right:25px; text-align:justify;", body = [
                [#span{style="font-size:24px; font-weight:bold;", body=[?_T("Players"), ": "]},
                    [begin
                        URL = site_utils:user_link(UId),
                        #span{body=#link{url=URL, text=UId ++ " ",
                            style = "font-weight:bold; margin-right:5px;" ++ case Color of
                                yellow ->  "color:#938b03;";
                                red -> "color:#c22323;";
                                green -> "color:#5ba108;"
                            end}
                        }
                    end
                    || {UId, _S1, _S2, Color, _} <- Users]
                ]
            ]},
            #link{class="tourlobby_view_mode_link", text=?_T("Full view"), postback={change_view, full}}];
        _ ->
            NdUsers = [{lists:nth(N, Users), N} || N <- lists:seq(1, length(Users))],
            [#table{class="tourlobby_table", rows=[
                #tablerow{class="tourlobby_table_head", cells=[
                    #tableheader{style="padding-left:16px;", text="KULLANICHI"},
                    #tableheader{style="text-align:center;", text="TOPLAM PUAN"},
                    #tableheader{style="text-align:center;", text="YETENEK PUANI"},
                    #tableheader{style="text-align:center;", text="DURUM"}
                ]},
                [[
                    user_table_row(Name, Score1, Score2, Color, N, RealName)
                ] || {{Name, Score1, Score2, Color, RealName}, N} <- NdUsers]
            ]},
            #link{class="tourlobby_view_mode_link", text=?_T("Short view"), postback={change_view, short}}]
    end.

user_table_row(UId, P1, P2, Color, N, RealName) ->
%    RealName = nsm_users:user_realname(UId),
    Avatar = avatar:get_avatar_by_username(UId, tiny),
    URL = site_utils:user_link(UId),

    #tablerow{cells=[
        #tablecell{body=[
            #singlerow{cells=[
                #tablecell{style="padding: 5px 5px 5px 16px;", body=#image{image=Avatar} },
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
                green ->
                    #image{image="/images/tournament/lobby/green_bullet.png"};
                _ ->
                    #image{image="/images/tournament/lobby/yellow_bullet.png"}
            end
        ]}
    ], style = case N rem 2 of
        0 -> "";
        1 -> "background-color:#f5f5f5;"
    end}.



add_chat_history(Messages) ->
    [process_chat(Action, User, Message) || {User, Action, Message} <- Messages].

process_chat("message", User, Message) ->
    chat_new_msg(User, Message).


chat_info(Info) ->
    Terms = #panel{class="info", body = Info},
    update_table_chat(Terms).

chat_error(Message) ->
    chat_info(#span{class=error, text= Message}).

chat_user_in(Username) ->
    Terms = #panel{class="user join",
		   body = [
			   ?_TS("User $username$ connected.", [{username, Username}])
			  ]},
    update_table_chat(Terms).

chat_user_out(Username) ->
    Terms = #panel{class="user left",
		   body = [
			   ?_TS("User $username$ has left.", [{username, Username}])
			  ]},
    update_table_chat(Terms).

chat_new_msg(User, Message) ->
    Terms = #panel{class="chat",
        body = [
            #span{class="username", style="font-weight:bold;", text = User},
            ":&nbsp;", #span{text = Message}
    ]},
    update_table_chat(Terms).


update_table_chat(Terms) ->
    wf:insert_bottom(chat_history, Terms),
    wf:wire("obj('chat_history').scrollTop = obj('chat_history').scrollHeight;"),
    wf:flush(),
    ok.


start_comet() ->
    User = wf:user(),
    TournamentId = wf:state(tournament_id),
    {ok, Pid} = wf:comet(fun()->
        CometProcess = self(),

        %% TODO: error handling when unable to subscribe
        (catch nsx_util_notification:subscribe_for_tournament(TournamentId, User, CometProcess)),
        comet_update(wf:user(), wf:state(tournament_id))
    end,  ?COMET_POOL),
    wf:state(comet_pid, Pid).


comet_update(User, TournamentId) ->
    receive
        {delivery, _, tournament_heartbeat} ->
            UserRecord = webutils:user_info(),

            nsx_util_notification:notify_tournament_heartbeat_reply(
                TournamentId, UserRecord),

            %% afer sleep send update userlist message to self
            timer:apply_after(?SLEEP, erlang, send, [self(), update_userlist]);

        %% start game section
        {delivery, ["tournament", TournamentId, "start"], {TourId}}  ->
            wf:session(TourId,TourId),
            wf:state(tour_long_id, TourId),
            wf:replace(attach_button, #link{id=attach_button, class="tourlobby_yellow_button", text=?_T("TAKE MY SEAT"), postback=attach}),
            wf:replace(start_button, ""),
            ?INFO(" +++ (in comet): start game TId: ~p, User: ~p, Data: ~p", [TournamentId, User, TourId]),
            Url = lists:concat([?_U("/client"), "/", ?_U("okey"), "/id/", TourId]),
            StartClient = webutils:new_window_js(Url),
            wf:wire(#script{script=StartClient}),
            wf:flush();

        %% chat section
        {delivery, ["tournament", TournamentId, "chat", _Action], {UserName, Action, Message}}  ->
            process_chat(Action, UserName, Message);

        %% local request
        update_userlist ->
            update_userlist();

        {delivery, Route, Other}  ->
            ?PRINT({other_in_comet, Route, Other, wf:user()})
    end,
    comet_update(User, TournamentId).

update_userlist() ->
    wf:update(players_table, user_table(get_tour_user_list())),
    wf:flush().

get_tour_user_list() ->
    TID = wf:state(tournament_id),
    ActiveUsers = sets:from_list([U#user.username || U <- nsm_tournaments:active_users(TID)]),
    JoinedUsers = sets:from_list([U#play_record.who || U <- nsm_tournaments:joined_users(TID)]),
    List = [begin 
               S1 = case nsm_accounts:balance(U, ?CURRENCY_GAME_POINTS) of
                         {ok,AS1} -> AS1;
                         {error,_} -> 0 end,
               S2 = case nsm_accounts:balance(U,  ?CURRENCY_KAKUSH) of
                         {ok,AS2} -> AS2;
                         {error,_} -> 0 end,
               ?INFO("User: ~p",[U]),
               {U,S1,S2,
                     case sets:is_element(U,JoinedUsers) of
                          false -> yellow;
                          true -> case sets:is_element(U,ActiveUsers) orelse wf:user() == U of
                                        true -> green;
                                        false -> red
                                end
                     end, nsm_users:user_realname(U)}
    end || U <- sets:to_list(sets:union(JoinedUsers,ActiveUsers)) ++ 
                case sets:is_element(wf:user(),JoinedUsers) of true -> []; false -> [wf:user()] end],
    lists:usort(List).

event(chat) ->
    User = wf:user(),
    TID = wf:state(tournament_id),
    Msg = wf:q(message_text_box),
    case string:strip(Msg) of
        "" ->
            ok;        % dont send empty messages
        Message ->
            case length(Message) > ?MAX_CHAT_LENGTH of
               true ->
                    chat_info(#span{class=error, text= ?_T("Message too long.")});
                false ->
                    wf:set(message_text_box, ""),
                    wf:wire("obj('message_text_box').focus();"),
                    nsx_util_notification:notify_tournament_chat(TID, "message", User, Msg)
           end,
           wf:flush()
    end;

event({change_view, Mode}) ->
    wf:session(tourlobby_view_mode, Mode),
    update_userlist();

event(join_tournament) ->
    User = wf:user(),
    TID = wf:state(tournament_id),
    nsm_tournaments:join(User, list_to_integer(TID)),
    wf:replace(join_button, #panel{id=join_button, class="tourlobby_orange_button_disabled", text="TURNUVAYA KATIL"}),
    update_userlist();    

event({start_tour, Id, NPlayers}) ->
    TourId = nsw_srv_sup:start_tournament(Id, 1, NPlayers),
    wf:replace(attach_button, #link{id=attach_button, class="tourlobby_yellow_button", text=?_T("TAKE MY SEAT"), postback=attach}),
    wf:replace(start_button, ""),
    wf:state(tour_long_id,TourId);

event(attach) ->
    TourId = wf:state(tour_long_id),
    ?INFO("TourId: ~p",[TourId]),
    case TourId of 
         [] ->  
            wf:wire(#alert{text=?_T("Please wait for Tournament start or Start it Mannually.")});
         _ ->
            URL = lists:concat([?_U("/client"),"/","okey","/id/", TourId]),
            StartClient = webutils:new_window_js(URL),
            wf:wire(#script{script=StartClient})
    end;

event(change_timer) ->
    wf:update(lobby_timer, get_timer_for_now()),
    wf:wire(#event{type=timer, delay=1000, postback=change_timer});

event(Any)->
    webutils:event(Any).

str_plus_0(N) ->
    case N<10 of
        true ->
            "0" ++ integer_to_list(N);
        false ->
            integer_to_list(N)
    end.

get_timer_for_now() ->
    TourTime = wf:state(tour_start_time),
    DTime = calendar:time_to_seconds(TourTime) - calendar:time_to_seconds(time()),
    case DTime =< 0 of
        true -> ?_T("NOW");
        false ->
            S = DTime rem 60,
            M = (DTime div 60) rem 60,
            H = (DTime div 3600),
            integer_to_list(H) ++ ":" ++ str_plus_0(M) ++ ":" ++ str_plus_0(S)
    end.

get_tournament(TrnId) ->
    Check = fun(undefined, _Value) -> true;
               (Param, Value) ->  Param == Value
            end,
    Cursor = fun() ->
                     qlc:cursor(qlc:q([V || {{_,_,_K},_, V = #game_table{trn_id=TId}} <- gproc:table(props),
                                            Check(TrnId, TId)]))
             end,
    Table = case qlc:next_answers(Cursor(), 1) of
                   [T] -> X = T#game_table.id, integer_to_list(X);
                     _ -> []
            end,
    ?INFO("~w:get_tournament Table = ~p", [?MODULE, Table]),
    Table.
