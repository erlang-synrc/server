%% -*- mode: nitrogen -*-
-module (tournament_lobby).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("nsm_db/include/common.hrl").
-include_lib("nsm_db/include/user.hrl").
-include_lib("nsm_db/include/accounts.hrl").
-include_lib("nsm_db/include/tournaments.hrl").
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
                {ok, {Gift, _}} -> {Gift#gift.gift_name, Gift#gift.image_small_url}
            end || GO <- GOs];
        false ->
            [{"?", "/images/tournament/new_tournament/question.png"},{"?", "/images/tournament/new_tournament/question.png"},{"?", "/images/tournament/new_tournament/question.png"}]
    end,
    {PN1, PI1} = hd(Prizes),
    {PN2, PI2} = hd(tl(Prizes)),
    {PN3, PI3} = hd(tl(tl(Prizes))),

    URL = lists:concat([?_U("/client"),"/","okey","/id/", Id]),
    AttachTourAction = #event{type=click, actions=webutils:new_window_js(URL)},

    case nsm_tournaments:chat_history(Id) of
        H when is_list(H) ->
            add_chat_history(H);
        _ ->
            ok
    end,

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
                #link{class="tourlobby_orange_button", text="TURNUVAYA KATIL", postback=join_tournament},
                #br{},
                #link{postback=red_button, class="tourlobby_red_button", text="TURNUVADAN AYRIL"},
                #br{},
                #link{postback=yellow_button, class="tourlobby_yellow_button", text="TURNUVADAN GİT"},
                #br{},
                #link{text=?_T("ATTACH"), actions=AttachTourAction},
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
                #label{class="tourlobby_every_plask_label", body=Date}
            ]
        },

        #panel{class="tourlobby_blue_plask", body=[
                #label{class="tourlobby_every_plask_title", body="KALAN ZAMAN"},
                #br{},
                #label{class="tourlobby_every_plask_label", body=Time}
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
                #textbox{id=message_text_box, class="tourlobby_chat_textarea"},
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
                    || {UId, _S1, _S2, Color} <- Users]
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
                    user_table_row(Name, Score1, Score2, Color, N)
                ] || {{Name, Score1, Score2, Color}, N} <- NdUsers]
            ]},
            #link{class="tourlobby_view_mode_link", text=?_T("Short view"), postback={change_view, short}}]
    end.

user_table_row(UId, P1, P2, Color, N) ->
    RealName = nsm_users:user_realname(UId),
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
        {delivery, ["tournament", TournamentId, User, "start_game"], Data}  ->
            ?INFO("(in comet): start game TId: ~p, User: ~p, Data: ~p", [TournamentId, User, Data]),
            Id = 10, % Game Id
            Url = lists:concat([?_U("/client"), "/", ?_U("okey"), "/id/", Id]),
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


get_tour_fake_user_list() ->
    [begin
        {ok, Score1} = nsm_accounts:balance(wf:user(), ?CURRENCY_GAME_POINTS),
        {ok, Score2} = nsm_accounts:balance(wf:user(), ?CURRENCY_KAKUSH),
        {UId, Score1, Score2, 
        case length(UId) rem 3 of
            0 -> red;
            1 -> green;
            2 -> yellow
        end}
    end || #user{username=UId} <- lists:flatten([nsm_db:all(user) || _N <- lists:seq(1, 3)])].

get_tour_user_list() ->
    TID = wf:state(tournament_id),
    ActiveUsers       = nsm_tournaments:active_users(TID),
    NotActiveUsers    = not_active_users(TID, ActiveUsers),
    AUL = [
        begin
            {ok, Score1} = nsm_accounts:balance(UId, ?CURRENCY_GAME_POINTS),
            {ok, Score2} = nsm_accounts:balance(UId, ?CURRENCY_KAKUSH),
            {UId, Score1, Score2, green}
        end
    || #user{username=UId} <- ActiveUsers],
    NUL = [
        begin
            {ok, Score1} = nsm_accounts:balance(UId, ?CURRENCY_GAME_POINTS),
            {ok, Score2} = nsm_accounts:balance(UId, ?CURRENCY_KAKUSH),
            {UId, Score1, Score2, red}
        end
    || #user{username=UId} <- NotActiveUsers],
    case lists:sum([1 || {UId, _, _, _} <- AUL, UId == wf:user()] ++ [1 || {UId, _, _, _} <- NUL, UId == wf:user()]) of
        0 ->
            {ok, Score1} = nsm_accounts:balance(wf:user(), ?CURRENCY_GAME_POINTS),
            {ok, Score2} = nsm_accounts:balance(wf:user(), ?CURRENCY_KAKUSH),
            lists:usort(AUL++NUL++[{wf:user(), Score1, Score2, yellow}]);
        _ ->
            lists:usort(AUL++NUL)
    end.


update_userlist() ->
    wf:update(players_table, user_table(get_tour_user_list())),
    wf:flush().


not_active_users(TID, ActiveUsers) ->
    CurrentUser = wf:user(),
    %% get play records for tournament
    AllUsers = nsm_tournaments:joined_users(TID),

    AllUsersCount = length(AllUsers),
    wf:update(user_count, wf:to_list(AllUsersCount)),

    ActiveKeysSet0 = sets:from_list([U#user.username    || U <- ActiveUsers]),
    AllKeysSet     = sets:from_list([PR#play_record.who || PR <- AllUsers]),

    %% add user to active list
    ActiveKeysSet = sets:add_element(CurrentUser, ActiveKeysSet0),
    NotActiveKeys = sets:subtract(AllKeysSet, ActiveKeysSet),

    lists:foldl(
        fun(User, Acc) ->
            case nsm_users:get_user(User) of
                {ok, U} ->
                    [U | Acc];
                _ ->
                    ?WARNING("user not forund: ~p", [User]),
                    Acc
            end
        end, [], sets:to_list(NotActiveKeys)).


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
    update_userlist();    

event(Any)->
    webutils:event(Any).


get_tournament(TrnId) ->
    Check = fun(undefined, _Value) -> true;
               (Param, Value) ->  Param == Value
            end,
    Cursor = fun() ->
                     qlc:cursor(qlc:q([V || {{_,_,_K},_,V=#game_table{trn_id=TId}
                                                <- gproc:table(props),
                                            Check(TrnId, TId),
             end,
    Tables = qlc:next_answers(Cursor(), 1),
    ?INFO("~w:get_tournament Table = ~p", [?MODULE, Tables]),
    Tables.
