-module (tournament_lobby).
-author('Alexander Kalenuk <akalenuk@synrc.com>').
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("nsm_db/include/common.hrl").
-include_lib("nsm_db/include/user.hrl").
-include_lib("nsm_db/include/accounts.hrl").
-include_lib("nsm_db/include/tournaments.hrl").
-include_lib("nsm_db/include/table.hrl").
-include_lib("nsm_db/include/config.hrl").
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
    wf:state(tournament_id, TournamentId),
    wf:state(tournament_int_id, list_to_integer(TournamentId)),

    TournamentInfo = nsm_tournaments:get(TournamentId),
    wf:state(tournament, TournamentInfo),

    start_comet(),

    webutils:js_for_main_authorized_game_stats_menu(),
    webutils:add_to_head({raw,              % this goes to styles.css. Still here for convenience of editing
    "
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
    Tours = T#tournament.tours,
    Game = case T#tournament.game_type of
        game_okey -> "OKEY";
        game_tavla -> "TAVLA";
        game_batak -> "BATAK";
        _ -> ?_T("Unknown")
    end,
    Zone = Id div 1000000,
    GameSrv = "game@srv" ++ integer_to_list(Zone) ++ ".kakaranet.com",
    NodeAtom = case Zone of
                    4 -> nsx_opt:get_env(nsm_db, game_srv_node, 'game@doxtop.cc');
                    _ -> list_to_atom(GameSrv)
               end,
    TourId = rpc:call(NodeAtom,game_manager,get_tournament,[Id]),
    wf:session(TourId,TourId),
    wf:state(tour_long_id, TourId),

    Date = integer_to_list(element(3, T#tournament.start_date)) ++ "." ++ 
           integer_to_list(element(2, T#tournament.start_date)) ++ "." ++ 
           integer_to_list(element(1, T#tournament.start_date)),

    wf:state(tour_start_time, T#tournament.start_time),
    wf:state(tour_start_date, T#tournament.start_date),
    wf:wire(#event{type=timer, delay=1000, postback=change_timer}),
    Timer = get_timer_for_now(),
    Time = integer_to_list(element(1, T#tournament.start_time)) ++ ":" ++ 
           integer_to_list(element(2, T#tournament.start_time)) ++
           case element(2, T#tournament.start_time) of 
                0 -> "0";
                _ -> ""
           end,
    NPlayers = T#tournament.players_count,
    Quota = T#tournament.quota,
    Speed = T#tournament.speed,
    Prizes = case is_list(T#tournament.awards) of
        true ->
            GOs = [nsm_gifts_db:get_gift(A) || A <- T#tournament.awards],
            [case GO of
                {error, notfound} -> {"", "/images/tournament/nothing.png"};
                {ok, {Gift, _}} -> {site_utils:decode_letters(Gift#gift.gift_name), Gift#gift.image_small_url}
            end || GO <- GOs];
        false ->
            [{"?", "/images/tournament/new_tournament/question.png"},{"?", "/images/tournament/new_tournament/question.png"},{"?", "/images/tournament/new_tournament/question.png"}]
    end,
    {PN1, PI1} = hd(Prizes),
    {PN2, PI2} = hd(tl(Prizes)),
    {PN3, PI3} = hd(tl(tl(Prizes))),

    case rpc:call(NodeAtom,nsm_srv_tournament_lobby,chat_history,[Id]) of
        H when is_list(H) ->
            add_chat_history(H);
        _ ->
            ok
    end,

    % is user joined already
    JoinedList = [P#play_record.who || P <- nsm_tournaments:joined_users(Id)],
    UserJoined = lists:member(wf:user(), JoinedList),

    {ok,PlanDesc1} = rpc:call(?GAMESRVR_NODE, game_okey_ng_trn_elim,get_plan_desc,[T#tournament.quota,
                                                                                   T#tournament.players_count,
                                                                                   T#tournament.tours]),
    PlanDesc = ling:join(PlanDesc1," / "),

    [
        #panel{class="tourlobby_title", body=[
            #label{class="tourlobby_title_label", body=?_T("TOURNAMENT LOBBY")}
        ]},

        #panel{body=[#label{style="margin-left:300px;margin-top:10px;font-size:16pt;",body=[lists:flatten(PlanDesc)]}]},


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
                        [
                            #panel{id=join_button, class="tourlobby_orange_button_disabled", text=?_T("JOIN TOURNAMENT")},
                            #br{},
                            #link{id=leave_button, class="tourlobby_red_button", text=?_T("LEAVE TOURNAMENT"), postback=leave_tournament}
                        ];
                    false ->
                        [
                            #link{id=join_button, class="tourlobby_orange_button", text=?_T("JOIN TOURNAMENT"), postback=join_tournament},
                            #br{},
                            #panel{id=leave_button, class="tourlobby_red_button_disabled", text=?_T("LEAVE TOURNAMENT")}
                        ]
                end,
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
                                #link{id=start_button, text=?_T("MANUAL START"), postback={start_tour, Id, NPlayers,Quota,Tours,Speed,T#tournament.awards}};
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
                #label{class="tourlobby_left_bottom_block_title", body=?_T("Tournament Info")},
                #br{},
                #label{class="tourlobby_left_bottom_block_label", body=?_T("Game Type: ") ++ Game},
                #br{},
                #label{class="tourlobby_left_bottom_block_label", body=?_T("Quota: ") ++ integer_to_list(Quota)}
            ]
         },
    
        %center - three panels with numbers
        #panel{class="tourlobby_orange_plask", body=[
                #label{class="tourlobby_every_plask_title", body=?_T("PARTICIPANTS")},
                #br{},
                #label{class="tourlobby_every_plask_label", body=integer_to_list(NPlayers)}
            ]
        },

        #panel{class="tourlobby_sky_plask", body=[
                #label{class="tourlobby_every_plask_title", body=?_T("START DATE")},
                #br{},
                #link{body=#label{class="tourlobby_every_plask_label", body=Date}, style="text-decoration:none", title=Time}
            ]
        },

        #panel{class="tourlobby_blue_plask", body=[
                #label{class="tourlobby_every_plask_title", body=?_T("TIME LEFT")},
                #br{},
                #label{id=lobby_timer, class="tourlobby_every_plask_label", body=Timer}
            ]
        },

        %prizes
        #panel{class="tourlobby_prizes", body=[
                #panel{class="tourlobby_prize_1",body=[
                        "<center>",
                        #panel{
                            style="background-color:888; height:135px; display:table-cell; vertical-align:middle;",
                            body=#image{style="max-width:130px; max-height:130px;", image=PI1}
                        },
                        #label{style="font-size:12px; color:#000;", body=PN1},
                        "</center>",
                        #panel{class="tourlobby_prize_star tourlobby_prize_star_1", body=
                            #label{class="tourlobby_prize_star_text", body="1"}
                        }
                    ]
                },
                #panel{class="tourlobby_prize_2", body=[
                        "<center>",
                        #panel{
                            style="background-color:888; height:135px; display:table-cell; vertical-align:middle;",
                            body=#image{style="max-width:130px; max-height:130px;", image=PI2}
                        },
                        #label{style="font-size:12px; color:#000;", body=PN2},
                        "</center>",
                        #panel{class="tourlobby_prize_star tourlobby_prize_star_2", body=
                            #label{class="tourlobby_prize_star_text", body="2"}
                        }
                    ]
                },
                #panel{class="tourlobby_prize_3", body=[
                        "<center>",
                        #panel{
                            style="background-color:888; height:135px; display:table-cell; vertical-align:middle;",
                            body=#image{style="max-width:130px; max-height:130px;", image=PI3}
                        },
                        #label{style="font-size:12px; color:#000;", body=PN3},
                        "</center>",
                        #panel{class="tourlobby_prize_star tourlobby_prize_star_3", body=
                            #label{class="tourlobby_prize_star_text", body="3"}
                        }
                    ]
                }
            ]
         },


        %chat
        #panel{class="tourlobby_chat", body=[
                #panel{class="tourlobby_chat_panel", body=[
                        #label{class="tourlobby_chat_title", body=?_T("CHAT")}
                    ]
                },
                %chat window
                #panel{id=chat_history, class="tourlobby_chat_window", body=[
                    ]
                },
                #textbox{id=message_text_box, class="tourlobby_chat_textarea", postback=chat},
                #link{id=chat_send_button, class="tourlobby_chat_button", text=?_T("Post"), postback=chat}
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
                    #tableheader{style="padding-left:16px;", text=?_T("USER")},
                    #tableheader{style="text-align:center;", text=?_T("TOTAL SCORE")},
                    #tableheader{style="text-align:center;", text=?_T("SKILL SCORE")},
                    #tableheader{style="text-align:center;", text=?_T("STATUS")}
                ]},
                [[
                    user_table_row(Name, Score1, Score2, Color, N, RealName)
                ] || {{Name, Score1, Score2, Color, RealName}, N} <- NdUsers]
            ]},
            #link{class="tourlobby_view_mode_link", text=?_T("Short view"), postback={change_view, short}}]
    end.

user_table_row(UId, P1, P2, Color, N, RealName) ->
    Avatar = avatar:get_avatar_by_username(UId, tiny),
    URL = site_utils:user_link(UId),

    #tablerow{cells=[
        #tablecell{body=[
            #singlerow{cells=[
                #tablecell{style="padding: 5px 5px 5px 16px;", body=#image{image=Avatar, class=
                    case nsm_accounts:user_paid(UId) of
                        true -> "paid_user_avatar";
                        _ -> ""
                    end
                } },
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
        (catch nsx_msg:subscribe_for_tournament(TournamentId, User, CometProcess)),
        comet_update(wf:user(), wf:state(tournament_id))
    end,  ?COMET_POOL),
    wf:state(comet_pid, Pid).


comet_update(User, TournamentId) ->
    receive
        {delivery, _, tournament_heartbeat} ->
            UserRecord = webutils:user_info(),

            nsx_msg:notify_tournament_heartbeat_reply(
                TournamentId, UserRecord),

            %% afer sleep send update userlist message to self
            timer:apply_after(?SLEEP, erlang, send, [self(), update_userlist]);

        %% start game section
        {delivery, ["tournament", TournamentId, "start"], {TourId}}  ->
            wf:session(TourId,TourId),
            wf:state(tour_long_id, TourId),
            wf:replace(attach_button, #link{id=attach_button, class="tourlobby_yellow_button", text=?_T("TAKE MY SEAT"), postback=attach}),
            wf:replace(start_button, ""),
            ?INFO("(in comet): start game TId: ~p, User: ~p, Data: ~p", [TournamentId, User, TourId]),
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
    TId = list_to_integer(TID),
    Zone = TId div 1000000,
    GameSrv = "game@srv" ++ integer_to_list(Zone) ++ ".kakaranet.com",
    NodeAtom = case Zone of
                    4 -> nsx_opt:get_env(nsm_db, game_srv_node, 'game@doxtop.cc');
                    _ -> list_to_atom(GameSrv)
               end,
%    ?INFO("NodeAtom: ~p",[NodeAtom]),
    ActiveUsers = sets:from_list([U#user.username || U <- rpc:call(NodeAtom,nsm_srv_tournament_lobby,active_users,[TID])]),
    JoinedUsers = sets:from_list([U#play_record.who || U <- nsm_tournaments:joined_users(TID)]),
    List = [begin 
               S1 = case nsm_accounts:balance(U, ?CURRENCY_GAME_POINTS) of
                         {ok,AS1} -> AS1;
                         {error,_} -> 0 end,
               S2 = case nsm_accounts:balance(U,  ?CURRENCY_KAKUSH) of
                         {ok,AS2} -> AS2;
                         {error,_} -> 0 end,
%               ?INFO("User: ~p",[U]),
               {U,S1,S2,
                     case sets:is_element(U,JoinedUsers) of
                          false -> yellow;
                          true -> case sets:is_element(U,ActiveUsers) orelse wf:user() == U of
                                        true -> green;
                                        false -> red
                                end
                     end, site_utils:decode_letters(nsm_users:user_realname(U))}
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
                    nsx_msg:notify_tournament_chat(TID, "message", User, Msg)
           end,
           wf:flush()
    end;

event({change_view, Mode}) ->
    wf:session(tourlobby_view_mode, Mode),
    update_userlist();

event(join_tournament) ->
    UId = wf:user(),
    TId = wf:state(tournament_id),

    FreeTournaments = ling:split(ling:clean(ling:clean(nsm_db:get_config("tournaments/free", ""), " "), "\""), ","),
    case nsm_accounts:user_paid(UId) of
        true ->
            event(actualy_join_tournament);
        false ->
            case lists:member(TId, FreeTournaments) of
                true ->
                    AllTournaments = nsm_db:all(tournament),
                    case lists:member(true, [nsm_tournaments:user_joined(ATId, UId) || #tournament{id = ATId} <- AllTournaments]) of
                        true ->
                            wf:wire(#alert{text=?_T("Sorry, only paid users can play two tournaments in a time.")});
                        _ ->
                            event(actualy_join_tournament)
                    end;
                false -> 
                    wf:wire(#alert{text=?_T("Sorry, only paid users can join this tournament.")})
            end
    end;

event(actualy_join_tournament) ->
    UId = wf:user(),
    TId = wf:state(tournament_id),
    nsx_msg:notify(["system", "tournament_join"], {UId, list_to_integer(TId)}),
%    nsm_tournaments:join(UId, list_to_integer(TId)),
    wf:replace(join_button, #panel{id=join_button, class="tourlobby_orange_button_disabled", text=?_T("JOIN TOURNAMENT")}),
    wf:replace(leave_button, #link{id=leave_button, class="tourlobby_red_button", text=?_T("LEAVE TOURNAMENT"), postback=leave_tournament}),
    update_userlist();

event(leave_tournament) ->
    UId = wf:user(),
    TId = wf:state(tournament_id),
    nsx_msg:notify(["system", "tournament_remove"], {UId, list_to_integer(TId)}),
%    nsm_tournaments:remove(UId, list_to_integer(TID)),
    wf:replace(join_button, #link{id=join_button, class="tourlobby_orange_button", text=?_T("JOIN TOURNAMENT"), postback=join_tournament}),
    wf:replace(leave_button, #panel{id=leave_button, class="tourlobby_red_button_disabled", text=?_T("LEAVE TOURNAMENT")}),
    update_userlist();

event({start_tour, Id, NPlayers,Q,T,S,P}) ->
    wf:state(tour_start_time, time()),
    Zone = Id div 1000000,
    GameSrv = "game@srv" ++ integer_to_list(Zone) ++ ".kakaranet.com",
    NodeAtom = case Zone of
                    4 -> nsx_opt:get_env(nsm_db, game_srv_node, 'game@doxtop.cc');
                    _ -> list_to_atom(GameSrv)
               end,
    TourId = rpc:call(NodeAtom, game_manager,start_tournament,[Id, 1, NPlayers,Q,T,S,P]),
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
    Id = list_to_integer(wf:q("id")),
    {ok, T} = nsm_db:get(tournament, Id),
    case T#tournament.status of
        canceled -> ?_T("CANCELED");
        _ ->
            TourTime = wf:state(tour_start_time),
            TourDate = wf:state(tour_start_date),
            DDays = calendar:date_to_gregorian_days(T#tournament.start_date) - calendar:date_to_gregorian_days(date()),
            case DDays of
                1 -> "1 " ++ ?_T("day");
                N -> 
                    case N>0 of
                        true -> integer_to_list(N) ++ " " ++ ?_T("days");
                        false -> 
                            DTime = case date() == TourDate of
                                true -> 
                                    case wf:state(tour_long_id) of 
                                        [] -> calendar:time_to_seconds(TourTime) - calendar:time_to_seconds(time());
                                        _ -> 0  % started tournament is always either NOW or FINISHED
                                    end;
                                false ->
                                    0
                            end,
                            case DTime =< 0 of
                                true -> 
                                    TId = wf:state(tournament_int_id),
                                    Zone = TId div 1000000,
                                    GameSrv = "game@srv" ++ integer_to_list(Zone) ++ ".kakaranet.com",
                                    NodeAtom = case Zone of
                                               4 -> nsx_opt:get_env(nsm_db, game_srv_node, 'game@doxtop.cc');
                                               _ -> list_to_atom(GameSrv)
                                    end,
                                    case rpc:call(NodeAtom, game_manager,get_tournament,[TId]) of
                                        [] -> ?_T("FINISHED");
                                        _ -> ?_T("NOW")
                                    end;
                                false ->
                                    S = DTime rem 60,
                                    M = (DTime div 60) rem 60,
                                    H = (DTime div 3600),
                                    integer_to_list(H) ++ ":" ++ str_plus_0(M) ++ ":" ++ str_plus_0(S)
                            end
                    end
            end
    end.

