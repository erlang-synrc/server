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
-define(JOINEDPERPAGE, 32). 

main() ->
    case wf:user() /= undefined of
        true  -> main_authorized();
        false -> wf:redirect_to_login(?_U("/login"))
    end.

main_authorized() ->
    wf:session(this_must_be_unique_joined_page, 1),
    TID = wf:q(id),
    wf:state(tournament_id, TID),
    webutils:add_raw("<link href='/nitrogen/guiders-js/guiders-1.2.8.css' rel='stylesheet'>
                                 <script src='/nitrogen/guiders-js/guiders-1.2.8.js'></script>"),
                 case webutils:guiders_ok("matchmaker_guiders_shown") of
                      true -> guiders_script();
                      false -> []
                 end,

    T = nsm_tournaments:get(TID),
    wf:state(tournament, T),

    case T#tournament.players_count < 510 of
         true -> webutils:add_script("/nitrogen/jquery.paginatetable.js");
         false -> skip end,

    #template { file=code:priv_dir(nsp_srv)++"/templates/bare.html" }.

title() -> webutils:title(?MODULE).

guiders_script() ->
   StdButtons = [{?_T("OK"),hide}],
    Guiders = [
        matchmaker:make_guider(show,?_T("JOIN TOURNAMENT"), ?_T("Join, Leave or Play tournaments here."), StdButtons,guider_20,guider_30,false,true,tournament_control,12)
    ],
    wf:wire(Guiders).

node_by_id(Id, Type) ->
    Zone = Id div 1000000,
    GameSrv = atom_to_list(Type) ++ "@srv" ++ integer_to_list(Zone) ++ ".kakaranet.com",
    case Zone of
         4 -> nsx_opt:get_env(nsm_db, game_srv_node, 'game@doxtop.cc');
         _ -> list_to_atom(GameSrv)
    end.

body() ->

      TimeStampMeasure1 = now(),

  T = wf:state(tournament),
  case T#tournament.id of
   undefined ->

      #panel{class="form-001", body=[?_T("Tournament not found"), #panel{style="height:10px;clear:both"}]};
  _ ->

    TID = T#tournament.id,
    CurrentUser = wf:user(),

    Title = T#tournament.name,
    Tours = T#tournament.tours,
    Game = case T#tournament.game_type of
        game_okey -> "OKEY";
        game_tavla -> "TAVLA";
        game_batak -> "BATAK";
        _ -> ?_T("Unknown")
    end,

    GamePoints = case nsm_accounts:balance(CurrentUser, ?CURRENCY_GAME_POINTS) of
            {ok,AS1} -> AS1;
            {error,_} -> 0 end,

    Kakush = case nsm_accounts:balance(CurrentUser,  ?CURRENCY_KAKUSH) of
            {ok,AS2} -> AS2;
            {error,_} -> 0 end,

    JoinedUsers0 = user_counter:joined_users(T#tournament.id), 

    NodeAtom = node_by_id(T#tournament.id, game),

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
            GOs = [nsm_db:get(gift,A) || A <- T#tournament.awards],
            [case GO of
                {error, notfound} -> {"", "/images/tournament/nothing.png"};
                {ok, Gift} -> {site_utils:decode_letters(Gift#gift.gift_name), Gift#gift.image_small_url}
            end || GO <- GOs];
        false ->
            [{"?", "/images/tournament/new_tournament/question.png"},{"?", "/images/tournament/new_tournament/question.png"},{"?", "/images/tournament/new_tournament/question.png"}]
    end,
    {PN1, PI1} = hd(Prizes),
    {PN2, PI2} = hd(tl(Prizes)),
    {PN3, PI3} = hd(tl(tl(Prizes))),

    {UserJoined,AddMe} = case lists:keyfind(CurrentUser,#play_record.who, JoinedUsers0) of
                      false -> {false, [#play_record{who = CurrentUser, game_points = GamePoints,
                                  kakush = Kakush, game_id = yellow,
                                  realname = CurrentUser }]};
                      #play_record{who = Who, game_id = yellow} -> {false,[]};
                      X -> {X,[]} end,
    Length = length(JoinedUsers0),
    DateTime = Date ++ " " ++ Time,

%    AddMe = case UserJoined of
%         false -> UserPlayRecord = [#play_record{who = CurrentUser, game_points = GamePoints,
%                                  kakush = Kakush, game_id = yellow,
%                                  realname = CurrentUser }];
%          _ -> [] end,


    JoinedUsers = AddMe ++ JoinedUsers0,

    start_comet(JoinedUsers,integer_to_list(T#tournament.id),CurrentUser,UserJoined),

    case rpc:call(NodeAtom,nsm_srv_tournament_lobby,chat_history,[T#tournament.id]) of
        H when is_list(H) -> add_chat_history(H);
        _ -> ok
    end,

    ui_paginate(T),

    TimeStampMeasure2 = now(),
    ?INFO("Load Page ~p",[timer:now_diff( TimeStampMeasure2, TimeStampMeasure1)]),

%    {ok,PlanDesc1} = case rpc:call(?GAMESRVR_NODE, game_okey_ng_trn_elim,get_plan_desc,[T#tournament.quota,
%                                                                                   T#tournament.players_count,
%                                                                                   T#tournament.tours]) of
%                            {ok,PX} -> {ok,PX};
%                             _ -> {ok,[]}
%                        end,

%    PlanDesc = ling:join(PlanDesc1," / "),
%    PlanI = lists:seq(1, length(PlanDesc1)),
%    PlanTable = #table{rows=[
%        #tablerow{cells=
%            [#tablecell{body=integer_to_list(I), style=case I of
%                    1 -> "padding-bottom:2px; text-align:center; border-bottom:1px solid #888;";
%                    _ -> "padding-bottom:2px; text-align:center; border-bottom:1px solid #888; border-left:1px solid #888;"
%                end
%            }
%            || I <- PlanI]
%        },
%        #tablerow{cells=
%            [#tablecell{body=lists:nth(I, PlanDesc1), style=case I of
%                    1 -> "padding-left:3px; padding-right:3px; padding-top:2px; text-align:center;";
%                    _ -> "padding-left:3px; padding-right:3px; padding-top:2px; text-align:center; border-left:1px solid #888;"
%                end
%            }
%            || I <- PlanI]
%        }
%    ], style="font-size:9px; color:#fff;"},

    [   "<br><br><section id='main'>",
        #panel{class="tourlobby_title", body=[#label{class="tourlobby_title_label", body=?_T("TOURNAMENT LOBBY")} ]},

%        #panel{body=[#label{style="margin-left:300px;margin-top:10px;font-size:16pt;",body=[%lists:flatten(PlanDesc)
%                                                                                           PlanTable]}]},

        % left top block
        #panel{class="tourlobby_left_top_block", body=[
                "<center>",
                #label{class="tourlobby_left_top_block_label", body=Title}, #br{}, #br{},
                "<span id='tournament_control'>", 
                case UserJoined of
                    false -> [ #link{id=join_button, class="tourlobby_orange_button", text=?_T("JOIN TOURNAMENT"), postback=join_tournament},"</span>", 
                               #br{}, #panel{id=leave_button, class="tourlobby_red_button_disabled", text=?_T("LEAVE TOURNAMENT")} ] ;
                    _ ->  [ #panel{id=join_button, class="tourlobby_orange_button_disabled", text=?_T("JOIN TOURNAMENT")},"</span>",
                               #br{}, #link{id=leave_button, class="tourlobby_red_button", text=?_T("LEAVE TOURNAMENT"), postback=leave_tournament} ] end,
                #br{},
                case wf:state(tournament_started) of
                    undefined ->  #panel{id=attach_button, class="tourlobby_yellow_button_disabled", text=?_T("TAKE MY SEAT")};
                    _ ->   #link{id=attach_button, class="tourlobby_yellow_button", text=?_T("TAKE MY SEAT"), postback=attach}
                end,
                #br{},
                case T#tournament.creator == CurrentUser of
                     true -> case wf:state(tournament_started) of
                            undefined -> #link{id=start_button, text=?_T("MANUAL START"), postback={start_tour, T#tournament.id, NPlayers,Quota,Tours,Speed,T#tournament.awards}};
                            _ -> "" end;
                    _ -> ""
                end,
                "</center>"
            ]
        },

        %left bottom block
        #panel{class="tourlobby_left_bottom_block", body=[
                #br{}, #label{class="tourlobby_left_bottom_block_title", body=?_T("Tournament Info")},
                #br{}, #label{class="tourlobby_left_bottom_block_label", body=?_T("Game Type: ") ++ Game},
                #br{}, #label{class="tourlobby_left_bottom_block_label", body=?_T("Quota: ") ++ integer_to_list(Quota)},
                       #label{class="tourlobby_left_bottom_block_label", body=?_T("Katılım: ") ++ integer_to_list(Length)},
                #br{}, #label{class="tourlobby_left_bottom_block_label", body=?_T("Starting") ++ ": " ++ DateTime}, #br{}
%                #label{class="tourlobby_left_bottom_block_label", body=?_T("Plan: ")},
%                #panel{style="left:26px; top:110px;", body=
%                    PlanTable
%                }
        ] },

        %tournament parameters
        #panel{class="tourlobby_orange_plask", body=[ #label{class="tourlobby_every_plask_title", body=?_T("PARTICIPANTS")}, #br{},
                #label{class="tourlobby_every_plask_label", body=integer_to_list(NPlayers)} ] },
        #panel{class="tourlobby_sky_plask", body=[#label{class="tourlobby_every_plask_title", body=?_T("START DATE")}, #br{},
                #link{body=#label{class="tourlobby_every_plask_label", body=Date}, style="text-decoration:none", title=Time} ] },
        #panel{class="tourlobby_blue_plask", body=[#label{class="tourlobby_every_plask_title", body=?_T("TIME LEFT")}, #br{}, 
                                                   #label{id=lobby_timer, class="tourlobby_every_plask_label", body=Timer} ] },

        %gifts
        #panel{class="tourlobby_prizes", body=[

                #panel{class="tourlobby_prize_1",body=[ "<center>", 
                          #panel{style="background-color:888; height:135px; display:table-cell; vertical-align:middle;",
                               body=#image{style="max-width:130px; max-height:130px;", image=PI1} },
                           #label{style="font-size:12px; color:#000;", body=PN1}, "</center>",
                        #panel{class="tourlobby_prize_star tourlobby_prize_star_1", body=#label{class="tourlobby_prize_star_text", body="1"} } ] },

                #panel{class="tourlobby_prize_2", body=[ "<center>", 
                           #panel{ style="background-color:888; height:135px; display:table-cell; vertical-align:middle;",
                              body=#image{style="max-width:130px; max-height:130px;", image=PI2} }, 
                           #label{style="font-size:12px; color:#000;", body=PN2}, "</center>",
                        #panel{class="tourlobby_prize_star tourlobby_prize_star_2", body=#label{class="tourlobby_prize_star_text", body="2"} } ] },

                #panel{class="tourlobby_prize_3", body=[ "<center>",
                        #panel{ style="background-color:888; height:135px; display:table-cell; vertical-align:middle;",
                            body=#image{style="max-width:130px; max-height:130px;", image=PI3} },
                        #label{style="font-size:12px; color:#000;", body=PN3}, "</center>",
                        #panel{class="tourlobby_prize_star tourlobby_prize_star_3", body=#label{class="tourlobby_prize_star_text", body="3"} } ] }
        ] },


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
            user_table(JoinedUsers,CurrentUser,UserJoined) %TourUserList)
        ]},
        "</section>"]
   
  end.

user_table(Users,CurrentUser,CurrentJoined) ->

    Tournament = wf:state(tournament),
    case Tournament#tournament.players_count > 500 of
         true ->
            [ #panel{style="font-size:16px; line-height:24px; margin-left:25px; margin-right:25px; text-align:justify;", body = [
                [#span{style="font-size:24px; font-weight:bold;", body=[?_T("Players"), ": "]},
                    [begin 
                        case site_utils:user_link(Who) of
                          undefined -> "";
                          "" -> "";
                          URL ->
                            #span{body=#link{url=URL, text=Who ++ " ",
                              style = "font-weight:bold; margin-right:5px;" ++ 
                                 case Who == CurrentUser of
                                                  true -> case CurrentJoined of
                                                                false -> "color:#938b03;"; % yellow
                                                                 _ -> "color:#5ba108;" end; % green
                                                  false -> case Color of 
                                                                undefined -> "color:#c22323;";
                                                                red -> "color:#c22323;";
                                                                yellow -> "color:#938b03;";
                                                                green -> "color:#5ba108;";
                                                                _ -> "" end end}
                            }
                        end
                    end || #play_record{who=Who, game_id=Color} <- Users]
                ]
            ]} ] ; %, #link{class="tourlobby_view_mode_link", text=?_T("Full view"), postback={change_view, full}}];
        _ ->

        [
          #panel{class="matchmaker-table-pager paging-2", body=[#panel{class="center", body=[
          #list{body=[#listitem{body=[#link{class="prevPage", text="<"}]}]},"<ul class='pageNumbers'></ul>",
          #list{body=[#listitem{body=[#link{class="nextPage", text=">"}]}]} ]} ]},

          #table{style="width: 100%", rows=[
                #tablerow{class="tourlobby_table_head", cells=[
                    #tableheader{style="width:40px; padding-left:16px;",text=""},
                    #tableheader{style="text-align:left;", text=?_T("USER")},
                    #tableheader{style="width:200px;text-align:center;", text=?_T("TOTAL SCORE")},
                    #tableheader{style="width:200px;text-align:center;", text=?_T("SKILL SCORE")},
                    #tableheader{style="width:100px;text-align:center;", text=?_T("STATUS")}
                 ]} ] } ,

          #table{class="paging-2 tourlobby_table", style="width: 100%", rows=[
            [ user_table_row(Name, Score1, Score2, case CurrentUser == Name of
                                                        false -> case Color of 
                                                                      undefined -> red;
                                                                      C -> C end;
                                                        true -> case CurrentJoined of
                                                                     false -> yellow;
                                                                     _ -> green end end, N, RealName) ||
                           {#play_record{who=Name, 
                                         game_points=Score1, 
                                         kakush=Score2, 
                                         game_id=Color, 
                                         realname=RealName}, N} <- lists:zip(Users,lists:seq(1,length(Users))) ] ]}

            % #link{class="tourlobby_view_mode_link", text=?_T("Short view"), postback={change_view, short}}

         ]
    end.

user_table_row(UId, P1, P2, Color, N, RealName) ->
    Avatar = avatar:get_avatar_by_username(UId, tiny),
    case site_utils:user_link(UId) of
      "" -> "";
      URL ->

        #tablerow{id=integer_to_list(N),cells=[
           #tablecell{class=cell1,style="width:40px;", body=#image{image=Avatar}},
           #tablecell{class=cell2,body=[#link{style="font-weight:bold;", url=URL, text=UId}, " &mdash; ", site_utils:decode_letters( RealName ) ]},
           #tablecell{class=cell3,style="width:200px;text-align:center;", body=[ integer_to_list(P1) ]},
           #tablecell{class=cell4,style="width:200px;text-align:center;", body=[ integer_to_list(P2) ]},
           #tablecell{class=cell5,style="width:100px;text-align:center;", body=[
            case Color of
                red ->      #image{image="/images/tournament/lobby/red_bullet.png"};
                undefined ->      #image{image="/images/tournament/lobby/red_bullet.png"};
                green ->    #image{image="/images/tournament/lobby/green_bullet.png"};
                yellow ->        #image{image="/images/tournament/lobby/yellow_bullet.png"}
            end ]} ], style = case N rem 2 of 0 -> ""; 1 -> "background-color:#f5f5f5;" end} end.

add_chat_history(Messages) -> [ process_chat(Action, User, Message) || {User, Action, Message} <- Messages ].
process_chat("message", User, Message) -> chat_new_msg(User, Message).
chat_info(Info) -> Terms = #panel{class="info", body = Info}, update_table_chat(Terms).
chat_error(Message) -> chat_info(#span{class=error, text= Message}).
chat_user_in(Username) -> Terms = #panel{class="user join", body = [ ?_TS("User $username$ connected.", [{username, Username}]) ]}, update_table_chat(Terms).
chat_user_out(Username) -> Terms = #panel{class="user left", body = [ ?_TS("User $username$ has left.", [{username, Username}]) ]}, update_table_chat(Terms).

chat_new_msg(User, Message) ->
    Terms = #panel{class="chat",
        body = [
            #span{class="username", style="font-weight:bold;", text = User},
            ":&nbsp;", #span{text = Message}
    ]},
    update_table_chat(Terms).

ui_paginate(T) ->
    case T#tournament.players_count < 500 of
         true -> wf:wire("$('.tourlobby_table').paginateTable({ rowsPerPage: 20, "
                         "pager: '.matchmaker-table-pager', maxPageNumbers:100 });");
                         %.find('tr:nth-child(2n)').addClass('color1');").
         false -> skip end.

update_table_chat(Terms) ->
    ?INFO("X"),
    wf:insert_bottom(chat_history, Terms),
    wf:wire("obj('chat_history').scrollTop = obj('chat_history').scrollHeight;"),
    wf:flush().

active_users(TID,PID) ->
    nsm_queries:map_reduce(tournament_lobby,active_users_raw,[TID,PID]).

active_users_raw(TID,PID) ->
    Check = fun(undefined, _Value) -> true; (Param, Value) ->  Param == Value end,
    Cursor = qlc:cursor(qlc:q([V || {{_,_,ProcId},_, V = {ProcId,PagePid,TourId,User,Color}} <- gproc:table(props), 
           Check(TourId, TID)])),
    Tables = qlc:next_answers(Cursor).

start_comet(TourUsers,TID,User,MeJoined) ->
    PagePid = self(),
    MyColor = case MeJoined of false -> yellow; _ -> green end,
    {ok, CometPid} = wf:comet(fun()-> 
        CometProcess = self(),
        nsx_msg:subscribe_for_tournament(TID, User, CometProcess),
        gproc:reg({p,l,CometProcess},{CometProcess,PagePid,TID,User,MyColor}),
        comet_update(User, TID, PagePid, TourUsers) end, ?COMET_POOL),
    ?INFO("COMET PID: ~p",[CometPid]),
    ActiveUsers = active_users(TID,CometPid),
    [ begin 
        Pid ! {make_active, MyColor, User, noreply},
        CometPid ! {make_active, Color, Joined, noreply}
    end || {Pid,_,TID,Joined,Color} <- ActiveUsers, Joined =/= User],
    wf:state(comet_pid, CometPid).

make_active(User, Color, TournamentId,PageProc,TourUsers0,Joined) ->
    Z = lists:keyfind(Joined,#play_record.who,TourUsers0),
    MeJoined = case User == Joined of
                    true -> case Color of
                                 green -> Z;
                                 _ -> false end;
                    false -> case lists:keyfind(User,#play_record.who,TourUsers0) of
                                  false -> false;
                                  #play_record{game_id = yellow} -> false;
                                  Me -> Me end 
    end,
    {TourUsers,A} = case Z of false -> AddNew = #play_record{who=Joined,game_id=Color},
                                        {[AddNew] ++ TourUsers0,AddNew};
                                X -> {TourUsers0,X} end,
    NewTourList = lists:keyreplace(Joined,#play_record.who,TourUsers, A#play_record{game_id = Color}),
    wf:update(players_table, user_table(NewTourList,User,MeJoined)),
    wf:flush(),
    {NewTourList,MeJoined}.

comet_update(User, TournamentId,PageProc,TourList) ->
    {U,T,P,L} = receive
        {make_active, Color, Joined, Reply} -> 
             ?INFO("~p make_active received ~p",[User,{Joined,Color}]),
             {NewTourList,MeJoined} = make_active(User, Color, TournamentId,PageProc,TourList,Joined), 
             case Reply of 
                  noreply -> skip;
                   _ -> Reply ! {make_active, case MeJoined of 
                                                   false -> yellow;
                                                    _ -> green end, User, noreply} end,
             {User, TournamentId,PageProc,NewTourList};

        {delivery, ["tournament", TournamentId, "start"], {TID}}  ->
            wf:replace(attach_button, #link{id=attach_button, class="tourlobby_yellow_button", text=?_T("TAKE MY SEAT"), postback=attach}),
            wf:replace(start_button, ""),
            ?INFO("(in comet): start game TId: ~p, User: ~p, Data: ~p", [TournamentId, User, TID]),
            Url = lists:concat([?_U("/client"), "/", ?_U("okey"), "/id/", TID]),
            StartClient = webutils:new_window_js(Url),
            wf:wire(#script{script=StartClient}),
            wf:flush(),
            {User,TournamentId,PageProc,TourList};

        {delivery, ["tournament", TournamentId, "chat", _Action], {UserName, Action, Message}}  ->
            process_chat(Action, UserName, Message),
            {User,TournamentId,PageProc,TourList};

        {delivery, Route, Other}  ->
            ?PRINT({other_in_comet, Route, Other, wf:user()}),
             {User,TournamentId,PageProc,TourList}

    end,
    comet_update(U,T,P,L).

api_event(Name, Tag, Args) -> webutils:api_event(Name, Tag, Args).

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
    wf:flush();

event({update_tour_list, TourList}) ->
    ?INFO("Update Tour List"),
    wf:state(tour_user_list, TourList);

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
    User = wf:user(),
    CometPid = wf:state(comet_pid),
    TID = wf:state(tournament_id),
    T = wf:state(tournament),
    ActiveUsers = active_users(TID,CometPid),
    JoinedUsers = user_counter:joined_users(T#tournament.id),
    Found = case lists:keyfind(User,#play_record.who,JoinedUsers) of
                 false -> #play_record{who = User};
                 X -> X end,
    user_counter:write_cache(T#tournament.id,Found#play_record{game_id=green}),
    CometPid ! {make_active, green, User, noreply},
    [ Pid ! {make_active, green, User, noreply} || {Pid,_,TID,Joined,Color} <- ActiveUsers, Joined =/= User],
    ?INFO("JOIN sent to ~p and all",[CometPid]),
%    nsx_msg:notify(["system", "tournament_join"], {UId, list_to_integer(TId)}),
    nsm_tournaments:join(User, list_to_integer(TID)),
    wf:replace(join_button, #panel{id=join_button, class="tourlobby_orange_button_disabled", text=?_T("JOIN TOURNAMENT")}),
    wf:replace(leave_button, #link{id=leave_button, class="tourlobby_red_button", text=?_T("LEAVE TOURNAMENT"), postback=leave_tournament}),
    ok;
%    wf:flush();

event(leave_tournament) ->
    User = wf:user(),
    CometPid = wf:state(comet_pid),
    TID = wf:state(tournament_id),
    T = wf:state(tournament),
    CometPid ! {make_active, yellow, User, noreply},
    ActiveUsers = active_users(TID,CometPid),
    JoinedUsers = user_counter:joined_users(T#tournament.id),
    Found = case lists:keyfind(User,#play_record.who,JoinedUsers) of
                 false -> #play_record{who = User};
                 X -> X end,
    user_counter:write_cache(T#tournament.id,Found#play_record{game_id=yellow}),
    [ Pid ! {make_active, yellow, User, noreply} || {Pid,_,TID,Joined,Color} <- ActiveUsers, Joined =/= User],
    ?INFO("LEAVE sent to ~p and all",[CometPid]),
%    nsx_msg:notify(["system", "tournament_remove"], {UId, list_to_integer(TId)}),
    nsm_tournaments:remove(User, list_to_integer(TID)),
    wf:replace(join_button, #link{id=join_button, class="tourlobby_orange_button", text=?_T("JOIN TOURNAMENT"), postback=join_tournament}),
    wf:replace(leave_button, #panel{id=leave_button, class="tourlobby_red_button_disabled", text=?_T("LEAVE TOURNAMENT")}),
    wf:flush();

event({start_tour, Id, NPlayers,Q,T,S,P}) ->
    wf:state(tour_start_time, time()),
    Zone = Id div 1000000,
    GameSrv = "game@srv" ++ integer_to_list(Zone) ++ ".kakaranet.com",
    NodeAtom = case Zone of
                    4 -> nsx_opt:get_env(nsm_db, game_srv_node, 'game@doxtop.cc');
                    _ -> list_to_atom(GameSrv)
               end,
    TourId = rpc:call(NodeAtom, game_manager,start_tournament,[Id, 1, NPlayers,Q,T,S,P]),
    ?INFO("Tournament Started: ~p",[TourId]),
    wf:replace(attach_button, #link{id=attach_button, class="tourlobby_yellow_button", text=?_T("TAKE MY SEAT"), postback=attach}),
    wf:replace(start_button, ""),
    wf:state(tour_long_id,TourId);

event(attach) ->
    TourId = wf:state(tour_long_id),
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
   T =  wf:state(tournament),
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
                                    Zone = Id div 1000000,
                                    GameSrv = "game@srv" ++ integer_to_list(Zone) ++ ".kakaranet.com",
                                    NodeAtom = case Zone of
                                               4 -> nsx_opt:get_env(nsm_db, game_srv_node, 'game@doxtop.cc');
                                               _ -> list_to_atom(GameSrv)
                                    end,
                                    case rpc:call(NodeAtom, game_manager,get_tournament,[Id]) of
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

