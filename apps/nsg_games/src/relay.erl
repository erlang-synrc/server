-module(relay). % simple game monitor for single boarded games
-author('Maxim Sokhatsky <maxim@synrc.com>').
-behaviour(gen_server).

-include_lib("nsg_srv/include/social_actions.hrl").
-include_lib("nsg_srv/include/logging.hrl").
-include_lib("nsg_srv/include/requests.hrl").
-include_lib("nsm_db/include/config.hrl").
-include_lib("nsm_db/include/table.hrl").
-include_lib("nsm_db/include/accounts.hrl").
-include_lib("nsx_config/include/log.hrl").
-include_lib("nsg_srv/include/settings.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([init/5, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([do_rematch/2, signal/3, publish/2, submit/3, republish/2, resubmit/3, cast_resubmit/3,
         notify_table/2, game/1, to_session/3, reg/2, start/1, start_link/1,
         subscribe/2, subscribe/3, subscribe/4, unsubscribe/2, get_requirements/2, start/5, start_link/5, stop/1, get_topic/1,
         get_player_state/2, get_table_info/1, update_gamestate/2, can_observe/2, unreg/2, im_ready/1]).

-record(subscriber, { pid, id, ref}).
-record(player, { id :: 'PlayerId'(), info :: #'PlayerInfo'{}, pid :: pid(), monref :: 'MonitorRef'(), is_bot = false }).

-record(state, {
          topic :: any(),                       %% game id
          parent_relay :: undefined | {atom(), pid()}, %% {ParentModule, ParentPid}
          table_id :: integer(),
          manager :: pid(),
          subs = ets:new(subs, [ordered_set, {keypos, #subscriber.pid}]), %% subscribed players/viewers
          players :: list(#player{}),           %% list of connected players, not undefined iff rules_module =/= chat
          reg_players :: list('PlayerId'()),    %% list of registered players, connected or not
          lobby_list :: list('PlayerId'()) | list(pid()),  %% wait list for lobby
          rematch_list :: list('PlayerId'()) | list(pid()),  %% wait list for rematch
          rules_pid :: pid(),                   %% game state machine pid
          rules_module :: atom(),               %% game state machine module
          rules_params :: list(tuple(atom(), any())), %% game state machine starting params
          gamestate :: atom(),
          rematch_timer :: tuple(running, any()) | expired | undefined,
          from_lobby = false,
          table_settings
         }).

-define(DISCONNECT_TIMEOUT, (60*1000)). % 1 Min

reg(Srv, User) -> gen_server:call(Srv, {reg, User}).
publish(Srv, Msg) -> gen_server:cast(Srv, {publish, Msg}).
republish(Srv, Msg) -> gen_server:cast(Srv, {republish, Msg}). %% Used by parent relay
notify_table(Srv, Msg) -> gen_server:cast(Srv, {notify_table, Msg}).
submit(Srv, _UserId, Msg) -> gen_server:call(Srv, {submit, Msg}).
resubmit(Srv, From, Msg) -> gen_server:cast(Srv, {resubmit, From, Msg}). %% Used by parent relay
cast_resubmit(Srv, PlayerId, Msg) -> gen_server:cast(Srv, {cast_resubmit, PlayerId, Msg}).
signal(Srv, _UserId, Msg) -> gen_server:cast(Srv, {signal, Msg}).
to_session(Srv, Session, Msg) -> gen_server:cast(Srv, {to_session, Session, Msg}).
unreg(Srv, Key) -> gen_server:call(Srv, {unreg, Key}).
subscribe(Srv, Pid, User, _RegNum) -> subscribe(Srv, Pid, User).
subscribe(Srv, Pid, User) -> gen_server:cast(Srv, {subscribe, Pid, User}).
subscribe(Srv, Pid) -> gen_server:cast(Srv, {subscribe, Pid, null}).
unsubscribe(Srv, Pid) -> gen_server:cast(Srv, {unsubscribe, Pid}).
do_rematch(Srv, Pid) -> gen_server:call(Srv, {do_rematch, Pid}).
get_topic(Srv) -> gen_server:call(Srv, get_topic).
get_player_state(Srv, UId) -> gen_server:call(Srv, {get_player_state, UId}).
get_table_info(Srv) -> gen_server:call(Srv, get_table_info).
update_gamestate(Srv, NewGameState) -> gen_server:cast(Srv, {update_gamestate, NewGameState}).
can_observe(Srv, Id) -> gen_server:call(Srv, {can_observe, Id}).
start_link(GameId, GameFSM, Params, Pids, Manager) -> gen_server:start_link(?MODULE, [GameId, GameFSM, Params, Pids, Manager], []).
start_link([GameId, GameFSM, Params, Pids, Manager]) -> gen_server:start_link(?MODULE, [GameId, GameFSM, Params, Pids, Manager], []).
start(GameId, GameFSM, Params, Pids, Manager) -> gen_server:start(?MODULE, [GameId, GameFSM, Params, Pids, Manager], []).
stop(Srv) -> gen_server:cast(Srv, stop).
im_ready(Srv) -> gen_server:cast(Srv, room_ready).

game(game_okey) -> okey;
game(game_tavla) -> tavla;
game(_) -> okey.

get_requirements(game_tavla,_) -> [{max_users,2},{min_users,2}];
get_requirements(game_okey,_) -> [{max_users,4},{min_users,4}];
get_requirements(_,_) -> [{max_users,2},{min_users,2}].

start([GameId, GameFSM, Params, Pids, Manager]) -> gen_server:start(?MODULE, [GameId, GameFSM, Params, Pids, Manager], []).

init(Topic, {lobby, GameFSM}, Params0, PlayerIds, Manager) -> init([Topic, {lobby, GameFSM}, Params0, PlayerIds, Manager]).

init([Topic, chat, _, _]) ->
    {ok, #state{topic = Topic, rules_pid = none, rules_module = chat,
                players = [], reg_players = [], gamestate = no_game}};


init([Topic, {lobby, GameFSM}, Params0, PlayerIds, Manager]) ->
    ?INFO("~ninit lobby ~p",[{GameFSM,Params0,PlayerIds,Manager}]),

    Settings = Params0,

    R = GameFSM:get_requirements(),
    NoOfPlayers = proplists:get_value(players, R),
    true = NoOfPlayers =/= undefined,
    true = NoOfPlayers =:= length(PlayerIds),

    TableName = proplists:get_value(table_name, Settings, "no table"),
    Rounds = proplists:get_value(rounds, Settings, 1),
    TableId = proplists:get_value(table_id, Settings, 1),
    ParentRelay = proplists:get_value(parent_relay, Settings, {?MODULE, self()}),
    GameMode = proplists:get_value(game_mode, Settings, standard),
    GameSpeed = proplists:get_value(speed, Settings, normal),
    DPR = proplists:get_value(default_pr, Settings, no),
    Slang = proplists:get_value(slang, Settings, false),
    GostergeFinish = proplists:get_value(gosterge_finish, Settings, false),
    DenyObservers = proplists:get_value(deny_observers, Settings, false),
    Paid = proplists:get_value(paid_only, Settings, false),
    Owner = proplists:get_value(owner, Settings, "maxim"), %% FIXME
  
    {Params,P,PE} = case DPR of
                       no ->  {ok, PR, PREx} = pointing_rules:get_rules(GameFSM, GameMode, Rounds),
                              {Params0 ++ [{pointing_rules, PR},{pointing_rules_ex, PREx}],PR,PREx};
                        _ ->  {Params0,#pointing_rule{rounds=1, game = game(GameFSM),
                                                     kakush_winner = 1, kakush_other = 1, quota = 1},
                              [#pointing_rule{rounds=1, game = game(GameFSM),
                                              kakush_winner = 1, kakush_other = 1, quota = 1}]}
                    end,
                
    FeelLucky = proplists:get_value(feel_lucky, Settings, false),
    ?INFO("Parent relay: ~p", [ParentRelay]),

    GProcVal = #game_table{game_type = GameFSM, 
                           game_process = self(),
                           game_module = element(1, ParentRelay),
                           id = Topic,
                           age_limit = [crypto:rand_uniform(20,30), crypto:rand_uniform(31,40)],
                           game_mode = GameMode,
                           game_speed = GameSpeed,
                           feel_lucky = FeelLucky,
                           owner = Owner,
                           creator = Owner,
                           rounds = Rounds,
                           slang = Slang,
                           paid_only = Paid,
                           gosterge_finish = GostergeFinish,
                           deny_observers = DenyObservers,
                           pointing_rules   = P,
                           pointing_rules_ex = PE,
                           users = [ case User of robot -> robot; _ -> erlang:binary_to_list(User) end || User <- PlayerIds],
                           name = TableName,
                           game_state = started
               },

    ?INFO("relay.erl GProc Registration: ~p",[GProcVal]),

    gproc:reg({p,l,self()},GProcVal),

    Manager ! {add_game, GameFSM},

    {RobotIds, HumanIds} = lists:partition(fun(robot) -> true; (_) -> false end, PlayerIds),

    State = #state{topic = Topic, rules_pid = none, rules_module = GameFSM, rules_params = Params,
                   players = [], reg_players = PlayerIds, lobby_list = HumanIds ++ RobotIds, manager = Manager,
                   gamestate = lobby, from_lobby = true, table_id = TableId, table_settings = Settings,
                   parent_relay = ParentRelay},


    ?INFO("State Lobby List: ~p",[State#state.lobby_list]),

     gen_server:cast(self(), {update_gamestate, State#state.gamestate}),

    ?INFO("Humans/Robots: ~p/~p",[length(HumanIds),length(RobotIds)]),

     gen_server:cast(self(), {start_robots, RobotIds}),
    ?INFO("Start Robots"),

    {ok, State}.

handle_call({submit, _Msg}, _From, #state{rules_module = chat} = State) ->
    {reply, {error, chat_has_no_game_fsm_module__cant_submit}, State};

handle_call({submit, Msg}, {FromPid, _}=From, #state{parent_relay = {PMod, PPid},
                                                     players = Players} = State) ->
    ?INFO("SUBMIT: ~p",[{FromPid,Msg}]),
    case lists:keyfind(FromPid, #player.pid, Players) of
        #player{id = PlayerId} ->
            ?INFO("SUBMIT: Owner of the message: ~p", [PlayerId]),
            if PPid == self() ->
                   resubmit0(Msg, From, State);
               true ->
                  ?INFO("SUBMIT: Calling parent relay: ~p, ~p, ~p, ~p", [PPid, Msg, From, PlayerId]),
                   PMod:submit(PPid, Msg, From, PlayerId)
            end,
            {noreply, State};
        false ->
            ?INFO("SUBMIT: Message from unknown sender: ~p", [FromPid]),
            {reply, {error, unknown_sender}, State}
    end;

%% handle_call({submit, Msg}, {FromPid, _}=From, #state{rules_pid = Pid, rules_module = GameFSM} = State) ->
%%     ?INFO("SUBMIT: ~p",[{Pid,FromPid,Msg}]),
%%     Res = GameFSM:make_move(Pid, FromPid, Msg),
%%     {reply, Res, State};

handle_call({do_rematch, _}, _From, State = #state{gamestate = lobby}) ->
    {reply, {error, cannot_rematch_in_lobby}, State};

handle_call({do_rematch, Pid}, _From, State) ->
    ?INFO("rematch A. Pid:~p", [Pid]),
    GameFSMPid = State#state.rules_pid,
    GameFSM = State#state.rules_module,
    ?INFO("rematch A. finished", []),
    GameFSM:signal(GameFSMPid, do_rematch),
    publish(self(), #game_rematched{game = State#state.topic}),
    PlayersPids = [ Player#player.pid || Player <- State#state.players],
    {reply, ok, State#state{rematch_list = PlayersPids, rematch_timer = undefined}};

handle_call(get_topic, _From, State) ->
    {reply, State#state.topic, State};

handle_call({update_reg, Key, Value}, _From, State) ->
    gproc:set_value({p,l,Key},Value),
    {reply, ok, State};

handle_call({get_player_state, UId}, _From, State) ->
    FSM = State#state.rules_pid,
    Game = State#state.rules_module,
    Res = Game:get_player_state(FSM, UId),
    {reply, Res, State};

handle_call(get_table_info, _From, State) ->
    ?INFO("get_table_info"),
    List = ets:tab2list(State#state.subs),
    Viewers = lists:map(fun(#subscriber{id = PlayerId}) -> PlayerId end, List),
    Players = [ PlayerId || #player{id = PlayerId} <- State#state.players ],
    GameStatus = list_to_binary(atom_to_list(State#state.gamestate)),
    GameName = api_utils:gamemodule_to_gametype(State#state.rules_module),
    Res = #'TableInfo'{viewers = Viewers, players = Players, chat = [], game = GameName, game_status = GameStatus},
    {reply, Res, State};

handle_call({can_observe, Id}, _From, State) ->
    Res = can_observe0(Id, State),
    {reply, Res, State};

handle_call({reg, User}, _From, State) -> % Emulate first level relay
    UserId = User#'PlayerInfo'.id,
    {reply, {ok, {UserId, {?MODULE, self()}, {?MODULE, self()}}}, State};

handle_call(Event, From, State) ->
    {stop, {unknown_call, Event, From}, State}.


handle_cast({start_robots, RobotIds}, State) ->
    [add_robot(State) || _ <- RobotIds],
    {noreply, State};

handle_cast(start_rematch_timer, State) ->
    {ok, Ref} = timer:send_after(?REMATCH_TIMEOUT, rematch_timer_ringing),
    {noreply, State#state{rematch_timer = {running, Ref}}};

handle_cast({update_gamestate, NewGameState}, State) ->
    ?INFO("update_gamestate: ~p", [NewGameState]),
    NewGameState == finished andalso
        gen_server:cast(self(), start_rematch_timer),
    {noreply, State#state{gamestate = NewGameState}};

handle_cast({signal, Msg}, State) ->
    #state{rules_pid = Pid, rules_module = GameFSM} = State,
    ?INFO("the signal: ~p", [Msg]),
    GameFSM:signal(Pid, Msg),
    {noreply, State};

handle_cast({to_session, Pid, #game_event{game = undefined} = Msg0}, State) ->
    Msg = Msg0#game_event{game = State#state.topic},
    handle_cast({to_session, Pid, Msg}, State);

handle_cast({to_session, Session, Msg}, State) ->
    Session ! Msg,
    {noreply, State};

handle_cast({publish, #game_event{game = undefined} = Msg0}, State) ->
    Msg = Msg0#game_event{game = State#state.topic},
    handle_cast({publish, Msg}, State);

handle_cast({publish, Msg}, State) ->
    publish0(Msg, State),
    {noreply, State};

handle_cast({republish, #game_event{game = undefined} = Msg0}, State) ->
    Msg = Msg0#game_event{game = State#state.topic},
    handle_cast({republish, Msg}, State);

handle_cast({republish, Msg}, State) ->
    republish0(Msg, State),
    {noreply, State};

handle_cast({resubmit, From, Msg}, State) ->
    resubmit0(Msg, From, State),
    {noreply, State};

handle_cast({cast_resubmit, PlayerId, Msg}, #state{rules_pid = Pid,
                                                   rules_module = GameFSM,
                                                   players = Players} = State) ->
    ?INFO("RELAY cast_resubmit Player: ~p Msg: ~p", [ PlayerId, Msg]),
    #player{pid = PlayerPid} = lists:keyfind(PlayerId, #player.id, Players),
    GameFSM:make_move(Pid, PlayerPid, Msg),
    {noreply, State};

handle_cast({notify_table, start}, State) ->
    gen_server:cast(self(), room_ready),
    {noreply, State};

handle_cast({notify_table, Msg}, State) ->
    ?INFO("~w:handle_cast Unhandled table notifivation: ~999p", [?MODULE, Msg]),
    {noreply, State};

handle_cast(room_ready, #state{rules_module = GameFSM, rules_params = Params,
                               players = Players, topic = Topic} = State) -> % deleyed fsm creation after all join the lobby
    RelPid = self(),
    Relay = {?MODULE, RelPid},
    Fun = fun() ->
                  ?INFO("Players: ~p",[Players]),
                  PidsWithPlayersInfo = [{Pid, Info} || #player{pid = Pid, info = Info} <- Players],
                  {ok, RPid} = GameFSM:start(Relay, PidsWithPlayersInfo, Topic, Params),
                  gen_server:cast(RelPid, {game_created, RPid})
          end,
    erlang:spawn_link(Fun),
    {noreply, State};

handle_cast({game_created, RPid}, State) ->
    ?INFO("room ready, starting game ~p~n", [{self(),RPid}]),
    GameFSM = State#state.rules_module,
    erlang:monitor(process, RPid),
    State2 = State#state{rules_pid = RPid, gamestate = started},
    GameFSM:signal(RPid, state_created),
    {noreply, State2};

handle_cast(stop, State) ->
    ?INFO("relay stop"),
    {stop, normal, State};

handle_cast({subscribe, Pid, UserInfo}, State = #state{gamestate = lobby}) -> % automatically unsubscribe when dead
    Ref = erlang:monitor(process, Pid),
    Pid ! ack,
    ets:insert(State#state.subs, #subscriber{pid = Pid, id = UserInfo#'PlayerInfo'.id, ref = Ref}),
    State1 = lobby_join(Ref, UserInfo, Pid, State),
    {noreply, State1};

handle_cast({subscribe, Pid, PlayerId}, #state{players=Players, subs=Subs}=State) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! ack,
    ets:insert(Subs, #subscriber{pid = Pid, id = PlayerId#'PlayerInfo'.id, ref = Ref}),
    %% Replace old session by the new one if the player id is matched.
    case lists:keyfind(PlayerId, #player.id, Players) of
        #player{} ->
            %% This is needed to avoid possible deadlock
            timer:send_after(5000, {replace, Pid, PlayerId#'PlayerInfo'.id, Ref});
        false ->
            void
    end,
    ?INFO("handle_cast(subscribe) subscription completed: Pid ~p", [Pid]),
    {noreply, State};

handle_cast({unsubscribe, Pid}, State = #state{gamestate = lobby}) ->
    State1 = lobby_leave(Pid, State),
    State2 = unsubscribe1(Pid, State1),
    {noreply, State2};

handle_cast({unsubscribe, Pid}, State) ->
    State2 = unsubscribe1(Pid, State),
    {noreply, State2};

handle_cast(kill_bots, State) ->
    {Bots, OtherPlayers} = lists:partition(fun(P) -> P#player.is_bot end, State#state.players),
    [ game_session:logout(Pid) || #player{pid = Pid} <- Bots ],
    {noreply, State#state{players = OtherPlayers}};

handle_cast({stop_game, Reason} = Msg, State) ->
    ?INFO("Received the directive to stop the game: ~p", [Msg]),
    [ gen_server:cast(Pid, {disconnect, Reason}) || #player{pid = Pid} <- State#state.players],
    {stop, normal, State#state{players = []}};

handle_cast(Event, State) ->
    {stop, {unknown_cast, Event}, State}.

handle_info(rematch_timer_ringing, State) ->
    State#state.rules_pid ! no_more_rematch,
    {noreply, State#state{rematch_timer = expired}};

handle_info({'DOWN', _, process, Pid, Reason}, State = #state{rules_pid = Pid}) ->
    ?INFO("relay is down. Reason: ~p", [Reason]),
    publish0(#game_crashed{game = State#state.topic}, State),
    {stop, {error, game_crashed}, State};

handle_info({'DOWN', _, process, Pid, _}, State = #state{gamestate = lobby}) ->
    ?INFO("relay leaves lobby: Pid ~p", [Pid]),
    State1 = lobby_leave(Pid, State),
    State2 = unsubscribe1(Pid, State1),
    {noreply, State2};

handle_info({'DOWN', _, process, Pid, _}, State = #state{gamestate = GS}) when GS == state_dead ->
    ?INFO("relay leaves table: Pid ~p", [Pid]),
    State2 = unsubscribe1(Pid, State),
    (no_of_subscribers(State2) == 0) andalso (self() ! die),
    {noreply, State2};

handle_info({'DOWN', _, process, Pid, _}, State) ->
    ?INFO("game session died (wait for user reconnection), Pid: ~p", [Pid]),
%    try 
%    gproc:unreg({p,l,self()}),
%    State#state.manager ! {remove_game, State#state.rules_module},
%    ?INFO("game manager notified ~p ! ~p",[State#state.manager,{remove_game, State#state.rules_module}])
%    catch _:_ -> noting
%    end,
    {ok, _}=timer:send_after(?DISCONNECT_TIMEOUT, {disconnect, Pid}),
    {noreply, State};

handle_info({unreg, _Key}, State) ->
    try 
    gproc:unreg({p,l,self()}),
    State#state.manager ! {remove_game, State#state.rules_module},
    ?INFO("game manager notified ~p ! ~p",[State#state.manager,{remove_game, State#state.rules_module}])
    catch _:_ -> noting
    end,
    {noreply, State};

handle_info({replace, Pid, PlayerId, Ref}, State) ->
    ?INFO("relay replace session: Pid ~p", [Pid]),
    State2=replace_session(Ref, Pid, PlayerId, State),
    {noreply, State2};

handle_info({disconnect, Pid}, State) ->
    ?INFO("Time to disconnect (if the user doesn't reconnect), Pid ~p", [Pid]),
    State2 = unsubscribe1(Pid, State),
    {noreply, State2};

handle_info(die, State) ->
    {stop, normal, State};

handle_info({get_second_level_relay, {Pid, Ref}, _UserId}, State) ->
    Self = self(),
    Pid ! {Self, {Ref, {ok, Self}}},
    {noreply, State};

handle_info(Info, State) ->
    ?INFO("handle_info: Unknown message: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ?INFO("Terminating relay. Unknown Reason: ~p", [_Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


unsubscribe1(Pid, #state{gamestate = Gamestate,
                         subs = Subs} = State) ->
    ?INFO("Unsubscripe in gamestate: ~p", [Gamestate]),
    %% Remove the gamesession from relay events subsrciber list
    case {ets:lookup(Subs, Pid), Gamestate} of
        {[#subscriber{ref = Ref}], state_dead} ->
            erlang:demonitor(Ref),
            ets:delete(Subs, Pid),
            State;
        {[#subscriber{ref = Ref}], state_finished} ->
            erlang:demonitor(Ref),
            ets:delete(Subs, Pid),
            State;
        {[#subscriber{ref = Ref}], _} ->
            erlang:demonitor(Ref),
            ets:delete(Subs, Pid),
            notify_if_user_leaving(Pid, State);
        {_, _} ->
            State
    end.

resubmit0(Msg, {FromPid, _}=From, #state{rules_pid = Pid, rules_module = GameFSM}) ->
    Res = GameFSM:make_move(Pid, FromPid, Msg),
    gen_server:reply(From, Res),
    ok.

publish0(Msg, #state{parent_relay = ParentRelay} = State) ->
    Self = self(),
    case ParentRelay of
        {_, Self} -> republish0(Msg, State);
        {ParentModule, ParentPid} -> ParentModule:publish(ParentPid, Msg)
    end,
    ok.

republish0(Msg, #state{subs = Subs}) ->
%%    [gen_server:cast(Pid, Msg)|| #subscriber{pid = Pid} <- Subs].
    F = fun(#subscriber{pid = Pid}, Acc) ->
                gen_server:cast(Pid, Msg),
                Acc + 1
        end,
    _C = ets:foldr(F, 0, Subs),
    ok.

no_of_subscribers(State) ->
    ets:info(State#state.subs, size).

lobby_join(Ref, User, Pid, State) ->
    ?INFO("Player ~p with pid ~p comes to ~p",[User#'PlayerInfo'.id,Pid,self()]),
    UserId = User#'PlayerInfo'.id,
    IsRobot = User#'PlayerInfo'.robot,
    WL = State#state.lobby_list,
    LeftPlayers = WL -- [case IsRobot of true -> robot; _ -> UserId end],
    NewState = case (lists:member(UserId, WL)or(IsRobot)) of
        true ->  Player = #player{id = UserId, monref = Ref, pid = Pid, is_bot = IsRobot, info = User},
                 Players = [Player | State#state.players],
                 ?INFO("Joining from wating list to signed-in ~p: ~p", [self(),{UserId,LeftPlayers,Players}]),
                 State#state{players = Players, lobby_list = LeftPlayers };
        false -> ?INFO("Player ~p should be one of the left: ~p", [UserId,WL]), State
    end,
    case LeftPlayers == [] of
        true ->  ?INFO("All players ready, starting the game ~p.",[NewState#state.players]),
                 {PRelMod, PRelPid} = State#state.parent_relay,
                 PRelMod:im_ready(PRelPid),
                 NewState;
        false -> NewState
    end.


lobby_leave(Pid, State) ->
    Players = State#state.players,
    case lists:keyfind(Pid, #player.pid, Players) of
        false ->
            State;
        #player{id = UserId} = P ->
            WL = State#state.lobby_list,
            State#state{lobby_list = [UserId | WL],
                        players = Players -- [P]}
    end.

notify_if_user_leaving(Pid, State) ->
    DenyRobots = proplists:get_value(deny_robots, State#state.table_settings, false) == true,
    AllowRobots = not DenyRobots,
    AllowReplacement = proplists:get_value(allow_replacement, State#state.table_settings, true),
    Player = lists:keyfind(Pid, #player.pid, State#state.players),
    LastHuman = is_last_human_player(Pid, State#state.players),
    GameAtom = State#state.rules_module,
    GameId = State#state.topic,
    ?INFO("replacement settings: ~p", [{AllowReplacement, AllowRobots, Player, LastHuman}]),
    case {AllowReplacement, AllowRobots, Player, LastHuman} of
        {_, _, false, _} ->
            %% not a player => no worries
            ?INFO("not a player => no worries", []),
            State;
        {true, true, #player{id = PlayerId} = Player, false} ->
            %% robot replacement allowed => do it
            ?INFO("robot replacement allowed => do it. Player left: ~p", [PlayerId]),
            replace_with_robot(Player, Pid, State);
        {_, _, #player{id = PlayerId}, _} ->
            %% last human => stop operations
            ?INFO("last human or replacement not allowed => stop operations", []),
            gen_server:cast(self(), {stop_game, opponent_out}),
            game_ended(PlayerId, Pid, State)
    end.

replace_session(MonRef, SessionPid, PlayerId, #state{players=Players,
                                                     subs=Subs,
                                                     rules_module=RMod,
                                                     rules_pid=RPid,
                                                     topic=Topic,
                                                     rematch_list=RL
                                                    }=State) ->

    case lists:keyfind(PlayerId, #player.id, Players) of
        #player{pid=OldSessionPid,
                monref=OldSessionMonRef
               }=OldPlayer ->

            ?INFO("replace_if_same; The user is already subscribed. The new session will replace the old one.", []),
            erlang:demonitor(OldSessionMonRef),
            ets:delete(Subs, OldSessionPid),
            game_session:logout(OldSessionPid),

            ?INFO("replace_if_same; Trying to get user info for the session pid:~w PlayerId:~w.~n", [SessionPid, PlayerId]),
            NewUser = game_session:get_player_info(SessionPid),
            ?INFO("replace_if_same; The user info for the pid:~w is :~w.~n", [SessionPid, NewUser]),
            {PlayerMsgs, _RobotData} =
                RMod:signal(RPid,
                            {replace_player, PlayerId, PlayerId,
                             NewUser, SessionPid}),

            [ SessionPid ! #game_event{game = Topic,
                                       event = api_utils:name(Msg),
                                       args = api_utils:members(Msg)}
              || Msg <- PlayerMsgs ],

            Msg = #player_left{player = PlayerId,
                               human_replaced = true,
                               replacement = NewUser},

            publish(self(), #game_event{event = api_utils:name(Msg),
                                        args = api_utils:members(Msg)}),

            NewPlayer = #player{pid = SessionPid,
                                id = PlayerId,
                                monref = MonRef},
            NewPlayers = utils:lists_replace(OldPlayer, NewPlayer, Players),

            NewRL = utils:lists_replace(OldSessionPid, SessionPid, RL),
            ?INFO("replace_if_same; The replacement is finished.", []),
            State#state{players = NewPlayers, rematch_list = NewRL};

        false ->
            State
    end.


%% replace_with_player(OldPlayer, OldPid, NewPid, State) ->
%%     ?INFO("replacement with player. Player left: ~p", [OldPlayer]),
%%     NewUser = game_session:get_player_info(NewPid),
%%     ?INFO("player info: ~p", [NewUser]),
%%     NewUId = NewUser#'PlayerInfo'.id,
%%     OldUId = OldPlayer#player.id,
%%     RMod = State#state.rules_module,
%%     RPid = State#state.rules_pid,
%%     {PlayerMsgs, _RobotData} = RMod:signal(RPid, {replace_player, OldUId, NewUId, NewUser, NewPid}),
%%     [ NewPid ! #game_event{game = State#state.topic, event = api_utils:name(Msg), args = api_utils:members(Msg)}|| Msg <- PlayerMsgs ],
%%     subscribe(self(), NewPid),
%%     Msg = #player_left{player = OldUId, human_replaced = true, replacement = NewUser},
%%     publish(self(), #game_event{event = api_utils:name(Msg), args = api_utils:members(Msg)}),
%%     NewPlayer = #player{pid = NewPid, id = NewUId, monref = erlang:monitor(process, NewPid)},
%%     Players = utils:lists_replace(OldPlayer, NewPlayer, State#state.players),
%%     RL = utils:lists_replace(OldPid, NewPid, State#state.rematch_list),
%%     State#state{players = Players, rematch_list = RL}.

replace_with_robot(Player, Pid, State) ->
    #player{id = PlayerId} = Player,
    ?INFO("replacement with robot. Player left: ~p", [PlayerId]),
    {NPid, NUId, User} = init_replacement_robot(PlayerId, State),
    subscribe(self(), NPid, User),
    ?INFO("new bot has been spawned: ~p", [NUId]),
    Msg = #player_left{player = PlayerId, bot_replaced = true, replacement = User},
    publish(self(), #game_event{event = api_utils:name(Msg), args = api_utils:members(Msg)}),
    NewPlayer = #player{is_bot = true, pid = NPid, id = NUId, info = User, monref = erlang:monitor(process, NPid)},
    Players = utils:lists_replace(Player, NewPlayer, State#state.players),
    ?INFO("players list has been updated", []),
%%    RL = utils:lists_replace(Pid, NPid, State#state.rematch_list),
    RL = [],
    ?INFO("rematch_list list has been updated", []),
    State#state{players = Players, rematch_list = RL}.

game_ended(PlayerId, Pid, State) ->
    GameFSMPid = State#state.rules_pid,
    GameFSM = State#state.rules_module,
    Msg = #player_left{player = PlayerId, bot_replaced = false},
    publish(self(), #game_event{event = api_utils:name(Msg), args = api_utils:members(Msg)}),
    GameFSM:signal(GameFSMPid, {player_left, Pid}),
    Players = lists:keydelete(Pid, #player.pid, State#state.players),
    State#state{players = Players}.

is_last_human_player(Pid, Players) ->
    L2 = lists:keydelete(Pid, #player.pid, Players),
    lists:all(fun (#player{is_bot = true}) -> true;
                  (#player{is_bot = _}) -> false end, L2).

bot_module(game_okey) -> game_okey_bot;
bot_module(game_tavla) -> game_tavla_bot.

init_replacement_robot(UId, State) ->
    {NPid, SPid, NUId, User} = create_robot(State),
    BM = bot_module(State#state.rules_module),
    RPid = State#state.rules_pid,
    RMod = State#state.rules_module,
    {_Msgs, _RobotInfo} = RMod:signal(RPid, {replace_player, UId, NUId, User, SPid}),
    BM:join_game(NPid),
    {SPid, NUId, User}.

add_robot(State) ->
    {NPid, SPid, NUId, _User} = create_robot(State),
    BM = bot_module(State#state.rules_module),
    BM:join_game(NPid),
    {SPid, NUId}.

create_robot(State) ->
    User = auth_server:robot_credentials(),
    NUId = User#'PlayerInfo'.id,
    ?INFO("Created Fake Credentials for Robot : ~p",[User]),
    BM = bot_module(State#state.rules_module),
    {ok, NPid} = BM:start(self(), User, State#state.topic),
    SPid = BM:get_session(NPid),
    {NPid, SPid, NUId, User}.

is_player(Pid, _State) when is_pid(Pid) -> erlang:error(not_implemented);
is_player(UId, State) ->  lists:member(UId, State#state.reg_players).
can_observe0(Id, State) -> proplists:get_value(observers, State#state.table_settings, true) orelse is_player(Id, State).

