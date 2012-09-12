-module(relay).
-behaviour(gen_server).

-include_lib("nsg_srv/include/social_actions.hrl").
-include_lib("nsg_srv/include/logging.hrl").
-include_lib("nsg_srv/include/requests.hrl").
-include_lib("nsg_srv/include/setup.hrl").
-include_lib("nsm_srv/include/table.hrl").
-include_lib("nsm_srv/include/accounts.hrl").
-include_lib("alog/include/alog.hrl").
-include_lib("nsg_srv/include/settings.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([do_rematch/2,
         signal/2,
         publish/2,
         submit/2,
         game/1,
         to_session/3,
         subscribe/2,
         subscribe/3,
         unsubscribe/2,
         start/4,
         start/5,
         stop/1,
         get_topic/1,
         get_player_state/2,
         get_table_info/1,
         update_gamestate/2,
         can_observe/2,
         qlc/0,
         unreg/2]).

-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-record(subscriber, {
          pid,
          id,
          ref}).

-record(player, {
          id     :: 'PlayerId'(),
          pid    :: pid(),
          monref :: 'MonitorRef'(),
          is_bot = false
         }).

-record(state, {
          topic :: any(),                       %% game id
          manager :: pid(),
          subs = ets:new(subs, [ordered_set, {keypos, #subscriber.pid}]), %% subscribed players/viewers
          players :: list(#player{}),           %% list of connected players, not undefined iff rules_module =/= chat
          reg_players :: list('PlayerId'()),    %% list of registered players, connected or not
          robots = [] :: list('PlayerId'()),
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

publish(Srv, Msg) -> % published instantly
    gen_server:cast(Srv, {publish, Msg}).

submit(Srv, Msg) -> % passed to game_fsm, with answer returned to caller
    gen_server:call(Srv, {submit, Msg}).

signal(Srv, Msg) -> % send cast to game engine, not waiting for reply
    gen_server:cast(Srv, {signal, Msg}).

to_session(Srv, Session, Msg) -> % used for game engine -> session communication
    gen_server:cast(Srv, {to_session, Session, Msg}).

unreg(Srv, Key) ->
    gen_server:call(Srv, {unreg, Key}).

subscribe(Srv, Pid, PlayerId) ->
    gen_server:cast(Srv, {subscribe, Pid, PlayerId}).

subscribe(Srv, Pid) ->
    gen_server:cast(Srv, {subscribe, Pid, null}).

unsubscribe(Srv, Pid) ->
    gen_server:cast(Srv, {unsubscribe, Pid}).

do_rematch(Srv, Pid) ->
    gen_server:call(Srv, {do_rematch, Pid}).

get_topic(Srv) ->
    gen_server:call(Srv, get_topic).

get_player_state(Srv, UId) ->
    gen_server:call(Srv, {get_player_state, UId}).

get_table_info(Srv) ->
    gen_server:call(Srv, get_table_info).

update_gamestate(Srv, NewGameState) ->
    gen_server:cast(Srv, {update_gamestate, NewGameState}).

can_observe(Srv, Id) ->
    gen_server:call(Srv, {can_observe, Id}).

start(GameId, GameFSM, Pids, Manager) ->
    start(GameId, GameFSM, [], Pids, Manager).
start(GameId, GameFSM, Params, Pids, Manager) ->
    ?INFO("relay:start/5 ~p~n",[{GameId, GameFSM, Params, Pids, Manager}]),
    gen_server:start(?MODULE, [GameId, GameFSM, Params, Pids, Manager], []).

stop(Srv) ->
    gen_server:cast(Srv, stop).

qlc() ->
    qlc:e(qlc:q([Val || {{_,_,Val},_,_} <- gproc:table(props)])).

game(game_okey) -> okey;
game(game_tavla) -> tavla;
game(_) -> okey.


init([Topic, chat, _, _]) ->
    {ok, #state{topic = Topic, rules_pid = none, rules_module = chat,
                players = [], reg_players = [], gamestate = no_game}};

init([Topic, {lobby, GameFSM}, Params0, PlayerIds, Manager]) ->
    ?INFO("init lobby ~p",[{GameFSM,Params0,PlayerIds,Manager}]),
    Settings = GameFSM:get_settings(Params0),
    R = GameFSM:get_requirements(),
    NoOfPlayers = proplists:get_value(players, R),
    true = NoOfPlayers =/= undefined,
    true = NoOfPlayers =:= length(PlayerIds),

    {table_name,TableName} = case TN =lists:keyfind(table_name,1,Settings) of false -> {table_name,"no table"}; _ -> TN end,
    {rounds,Rounds} = case Rnds =lists:keyfind(rounds,1,Settings) of false -> {rounds,1}; _ -> Rnds end,
    {game_mode,GameMode} = case GM =lists:keyfind(game_mode,1,Settings) of false -> {game_mode,standard}; _ -> GM end,
    {owner,Owner} = case O =lists:keyfind(owner,1,Settings) of false -> {owner,"maxim"}; _ -> O end,

    {Params,P,PE} = case rpc:call(?APPSERVER_NODE,pointing_rules,get_rules,[GameFSM, GameMode, Rounds]) of
		     {ok, PR, PREx} -> {Params0 ++ [{pointing_rules, PR},{pointing_rules_ex, PREx}],PR,PREx};
		     _ -> {Params0,#pointing_rule{rounds=1, game = game(GameFSM),
							  kakush_winner = 1, kakush_other = 1, quota = 1},
                                   [#pointing_rule{rounds=1, game = game(GameFSM),
							  kakush_winner = 1, kakush_other = 1, quota = 1}]}
    end,

%    Params = Params0 ++ [{pointing_rules, PR},{pointing_rules_ex, PREx}],

    GProcVal = #game_table{game_type = GameFSM, 
                           game_process = self(),
                           id = Topic,
                           age_limit = crypto:rand_uniform(20,30),
                           game_mode = GameMode,
                           game_speed = normal,
                           owner = Owner,
                           creator = Owner,
                           rounds = Rounds,
                           pointing_rules   = P,
                           pointing_rules_ex = PE,
       
                           users = [ case User of robot -> robot; _ -> erlang:binary_to_list(User) end || User <- PlayerIds],
                           name = TableName ++ " gaming " ++ erlang:integer_to_list(Topic) ++ " "
               },

    ?INFO("GProc Registration: ~p",[GProcVal]),

    gproc:reg({p,g,self()},GProcVal),

    Manager ! {add_game, GameFSM },

    {_RobotIds, HumanIds} = lists:partition(fun(robot) -> true; (_) -> false end, PlayerIds),

    {only_robots_table, false} = {only_robots_table, length(HumanIds) < 1},

    Relay = self(),
    State = #state{topic = Topic, rules_pid = none, rules_module = GameFSM, rules_params = Params,
		   players = [], reg_players = PlayerIds, lobby_list = HumanIds, manager = Manager,
                   gamestate = lobby, from_lobby = true, table_settings = Settings},
    ?INFO("State Lobby List: ~p",[State#state.lobby_list]),
    gen_server:cast(Relay, {update_gamestate, State#state.gamestate}),
    {ok, State};

init([Topic, GameFSM, Params, Pids]) -> % TODO: remove this code (maxim)
    ?INFO("INIT FSM"),
    Settings = GameFSM:get_settings(Params),
    try
        Players = lists:map(fun(Session) ->
                                    PlayerId = game_session:get_player_id(Session),
                                    MonRef = erlang:monitor(process, Session),
                                    #player{id = PlayerId, pid = Session, monref = MonRef}
                            end, Pids),
        PlayerIds = [ P#player.id || P <- Players ],
       %% TODO: handle tournaments Params
       %% NOTE: tournamets will call GenFSM:start from itself
        {ok, FsmPid} = GameFSM:start(self(), Pids, Topic, Params),
        erlang:monitor(process, FsmPid),
        Relay = self(),
        State = #state{topic = Topic,
                       rules_pid = FsmPid, rules_module = GameFSM, rules_params = Params,
                       players = Players, reg_players = PlayerIds, rematch_list = Pids,
                       lobby_list = [], gamestate = started, table_settings = Settings},
        gen_server:cast(Relay, {update_gamestate, State#state.gamestate}),
        {ok, State}
    catch
        _Class:Reason ->
            ?INFO("normal init - errored", []),
            {error, Reason}
    end.

handle_call({submit, _Msg}, _From, #state{rules_module = chat} = State) ->
    {reply, {error, chat_has_no_game_fsm_module__cant_submit}, State};

handle_call({submit, Msg}, {From, _}, #state{rules_pid = Pid, rules_module = GameFSM} = State) ->
    ?INFO("SUBMIT: ~p",[{Pid,From,Msg}]),
    Res = GameFSM:make_move(Pid, From, Msg),
    {reply, Res, State};

handle_call({do_rematch, _}, _From, State = #state{gamestate = lobby}) ->
    {reply, {error, cannot_rematch_in_lobby}, State};

handle_call({do_rematch, Pid}, _From, State) ->
%   State = #state{rematch_list = [Pid], rematch_timer = {running, Timer}}) ->
%   timer:cancel(Timer),
    ?INFO("rematch A. Pid:~p", [Pid]),
%   RematchList = State#state.rematch_list,
%   WLStatus = lists:member(Pid, RematchList),
    GameFSMPid = State#state.rules_pid,
    GameFSM = State#state.rules_module,
    ?INFO("rematch A. finished", []),
    GameFSM:signal(GameFSMPid, do_rematch),
    publish(self(), #game_rematched{game = State#state.topic}),
    PlayersPids = [ Player#player.pid || Player <- State#state.players],
    {reply, ok, State#state{rematch_list = PlayersPids, rematch_timer = undefined}};

%handle_call({do_rematch, Pid}, _From, State) -> % = #state{rematch_timer = {running, _}}) ->
%    ?INFO("rematch B. Pid:~p", [Pid]),
%    RematchList = State#state.rematch_list,
%    WLStatus = lists:member(Pid, RematchList),
%    case WLStatus of
%        true ->
%            {reply, ok, State#state{rematch_list = RematchList--[Pid]}};
%        {_, _} ->
%            {reply, {error, you_are_not_a_player}, State}
%    end;

%handle_call({do_rematch, Pid}, _From, State) ->
%    ?INFO("Error. Pid ~p has asked for rematch. Not possible!~nTimer state: ~p", [Pid, State#state.rematch_timer]),
%    {reply, {error, rematch_not_possible}, State};

handle_call(get_topic, _From, State) ->
    {reply, State#state.topic, State};

handle_call({update_reg, Key, Value}, _From, State) ->
    gproc:set_value({p,g,Key},Value),
    {reply, ok, State};

handle_call({get_player_state, UId}, _From, State) ->
    FSM = State#state.rules_pid,
    Game = State#state.rules_module,
    Res = Game:get_player_state(FSM, UId),
    {reply, Res, State};

handle_call(get_table_info, _From, State) ->
    List = ets:tab2list(State#state.subs),
    Viewers = lists:map(fun(#subscriber{id = PlayerId}) ->
                                PlayerId
                        end, List),
    Players = [ PlayerId || #player{id = PlayerId} <- State#state.players ],
    GameStatus = list_to_binary(atom_to_list(State#state.gamestate)),
    GameName = api_utils:gamemodule_to_gametype(State#state.rules_module),
    Res = #'TableInfo'{viewers = Viewers, players = Players, chat = [],
                       game = GameName, game_status = GameStatus},
    {reply, Res, State};

handle_call({can_observe, Id}, _From, State) ->
    Res = can_observe0(Id, State),
    {reply, Res, State};

handle_call(Event, From, State) ->
    {stop, {unknown_call, Event, From}, State}.

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

handle_cast(room_ready, State) -> % deleyed fsm creation after all join the lobby
    Relay = self(),
    Fun = fun() ->
                  GameFSM = State#state.rules_module,
                  Pids = [ P#player.pid || P <- State#state.players ],
                  {ok, RPid} = GameFSM:start(Relay, Pids, State#state.topic, State#state.rules_params),
                  gen_server:cast(Relay, {game_created, RPid})
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

handle_cast({subscribe, Pid, PlayerId}, State = #state{gamestate = lobby}) -> % automatically unsubscribe when dead
    Ref = erlang:monitor(process, Pid),
    Pid ! ack,
    ets:insert(State#state.subs, #subscriber{pid = Pid, id = PlayerId, ref = Ref}),
    State1 = lobby_join(Ref, PlayerId, Pid, State),
    {noreply, State1};

handle_cast({subscribe, Pid, PlayerId}, #state{players=Players, subs=Subs}=State) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! ack,
    ets:insert(Subs, #subscriber{pid = Pid, id = PlayerId, ref = Ref}),
    %% Replace old session by the new one if the player id is matched.
    case lists:keyfind(PlayerId, #player.id, Players) of
        #player{} ->
            %% This is needed to avoid possible deadlock
            timer:send_after(5000, {replace, Pid, PlayerId, Ref});
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
    ?INFO("relay session died (wait for user reconnection), Pid: ~p", [Pid]),
    try 
    gproc:unreg({p,g,self()}),
    State#state.manager ! {remove_game, State#state.rules_module},
    ?INFO("game manager notified ~p ! ~p",[State#state.manager,{remove_game, State#state.rules_module}])
    catch _:_ -> noting
    end,
    {ok, _}=timer:send_after(?DISCONNECT_TIMEOUT, {disconnect, Pid}),
    {noreply, State};

handle_info({unreg, _Key}, State) ->
    try 
    gproc:unreg({p,g,self()}),
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


handle_info(Info, State) ->
    {stop, {unknown_info, Info}, State}.

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

publish0(Msg, State) ->
    % Start = now(),
    F = fun(#subscriber{pid = Pid}, Acc) ->
                gen_server:cast(Pid, Msg),
                Acc + 1
        end,
    process_flag(priority, high),
    _C = ets:foldr(F, 0, State#state.subs),
    % ?INFO("msg ~p sent to ~p parties", [Msg, C]),
    % io:format("time: ~p~n, Msg: ~p", [timer:now_diff(now(), Start) / 1000, Msg]),
    process_flag(priority, normal),
    ok.

no_of_subscribers(State) ->
    ets:info(State#state.subs, size).

lobby_join(Ref, UserId, Pid, State = #state{lobby_list = [UserId]}) ->
%    ?INFO("joining last player to lobby: ~p", [UserId]),
    R = (State#state.rules_module):get_requirements(),
    NoOfPlayers = proplists:get_value(players, R),
     Player = #player{id = UserId,
                     monref = Ref,
                     pid = Pid,
                     is_bot = lists:member(UserId, State#state.robots)
                    },
    Players = lists:reverse([Player | State#state.players]),
    RL = [ P#player.pid || P <- Players ],
    {RobotIds, HumanIds} = lists:partition(fun(robot) -> true; (_) -> false end,
                                           State#state.reg_players),
    case length(Players) of
        NoOfPlayers ->
            gen_server:cast(self(), room_ready),
            State#state{players = Players, rematch_list = RL, lobby_list = []};
        _ ->
            {_, RobotUIds} = lists:unzip([ add_robot(State) || _ <- RobotIds ]),
            State#state{players = Players, rematch_list = RL, robots = RobotUIds,
                        reg_players = HumanIds ++ RobotUIds, lobby_list = RobotUIds}
    end;

lobby_join(Ref, UserId, Pid, State) ->
%    ?INFO("joining player to lobby: ~p", [UserId]),
    WL = State#state.lobby_list,
    case lists:member(UserId, WL) of
        true ->
            Player = #player{id = UserId,
                             monref = Ref,
                             pid = Pid,
                             is_bot = lists:member(UserId, State#state.robots)
                            },
            Players = [Player | State#state.players],
%            Val = gproc:get_value(players),
%            gproc:set_value(Val+1),
            State#state{players = Players, lobby_list = WL -- [UserId]};
        false ->
            ?INFO("not a player. Players left: ~p", [length(WL)]),
            State
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
    AllowReplacement = proplists:get_value(allow_replacement, State#state.table_settings, false),
    IsPlayer = lists:keyfind(Pid, #player.pid, State#state.players),
    LastHuman = is_last_human_player(Pid, State#state.players),
    GameAtom = State#state.rules_module,
    GameId = State#state.topic,
    ?INFO("replacement settings: ~p", [{AllowReplacement, AllowRobots, IsPlayer, LastHuman}]),
    case {AllowReplacement, AllowRobots, IsPlayer, LastHuman} of
        {_, _, false, _} ->
            %% not a player => no worries
            ?INFO("not a player => no worries", []),
            State;
        {false, false, #player{id = PlayerId}, _} ->
            %% replacements forbidden => stop operations
            ?INFO("replacements forbidden => stop operations", []),
            gen_server:cast(self(), kill_bots),
            game_ended(PlayerId, Pid, State);
        {_, _, #player{id = PlayerId}, true} ->
            %% last human => stop operations
            ?INFO("last human => stop operations", []),
            gen_server:cast(self(), kill_bots),
            game_ended(PlayerId, Pid, State);
        {true, _, #player{id = PlayerId} = Player, false} ->
            %% human replaced allowed => do it
            ?INFO("human replaced allowed => do it", []),
%            case {match_maker:get_replacement(GameId, GameAtom), AllowRobots} of
            case {false,true} of
                {false, true} ->
                    ?INFO("replacement human; no humans in queue; will replace with robot", []),
                    replace_with_robot(Player, Pid, State);
                {false, false} ->
                    ?INFO("replacement human; no humans in queue; ending game", []),
                    game_ended(PlayerId, Pid, State);
                {{ok, NPid}, _} ->
                    ?INFO("replacement human; got human. His session pid: ~p", [NPid]),
                    replace_with_player(Player, Pid, NPid, State)
            end;
        {_, true, #player{id = PlayerId} = Player, false} ->
            %% robot replacement allowed => do it
            ?INFO("robot replacement allowed => do it. Player left: ~p", [PlayerId]),
            replace_with_robot(Player, Pid, State)
    end.

%%--------------------------------------------------------------------
%% Function: replace_session(MonRef, SessionPid, PlayerId, State1) -> State2
%% Types:
%%     MonRef = reference()
%%     SessionPid = pid()
%%     PlayerId = 'PlayerId'()
%%     State1 = State2 = state()
%% Description: Replace the old user session by the new one if the old session
%%     exists.

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


replace_with_player(OldPlayer, OldPid, NewPid, State) ->
    ?INFO("replacement with player. Player left: ~p", [OldPlayer]),
    NewUser = game_session:get_player_info(NewPid),
    ?INFO("player info: ~p", [NewUser]),
    NewUId = NewUser#'PlayerInfo'.id,
    OldUId = OldPlayer#player.id,

    RMod = State#state.rules_module,
    RPid = State#state.rules_pid,
    {PlayerMsgs, _RobotData} = RMod:signal(RPid, {replace_player, OldUId, NewUId, NewUser, NewPid}),
    [ NewPid ! #game_event{game = State#state.topic,
                           event = api_utils:name(Msg),
                           args = api_utils:members(Msg)}
      || Msg <- PlayerMsgs ],
    subscribe(self(), NewPid),
    Msg = #player_left{player = OldUId, human_replaced = true, replacement = NewUser},
    publish(self(), #game_event{event = api_utils:name(Msg), args = api_utils:members(Msg)}),
    NewPlayer = #player{pid = NewPid, id = NewUId, monref = erlang:monitor(process, NewPid)},
    Players = utils:lists_replace(OldPlayer, NewPlayer, State#state.players),
    RL = utils:lists_replace(OldPid, NewPid, State#state.rematch_list),
    State#state{players = Players, rematch_list = RL}.

replace_with_robot(Player, Pid, State) ->
    #player{id = PlayerId} = Player,
    ?INFO("replacement with robot. Player left: ~p", [PlayerId]),
    {NPid, NUId, User} = init_replacement_robot(PlayerId, State),
    subscribe(self(), NPid),
    Msg = #player_left{player = PlayerId, bot_replaced = true, replacement = User},
    publish(self(), #game_event{event = api_utils:name(Msg), args = api_utils:members(Msg)}),
    NewPlayer = #player{is_bot = true, pid = NPid, id = NUId, monref = erlang:monitor(process, NPid)},
    Players = utils:lists_replace(Player, NewPlayer, State#state.players),
    RL = utils:lists_replace(Pid, NPid, State#state.rematch_list),
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
    lists:all(fun
                  (#player{is_bot = true}) ->
                      true;
                  (#player{is_bot = _}) ->
                      false
              end, L2).

bot_module(game_okey) ->
    game_okey_bot;

bot_module(game_tavla) ->
    game_tavla_bot.

init_replacement_robot(UId, State) ->
    {NPid, SPid, NUId, User} = create_robot(State),
    BM = bot_module(State#state.rules_module),
    RPid = State#state.rules_pid,
    RMod = State#state.rules_module,
    {Msgs, RobotInfo} = RMod:signal(RPid, {replace_player, UId, NUId, User, SPid}),
    BM:init_state(NPid, {Msgs, RobotInfo}),
    {SPid, NUId, User}.

add_robot(State) ->
    {NPid, SPid, NUId, _User} = create_robot(State),
    BM = bot_module(State#state.rules_module),
    BM:join_game(NPid),
    {SPid, NUId}.

create_robot(State) ->
    User = auth_server:robot_credentials(),
    NUId = User#'PlayerInfo'.id,
    BM = bot_module(State#state.rules_module),
    {ok, NPid} = BM:start(self(), User, State#state.topic),
    SPid = BM:get_session(NPid),
    {NPid, SPid, NUId, User}.

is_player(Pid, _State) when is_pid(Pid) ->
    erlang:error(not_implemented);
is_player(UId, State) ->
    PL = State#state.reg_players,
    lists:member(UId, PL).

can_observe0(Id, State) ->
    ObsAllowed = proplists:get_value(observers, State#state.table_settings, true),
    ObsAllowed orelse is_player(Id, State).

