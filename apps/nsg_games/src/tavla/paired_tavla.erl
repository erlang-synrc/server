-module(paired_tavla).
-author('Serge Polkovnikov <serge.polkovnikov@gmail.com>').
-behaviour(gen_server).

-export([
         %%do_rematch/2,
         signal/2,
         publish/2,
         notify_tables/2,
         submit/4,
         %%to_session/3,
         get_requirements/2,
         %%subscribe/2, subscribe/3, unsubscribe/2,
         start_link/5,
         stop/1,
         get_topic/1,
         get_player_state/2,
         %%get_table_info/1, update_gamestate/2,
         can_observe/2,
         unreg/2,
         im_ready/1
        ]).

-include_lib("nsg_srv/include/social_actions.hrl").
%%-include_lib("nsg_srv/include/logging.hrl").
-include_lib("nsg_srv/include/requests.hrl").
-include_lib("nsg_srv/include/setup.hrl").
-include_lib("nsg_srv/include/basic_types.hrl").
-include_lib("nsg_srv/include/game_tavla.hrl").
-include_lib("nsm_db/include/table.hrl").
-include_lib("nsm_db/include/accounts.hrl").
-include_lib("alog/include/alog.hrl").
-include_lib("nsg_srv/include/settings.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-define(STATE_WAIT_TABLES, state_wait_tables).
-define(STATE_FIRST_MOVE_DETERMINATION, state_first_move_determination).
-define(STATE_MOVES, state_moves).
-define(STATE_DICES, state_dices).
-define(STATE_NEXT_ROUND_CONFIRMATION, state_next_round_confirmation).
-define(STATE_FINISHED, state_finished).

%% -record(subscriber, {
%%           pid,
%%           id,
%%           ref}).

-record(player, {
          id     :: 'PlayerId'(),
          table  :: pos_integer(),
          is_bot = false
         }).

-record(state, {
          tstate :: atom(),
          topic :: any(),         %% game id
          tables_users :: list(), %% list({table_id(), user_id(), user_id}) 
          tables_pids :: list(),  %% list({table_id(), pid()}) list with tables relays
          tables_num :: integer(),
          manager :: pid(),
          main_table :: list('PlayerId'()),
%%          subs = ets:new(subs, [ordered_set, {keypos, #subscriber.pid}]), %% subscribed players/viewers
          players :: list(#player{}),           %% list of connected players
          reg_players :: list('PlayerId'()),    %% list of registered players, connected or not
          robots = [] :: list('PlayerId'()),
          lobby_list :: list('PlayerId'()) | list(pid()),  %% wait list for lobby
          rematch_list :: list('PlayerId'()) | list(pid()),  %% wait list for rematch
          rules_module :: atom(),               %% game state machine module
          rules_params :: list(tuple(atom(), any())), %% game state machine starting params
          rematch_timer :: tuple(running, any()) | expired | undefined,
          table_settings,
          ready_counter :: integer(),
          round_tables_counter :: integer(), %% Stores number of tables which game is not finished yet
          round_results :: list(),           %% Stores results of game in current round
          dices_mode :: main_table | tournament_master, %% Who responsible for dices values generation
          cur_move_color :: undefined | 1 | 2,
          moves_counter :: integer(),        %% How many tables should make move before next dices roll
          confirm_wl :: list('PlayerId'())
         }).



signal(Srv, Msg) -> gen_server:cast(Srv, {signal, Msg}).
%%to_session(Srv, Session, Msg) -> gen_server:cast(Srv, {to_session, Session, Msg}).
unreg(Srv, Key) -> gen_server:call(Srv, {unreg, Key}).
%%subscribe(Srv, Pid, PlayerId) -> gen_server:cast(Srv, {subscribe, Pid, PlayerId}).
%%subscribe(Srv, Pid) -> gen_server:cast(Srv, {subscribe, Pid, null}).
%%unsubscribe(Srv, Pid) -> gen_server:cast(Srv, {unsubscribe, Pid}).
%%do_rematch(Srv, Pid) -> gen_server:call(Srv, {do_rematch, Pid}).
get_topic(Srv) -> gen_server:call(Srv, get_topic).
get_player_state(Srv, UId) -> gen_server:call(Srv, {get_player_state, UId}).
%%get_table_info(Srv) -> gen_server:call(Srv, get_table_info).
%%update_gamestate(Srv, NewGameState) -> gen_server:cast(Srv, {update_gamestate, NewGameState}).
can_observe(Srv, Id) -> gen_server:call(Srv, {can_observe, Id}).
stop(Srv) -> gen_server:cast(Srv, stop).
start_link(GameId, GameFSM, Params, Pids, Manager) -> gen_server:start_link(?MODULE, [GameId, GameFSM, Params, Pids, Manager], []).
get_requirements(GameFSM,Mode) -> [{max_users,10},{min_users,4}].
%% 2nd level relay API
publish(Srv, Msg) -> Self = self(), gen_server:cast(Srv, {publish, Self, Msg}).
notify_tables(Srv, Msg) -> Self = self(), gen_server:cast(Srv, {notify_tables, Self, Msg}).
im_ready(Srv) -> Self = self(), gen_server:cast(Srv, {im_ready, Self}).
submit(Srv, Msg, From, Player) -> Self = self(), gen_server:cast(Srv, {submit, Self, Msg, From, Player}).


init([Topic, {lobby, GameFSM}, Params0, PlayerIds, Manager]) ->

    ?INFO("init paired tavla lobby ~p",[{GameFSM,Params0,PlayerIds,Manager}]),

    Settings = Params0,

    TableName = proplists:get_value(table_name, Settings, "no table"),
    Rounds = proplists:get_value(rounds, Settings, 1),
    GameMode = proplists:get_value(game_mode, Settings, standard),
    MainUsers = proplists:get_value(main_users, Settings, []),
    GameSpeed = proplists:get_value(speed, Settings, normal),
    Owner = proplists:get_value(owner, Settings, "maxim"), %% FIXME

    ?INFO("~w:init/1 Owner: ~p", [?MODULE, Owner]),
    {Params,P,PE} = case rpc:call(?APPSERVER_NODE,pointing_rules,get_rules,[GameFSM, GameMode, Rounds]) of
                        {ok, PR, PREx} -> {Params0 ++ [{pointing_rules, PR},{pointing_rules_ex, PREx}],PR,PREx};
                        _ -> {Params0,#pointing_rule{rounds=1, game = tavla,
                                                     kakush_winner = 1, kakush_other = 1, quota = 1},
                              [#pointing_rule{rounds=1, game = tavla,
                                              kakush_winner = 1, kakush_other = 1, quota = 1}]}
                    end,
    FeelLucky = proplists:get_value(feel_lucky, Settings, false),

    GProcVal = #game_table{game_type = GameFSM,
                           game_process = self(),
                           id = Topic,
                           age_limit = crypto:rand_uniform(20,30),
                           game_mode = GameMode,
                           game_speed = GameSpeed,
                           feel_lucky = FeelLucky,
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

    {TabSpread, UsersSpread} = spread_users(list_to_binary(Owner), PlayerIds),
    ?INFO("Spread: ~p",[ {TabSpread, UsersSpread}]),
    TablesNum = length(TabSpread),
    Tables = spawn_tables(TabSpread, [], Topic, GameFSM, Params0, Manager, TablesNum),
    ?INFO("Paired Tavla Tables: ~p",[Tables]),

    {_RobotIds, HumanIds} = lists:partition(fun(robot) -> true; (_) -> false end, PlayerIds),

    {only_robots_table, false} = {only_robots_table, length(HumanIds) < 1},

    Players = [#player{id = UserId,
                       table = TabId
                      }
                      || {UserId, TabId} <- UsersSpread],

    State = #state{tstate = ?STATE_WAIT_TABLES,
                   topic = Topic,
                   main_table = MainUsers,
                   rules_module = GameFSM,
                   rules_params = Params,
                   players = Players,
                   reg_players = PlayerIds,
                   lobby_list = HumanIds,
                   manager = Manager,
                   table_settings = Settings,
                   tables_users = TabSpread,
                   tables_pids = Tables,
                   tables_num = TablesNum,
                   ready_counter = TablesNum
                  },
    {ok, State}.



%% handle_call({submit, _Msg}, _From, #state{rules_module = chat} = State) ->
%%     {reply, {error, chat_has_no_game_fsm_module__cant_submit}, State};
%% 
%% handle_call({submit, Msg}, {From, _}, #state{rules_pid = Pid, rules_module = GameFSM} = State) ->
%%     Res = GameFSM:make_move(Pid, From, Msg),
%%     {reply, Res, State};

%% handle_call({do_rematch, _}, _From, State = #state{gamestate = lobby}) ->
%%     {reply, {error, cannot_rematch_in_lobby}, State};

handle_call(get_topic, _From, State) ->
    {reply, State#state.topic, State};

handle_call({update_reg, Key, Value}, _From, State) ->
    gproc:set_value({p,g,Key},Value),
    {reply, ok, State};

%% handle_call({get_player_state, UId}, _From, State) ->
%%     FSM = State#state.rules_pid,
%%     Game = State#state.rules_module,
%%     Res = Game:get_player_state(FSM, UId),
%%     {reply, Res, State};

%% handle_call(get_table_info, _From, State) ->
%%     List = ets:tab2list(State#state.subs),
%%     Viewers = lists:map(fun(#subscriber{id = PlayerId}) ->
%%                                 PlayerId
%%                         end, List),
%%     Players = [ PlayerId || #player{id = PlayerId} <- State#state.players ],
%%     GameStatus = list_to_binary(atom_to_list(State#state.gamestate)),
%%     GameName = paired_tavla, %api_utils:gamemodule_to_gametype(State#state.rules_module),
%%     Res = #'TableInfo'{viewers = Viewers, players = Players, chat = [],
%%                        game = GameName, game_status = GameStatus},
%%     {reply, Res, State};

handle_call({can_observe, Id}, _From, State) ->
    Res = can_observe0(Id, State),
    {reply, Res, State};

handle_call(Event, From, State) ->
    {stop, {unknown_call, Event, From}, State}.

handle_cast(start_rematch_timer, State) ->
    {ok, Ref} = timer:send_after(?REMATCH_TIMEOUT, rematch_timer_ringing),
    {noreply, State#state{rematch_timer = {running, Ref}}};

%% handle_cast({update_gamestate, NewGameState}, State) ->
%%     ?INFO("update_gamestate: ~p", [NewGameState]),
%%     NewGameState == finished andalso
%%         gen_server:cast(self(), start_rematch_timer),
%%     {noreply, State#state{gamestate = NewGameState}};

%% handle_cast({signal, Msg}, State) ->
%%     #state{rules_pid = Pid, rules_module = GameFSM} = State,
%%     ?INFO("the signal: ~p", [Msg]),
%%     GameFSM:signal(Pid, Msg),
%%     {noreply, State};

%% handle_cast({to_session, Pid, #game_event{game = undefined} = Msg0}, State) ->
%%     Msg = Msg0#game_event{game = State#state.topic},
%%     handle_cast({to_session, Pid, Msg}, State);

%% handle_cast({to_session, Session, Msg}, State) ->
%%     Session ! Msg,
%%     {noreply, State};

handle_cast({submit, _Sender, #game_action{action = tavla_ready, args = Args}, From, Player},
             #state{confirm_wl = WL,
                    players = Players} = State) ->
    ?INFO("PAIRED_TAVLA Next round confirmation received. Table: ~p Player: ~p.", [proplists:get_value(table_id, Args), Player]),
    gen_server:reply(From, ok), %% Eat the action
    if WL == [Player] -> %% All players confirm next round
           ?INFO("PAIRED_TAVLA All next round confirmation received.", []),
           %% TODO: Pass Ready action to players tables
           init_state_first_move_determination(State);
       true ->
           {noreply, State#state{confirm_wl = WL -- [Player]}}
    end;

handle_cast({submit, Sender, Msg, From, _Player}, State) ->
    relay:resubmit(Sender, From, Msg),
    {noreply, State};


handle_cast({publish, _Sender, #game_event{event = tavla_game_ended, args = Args} = Msg},
            #state{tstate = TState,
                   round_tables_counter = TablesCounter,
                   moves_counter = MovesCounter,
                   round_results = Results,
                   dices_mode = CurDicesMode} = State) ->
    ?INFO("PAIRED_TAVLA Table ~p finished their game", [proplists:get_value(table_id, Args)]),
    publish0(Msg, State),
    NewResults = [Args | Results],
    if TablesCounter == 1 -> %% Last table has finished a game
           ?INFO("PAIRED_TAVLA All tables finished their games", []),
           {noreply, State#state{tstate = ?STATE_NEXT_ROUND_CONFIRMATION,
                                 round_results = NewResults}};
       true ->
           TableId = proplists:get_value(table_id, Args),
           DicesMode = if TableId == 1 -> tournament_master;
                          true -> CurDicesMode
                       end,
           NewState = State#state{round_tables_counter = TablesCounter - 1,
                                  round_results = NewResults,
                                  dices_mode = DicesMode},
           if TState == ?STATE_MOVES ->
                  if MovesCounter == 1 -> %% Last table moves
                         init_next_move(NewState);
                     true ->
                         {noreply, NewState#state{moves_counter = MovesCounter - 1}}
                  end;
              TState == ?STATE_DICES andalso TableId == 1 ->
                  init_next_move(NewState);
              true ->
                  {noreply, NewState}
           end
    end;

handle_cast({publish, _Sender, #game_event{event = tavla_rolls, args = Args} = Msg},
            #state{tstate = ?STATE_FIRST_MOVE_DETERMINATION,
                   tables_pids = Tables,
                   round_tables_counter = RoundTablesCounter} = State) ->
    case length(proplists:get_value(dices, Args)) == 2 of
        true ->
            FirstMoveColor = proplists:get_value(color, Args),
            ?INFO("PAIRED_TAVLA First dices rolled. First move color: ~p", [FirstMoveColor]),
            %% Dices of main table passed to all tables like as original
            [begin
                 NewArgs = replace_table_id(Args, TabId),
                 publish0(Msg#game_event{args = NewArgs}, State)
             end || {TabId, _TabPid} <- Tables],
            {noreply, State#state{tstate = ?STATE_MOVES,
                                  cur_move_color = FirstMoveColor,
                                  moves_counter = RoundTablesCounter
                                 }};
        false ->
            ?INFO("PAIRED_TAVLA A dace rolled: ~p", [proplists:get_value(dices, Args)]),
            publish0(Msg, State),
            {noreply, State}
    end;

handle_cast({publish, _Sender, #game_event{event = tavla_rolls, args = Args} = Msg},
            #state{tstate = ?STATE_DICES,
                   dices_mode = main_table,
                   tables_pids = Tables,
                   round_tables_counter = RoundTablesCounter} = State) ->
    ?INFO("PAIRED_TAVLA Main table has roll dices: ~p. Move color:~p", [proplists:get_value(dices, Args), proplists:get_value(color, Args)]),
    %% Dices of main table passed to all tables like as original
    [begin
         NewArgs = replace_table_id(Args, TabId),
         publish0(Msg#game_event{args = NewArgs}, State)
     end || {TabId, _TabPid} <- Tables],
    MoveColor = proplists:get_value(color, Args),
    {noreply, State#state{tstate = ?STATE_MOVES,
                          cur_move_color = MoveColor,
                          moves_counter = RoundTablesCounter
                         }};

%% handle_cast({publish, _Sender, #game_event{event = tavla_moves, args = Args} = Msg},
%%             #state{tstate = ?STATE_MOVES,
%%                    moves_counter = MovesCounter} = State) ->
%%     ?INFO("PAIRED_TAVLA Table ~p made his move. Tables left:~p", [proplists:get_value(table_id, Args), MovesCounter - 1]),
%%     publish0(Msg, State),
%%     if MovesCounter == 1 -> %% Last table moves
%%            ?INFO("PAIRED_TAVLA All tables made their moves. Waiting for dices roll.", []),
%%            init_next_move(State);
%%        true ->
%%            {noreply, State#state{moves_counter = MovesCounter - 1}}
%%     end;

handle_cast({publish, _Sender, #game_event{event = tavla_next_turn, args = Args} = Msg},
            #state{tstate = ?STATE_MOVES,
                   moves_counter = MovesCounter} = State) ->
    ?INFO("PAIRED_TAVLA Table ~p made his move. Tables left:~p", [proplists:get_value(table_id, Args), MovesCounter - 1]),
    publish0(Msg, State),
    if MovesCounter == 1 -> %% Last table moves
           ?INFO("PAIRED_TAVLA All tables made their moves. Waiting for dices roll.", []),
           init_next_move(State);
       true ->
           {noreply, State#state{moves_counter = MovesCounter - 1}}
    end;

handle_cast({publish, _Sender, Msg}, State) ->
    publish0(Msg, State),
    {noreply, State};

handle_cast({notify_tables, Sender, Msg}, State) ->
    notify_tables0(Msg, Sender, State),
    {noreply, State};

%% handle_cast({game_created, RPid}, State) ->
%%     ?INFO("room ready, starting game ~p~n", [{self(),RPid}]),
%%     GameFSM = State#state.rules_module,
%%     erlang:monitor(process, RPid),
%%     State2 = State#state{rules_pid = RPid, gamestate = started},
%%     GameFSM:signal(RPid, state_created),
%%     {noreply, State2};

handle_cast({im_ready, _ChPid}, #state{tstate = ?STATE_WAIT_TABLES,
                                       ready_counter = Counter} = State) ->
    ?INFO("I'm ready, starting game ~p~n", [{self(),_ChPid}]),
    if Counter == 1 -> %% All relays are ready (all clients connected)
           notify_tables0(start, self(), State),
           ?INFO("PAIRED_TAVLA All clients ready, start tournament", []),
           init_state_first_move_determination(State);
       true ->
           {noreply, State#state{ready_counter = Counter -1}}
    end;

handle_cast(stop, State) ->
    ?INFO("relay stop"),
    {stop, normal, State};

handle_cast(_Event, State) ->
    {noreply, State}.

%% handle_info(rematch_timer_ringing, State) ->
%%     State#state.rules_pid ! no_more_rematch,
%%     {noreply, State#state{rematch_timer = expired}};

%% handle_info({'DOWN', _, process, Pid, Reason}, State = #state{rules_pid = Pid}) ->
%%     ?INFO("relay is down. Reason: ~p", [Reason]),
%%     publish0(#game_crashed{game = State#state.topic}, self(), State),
%%     {stop, {error, game_crashed}, State};
%% 
%% handle_info({'DOWN', _, process, Pid, _}, State) ->
%%     ?INFO("relay session died (wait for user reconnection), Pid: ~p", [Pid]),
%%     try 
%%     gproc:unreg({p,g,self()}),
%%     State#state.manager ! {remove_game, State#state.rules_module},
%%     ?INFO("game manager notified ~p ! ~p",[State#state.manager,{remove_game, State#state.rules_module}])
%%     catch _:_ -> noting
%%     end,
%%     {ok, _}=timer:send_after(?DISCONNECT_TIMEOUT, {disconnect, Pid}),
%%     {noreply, State};
%% 
%% handle_info({unreg, _Key}, State) ->
%%     try 
%%     gproc:unreg({p,g,self()}),
%%     State#state.manager ! {remove_game, State#state.rules_module},
%%     ?INFO("game manager notified ~p ! ~p",[State#state.manager,{remove_game, State#state.rules_module}])
%%     catch _:_ -> noting
%%     end,
%%     {noreply, State};

handle_info(die, State) ->
    {stop, normal, State};

handle_info({get_second_level_relay, {Pid, Ref}, User}, #state{tables_pids = TablesPids,
                                                                 tables_users = Tables
                                                                }=State) ->

    ?INFO("~w:handle_info(get_second_level_relay) UserId = ~p", [?MODULE, User]),
    ?INFO("~w:handle_info(get_second_level_relay) Tables = ~p", [?MODULE, Tables]),

    {First,Second} = case User#'PlayerInfo'.robot of
      true ->  {lists:keyfind(robot, 2, Tables),
                lists:keyfind(robot, 3, Tables)};
      false -> {lists:keyfind(User#'PlayerInfo'.id, 2, Tables),
                lists:keyfind(User#'PlayerInfo'.id, 3, Tables)}
     end,

     {Res,Pos} = case First of false -> {Second,3}; _ -> {First,2} end,
     {TabId,_,_} = Res,
     {TabId,TabPid} = lists:keyfind(TabId,1,TablesPids),
     NewTableArray  = Tables -- [Res],
     ?INFO("New Tables Array: ~p",[ {NewTableArray,Res} ]),
     TablesUsers = NewTableArray ++ [setelement(Pos,Res,User#'PlayerInfo'.id)],
     ?INFO("Tables Users: ~p",[ TablesUsers ]),
     ?INFO("Free Slot Found: ~p : ~p",[TabId,TabPid]),
     Pid ! { self(), {Ref, {ok, TabPid}} },

    {noreply, State#state{tables_users = TablesUsers}};

handle_info(Info, State) ->
    {stop, {unknown_info, Info}, State}.

terminate(_Reason, _State) ->
    ?INFO("Terminating first level relay ~w. Unknown Reason: ~p", [self(), _Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%-------------------------------------------------------------------
%% Local functions
%%-------------------------------------------------------------------

next_color(1) -> 2;
next_color(2) -> 1.


roll(N) ->
    [crypto:rand_uniform(1, 7) || _X <- lists:seq(1, N)].


init_next_move(#state{dices_mode = DicesMode,
                      cur_move_color = CurMoveColor,
                      tables_pids = Tables,
                      round_tables_counter = RoundTablesCounter} = State) ->
    if DicesMode == tournament_master -> %% Pass tavla_rolls event to clients
           MoveColor = next_color(CurMoveColor),
           Dices = roll(2),
           ?INFO("PAIRED_TAVLA TournMaster has roll dices: ~p. Move color:~p", [Dices, MoveColor]),
           [begin
                RollsMsg = #game_event{event = tavla_rolls, args = [{table_id, TabId}, {dices, Dices}, {color, MoveColor}]},
                publish0(RollsMsg, State)
            end || {TabId, _TabPid} <- Tables],
           {noreply, State#state{tstate = ?STATE_MOVES,
                                 moves_counter = RoundTablesCounter,
                                 cur_move_color = MoveColor
                                }};
       true ->
           ?INFO("PAIRED_TAVLA Waiting for main table roll.", []),
           {noreply, State#state{tstate = ?STATE_DICES}}
    end.


init_state_first_move_determination(#state{players = Players,
                                           tables_num = TablesNum} = State) ->
    PlayersList = [P#player.id || P <- Players],
    ?INFO("PAIRED_TAVLA Waiting for main table first roll.", []),
    {noreply, State#state{tstate = ?STATE_FIRST_MOVE_DETERMINATION,
                          round_tables_counter = TablesNum,
                          round_results = [],
                          dices_mode = main_table,
                          confirm_wl = PlayersList
                         }}.


%% @spec spread_users(Owner, PlayerIds) -> [TabsSpread, UsersSpread]
%% @doc
%% Types:
%%     Owner = user_id()
%%     PlayersId = list(user_id() | robot)
%%     TabsSpread = list({table_id(), user_id(), user_id()})
%%     UsersSpread = list({user_id(), table_id()})
%% @end

spread_users(Owner, PlayerIds) ->
    Players = [Owner | PlayerIds -- [Owner]], %% Move the Owner to the first pos
    spread_users(Players, 1, [], []).


spread_users([A,B|Rest], TabId, TabsAcc, UsersAcc) ->
    NewTabsAcc = [{TabId, A, B} | TabsAcc],
    NewUsersAcc1 = if A =/=robot -> [{A, TabId} | UsersAcc]; true -> UsersAcc end,
    NewUsersAcc2 = if B =/=robot -> [{B, TabId} | NewUsersAcc1]; true -> NewUsersAcc1 end,
    spread_users(Rest, TabId+1, NewTabsAcc, NewUsersAcc2);
spread_users([], _, TabsAcc, UsersAcc) -> {TabsAcc, UsersAcc}.

%% @spec spawn_tables(Specs, Tables, Topic, GameFSM, Params, Manager, TablesNum) -> List
%% @doc
%% Types:
%%     Specs = list({table_id(), user_id(), user_id()})
%%     TablesNum = pos_integer()
%%     List = list({table_id(), pid()})
%% @end

spawn_tables([{TabId, A, B} | Rest], Tables, Topic, GameFSM, Params, Manager, TablesNum) ->
    {ok, TabPid} =
        relay:start_link(Topic,
                    {lobby, GameFSM},
                    [{table_id, TabId},
                     {parent_relay, {?MODULE, self()}},
                     {tables_num, TablesNum}
                      | Params],
                    [A,B],
                    Manager), % create simple tavla boards
    spawn_tables(Rest,  [{TabId, TabPid} | Tables], Topic, GameFSM, Params, Manager, TablesNum);

spawn_tables([], Tables, _Topic, _GameFSM, _Params, _Manager, _TablesNum) -> Tables.

notify_tables0(Msg, Sender, #state{tables_pids = Tables}) -> [relay:notify_table(Pid,Msg)||{_,Pid}<-Tables, Pid =/= Sender].

publish0(Msg, #state{tables_pids = Tables}) ->
    ?INFO("~w:publish0 Msg: ~p", [?MODULE, Msg]),
    [relay:republish(TabPid, Msg) || {_TabId, TabPid} <- Tables],
    ok.


replace_table_id(Args, TabId) ->
    case lists:member({table_id, 1}, Args) of
        true -> [{table_id, TabId} | Args -- [{table_id, 1}]];
        false -> Args
    end.

bot_module(game_tavla) -> game_tavla_bot.

%% init_replacement_robot(UId, State) ->
%%     {NPid, SPid, NUId, User} = create_robot(State),
%%     BM = bot_module(State#state.rules_module),
%%     RPid = State#state.rules_pid,
%%     RMod = State#state.rules_module,
%%     {Msgs, RobotInfo} = RMod:signal(RPid, {replace_player, UId, NUId, User, SPid}),
%%     BM:init_state(NPid, {Msgs, RobotInfo}),
%%     {SPid, NUId, User}.

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

