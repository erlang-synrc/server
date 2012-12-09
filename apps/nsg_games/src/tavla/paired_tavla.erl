-module(paired_tavla).
-author('Serge Polkovnikov <serge.polkovnikov@gmail.com>').
-behaviour(gen_server).

-export([reg/2,
         signal/2,
         publish/2,
         notify_tables/2,
         submit/4,
         get_requirements/2,
         start/1,
         start/5,
         start_link/5,
         stop/1,
         get_topic/1,
         get_player_state/2,
         can_observe/2,
         unreg/2,
         im_ready/1
        ]).

-include_lib("nsg_srv/include/social_actions.hrl").
-include_lib("nsg_srv/include/requests.hrl").
-include_lib("nsm_db/include/config.hrl").
-include_lib("nsg_srv/include/basic_types.hrl").
-include_lib("nsg_srv/include/game_tavla.hrl").
-include_lib("nsm_db/include/table.hrl").
-include_lib("nsm_db/include/accounts.hrl").
-include_lib("nsx_config/include/log.hrl").
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

-define(TAB_MOD, relay).

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
          confirm_wl :: list('PlayerId'()),
          rounds_counter :: integer()        %% How many rounds left to play
         }).


reg(Srv, User) -> gen_server:call(Srv, {reg, User}, 10000).
signal(Srv, Msg) -> gen_server:cast(Srv, {signal, Msg}).
unreg(Srv, Key) -> gen_server:call(Srv, {unreg, Key}).
get_topic(Srv) -> gen_server:call(Srv, get_topic).
get_player_state(Srv, UId) -> gen_server:call(Srv, {get_player_state, UId}).
can_observe(Srv, Id) -> gen_server:call(Srv, {can_observe, Id}).
stop(Srv) -> gen_server:cast(Srv, stop).
start(GameId, GameFSM, Params, Pids, Manager) -> gen_server:start(?MODULE, [GameId, GameFSM, Params, Pids, Manager], []).
start_link(GameId, GameFSM, Params, Pids, Manager) -> gen_server:start_link(?MODULE, [GameId, GameFSM, Params, Pids, Manager], []).
get_requirements(_GameFSM, _Mode) -> [{max_users,10},{min_users,4}].
%% 2nd level relay API
publish(Srv, Msg) -> Self = self(), gen_server:cast(Srv, {publish, Self, Msg}).
notify_tables(Srv, Msg) -> Self = self(), gen_server:cast(Srv, {notify_tables, Self, Msg}).
im_ready(Srv) -> Self = self(), gen_server:cast(Srv, {im_ready, Self}).
submit(Srv, Msg, From, Player) -> Self = self(), gen_server:cast(Srv, {submit, Self, Msg, From, Player}).

start(P) -> gen_server:start_link(?MODULE, P, []).

init([Topic, {lobby, GameFSM}, Params0, PlayerIds, Manager]) ->

    ?INFO(" +++ init paired tavla lobby ~p",[{GameFSM,Params0,PlayerIds,Manager}]),
    Settings = Params0,

    TableName = proplists:get_value(table_name, Settings, "no table"),
    Rounds = proplists:get_value(rounds, Settings, 1),
    GameMode = proplists:get_value(game_mode, Settings, paired),
    MainUsers = proplists:get_value(main_users, Settings, []),
    GameSpeed = proplists:get_value(speed, Settings, normal),
    Owner = proplists:get_value(owner, Settings, "maxim"), %% FIXME

    ?INFO("~w:init/1 Owner: ~p", [?MODULE, Owner]),
    {Params,P,PE} = case pointing_rules:get_rules(GameFSM, GameMode, Rounds) of
                        {ok, PR, PREx} -> {Params0 ++ [{pointing_rules, PR},{pointing_rules_ex, PREx}],PR,PREx};
                        _ -> {Params0,#pointing_rule{rounds=1, game = tavla,
                                                     kakush_winner = 1, kakush_other = 1, quota = 1},
                              [#pointing_rule{rounds=1, game = tavla,
                                              kakush_winner = 1, kakush_other = 1, quota = 1}]}
                    end,
    FeelLucky = proplists:get_value(feel_lucky, Settings, false),

    GProcVal = #game_table{game_type = GameFSM,
                           game_process = self(),
                           game_module = ?MODULE,
                           id = Topic,
                           age_limit = [crypto:rand_uniform(20,30), crypto:rand_uniform(31,40)],
                           game_mode = GameMode,
                           game_speed = GameSpeed,
                           feel_lucky = FeelLucky,
                           owner = Owner,
                           creator = Owner,
                           rounds = Rounds,
                           pointing_rules   = P,
                           pointing_rules_ex = PE,
                           users = [ case User of robot -> robot; _ -> erlang:binary_to_list(User) end || User <- PlayerIds],
                           name = TableName,
                           tournament_type = paired
               },

    ?INFO(" +++ paired_tavla.erl GProc Registration: ~p",[GProcVal]),

    gproc:reg({p,l,self()},GProcVal),

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
                   ready_counter = TablesNum,
                   rounds_counter = Rounds
                  },
    {ok, State}.


handle_call(get_topic, _From, State) ->
    {reply, State#state.topic, State};

handle_call({update_reg, Key, Value}, _From, State) ->
    gproc:set_value({p,l,Key},Value),
    {reply, ok, State};


handle_call({can_observe, Id}, _From, State) ->
    Res = can_observe0(Id, State),
    {reply, Res, State};

handle_call({reg, User}, _From, #state{topic = Topic,
                                       tables_pids = TablesPids,
                                       tables_users = Tables
                                      }=State) ->
    UserId = User#'PlayerInfo'.id,
    ?INFO("PAIRED_TAVLA <~p> Registration claim from user ~p.", [Topic, UserId]),

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
     TablesUsers = NewTableArray ++ [setelement(Pos,Res,User#'PlayerInfo'.id)],
    ?INFO("PAIRED_TAVLA <~p> User ~p registered.", [Topic, UserId]),
    {reply, {ok, {UserId, {?TAB_MOD, TabPid}, {?TAB_MOD, TabPid}}}, State#state{tables_users = TablesUsers}};


handle_call(Event, From, State) ->
    {stop, {unknown_call, Event, From}, State}.



handle_cast(start_rematch_timer, State) ->
    {ok, Ref} = timer:send_after(?REMATCH_TIMEOUT, rematch_timer_ringing),
    {noreply, State#state{rematch_timer = {running, Ref}}};

handle_cast({submit, _Sender, #game_action{action = Action, args = Args}, From, Player},
             #state{confirm_wl = WL,
                    tables_pids = Tables,
                    tables_users = TablesUsers,
                    rounds_counter = RoundsCounter} = State) when
  Action == tavla_ready;
  Action == <<"tavla_ready">> ->
    ?INFO("PAIRED_TAVLA Next round confirmation received. Table: ~p Player: ~p. Waiting for:~p",
           [proplists:get_value(table_id, Args), Player, WL -- [Player]]),
    gen_server:reply(From, ok), %% Eat the action
    if WL == [Player] -> %% All players confirm next round
           ?INFO("PAIRED_TAVLA All next round confirmation received.", []),
           [begin
                {_, TabPid} = lists:keyfind(TabId, 1, Tables),
                ReadyAction = #game_action{action = tavla_ready, args = [{table_id, TabId}]},
                relay:cast_resubmit(TabPid, P1Id, ReadyAction),
                relay:cast_resubmit(TabPid, P2Id, ReadyAction)
            end || {TabId,  P1Id, P2Id} <- TablesUsers],
           if RoundsCounter == 1 ->
                  ?INFO("PAIRED_TAVLA All rounds completed. Finishing.", []),
                  publish0(#game_event{event = tavla_series_ended}, State),
                  {noreply, State#state{tstate = ?STATE_FINISHED}};
              true ->
                  NewRoundsCounter = RoundsCounter - 1,
                  ?INFO("PAIRED_TAVLA Initiating next round. Rounds left: ~p.", [NewRoundsCounter]),
                  init_state_first_move_determination(State#state{rounds_counter = NewRoundsCounter})
           end;
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
    Dices = proplists:get_value(dices, Args),
    case length(Dices) == 2 of
        true ->
            FirstMoveColor = proplists:get_value(color, Args),
            ?INFO("PAIRED_TAVLA First dices rolled: ~p. First move color: ~p", [Dices, FirstMoveColor]),
            %% Pass dices of the main table to other tables like as original
            [begin
                 NewArgs = replace_table_id(Args, TabId),
                 publish0(Msg#game_event{args = NewArgs}, State)
             end || {TabId, _TabPid} <- Tables],
            {noreply, State#state{tstate = ?STATE_MOVES,
                                  cur_move_color = FirstMoveColor,
                                  moves_counter = RoundTablesCounter
                                 }};
        false ->
            ?INFO("PAIRED_TAVLA A dace rolled: ~p", [Dices]),
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


%% handle_info({'DOWN', _, process, Pid, Reason}, State = #state{rules_pid = Pid}) ->
%%     ?INFO("relay is down. Reason: ~p", [Reason]),
%%     publish0(#game_crashed{game = State#state.topic}, self(), State),
%%     {stop, {error, game_crashed}, State};
%% 

handle_info(die, State) ->
    {stop, normal, State};


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


init_state_first_move_determination(#state{tables_users = TablesUsers,
                                           tables_num = TablesNum} = State) ->
    PlayersList = lists:flatmap(fun({_, P1, P2}) -> [P1, P2] end, TablesUsers),
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
    Players = case lists:member(Owner, PlayerIds) of
                  true -> [Owner | PlayerIds -- [Owner]]; %% Move the Owner to the first pos
                  false -> PlayerIds
              end,
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

