-module(game_tavla).
-author('Maxim Sokhatsky <maxim@synrc.com>').
-behaviour(gen_fsm).

-export([start_link/2, start/3, start/4]).
-export([signal/2, make_move/3, get_requirements/0, get_settings/1]).
-export([setup_board/0, get_timeout/2,roll/1, get_player_stats/1]).
-export([state_wait/3,
		 state_start_rolls/3,
		 state_move/3,
		 state_rolls/3,
         state_finished/3,
		 state_vido/3,
		 state_surrender/3,
		 state_ready_next_round/3,
		 state_paused/2,
		 state_paused/3]).
-export([init/1, state_name/2, state_name/3, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-include_lib("nsg_srv/include/requests.hrl").
-include_lib("nsm_db/include/accounts.hrl").
-include_lib("nsg_srv/include/setup.hrl").
-include_lib("nsg_srv/include/game_tavla.hrl").
-include_lib("nsg_srv/include/types.hrl").
-include_lib("alog/include/alog.hrl").
-include_lib("eunit/include/eunit.hrl").

-record(state, {
		  game_id               :: any(),
          players               :: list(#'TavlaPlayer'{}),
          c_rolls = []          :: list(integer),                             %% unused dices values
          b_rolls = []          :: list(tuple(pid(), integer)),               %% used when determing player making first move
          board = setup_board() :: list(tuple('Color'(), integer) | null),
          bar = []              :: list('Color'()),
          wait_list             :: list(pid()),
          stats			:: pid(),
          relay                 :: pid(),
          vido                  :: integer(),
          game_mode = undefined :: atom(),
          table_id = 1          :: integer(),
          tables_num = 1        :: integer(),
          relay_monitor		:: pid(),
          rounds                :: integer(),
          current_round         :: integer(),
          scoring_mode		:: atom(),
          ready                 :: integer(),
          table_name		:: binary(),
          speed                 :: integer(),
          turn_timeout                     :: integer(),
          challenge_timeout                :: integer(),
          ready_timeout                    :: integer(),
          robot_delay                      :: integer(),

          mul_factor            :: pos_integer(),
          slang_flag            :: boolean(),
          observer_flag         :: boolean(),

          set_state		:: #'TavlaSetState'{},
          settings              :: proplist(),
          results               :: #'TavlaGameResults'{},
          game_info = []            :: [{atom(), any()}],
          series_results = []     :: list(#'TavlaGameResults'{}),
          paused_statename                 :: atom()
         }).

start_link(Relay, Pids) ->
    gen_fsm:start_link(?MODULE, [Relay, Pids], []).

start(Relay, Pids, GameId) ->
    start(Relay, Pids, GameId, []).
start(Relay, Pids, GameId, Params) ->
    gen_fsm:start(?MODULE, [Relay, Pids, GameId, Params], []).

signal(Server, Msg) ->
    gen_fsm:sync_send_all_state_event(Server, {signal, Msg}).

make_move(Server, Who, #game_action{action = Action, args = Args}) ->
    Msg = try api_utils:to_known_record(Action, Args)
          catch Type:Reason ->
                  {Type, Reason}
          end,
    case Msg of
        {error, _} ->
            Msg;
        _ ->
            make_move(Server, Who, Msg)
    end;
make_move(Server, Who, Msg) ->
    gen_fsm:sync_send_event(Server, {Msg, Who}).

get_requirements() ->
    [{players, 2}].

get_settings(Server) when is_pid(Server) ->
    gen_fsm:sync_send_all_state_event(Server, get_settings);
get_settings(Settings00) ->
    get_settings0(Settings00).

-spec get_settings0(proplist()) -> proplist().
get_settings0(Settings00) ->
    Settings0 = proplists:substitute_aliases([{tours, sets}, {tour, sets}, {turns, rounds}], Settings00),
    DefaultSettings = [
                       {game_mode, standard},
                       {allow_replacement, true},
                       {deny_robots, false},
                       {speed, normal},
%%                       {observers, false},
                       {sets, 1},
                       {rounds, 1},
                       {table_name, "default name"}
                      ],
    utils:apply_defauls(DefaultSettings, Settings0).

-spec get_timeout(atom(), iolist()) -> integer().
get_timeout(turn, fast) ->
    {ok, Val} = rpc:call(?APPSERVER_NODE,nsm_db,get,[config,"games/tavla/turn_timeout_fast", 15000]), Val;
get_timeout(turn, normal) ->
    {ok, Val} = rpc:call(?APPSERVER_NODE,nsm_db,get,[config,"games/tavla/turn_timeout_normal", 30000]), Val;
get_timeout(turn, slow) ->
    {ok, Val} = rpc:call(?APPSERVER_NODE,nsm_db,get,[config,"games/tavla/turn_timeout_slow", 60000]), Val;

get_timeout(challenge, fast) ->
    {ok, Val} = rpc:call(?APPSERVER_NODE,nsm_db,get,[config,"games/tavla/challenge_timeout_fast", 15000]), Val;
get_timeout(challenge, normal) ->
    {ok, Val} = rpc:call(?APPSERVER_NODE,nsm_db,get,[config,"games/tavla/challenge_timeout_normal", 30000]), Val;
get_timeout(challenge, slow) ->
    {ok, Val} = rpc:call(?APPSERVER_NODE,nsm_db,get,[config,"games/tavla/challenge_timeout_slow", 60000]), Val;

get_timeout(ready, fast) ->
    {ok, Val} = rpc:call(?APPSERVER_NODE,nsm_db,get,[config,"games/tavla/ready_timeout_fast", 15000]), Val;
get_timeout(ready, normal) ->
    {ok, Val} = rpc:call(?APPSERVER_NODE,nsm_db,get,[config,"games/tavla/ready_timeout_normal", 25000]), Val;
get_timeout(ready, slow) ->
    {ok, Val} = rpc:call(?APPSERVER_NODE,nsm_db,get,[config,"games/tavla/ready_timeout_slow", 45000]), Val;

get_timeout(robot, Speed) ->
    get_timeout(robot_production, Speed);
get_timeout(robot_production, fast) ->
    {ok, Val} = rpc:call(?APPSERVER_NODE,nsm_db,get,[config,"games/tavla/robot_delay_fast", 6000]), Val;
get_timeout(robot_production, normal) ->
    {ok, Val} = rpc:call(?APPSERVER_NODE,nsm_db,get,[config,"games/tavla/robot_delay_normal", 9000]), Val;
get_timeout(robot_production, slow) ->
    {ok, Val} = rpc:call(?APPSERVER_NODE,nsm_db,get,[config,"games/tavla/robot_delay_slow", 15000]), Val.

get_player_stats(_PlayerId) ->
    #'PlayerTavlaStats'{}.

% INIT

init([Relay, Pids, GameId, Settings]) ->

%    ?INFO("Settings: ~p Pids: ~p",[Settings,Pids]),
%    ?INFO("Get Settings: ~p",[get_settings(Settings)]),

    ?INFO("Starting Tavla Game ~p with Players: ~p",[GameId,Pids]),
    Players = lists:map(fun(Pid) -> 
                             PI = game_session:get_player_info(Pid),
                             ?INFO("Session PI: ~p",[PI]),
                             #'TavlaPlayer'{pid = Pid, player_id = PI#'PlayerInfo'.id, player_info = PI,collected=0 }
                        end, Pids),
    TName = proplists:get_value(table_name, get_settings(Settings)),
    Rounds  = proplists:get_value(rounds, get_settings(Settings)),
    ColoredPlayers = lists:zipwith(fun(TavlaPlayer,Color) ->
                                 TavlaPlayer#'TavlaPlayer'{color=Color}
                               end,Players, [1,2]),
    State = #state{players = ColoredPlayers, wait_list = Pids, relay = Relay},
    Speed = proplists:get_value(speed, Settings),
    TableId = proplists:get_value(table_id, Settings),
    TablesNum = proplists:get_value(tables_num, Settings),
    MulFactor = proplists:get_value(double_points, Settings, 1),
    SlangFlag = proplists:get_value(slang, Settings, false),
    ObserverFlag = proplists:get_value(deny_observers, Settings, false),
    RobotDelay = get_timeout(robot, Speed),
    Scores = [ #'TavlaPlayerScore'{ player_id = P#'TavlaPlayer'.player_id, score = 0} || P <- Players],
    Results = #'TavlaGameResults'{ players = Scores},
    DP = proplists:get_value(double_points, get_settings(Settings), 1),
    GameMode = proplists:get_value(game_mode, get_settings(Settings), 1),
    Mode = proplists:get_value(game_mode, get_settings(Settings)),
    PR = proplists:get_value(pointing_rules, get_settings(Settings)),
    PlayersWithInfo = [game_session:get_player_info(Pid) || Pid <- Pids],
%    [PRLucky] = proplists:get_value(pointing_rules_ex, get_settings(Settings)),
    UserOpts = [],%proplists:get_value(users_options, get_settings(Settings), []),

    TestPR = #pointing_rule{game = tavla, game_type = Mode, kakush_winner = 15,
                            kakush_other = 2, game_points = 15, quota = 20},
    TestLuckyPR = #pointing_rule{game = tavla, game_type = feellucky, kakush_winner = 0,
                                 kakush_other = 0, game_points = 0, quota = 1},

    GameInfo = [{id, GameId},
                {double_points, DP},
                {mode, Mode},
                {pointing_rules, PR},
                {initial_players, PlayersWithInfo},
                {pointing_rules_lucky, TestLuckyPR},
                {users_options, UserOpts}],

    ?INFO("GameInfo: ~p",[GameInfo]),

    {ok, state_created, State#state{game_id=GameId,
                                    table_id = TableId,
                                    tables_num = TablesNum,
                                    table_name = iolist_to_binary(TName), 
                                    results = Results, 
                                    turn_timeout = get_timeout(turn, Speed),
                                    challenge_timeout = get_timeout(challenge, Speed),
                                    ready_timeout = get_timeout(ready, Speed),
                                    game_mode = GameMode,
                                    robot_delay = RobotDelay,
                                    rounds = Rounds, 
                                    game_info = GameInfo,
                                    current_round = 1,
                                    ready=0,
                                    mul_factor = MulFactor,
                                    slang_flag = SlangFlag,
                                    observer_flag = ObserverFlag
                                    }}.

state_wait({#tavla_ready{}, Pid}, _, #state{wait_list = List} = State) ->
    Relay = State#state.relay,
    TableId = State#state.table_id,
    NList =
        case lists:member(Pid, List) of
            true ->
                PI = lists:keyfind(Pid, #'TavlaPlayer'.pid, State#state.players),
                publish_event(Relay, #tavla_player_ready{table_id = TableId, player = PI#'TavlaPlayer'.player_id}),
                lists:delete(Pid, List);
            false ->
                ?INFO("not a member: ~p", [self()]),
                {ok, List}
        end,
    case NList of
        [] ->
            Relay = State#state.relay,
            publish_event(Relay, #tavla_game_started{table_id = TableId}),
            Pids = [P#'TavlaPlayer'.pid || P <- State#state.players],
            {reply, ok, state_start_rolls, State#state{wait_list = Pids}};
        _ ->
            {reply, ok, state_wait, State#state{wait_list = NList}}
    end.

vido_answer(From,To,A,_Pid,State) ->
    Relay = State#state.relay,
    Vido = State#state.vido,
    TableId = State#state.table_id,
    case A of
       true  -> publish_event(Relay, #tavla_ack{table_id = TableId, type=vido,from=From,to=To,answer=A}),
                {reply, ok, state_rolls, State#state{vido = 2 * Vido}};
       false -> Results = #'TavlaGameResults'{table_id = TableId,
                   players = [#'TavlaPlayerScore'{player_id = To, score = 2 * Vido, winner = <<"true">>}, 
                   #'TavlaPlayerScore'{player_id = From, score = 0, winner = <<"none">>}]},

                %game_stats:assign_points(Results, State#state.game_info),

                Message = #tavla_game_ended{table_id = TableId,
                              winner = To,
                              results = Results},
                publish_event(Relay, Message),
                next_round(State#state{series_results = State#state.series_results ++ [Results]}, Results)
              % {reply, ok, state_finished, State}
    end.

tavla_roll(Pid,State) ->
    TableId = State#state.table_id,
    Diceroll = roll(2),
    Relay = State#state.relay,
    PI = lists:keyfind(Pid, #'TavlaPlayer'.pid, State#state.players),
    publish_event(Relay, #tavla_rolls{table_id = TableId, player = PI#'TavlaPlayer'.player_id,dices=Diceroll}),
    {reply, ok, state_move, State}.


tavla_move(Moves, Player, _Pid, State) ->
    TableId = State#state.table_id,
    Relay = State#state.relay,
    ?INFO("tavla_move{} received from client ~p",[{Moves,Player}]),
    TP1 = lists:nth(1,State#state.players),
    TP2 = lists:nth(2,State#state.players),
    P1 = TP1#'TavlaPlayer'.player_id,
    P2 = TP2#'TavlaPlayer'.player_id,
    Incs = lists:map(fun(A) ->
        publish_event(Relay, #tavla_moves{table_id = TableId, player = Player,
                                         from = A#'TavlaAtomicMove'.from, 
                                         to = A#'TavlaAtomicMove'.to}),
        {Player, case A#'TavlaAtomicMove'.to of
                   27 -> 1;
                   0 -> 1;
                   _ -> 0
                 end}
    end, Moves),
    {P1Incs,P2Incs} = lists:partition(fun({P,_}) -> P1 =:= P end, Incs),
    P1Collected = lists:foldl(fun({_,C}, Sum) -> Sum + C end,TP1#'TavlaPlayer'.collected,P1Incs),
    P2Collected = lists:foldl(fun({_,D}, Sum2) -> Sum2 + D end,TP2#'TavlaPlayer'.collected,P2Incs),
    NewState = State#state{players=[TP1#'TavlaPlayer'{collected=P1Collected},
                                    TP2#'TavlaPlayer'{collected=P2Collected}]},
    case check_game_ended(P1Collected, P2Collected, P1, P2, Relay, Player,State) of
        no -> {reply, ok, state_rolls, NewState};
        {yes, Results} -> 
               %game_stats:assign_points(Results, State#state.game_info),
               next_round(NewState#state{series_results = NewState#state.series_results ++ [Results]}, Results) 
               % {reply, ok, state_finished, NewState}
    end.

% MOVE

check_game_ended(P1Collected, P2Collected, P1, P2, Relay, Player,State) ->
    TableId = State#state.table_id,
    Vido = State#state.vido,
    {Message,Results} = case P1Collected of
        15 -> case P2Collected of
                 0 -> R = #'TavlaGameResults'{
                            players = [#'TavlaPlayerScore'{player_id = P1, score = 2 * Vido, winner = <<"true">>}, 
                                       #'TavlaPlayerScore'{player_id = P2, score = 0}]},
                      {#tavla_game_ended{table_id = TableId, winner = P1, results = R},R};
                 _ -> R = #'TavlaGameResults'{
                            players = [#'TavlaPlayerScore'{player_id = P1, score = 1 * Vido, winner = <<"true">>}, 
                                       #'TavlaPlayerScore'{player_id = P2, score = 0}]},

                      {#tavla_game_ended{table_id = TableId, winner = P1, results = R}, R}

              end;
        _ -> case P2Collected of
            15 -> case P1Collected of
                  0 -> R = #'TavlaGameResults'{
                            players = [#'TavlaPlayerScore'{player_id = P2, score = 0}, 
                                       #'TavlaPlayerScore'{player_id = P1, score = 2 * Vido, winner = <<"true">>}]},
                       {#tavla_game_ended{table_id = TableId, winner = P2, results = R},R};
                  _ -> R = #'TavlaGameResults'{
                            players = [#'TavlaPlayerScore'{player_id = P2,score = 0}, 
                                       #'TavlaPlayerScore'{player_id = P1,score = 1 * Vido, winner = <<"true">>}]},

                       {#tavla_game_ended{table_id = TableId, winner = P2, results = R},R}
                  end;
            _ ->  {#tavla_next_turn{table_id = TableId, player = case Player of P1 -> P2; P2 -> P1 end },0}
        end
    end,
    publish_event(Relay, Message),
    case Message of
        #tavla_next_turn{} -> no;
        _ -> {yes,Results}
    end.

state_move({#tavla_vido_answer{from=From,to=To,answer=A}, Pid}, _, State) ->
    vido_answer(From,To,A,Pid,State),
    {reply, ok, state_move, State};

state_move({#tavla_surrender{}, Pid}, _, State) ->
    TableId = State#state.table_id,
    Relay = State#state.relay,
    PI = lists:keyfind(Pid, #'TavlaPlayer'.pid, State#state.players),
    publish_event(Relay, #tavla_surrender_request{table_id = TableId, from = PI#'TavlaPlayer'.player_id}),
    {reply, ok, state_surrender, State};

state_move({#tavla_roll{}, Pid}, _,  #state{wait_list = _List} = State ) ->
    tavla_roll(Pid,State);

state_move({#tavla_move{moves = Moves, player = Player}, Pid}, _, #state{wait_list = _List} = State) ->
    tavla_move(Moves,Player,Pid,State).


% VIDO

state_vido({#tavla_vido_answer{from=From,to=To,answer=A}, Pid}, _, State) ->
    vido_answer(From,To,A,Pid,State);

state_vido({#tavla_vido_request{}, _Pid}, _, State) ->
    {reply, ok, state_vido, State}.

tavla_surrender_answer(From,To,A, _Pid, State) ->
    Relay = State#state.relay,
    TableId = State#state.table_id,
    Vido = State#state.vido,
    case A of
       false -> publish_event(Relay, #tavla_ack{table_id = TableId, type=surrender,from=From,to=To,answer=A}),
               {reply, ok, state_rolls, State};
       true ->  Results = #'TavlaGameResults'{
                   players = [#'TavlaPlayerScore'{player_id = From, score = 2 * Vido, winner = <<"true">>}, 
                   #'TavlaPlayerScore'{player_id = To, score = 0, winner = <<"none">>}]},

                ?INFO("Assign Points: ~p",[{Results,State#state.game_info}]),

                Message = #tavla_game_ended{table_id = TableId,
                              winner = From,
                              results = Results},
                 publish_event(Relay, Message),
                next_round(State#state{series_results = State#state.series_results ++ [Results]}, Results)
               %next_round(State, Results)
    end.

% SURRENDER

state_surrender({#tavla_move{moves = Moves, player = Player}, Pid}, _, #state{wait_list = _List} = State) ->
    tavla_move(Moves,Player,Pid,State); 

state_surrender({#tavla_surrender_answer{from=From,to=To,answer=A}, _Pid}, _, State) ->
    tavla_surrender_answer(From,To,A,_Pid,State);

state_surrender({#tavla_surrender_request{}, _Pid}, _, State) ->
    {reply, ok, state_vido, State}.

% ROLLS/VIDO/SURRENDER

state_rolls({#tavla_surrender_answer{from=From,to=To,answer=A}, _Pid}, _, State) ->
    tavla_surrender_answer(From,To,A,_Pid,State);

state_rolls({#tavla_skip{}, Pid}, _, State) ->
    TableId = State#state.table_id,
    Relay = State#state.relay,
    TP1 = lists:nth(1,State#state.players),
    TP2 = lists:nth(2,State#state.players),
    P1 = TP1#'TavlaPlayer'.player_id,
    P2 = TP2#'TavlaPlayer'.player_id,
    PI = lists:keyfind(Pid, #'TavlaPlayer'.pid, State#state.players),
    Player = PI#'TavlaPlayer'.player_id,
    publish_event(Relay, #tavla_next_turn{table_id = TableId, player = case Player of P1 -> P2; P2 -> P1 end}),
    {reply, ok, state_rolls, State};

state_rolls({#tavla_move{moves = Moves, player = Player}, Pid}, _, #state{wait_list = _List} = State) ->
    tavla_move(Moves,Player,Pid,State);

state_rolls({#tavla_vido{}, Pid}, _, State) ->
    TableId = State#state.table_id,
    Relay = State#state.relay,
    PI = lists:keyfind(Pid, #'TavlaPlayer'.pid, State#state.players),
    publish_event(Relay, #tavla_vido_request{table_id = TableId, from = PI#'TavlaPlayer'.player_id}),
    {reply, ok, state_vido, State};

state_rolls({#tavla_surrender{}, Pid}, _, State) ->
    TableId = State#state.table_id,
    Relay = State#state.relay,
    PI = lists:keyfind(Pid, #'TavlaPlayer'.pid, State#state.players),
    publish_event(Relay, #tavla_surrender_request{table_id = TableId, from = PI#'TavlaPlayer'.player_id}),
    {reply, ok, state_surrender, State};

state_rolls({#tavla_vido_answer{from=From,to=To,answer=A}, Pid}, _, State) ->
    vido_answer(From,To,A,Pid,State);

state_rolls({#tavla_roll{}, Pid}, _,  #state{wait_list = _List} = State ) ->
    tavla_roll(Pid,State).

state_ready_next_round({#tavla_ready{}, _Pid}, _, State) ->
    case State#state.ready of
       1 -> start_game(State#state{ready = 0}), 
            Pids = [P#'TavlaPlayer'.pid || P <- State#state.players],
            {reply, ok, state_start_rolls, State#state{ready = 0, b_rolls = [], c_rolls = [], wait_list = Pids}};
       0 -> {reply, ok, state_ready_next_round, State#state{ready = State#state.ready + 1}};
       _ -> error
    end.

state_finished({#tavla_game_ended{}, _Pid}, _, State ) ->
    {reply, ok, state_finished, State};

state_finished({#tavla_ready{}, _Pid}, _, State) ->
    {reply,ok,state_finished, State}.

state_start_rolls({#tavla_roll{}, Pid}, _,  #state{wait_list = List} = State ) ->
    BRolls = State#state.b_rolls,
    TableId = State#state.table_id,
    Relay = State#state.relay,
    ?INFO("TAVLA ROLL: ~p",[{BRolls,lists:member(Pid, List), lists:keymember(Pid, 1, BRolls), List}]),
    case {lists:member(Pid, List), lists:keymember(Pid, 1, BRolls), List} of
        {true, true, _} ->
            {reply, already_rolled, state_start_rolls, State};
        {true, false, [Pid]} ->
            [Diceroll] = roll(1),
            PI = lists:keyfind(Pid, #'TavlaPlayer'.pid, State#state.players),
            publish_event(Relay, #tavla_rolls{table_id = TableId, player = PI#'TavlaPlayer'.player_id,dices=[Diceroll]}),
            BRolls2 = lists:keysort(2, [{Pid, Diceroll} | BRolls]),
            case BRolls2 of
                [{_Loser, X}, {Winner, X}] ->
                    ?INFO("rolls equals: ~p",[BRolls2]),
                    _Pids = [P#'TavlaPlayer'.pid || P <- State#state.players],
                    PI2 = lists:keyfind(Winner, #'TavlaPlayer'.pid, State#state.players),
                    publish_event(Relay, #tavla_next_turn{table_id = TableId, player = PI2#'TavlaPlayer'.player_id}),
                    publish_event(Relay, #tavla_rolls{table_id = TableId, player = PI2#'TavlaPlayer'.player_id, dices = [X, X]}),
                    State2 = State#state{wait_list = [], c_rolls = BRolls2},
                    {reply, ok, state_move, State2#state{vido = 1}};
                [{Loser, X}, {Winner, Y}] ->
                    ?INFO("rolls normal: ~p",[{{Winner,X},{Loser,Y}}]),
                    PI2 = lists:keyfind(Winner, #'TavlaPlayer'.pid, State#state.players),
                    publish_event(Relay, #tavla_next_turn{table_id = TableId, player = PI2#'TavlaPlayer'.player_id}),
                    publish_event(Relay, #tavla_rolls{table_id = TableId, player = PI2#'TavlaPlayer'.player_id, dices = [X, Y]}),
                    State2 = State#state{wait_list = [], c_rolls = BRolls2},
                    {reply, ok, state_move, State2#state{vido = 1}}
            end;
        {true, false, _} ->
            ?INFO("tavla: rolls init: ~p",[BRolls]),
            [Diceroll] = roll(1),
            PI3 = lists:keyfind(Pid, #'TavlaPlayer'.pid, State#state.players),
            publish_event(Relay, #tavla_rolls{table_id = TableId, player = PI3#'TavlaPlayer'.player_id, dices=[Diceroll]}),
            List2 = lists:delete(Pid, List),
            State2 = State#state{wait_list = List2, b_rolls = [{Pid, Diceroll} | BRolls]},
            {reply, ok, state_start_rolls, State2};
        {false, _, _} ->
            {reply, not_your_turn, state_start_rolls, State}
    end.

state_name(_Event, State) ->
    {next_state, state_name, State}.

state_name(_Event, _From, State) ->
    Reply = ok,
    {reply, Reply, state_name, State}.

state_paused(_Event, State) ->
    ?INFO("STATE_PAUSED/2: Unexpected event arrived: ~p", [{_Event}]),
	{next_state, state_paused, State}.

state_paused(_Event, _From, State) ->
	?INFO("STATE_PAUSED/3: Unexpected sync event arrived: ~p",[{_Event,_From}]),
	{reply, {error, invalid_action}, state_paused, State}.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(get_settings, _From, StateName, #state{} = State) ->
    Res = State#state.settings,
    {reply, Res, StateName, State}; %settings
handle_sync_event({signal, state_created}=Event, _From, state_created=StateName, #state{} = State) ->
    ?INFO("HANDLE_SYNC_EVENT: ~p",[{Event,_From,StateName}]),
    _Relay = State#state.relay,
    game_stats:charge_quota(State#state.game_info),
    start_game(State),
    {reply, ok, state_start_rolls, State};
handle_sync_event({signal, do_rematch}, _From, _StateName, State) ->
    Pids = [P#'TavlaPlayer'.pid || P <- State#state.players],
    NewState = State#state{ready = 0, b_rolls = [], c_rolls = [], wait_list = Pids, current_round = 1},
    start_game(NewState),
    {reply, ok, state_start_rolls, NewState};

handle_sync_event({signal, {pause_game, Pid}}, _From, StateName, State) ->
    Relay = State#state.relay,
    TableId = State#state.table_id,
    case {is_player(Pid, State), StateName} of
        {_, X} when X == state_paused; X == state_dead; X == state_finished ->
            {reply, {error, pause_not_possible}, StateName, State};
        {true, _} ->
            UID = (get_player(Pid, State))#'TavlaPlayer'.player_id,
            Event = #game_paused{table_id = TableId, game = State#state.game_id, who = UID,
                                 action = <<"pause">>, retries = 0},
			?INFO("~w:handle_sync_event/4 Publishing the pause "
					  "event from user: ~p",[?MODULE, UID]),
            relay:publish(Relay, Event),
            {reply, 0, state_paused,
             State#state{paused_statename = StateName}
            };
        {false, _} ->
            {reply, {error, you_are_not_a_player}, StateName, State}
    end;

handle_sync_event({signal, {resume_game, Pid}}, _From, StateName, State) ->
    Relay = State#state.relay,
    TableId = State#state.table_id,
    case {StateName, is_player(Pid, State)} of
        {state_paused, true} ->
            ResumedState = State#state.paused_statename,
            UID = (get_player(Pid, State))#'TavlaPlayer'.player_id,
            Event = #game_paused{table_id = TableId, game = State#state.game_id, who = UID,
                                 action = <<"resume">>, retries = 0},
			
			?INFO("~w:handle_sync_event/4 Publishing the resume event"
					  " from user: ~p", [?MODULE, UID]),
            relay:publish(Relay, Event),
            { reply, 0, ResumedState,
              State#state{paused_statename = undefined}};
        {state_paused, false} ->
            {reply, {error, you_are_not_a_player}, state_paused, State};
        {_, _} ->
            {reply, {error, game_is_not_paused}, StateName, State}
    end;


handle_sync_event(_Event, _From, StateName, State) ->
    ?INFO("~w:handle_sync_event/4 Unhandled event ~p",
		  [?MODULE, {_Event,_From,StateName,State}]),
    Reply = ok,
    {reply, Reply, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

next_round(State,Results) ->
    Relay = State#state.relay,
    Players = [P#'TavlaPlayer'{collected = 0} || P <- State#state.players],
    StateResults = State#state.results,
    SortedLocalResults = lists:keysort(1, Results#'TavlaGameResults'.players),
    SortedStateResults = lists:keysort(1, StateResults#'TavlaGameResults'.players),
    PlayerScores = lists:zipwith(fun(LocalR,StateR) ->
                                 StateR#'TavlaPlayerScore'{score = LocalR#'TavlaPlayerScore'.score + 
                                                                  StateR#'TavlaPlayerScore'.score }
                               end, SortedLocalResults, SortedStateResults),

    State2 = State#state{players = Players, results = #'TavlaGameResults'{players = PlayerScores}, 
                         current_round = State#state.current_round + 1},
    Rounds = State2#state.rounds,
    ?INFO("Results: ~p",[State2#state.results]),
    case State2#state.current_round - 1 of
        Rounds -> publish_event(Relay, #tavla_series_ended{}),

    TP1 = lists:nth(1,State#state.players),
    TP2 = lists:nth(2,State#state.players),
    P1 = TP1#'TavlaPlayer'.player_id,
    P2 = TP2#'TavlaPlayer'.player_id,


    Unflattened = [ PlayerScores || #'TavlaGameResults'{players = PlayerScores} <- State#state.series_results],
    ?INFO("Unflattened: ~p",[Unflattened]),
    Flattened = lists:flatten(Unflattened),
    ?INFO("Flattened: ~p",[Flattened]),
		  
    

                  {N1,S1,W1} = lists:foldl(fun({'TavlaPlayerScore',Name,Reason,Winner,Score}, 
                                                          {N,S,Wins}) ->
                                       case Name =:= N of
                                            true -> case Winner =:= <<"true">> of
                                                         true -> {N,S + Score,Wins + 1};
                                                         false -> {N,S + Score,Wins}
                                                    end;
                                            false -> {N,S,Wins}
                                       end
                                       end,{P1,0,0},
                                      Flattened),
    ?INFO("P1: ~p",[{N1,S1,W1}]),

                  {N2,S2,W2} = lists:foldl(fun({'TavlaPlayerScore',Name,Reason,Winner,Score}, 
                                                          {N,S,Wins}) ->
                                       case Name =:= N of
                                            true -> case Winner =:= <<"true">> of
                                                         true -> {N,S + Score,Wins + 1};
                                                         false -> {N,S + Score,Wins}
                                                    end;
                                            false -> {N,S,Wins}
                                       end
                                       end,{P2,0,0},
                                      Flattened),
    ?INFO("P2: ~p",[{N2,S2,W2}]),

                  Overal = case W2 > W1 of
                              true -> #'TavlaGameResults'{
                                     players = [#'TavlaPlayerScore'{player_id = P1, score = S1, winner = <<"none">>}, 
                                      #'TavlaPlayerScore'{player_id = P2, score = S2, winner = <<"true">>}]};
                              false -> #'TavlaGameResults'{
                                     players = [#'TavlaPlayerScore'{player_id = P1, score = S1, winner = <<"true">>}, 
                                      #'TavlaPlayerScore'{player_id = P2, score = S2, winner = <<"none">>}]}
                  end,

                  game_stats:assign_points(Overal, State#state.game_info),
                  {reply, ok, state_finished, State2};
        _ -> {reply, ok, state_ready_next_round, State2}
    end.

start_game(State) ->
    TP1 = lists:nth(1,State#state.players),
    TP2 = lists:nth(2,State#state.players),
    P1 = TP1#'TavlaPlayer'.player_id,
    P2 = TP2#'TavlaPlayer'.player_id,
    Relay = State#state.relay,
    _Mode = State#state.scoring_mode,
    Players = State#state.players,

    TableId = State#state.table_id,
    TablesNum = State#state.tables_num,
    TableName = State#state.table_name,
    PInfos = lists:map(fun(#'TavlaPlayer'{player_info = PI}) ->
                               PI
                       end, Players),

    timer:sleep(500), % sync error in client

    publish_event(Relay, #tavla_game_info{game_type = game_tavla, 
                                          table_name = TableName,
                                          table_id = TableId,
                                          tables_num = TablesNum,
                                          current_round = State#state.current_round,
                                          rounds = State#state.rounds,
                                          players = PInfos, 
                                          game_mode = State#state.game_mode,
                                          turn_timeout = State#state.turn_timeout,
                                          challenge_timeout = State#state.challenge_timeout,
                                          ready_timeout = State#state.ready_timeout,
                                          timeout = State#state.turn_timeout,
                                          mul_factor = State#state.mul_factor,
                                          slang_flag = State#state.slang_flag,
                                          observer_flag = State#state.observer_flag
                                         }),

    M = #tavla_game_started{ table_id = TableId, players= [#tavla_color_info{ name = P1, color = 1},
                                       #tavla_color_info{ name = P2, color = 2}]},

    ?INFO("tavla game started: ~p",[M]),
    ?INFO("with game_info: ~p",[State#state.game_info]),
    publish_event(Relay, M),

    #'TavlaPlayer'{player_id = PlayerID} = get_current(State),
    publish_event(Relay, #tavla_next_turn{table_id = TableId, player = PlayerID}).

get_current(State) ->
    [First | _Rest] = State#state.players,
    First.

roll(N) ->
    [ crypto:rand_uniform(1, 7) || _X <- lists:seq(1, N) ].

setup_board() ->
    B = array:new(26, {default, null}),
    B1 = set_side(B, 1),
    B2 = array_reverse(B1),
    R = set_side(B2, 2),
    array:to_list(array_reverse(R)) ++ [null,null].

array_reverse(Arr) ->
    array:from_list(lists:reverse(array:to_list(Arr)), null).

set_side(A, Color) ->
    A1 = array:set(19, {Color, 5}, A),
    A2 = array:set(17, {Color, 3}, A1),
    A3 = array:set(12, {Color, 5}, A2),
         array:set(1,  {Color, 2}, A3).

publish_event(Relay, MsgList) when is_list(MsgList) ->
    lists:map(fun(Msg) ->
                      publish_event(Relay, Msg)
              end, MsgList);
publish_event(Relay, Msg) when is_tuple(Msg) ->
    Event = api_utils:name(Msg),
    Args = api_utils:members(Msg),
    relay:publish(Relay, #game_event{event = Event, args = Args}).

is_player(Pid, State) ->
    false /= get_player(Pid, State).

get_player(Pid, State) when is_pid(Pid) ->
    Players = State#state.players,
    lists:keyfind(Pid, #'TavlaPlayer'.pid, Players);
get_player(UId, State) ->
    Players = State#state.players,
    lists:keyfind(UId, #'TavlaPlayer'.player_id, Players).
