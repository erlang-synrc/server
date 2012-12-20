-module(game_okey).
-author('Paul Peregud <paulperegud@gmail.com>').
-behaviour(gen_fsm).

-export([start/3, start/4, start/1]).
-export([get_requirements/0, get_settings/1]).
-export([signal/2, make_move/3]).
-export([status/1, get_player_stats/1]).
-export([get_timeout/2]).
-export([get_okey/1, is_pair/1, split_by_delimiter/2, normalize_hand/2]).
-export([is_winning_hand/2, is_same_hand/2, hand_size/0, hand_out_pieces/0]).
-export([get_scoring_mode/2, is_set/1, is_run/1, generate_hand/2, generate_hand/0,
         create_game_info/1,get_player/2,get_player_id/2,get_players_pids/1,start_game/1]).

-include_lib("nsg_srv/include/conf.hrl").
-include_lib("nsg_srv/include/settings.hrl").
-include_lib("nsg_srv/include/requests.hrl").
-include_lib("nsm_db/include/config.hrl").
-include_lib("nsg_srv/include/game_okey.hrl").
-include_lib("nsx_config/include/config.hrl").
-include_lib("nsg_srv/include/types.hrl").
-include_lib("nsm_db/include/accounts.hrl").
-include_lib("nsx_config/include/log.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([state_wait/3, state_take/3, state_discard/3, state_challenge/3, state_finished/3, state_dead/3]).
-export([state_wait/2, state_take/2, state_discard/2, state_challenge/2]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-define(HS, 14).

-record(state, {
          game_id                          :: any(),
          initial                          :: list(#okey_player{}),     %% initial list of players (they will be scored)
          players                          :: list(#okey_player{}),     %% list of active player/bot pids
          pairs = null                     :: none() | list(list('PlayerId'())),
          table_name                       :: binary(),
          scoring_mode                     :: atom(),
          settings                         :: proplist(),
          mul_factor                       :: pos_integer(),
          slang_flag                       :: boolean(),
          observer_flag                    :: boolean(),
          game_info = []            :: [{atom(), any()}],

          stats                            :: pid(),
          relay                            :: {atom(), pid()},
          relay_monitor                    :: reference(),

          set_state                        :: #'OkeySetState'{},
          pile0                            :: list(#'OkeyPiece'{}),
          gosterge                         :: #'OkeyPiece'{},           %% Tash that is shown face up on game start
          reveal_successful                :: boolean(),                %% True if reveal was successful.
          reveal_last_discard              :: #'OkeyPiece'{},
          reveal_hand                      :: list(null | #'OkeyPiece'{}),
          challengers = []                 :: list(tuple(boolean(), pid())), %% list of players who has not yet answered on challenge
          who_revealed                     :: none() | #okey_player{},
          wait_list = []                   :: list(pid()),              %%  players that has not performed an action
          start_timestamp                  :: integer(),
          time_mark                        :: integer(),                %% used to properly guarantee turn timeouts
          okey_blocked = false             :: boolean(),                %% true if someone has blocked okey
          next                             :: any(),

          turn_timeout                     :: integer(),
          challenge_timeout                :: integer(),
          ready_timeout                    :: integer(),
          robot_delay                      :: integer(),
          game_type :: atom(),
          game_speed :: atom(),

          paused_statename                 :: atom(),
          paused_timeout_value             :: integer(),
          paused_who                       :: 'PlayerId'()
         }).

hand_size() ->
    ?HS.

get_requirements() ->
    [{players, 4}].

signal(Server, Msg) ->
    gen_fsm:sync_send_all_state_event(Server, {signal, Msg}).

get_settings(Server) when is_pid(Server) ->
    gen_fsm:sync_send_all_state_event(Server, get_settings);
get_settings(Settings00) ->
    get_settings0(Settings00).

status(Server) ->
    gen_fsm:sync_send_all_state_event(Server, status).

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
make_move(Server, Who, #okey_has_gosterge{} = Msg) ->
    gen_fsm:sync_send_all_state_event(Server, {Msg, Who});
make_move(Server, Who, Msg) ->
    ?INFO("MAKEMOVE: ~p",[{Server,Who,Msg}]),
    gen_fsm:sync_send_event(Server, {Msg, Who}).

get_player_stats(PlayerId) ->
    {ok, GameStats} = game_stats:get_game_points(okey, PlayerId),
    {ok, Skill} = game_stats:get_skill(PlayerId),
    {ok, PlayerStats} = game_stats:get_player_stats(PlayerId),
    #'PlayerOkeyStats'{playerId = PlayerId,
                       level = Skill,
                       score = proplists:get_value(game_points, GameStats),
                       numberOkey = proplists:get_value(finished_with_okey, GameStats),
                       number8Tashes = proplists:get_value(finished_with_8_tashes, GameStats),
                       totalWins = proplists:get_value(total_wins, PlayerStats),
                       totalLose = proplists:get_value(total_loses, PlayerStats),
                       totalDisconnects = proplists:get_value(total_disconnects, PlayerStats),
                       overalSuccessRatio = proplists:get_value(overall_success_ratio, PlayerStats),
                       averagePlayDuration = proplists:get_value(average_play_time, PlayerStats)
                      }.

start([Relay, Pids, GameId]) -> start(Relay, Pids, GameId);
start([Relay, Pids, GameId, P]) -> start(Relay, Pids, GameId, P).
start(Relay, Pids, GameId) -> start(Relay, Pids, GameId, []).
start(Relay, Pids, GameId, Params) -> gen_fsm:start_link(?MODULE, [Relay, Pids, GameId, Params], []).


init([Relay, PidsWithPlayersInfo, GameId, Settings0]) ->
    Settings1 = get_settings(Settings0),

    Mode = proplists:get_value(game_mode, Settings1),
    TestPR = #pointing_rule{game = okey, game_type = Mode, kakush_winner = 15,
                            kakush_other = 2, game_points = 15, quota = 20},
    TestLuckyPR = #pointing_rule{game = okey, game_type = feellucky, kakush_winner = 0,
                                 kakush_other = 0, game_points = 0, quota = 0},

    Rounds = case Mode of countdown -> inifinity; _ -> proplists:get_value(rounds, Settings1) end,
    PR = proplists:get_value(pointing_rules, Settings1, TestPR),
    [PRLucky] = proplists:get_value(pointing_rules_ex, Settings1, [TestLuckyPR]),
    Sets = 1,

    Settings2 = lists:keystore(rounds, 1, Settings1, {rounds, Rounds}),
    Settings = lists:keystore(sets, 1, Settings2, {sets, Sets}),
    UserOpts = proplists:get_value(users_options, Settings, []),
    {_, PlayersWithInfo} = lists:unzip(PidsWithPlayersInfo),
%%    PlayersWithInfo = [game_session:get_player_info(Pid) || Pid <- Pids],
    DP = proplists:get_value(double_points, Settings, 1),
    SlangFlag = proplists:get_value(slang, Settings, false),
    LuckyFlag = proplists:get_value(lucky, Settings, false),
    ObserverFlag = proplists:get_value(deny_observers, Settings, false),
    TName = proplists:get_value(table_name, Settings),

    GameInfo = [{id, GameId},
                {name, TName},
                {game_type, game_okey},
                {double_points, DP},
                {mode, Mode},
                {lucky, LuckyFlag},
                {pointing_rules, PR},
                {initial_players, PlayersWithInfo},
                {pointing_rules_lucky, PRLucky},
                {users_options, UserOpts}],
    ?INFO("GameInfo: ~p",[GameInfo]),

    Settings3 = lists:keystore(game_info, 1, Settings, {game_info, GameInfo}),
    {ok, StatsPid} = game_okey_scoring:start_link(Settings3),

    ?INFO("scoring mode: ~p, sets: ~p, rounds: ~p", [Mode, Sets, Rounds]),
    SS = #'OkeySetState'{
      round_cur = 1,
      round_max = Rounds,
      set_cur = 1,
      set_max = Sets},
    Speed = proplists:get_value(speed, Settings),
    RobotDelay = get_timeout(robot, Speed),
    true = RobotDelay /= undefined,
    ?INFO("RobotDelay = ~p", [RobotDelay]),
    State0 = #state{stats = StatsPid,
                    scoring_mode = Mode,
                    table_name = iolist_to_binary(TName),
                    set_state = SS,
                    settings = Settings,
                    game_info = GameInfo,
                    game_speed = Speed,
                    game_type = Mode,
                    turn_timeout = get_timeout(turn, Speed),
                    challenge_timeout = get_timeout(challenge, Speed),
                    ready_timeout = get_timeout(ready, Speed),
                    robot_delay = RobotDelay,
                    mul_factor = DP,
                    slang_flag = SlangFlag,
                    observer_flag = ObserverFlag
                   },
    ?INFO("init game relay: ~p, players: ~p, gameid: ~p", [Relay, PlayersWithInfo, GameId]),
    State = init_game(Relay, PidsWithPlayersInfo, GameId, State0),
    Gosterge = State#state.gosterge,
    GameSubmode = game_okey:get_scoring_mode(Mode, Gosterge),
    game_okey_scoring:set_chanak_points(StatsPid,game_okey_scoring:new_chanak(GameSubmode)),
    {ok, state_created, State}.

init_game({_, RPid} = Relay, PInfos, GameId, State) ->
    Ref = erlang:monitor(process, RPid),
    {Gosterge, Hands, Pile0} = hand_out_pieces(),
%%     PInfos = lists:map(fun(Pid0) ->
%%                                game_session:get_player_info(Pid0)
%%                        end, Pids),
%%    Comb = lists:zip3(Pids, Hands, PInfos),
    Comb = [{Pid, Hand, PInfo} || {Hand, {Pid, PInfo}} <- lists:zip(Hands, PInfos)],
    Players0 = lists:map(fun({Pid, Hand, PI}) ->
                                 SP = case game_stats:get_skill(PI#'PlayerInfo'.id) of
                                          {ok, S} -> S;
                                          {error, _} -> ?SKILL_SEED
                                      end,
                                 #okey_player{pid = Pid, hand = Hand,
                                              player_id = PI#'PlayerInfo'.id,
                                              player_info = PI, skill = SP
                                             }
                         end, Comb),

    [P1 | Rest] = Players0,
    [T | Pile] = Pile0,
    NHand = [T | P1#okey_player.hand],
    P2 = P1#okey_player{hand = NHand},
    Players1 = [P2 | Rest],

    Players = queue:from_list(Players1),

    {Pids,_} = lists:unzip(PInfos),
    ?INFO("init wait_list: ~p", [Pids]),
    State#state{initial = Players, players = Players,
                gosterge = Gosterge, pile0 = Pile,
                wait_list = Pids, relay = Relay, game_id = GameId,
                relay_monitor = Ref}.

state_wait({#okey_ready{}, Pid}=Event, _, #state{wait_list = [Pid]} = State) ->
    ?INFO("OKEY GAME state_wait: ~p", [Event]),
    PI = get_player(Pid, State),
    publish_ge(State#state.relay,
               #okey_player_ready{player = PI#okey_player.player_id}),
    update_table_state(State, State#state.next),
    {reply, ok, state_finished, State#state{start_timestamp = mnow(),
                                      wait_list = [],
                                      time_mark = timeout_at(State#state.turn_timeout)},
     State#state.turn_timeout};
state_wait({#okey_ready{}, Pid}=Event, _, #state{wait_list = List} = State) ->
    ?INFO("OKEY GAME state_wait: ~p", [Event]),
    Relay = State#state.relay,
    case lists:member(Pid, List) of
        true ->
            ?INFO("okey_ready B 1.", []),
            PI = get_player(Pid, State),
            publish_ge(Relay, #okey_player_ready{player = PI#okey_player.player_id}),
            {reply, ok, state_wait, State#state{wait_list = List--[Pid]}, calc_timeout(State)};
        false ->
            ?INFO("okey_ready B 2.", []),
            {reply, {error, you_are_not_a_player}, state_wait, State, calc_timeout(State)}
    end;

state_wait(Request, From, State) ->
    ?INFO("OKEY GAME state_wait: Unexpected request: ~p from: ~p", [Request, From]),
    {reply, {error, unexpected_request}, state_wait, State, calc_timeout(State)}.


state_wait(timeout=Event, State) ->
    ?INFO("OKEY GAME state_wait: ~p", [Event]),
    lists:map(fun(Pid) ->
                      PI = get_player(Pid, State),
                      publish_ge(State#state.relay,
                                 #okey_player_ready{player = PI#okey_player.player_id})
              end, State#state.wait_list),
    update_table_state(State, State#state.next),
    {next_state, state_finished, State#state{start_timestamp = mnow(),
                                       wait_list = [],
                                       time_mark = timeout_at(State#state.turn_timeout)}, State#state.turn_timeout};

state_wait(Event, State) ->
    ?INFO("OKEY GAME state_wait: Unexpected event: ~p. Ignoring", [Event]),
    {next_state, state_wait, State, calc_timeout(State)}.

add_tash_to_hand(#okey_player{can_show_gosterge = Old,
                              hand = OldHand} = Player, PileNum, DrawnTash, State) ->
    Player#okey_player{hand = [DrawnTash | OldHand],
                       can_show_gosterge = check_gosterge_restriction(Old, PileNum, DrawnTash, State)
                      }.

check_gosterge_restriction(false, _PileNum, _DrawnTash, _State) -> false;
check_gosterge_restriction(_,     0,        _DrawnTash, _State) -> false;
check_gosterge_restriction(_,     1,        Gosterge,   #state{gosterge = Gosterge}) -> false;
check_gosterge_restriction(_,     _,        _,          _) -> true.

state_take(timeout, State) ->
    ?INFO("OKEY GAME state_take: ~p",[timeout]),
    invariables(State),
    {Events, TashTaken, NState} = timeout_take(State),
    NState2 = timeout_discard(Events, TashTaken, NState),
    NState3 = NState2#state{okey_blocked = false},
    check_timeout_game_end(NState3).

state_discard(timeout, State) ->
    ?INFO("OKEY GAME state_discard: ~p",[timeout]),
    invariables(State),
    NState = timeout_discard([], null, State),
    check_timeout_game_end(NState).

state_take({#okey_take{pile = PileNum}, Pid}=Event, _, State) ->
    ?INFO("OKEY GAME state_take: ~p",[Event]),
    invariables(State),
    Relay = State#state.relay,
    UId = (get_current(State))#okey_player.player_id,
    case {is_current(Pid, State), PileNum, State#state.okey_blocked} of
        {true, 1, true} ->
            {reply, {error, okey_is_blocked}, state_take, State, calc_timeout(State)};
        {true, _, _} ->
            {DrawnTash, TashRevealed, PileHeight, State1} = take_tile(PileNum, State),
            A = #okey_tile_taken{player = UId,
                                 pile = PileNum,
                                 revealed = TashRevealed,
                                 pile_height = PileHeight
                                },
            publish_ge(Relay, A),
            Player = get_current(State1),
            Player1 = add_tash_to_hand(Player, PileNum, DrawnTash, State),
            State2 = update_current(Player1, State1),
            State3 = State2#state{okey_blocked = false},
            {reply, DrawnTash, state_discard, State3, calc_timeout(State3)};
        {false, _, _} ->
            {reply, {error, not_your_turn}, state_take, State, calc_timeout(State)}
    end;
state_take({#okey_reveal{}, _Pid}=Event, _, State) ->
    ?INFO("OKEY GAME state_take: ~p",[Event]),
    {reply, {error, take_tash_first}, state_take, State, calc_timeout(State)};
state_take({#okey_debug{}, Pid}=Event, _, State) ->
    ?INFO("OKEY GAME state_take: ~p",[Event]),
    invariables(State),
    send_debug(state_take, Pid, State);
state_take({#okey_i_saw_okey{}=Event, Pid}, _, State) ->
    ?INFO("OKEY GAME state_take: ~p",[Event]),
    i_saw_okey(state_take, Pid, State);
state_take({#okey_discard{}, Pid}=Event, _, State) ->
    ?INFO("OKEY GAME state_discard: ~p",[Event]),
    Reason = case is_current(Pid, State) of
                 false ->
                     not_your_turn;
                 true ->
                     take_tash_first
             end,
    {reply, {error, Reason}, state_take, State, calc_timeout(State)};
state_take({Msg, Pid}, _, State) ->
    ?INFO("unrecognized message_not_valid_for_state_take, msg ~p, author ~p", [Msg, Pid]),
    {reply, {error, message_not_valid_for_state_take}, state_take, State, calc_timeout(State)}.

state_discard({#okey_i_saw_okey{}, Pid}=Event, _, State) ->
    ?INFO("OKEY GAME state_discard: ~p",[Event]),
    i_saw_okey(state_discard, Pid, State);
state_discard({#okey_debug{}, Pid}=Event, _, State) ->
    ?INFO("OKEY GAME state_discard: ~p",[Event]),
    send_debug(state_discard, Pid, State);
state_discard({#okey_discard{tile = Tash}=Event, Pid}, _, State) ->
    ?INFO("OKEY GAME state_discard: ~p tile ~p",[Event,Tash]),
    invariables(State),
    Player = get_current(State),
    Relay = State#state.relay,
    Hand = Player#okey_player.hand,
    PileZeroL = length(State#state.pile0),
    case {is_current(Pid, State), lists:member(Tash, Hand), PileZeroL} of
        {true, _, 0} ->
            {ok, Next, NewState} = game_nowinner_results(State),
            before_state_wait(reply, ok, state_wait, NewState#state{next = Next, wait_list = get_players_pids(NewState)});
        {true, true, _} ->
            State1 = discard_tile(Tash, State),
            A = #okey_tile_discarded{player = Player#okey_player.player_id,
                                     tile = Tash
                                    },
            publish_ge(Relay, A),
            {NextPlayer, State2} = set_next(State1),
            C = #okey_next_turn{player = NextPlayer#okey_player.player_id},
            publish_ge(Relay, C),
            {reply, ok, state_take, State2#state{time_mark = timeout_at(State#state.turn_timeout)}, State#state.turn_timeout};
        {true, false, _} ->
            ?INFO("ERROR. Session: ~p. State:~n~p~nMsg: discard_tile, tile: ~p", [Pid, State, Tash]),
            {reply, {error, no_such_tash}, state_discard, State, calc_timeout(State)};
        _ ->
            {reply, {error, not_your_turn}, state_discard, State, calc_timeout(State)}
    end;
state_discard({#okey_take{}, Pid}=Event, _, State) ->
    ?INFO("OKEY GAME state_discard: ~p",[Event]),
    R = case {get_player(Pid, State), is_current(Pid, State)} of
            {false, _} ->
                you_are_not_a_player;
            {_, false} ->
                not_your_turn;
            {_, true} ->
                cant_take_do_discard
        end,
    {reply, {error, R}, state_discard, State, calc_timeout(State)};

state_discard({#okey_reveal{discarded = Tash, hand = TashPlaces}, Pid}=Event, _, State) ->
    ?INFO("OKEY GAME state_discard: ~p",[Event]),
    invariables(State),
    Gosterge = State#state.gosterge,
    Player = get_current(State),
    Relay = State#state.relay,
    Hand0 = Player#okey_player.hand,
    Member = lists:member(Tash, Hand0),
    Hand = lists:delete(Tash, Hand0),
    SameHand = is_same_hand(TashPlaces, Hand),
    WinningHand = is_winning_hand(TashPlaces, Gosterge),

    case {is_current(Pid, State), Member, length(Hand), SameHand} of
        {true, true, ?HS, true} ->
            A = #okey_revealed{player = Player#okey_player.player_id,
                               discarded = Tash,
                               hand = TashPlaces},
            Challengers = lists:delete(Player, [X || X <- queue:to_list(State#state.players)]),
            lists:map(fun(Ch) ->
                              B = #okey_next_turn{player = Ch#okey_player.player_id,
                                                  can_challenge = true
                                                 },
                              message_session(Relay, Ch#okey_player.pid, A),
                              message_session(Relay, Ch#okey_player.pid, B)
                      end, Challengers),
            Pids = [ X#okey_player.pid || X <- Challengers ],
            {_, State2} = set_next(State),
            {reply, ok, state_challenge, State2#state{reveal_successful = WinningHand,
                                                      reveal_last_discard = Tash,
                                                      reveal_hand = TashPlaces,
                                                      who_revealed = Player,
                                                      challengers = [],
                                                      wait_list = Pids,
                                                      time_mark = timeout_at(State2#state.challenge_timeout)
                                                     }, State2#state.challenge_timeout};
        {true, false, _, _} ->
            {reply, {error, no_such_tash}, state_discard, State, calc_timeout(State)};
        {true, true, _, _} ->
            {reply, {error, discarded_hand_does_not_match_server_state}, state_discard, State, calc_timeout(State)};
        {false, _, _, _} ->
            {reply, {error, not_your_turn}, state_discard, State, calc_timeout(State)}
    end;
state_discard({Msg, Pid}, _, State) ->
    ?INFO("unrecognized message_not_valid_for_state_discard Msg: ~p, Pid: ~p", [Msg, Pid]),
    {reply, {error, message_not_valid_for_state_discard}, state_discard, State, calc_timeout(State)}.

state_challenge(timeout=Event, State) ->
    ?INFO("OKEY GAME state_challenge: ~p",[Event]),
    TO = lists:map(fun(Pid) ->
                           {Pid, false}
                   end, State#state.wait_list),
    {ok, Next, NewState} = game_results(State#state{challengers = TO ++ State#state.challengers}),
    before_state_wait(next_state, state_wait, NewState#state{next = Next, wait_list = get_players_pids(NewState)}).

state_challenge({#okey_challenge{challenge = DoCh}, Pid}=Event, _From, State = #state{}) ->
    ?INFO("OKEY GAME state_challenge: ~p",[Event]),
    invariables(State),
    WaitList0 = State#state.wait_list,
    Challengers0 = State#state.challengers,
    Member = lists:member(Pid, WaitList0),
    #okey_player{player_id = _UId} = get_player(Pid, State),
    {WaitList, Challengers} = case Member of
                                  true ->
                                      X = lists:keystore(Pid, 1, Challengers0, {Pid, DoCh}),
                                      {lists:delete(Pid, WaitList0), X};
                                  false ->
                                      {WaitList0, Challengers0}
                              end,
    case {Member, WaitList, get_player(Pid, State)} of
        {_, _, false} ->
            {reply, {error, you_are_not_a_player}, state_challenge, State, calc_timeout(State)};
        {false, _, _} ->
            {reply, {error, not_your_turn}, state_challenge, State, calc_timeout(State)};
        {true, [], _} ->
            {ok, Next, NewState} = game_results(State#state{challengers = Challengers}),
            before_state_wait(reply, ok, state_wait, NewState#state{next = Next, wait_list = get_players_pids(NewState)});
        {true, _, _} ->
            {reply, ok, state_challenge, State#state{wait_list = WaitList, challengers = Challengers}, calc_timeout(State)}
    end;
state_challenge({_, _}, _From, State) ->
    {reply, {error, message_not_valid_for_this_game_state}, state_challenge, State, calc_timeout(State)}.

state_finished({#okey_ready{}, Pid}, _, State) ->
    case lists:member(Pid, get_players_pids(State)) of
        true ->
            PI = get_player(Pid, State),
            publish_ge(State#state.relay,
                       #okey_player_ready{player = PI#okey_player.player_id}),
            {reply, ok, state_finished, State};
        false ->
            {reply, {error, you_are_not_a_player}, state_finished, State}
    end;

state_finished(_Msg, _, State) ->
    ?INFO("game has ended, still receiving messages: ~p", [_Msg]),
    {reply, {error, game_has_already_ended}, state_finished, State}.

state_dead(_Msg, _, State) ->
    ?INFO("game is dead, still receiving messages: ~p", [_Msg]),
    {reply, {error, game_has_already_ended}, state_dead, State}.

handle_event(_Event, StateName, State) ->
    ?INFO("OKEY GAME EVENT: ~p",[{_Event,StateName}]),
    {next_state, StateName, State}.

handle_sync_event(get_settings=Event, _From, StateName, #state{} = State) ->
    ?INFO("OKEY GAME SYNCEVENT: ~p",[{Event,_From,StateName}]),
    Res = State#state.settings,
    {reply, Res, StateName, State}; %settings
handle_sync_event({signal, state_created}=Event, _From, state_created, #state{relay={RMod, RPid}} = State) ->
    ?INFO("OKEY GAME SYNCEVENT: ~p",[{Event,_From,state_created}]),
    publish_okey_game_info(State),
    RMod:update_gamestate(RPid, playing),
    %% charge quota from players
    start_game(State),
    { reply, ok, state_discard,
      State#state{start_timestamp = mnow(),
                  time_mark = timeout_at(State#state.turn_timeout)},
      State#state.turn_timeout };

handle_sync_event({signal, {pause_game, Pid}}, _From, StateName, #state{relay={RMod, RPid}}=State) ->
    Timeout = calc_timeout(StateName, State),
    case {is_player(Pid, State), StateName} of
        {_, X} when X == state_paused; X == state_dead; X == state_finished ->
            {reply, {error, pause_not_possible}, StateName, State, Timeout};
        {true, _} ->
            UID = (get_player(Pid, State))#okey_player.player_id,
            Event = #game_paused{game = State#state.game_id, who = UID,
                                 action = <<"pause">>, retries = 0},
            RMod:publish(RPid, Event),
            {reply, 0, state_paused,
             State#state{time_mark = undefined,
                         paused_who = UID,
                         paused_statename = StateName,
                         paused_timeout_value = Timeout},
             infinity};
        {false, _} ->
            {reply, {error, you_are_not_a_player}, StateName, State, Timeout}
    end;

handle_sync_event({signal, {resume_game, Pid}}, _From, StateName, #state{relay={RMod, RPid}} = State) ->
    case {StateName, is_player(Pid, State)} of
        {state_paused, true} ->
            ResumedState = State#state.paused_statename,
            ResumedTimeout = State#state.paused_timeout_value,
            UID = (get_player(Pid, State))#okey_player.player_id,
            Event = #game_paused{game = State#state.game_id, who = UID,
                                 action = <<"resume">>, retries = 0},
            RMod:publish(RPid, Event),
            { reply, 0, ResumedState,
              State#state{time_mark = mnow() + ResumedTimeout,
                          paused_statename = undefined,
                          paused_timeout_value = undefined},
              ResumedTimeout };
        {state_paused, false} ->
            {reply, {error, you_are_not_a_player}, state_paused, State, infinity};
        {_, _} ->
            {reply, {error, game_is_not_paused}, StateName, State, calc_timeout(State)}
    end;

handle_sync_event({signal, {replace_player, UId, _, _, _} = Msg},
                  F, state_paused, State = #state{paused_who = X}) when X == UId ->
    ?INFO("AAA pause detected while replacing player", []),
    Pid = (get_player(UId, State))#okey_player.pid,
    Res = handle_sync_event({signal, {resume_game, Pid}}, F, state_paused, State),
    ?INFO("AAA pause removed. ~p", [length(tuple_to_list(Res))]),
    {reply, _Answer, StateName, NewState, _NewTimeout} = Res,
    ?INFO("AAA proper replacing", []),
    handle_sync_event({signal, Msg}, F, StateName, NewState);

handle_sync_event({signal, {replace_player, UId, NUId, NUserInfo, NPid}}, _, StateName,
                  #state{relay = Relay} = State) ->
    ?INFO("RobotDelay 2 = ~p", [State#state.robot_delay]),
    ?INFO("Replace player: UId:~p NUId: ~p ", [UId, NUId]),
    P = get_player(UId, State),
    Cur = get_current(State),
    StatsPid = State#state.stats,
    {CurId, NewTimeout} = case (P =:= Cur) of
                              false ->
                                  {Cur#okey_player.player_id, calc_timeout(State)};
                              true ->
                                  {NUId, statename_to_timeout(StateName, State)}
                          end,
    P1 = P#okey_player{player_id = NUId, player_info = NUserInfo, pid = NPid},
    S1 = update_player(P, P1, State),
    game_okey_scoring:replace_uid(StatsPid, UId, NUId),
    Pid = P#okey_player.pid,
    WL = utils:lists_replace(Pid, NPid, State#state.wait_list),
    Challengers = lists:map(fun({X, Val}) when X == Pid -> {Pid, Val};
                               (Other) -> Other
                            end, S1#state.challengers),
    RS = S1#state.set_state,
    S2 = S1#state{wait_list = WL, challengers = Challengers},
    Players1 = queue:to_list(S2#state.players),
    PidPiles = lists:map(fun(X) ->
                                 case X#okey_player.pile of
                                     [] -> {X#okey_player.pid, null};
                                     [H|_] -> {X#okey_player.pid, H}
                                 end
                         end, Players1),
    Gosterge = S2#state.gosterge,
    Mode = S2#state.scoring_mode,
    GI = create_okey_game_info(S2),
    {_, RotatedPiles} = lists:unzip(rotate_until_face(NPid, PidPiles)),
    PlayerState = #okey_game_player_state{
      whos_move = CurId,
      game_state = statename_to_api_string(StateName),
      piles = RotatedPiles,
      tiles = P1#okey_player.hand,
      gosterge = Gosterge,
      pile_height = length(S2#state.pile0),
      current_round = RS#'OkeySetState'.round_cur,
      game_sub_type = game_okey:get_scoring_mode(Mode, Gosterge),
      next_turn_in = NewTimeout
     },
    message_session(Relay, NPid, [GI, PlayerState]),
    PS = {[GI, PlayerState], {S2#state.next, S2#state.robot_delay}}, %% TODO replace by 'ok'
    {reply, PS, StateName, S2, NewTimeout};

handle_sync_event({signal, do_rematch}, From, StateName, State) ->
    SS0 = State#state.set_state,
    SS = #'OkeySetState'{round_cur = 1, round_max = SS0#'OkeySetState'.round_max,
                         set_cur = 1, set_max = SS0#'OkeySetState'.set_max},
    State2 = State#state{set_state = SS},
    publish_okey_game_info(State2),
    game_okey_scoring:reset_scoring(State#state.stats),
    handle_sync_event({signal, {next_round, SS}}, From,
                      StateName, State2);

handle_sync_event({signal, {next_set, Data}}, From, StateName, State) ->
    ?INFO("doing next_set", []),
    State2 = State#state{set_state = Data},
    publish_okey_game_info(State2),
    handle_sync_event({signal, {next_round, Data}}, From,
                      StateName, State2);

handle_sync_event({signal, {next_round, RoundData}}, _From, _StateName, #state{relay={RMod,RPid}}=State0) ->
    #'OkeySetState'{round_cur = CurRound, set_cur = CurSet} = RoundData,
    ?INFO("setting up next round. Current round: ~p, Current set: ~p", [CurRound, CurSet]),
    PlayersList0 = queue:to_list(State0#state.players),
    {Gosterge, Hands, Pile0} = hand_out_pieces(),
    PlayersList1 = lists:map(fun({#okey_player{} = Player, Hand}) ->
                                     Player#okey_player{hand = Hand,
                                                        can_show_gosterge = true,
                                                        pile = []}
                             end, lists:zip(PlayersList0, Hands)),
    [P1 | Rest] = PlayersList1,
    [T | Pile] = Pile0,
    NHand = [T | P1#okey_player.hand],
    P2 = P1#okey_player{hand = NHand},
    PlayersList = [P2 | Rest],

    Pids = [ Player#okey_player.pid || Player <- PlayersList ],
    PlayersDict = queue:from_list(PlayersList),
    State = State0#state{initial = PlayersDict, players = PlayersDict, wait_list = Pids,
                         pile0 = Pile, gosterge = Gosterge,
                         challengers = [], who_revealed = undefined,
                         time_mark = undefined},
    RMod:update_gamestate(RPid, playing),
    State1 = State#state{set_state = RoundData, time_mark = timeout_at(State#state.turn_timeout)},
    start_game(State1),
    {reply, ok, state_discard, State1, State1#state.turn_timeout};

handle_sync_event({signal, {player_left, _Pid}}, _From, StateName, #state{relay={RMod,RPid}}=State)
  when StateName == state_finished ->
    RMod:update_gamestate(RPid, state_dead),
    {reply, ok, state_dead, State};

handle_sync_event({signal, {player_left, Pid}}, _From, StateName, #state{relay={RMod,RPid}}=State) when
  StateName == state_take;
  StateName == state_discard;
  StateName == state_challenge ->
    #state{stats = Scoring} = State,
    #okey_player{player_id = Id} = lists:keyfind(Pid, #okey_player.pid, queue:to_list(State#state.players)),
    game_okey_scoring:add_event(Scoring, Id, disconnected),
    RMod:update_gamestate(RPid, state_dead),
    {reply, ok, state_dead, State};

handle_sync_event({signal, {player_left, _Pid}}, _From, _StateName, State) ->
    {reply, ok, state_dead, State};

handle_sync_event(status, _From, StateName, State) ->
    Reply = StateName,
    {reply, Reply, StateName, State, calc_timeout(StateName, State)};

handle_sync_event({#okey_has_gosterge{}, Pid}=Ev, _From, StateName, State) when StateName == state_take;
                                                                             StateName == state_discard ->
    ?INFO("OKEY GAME SYNCEVENT: ~p",[{Ev,_From,StateName}]),
    case get_player(Pid, State) of
        false ->
            {reply, {error, you_are_not_a_player}, StateName, State, calc_timeout(StateName, State)};
        #okey_player{can_show_gosterge = false} = _Player ->
            {reply, false, StateName, State, calc_timeout(StateName, State)};
        Player ->
            Gosterge = State#state.gosterge,
            StatsPid = State#state.stats,
            ShownGosterge = game_okey_scoring:show_gosterge(StatsPid,
                                                            Player#okey_player.player_id,
                                                            Gosterge,
                                                            Player#okey_player.hand),
            case ShownGosterge of
                {true, RoundResult, SeriesResult} ->
                    ?INFO("INSTANT", []),
                    A = #okey_player_has_gosterge{player = Player#okey_player.player_id},
                    B = #okey_round_ended{good_shot = false,
                                          reason = <<"gosterge_instant">>,
                                          results = RoundResult,
                                          next_action = done
                                         },
                    publish_ge(State#state.relay, [A, B]),
                    publish_okey_series_ended(State, SeriesResult),
                    ?INFO("INSTANT2", []),
                    {reply, true, state_finished, State#state{next = {done, SeriesResult}, 
                                                              wait_list = get_players_pids(State)}};
                true ->
                    A = #okey_player_has_gosterge{player = Player#okey_player.player_id},
                    publish_ge(State#state.relay, A),
                    {reply, true, StateName, State, calc_timeout(StateName, State)};
                false ->
                    {reply, false, StateName, State, calc_timeout(StateName, State)}
            end
    end;

handle_sync_event({#okey_has_gosterge{}, _Pid}, _From, StateName, State) ->
    {reply, false, StateName, State, calc_timeout(StateName, State)};

handle_sync_event(_Event, _From, _StateName, State) ->
    {stop, {unknown_sync_event, {_Event, _From, _StateName}}, State}.

handle_info({'DOWN', Ref, _, _, Reason}, _StateName, #state{relay_monitor = Ref} = State) ->
    ?INFO("relay terminates with reason ~p, so does the game", [Reason]),
    {stop, Reason, State};
handle_info(no_more_rematch, state_finished, State) ->
    ?INFO("got no_more_rematch, finished->dead", []),
    {next_state, state_dead, State};
handle_info(no_more_rematch, state_dead, State) ->
    ?INFO("got no_more_rematch, dead->dead", []),
    {next_state, state_dead, State};
handle_info(Info, StateName, State) ->
    ?INFO("unknown info: ~p, StateName: ~p; dying", [Info, StateName]),
    {stop, {unknown_info, Info}, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

-spec get_settings0(proplist()) -> proplist().
get_settings0(Settings00) ->
    Settings0 = proplists:substitute_aliases([{tours, sets}, {tour, sets}, {turns, rounds}], Settings00),
    DefaultSettings = [
                       {game_mode, standard},
                       {gosterge_finish, false},
                       {allow_replacement, true},
                       {deny_robots, false},
                       {speed, normal},
                       {observers, true},
                       {sets, 1},
                       {rounds, 1},
                       {table_name, "default okey"}
                      ],
    utils:apply_defauls(DefaultSettings, Settings0).

get_timeout(turn, fast) -> {ok, Val}   = nsm_db:get(config,"games/okey/turn_timeout_fast", 15000), Val;
get_timeout(turn, normal) -> {ok, Val} = nsm_db:get(config,"games/okey/turn_timeout_normal", 30000), Val;
get_timeout(turn, slow) -> {ok, Val}   = nsm_db:get(config,"games/okey/turn_timeout_slow", 60000), Val;

get_timeout(challenge, fast) ->  {ok, Val}   = nsm_db:get(config,"games/okey/challenge_timeout_fast", 5000), Val;
get_timeout(challenge, normal) ->  {ok, Val} = nsm_db:get(config,"games/okey/challenge_timeout_normal", 10000), Val;
get_timeout(challenge, slow) -> {ok, Val}    = nsm_db:get(config,"games/okey/challenge_timeout_slow", 20000), Val;

get_timeout(ready, fast) -> {ok, Val}   = nsm_db:get(config,"games/okey/ready_timeout_fast", 15000), Val;
get_timeout(ready, normal) -> {ok, Val} = nsm_db:get(config,"games/okey/ready_timeout_normal", 25000), Val;
get_timeout(ready, slow) -> {ok, Val}   = nsm_db:get(config,"games/okey/ready_timeout_slow", 45000), Val;

get_timeout(robot, Speed) -> case ?IS_TEST of true -> 1; false -> get_timeout(robot_production, Speed) end;
get_timeout(robot_production, fast) -> {ok, Val}   = nsm_db:get(config,"games/okey/robot_delay_fast", 6000), Val;
get_timeout(robot_production, normal) -> {ok, Val} = nsm_db:get(config,"games/okey/robot_delay_normal", 9000), Val;
get_timeout(robot_production, slow) -> {ok, Val}   = nsm_db:get(config,"games/okey/robot_delay_slow", 15000), Val.

-spec i_saw_okey(atom(),pid(),#state{}) -> {atom(),atom(),atom()|tuple(atom(),atom()),#state{},integer()}.
i_saw_okey(StateName, Pid, State) ->
    Cur = get_current(State),
    Okey = get_okey(State),
    {DrawnTash, _TashRevealed, _PileHeight, _State1} = take_tile(1, State),
    case {get_player(Pid, State), State#state.okey_blocked, DrawnTash} of
        {false, _, _} ->
            {reply, {error, you_are_not_a_player}, StateName, State, calc_timeout(StateName, State)};
        {_, true, _} ->
            {reply, ok, StateName, State, calc_timeout(State)};
        {Author, _, Okey} ->
            A = #okey_disable_okey{player = Cur#okey_player.player_id,
                                   who_disabled = Author#okey_player.player_id},
            publish_ge(State#state.relay, [A]),
            % ?INFO("disabling okey tash: ~p", [A]),
            {reply, ok, StateName, State#state{okey_blocked = true}, calc_timeout(StateName, State)};
        {_, _, _} ->
            {reply, {error, there_is_no_okey_there}, StateName, State, calc_timeout(StateName, State)}
    end.

-spec timeout_take(#state{}) -> {list(#okey_tile_taken{}), any(), #state{}}.
timeout_take(State) ->
    Player = get_current(State),
    {DrawnTash, TashRevealed, PileHeight, State1} = take_tile(0, State),
    A = #okey_tile_taken{player = Player#okey_player.player_id,
                         pile = 0,
                         revealed = TashRevealed,
                         pile_height = PileHeight
                        },
    Player1 = add_tash_to_hand(Player, 0, DrawnTash, State),
    State2 = update_current(Player1, State1),
    {[A], DrawnTash, State2}.

timeout_discard(Events, TashTaken, State) ->
    Relay = State#state.relay,
    Player = get_current(State),
    PlayerPid = Player#okey_player.pid,
    Hand = Player#okey_player.hand,
    {DiscardTash, Hand1} = kakamath:draw_random(Hand),
    Msg = #okey_turn_timeout{tile_taken = TashTaken,
                             tile_discarded = DiscardTash},
    message_session(Relay, PlayerPid, Msg),
    B = #okey_tile_discarded{player = Player#okey_player.player_id,
                             tile = DiscardTash,
                             timeouted = true
                            },
    Player1 = Player#okey_player{hand = Hand1},
    NState = update_current(Player1, State),
    {NextPlayer, NState1} = set_next(NState),
    NextPlayer1 = NextPlayer#okey_player{pile = [DiscardTash |  NextPlayer#okey_player.pile]},
    NState2 = update_current(NextPlayer1, NState1),
    C = #okey_next_turn{player = NextPlayer#okey_player.player_id},
    publish_ge(Relay, Events ++ [B, C]),
    NState2.

check_timeout_game_end(State) ->
    case length(State#state.pile0) of
        0 ->
            {ok, Next, NewState} = game_nowinner_results(State),
            before_state_wait(next_state, state_wait, NewState#state{next = Next, wait_list = get_players_pids(NewState)});
        _ ->
            {next_state, state_take, State#state{time_mark = timeout_at(State#state.turn_timeout)}, State#state.turn_timeout}
    end.

send_debug(LastState, Pid, State) ->
    invariables(State),
    Player = get_current(State),
    case is_current(Pid, State) of
        true ->
            {reply, Player#okey_player.hand, LastState, State};
        false ->
            {reply, {error, not_your_turn}, LastState, State}
    end.

basic_game_results(State, Results) ->
    #'OkeyGameResults'{
                    game_id = State#state.game_id,
                    start_datetime = State#state.start_timestamp,
                    end_datetime = mnow(),
                    results = Results
                   }.

publish_okey_series_ended(#state{relay={RMod,RPid}}=State, Scoring) ->
    Standings = game_okey_scoring:standings(Scoring),
    A = #okey_series_ended{standings = Standings},
    publish_ge(State#state.relay, A),
    game_stats:assign_points(#'OkeyGameResults'{series_results = Standings}, State#state.game_info),
    RMod:update_gamestate(RPid, state_finished),
    RPid ! {unreg, RPid}, %% XXX WTF?
    ?INFO("SERVER SERIES ENDED: ~p", [A]),
    ok.

%% jumps directly to finished state if series has ended
before_state_wait(reply, Reply, _StateName, State = #state{next = {done, Scoring}}) ->
    publish_okey_series_ended(State, Scoring),
    {reply, Reply, state_finished, State};
before_state_wait(reply, Reply, StateName, State) ->
    {reply, Reply, StateName, State}.

before_state_wait(next_state, _StateName, State = #state{next = {done, Scoring}}) ->
    publish_okey_series_ended(State, Scoring),
    {next_state, state_finished, State};
before_state_wait(Action, Param, State) ->
    {Action, Param, State}.

update_table_state(State, {done, Scoring}) ->
    ?INFO("update_table_state A: series of set finished", []),
    publish_okey_series_ended(State, Scoring);
update_table_state(_State, {next_set, Data}) ->
    ?INFO("update_table_state B: set finished. Data: ~p", [Data]),
    Self = self(),
    ?INFO("spawning next_set", []),
    proc_lib:spawn_link(fun() -> game_okey:signal(Self, {next_set, Data}) end);
update_table_state(_State, {next_round, Data}) ->
    ?INFO("update_table_state C: round finished. Data: ~p", [Data]),
    Self = self(),
    ?INFO("spawning next_round", []),
    proc_lib:spawn_link(fun() -> game_okey:signal(Self, {next_round, Data}) end).

game_nowinner_results(#state{stats = StatsPid} = State) ->
    Players = queue:to_list(State#state.players),
    Res = basic_game_results(State,
                             lists:map(fun(#okey_player{player_id = Id, skill = SP}) ->
                                               #'OkeyGameR'{player_id = Id,
                                                            skill = SP,
                                                            skill_delta = 0.25,
                                                            winner = <<"none">>,
                                                            disconnected = false,
                                                            score_delta = 0}
                                       end, queue:to_list(State#state.players))),
    [ begin
          game_okey_scoring:add_event(StatsPid, U#okey_player.player_id, out_of_tashes)
      end || U <- Players ],
    Hands = [ {U#okey_player.player_id, U#okey_player.hand} || U <- Players ],
    {ok, Next, Scoring} = game_okey_scoring:finish_round(StatsPid, Res, Hands, State#state.gosterge),
    A = #okey_round_ended{good_shot = false,
                          reason = <<"draw">>,
                          results = Scoring,
                          next_action = simplify_next(Next)},
    ?INFO("round ended: ~p", [A]),
    publish_ge(State#state.relay, A),
    ?INFO("CHANAK NOWIN ROUND UPDATE: ~p",[game_okey_scoring:get_chanak_points(StatsPid)]),
    {ok, Next, State}.

%% TODO: do some cleanup here!
game_results(State = #state{}) ->
    StatsPid = State#state.stats,
    GoodReveal = State#state.reveal_successful,
    Relay = State#state.relay,
    Challengers = State#state.challengers,
    CPids = [ CPid || {CPid, _} <- Challengers ],
    Bluff = not lists:any(fun({_, S}) -> not S end, Challengers),
    FinisherWinsBool = ((not GoodReveal) andalso Bluff) orelse (GoodReveal),
    Players = queue:to_list(State#state.players),
    {Winners, Losers} = lists:partition(fun(#okey_player{pid = Pid}) ->
                                                case FinisherWinsBool of
                                                    true  -> not lists:member(Pid, CPids);
                                                    false -> lists:member(Pid, CPids)
                                                end
                                        end, queue:to_list(State#state.players)),
    ?INFO("Wrong Reveal: ~p",[{GoodReveal,Bluff,Challengers}]),
    case FinisherWinsBool of
        true ->
            [#okey_player{player_id = UId}] = Winners,
            [ game_okey_scoring:add_event(StatsPid, pid_to_uid(Pid, State), rejected_good_hand)
              || {Pid, true} <- Challengers ],
            game_okey_scoring:add_event(StatsPid, UId, reveal);
        false ->
            [ game_okey_scoring:add_event(StatsPid, XUId, caught_bluff)
              || #okey_player{player_id = XUId} <- Winners ],
            [#okey_player{player_id = UId}] = Losers,
            game_okey_scoring:add_event(StatsPid, UId, wrong_reveal)
    end,

    {FinisherWins, OtherWins} = case FinisherWinsBool of
                                    true -> {1, 0};
                                    false -> {0.0, 0.33333}
                                end,
    Stats = lists:map(fun(#okey_player{player_id = Id, skill = SP, pid = Pid} = Player) ->
                              GS = case lists:keyfind(Pid, 1, Challengers) of
                                       false -> FinisherWinsBool;
                                       {_, Shot} -> (Shot =/= GoodReveal)
                                   end,
                              IsWinner = lists:member(Player, Winners),
                              Delta = case IsWinner of
                                          true -> OtherWins;
                                          false -> FinisherWins
                                      end,
                              #'OkeyGameR'{player_id = Id,
                                           skill = SP,
                                           skill_delta = Delta,
                                           disconnected = false,
                                           winner = list_to_binary(atom_to_list(IsWinner)),
                                           good_shot = GS,
                                           score_delta = 0}
                      end, queue:to_list(State#state.players)),
    Res = basic_game_results(State, Stats),
    Hands = [ {U#okey_player.player_id, U#okey_player.hand} || U <- Players ],
    {ok, Next, Scoring} = game_okey_scoring:finish_round(StatsPid, Res, Hands, State#state.gosterge, Winners,
                                                         State#state.reveal_last_discard, State#state.reveal_hand),
    lists:map(fun({CPid, Shot}) ->
                      GS = (Shot =/= GoodReveal),
                      A = #okey_round_ended{good_shot = GS,
                                            reason = <<"reveal">>,
                                            results = Scoring,
                                            next_action = simplify_next(Next)},
                      message_session(Relay, CPid, A)
              end, Challengers),

    Spin = fun(#'OkeyGameResults'{results = List}) ->
                   lists:map(fun(X) -> {X#'OkeyGameR'.player_id, X#'OkeyGameR'.score_delta, X#'OkeyGameR'.score} end, List)
           end,
    ?INFO("full scores: ~p", [Scoring]),
    ?INFO("scores: ~p", [Spin(Scoring)]),
    PlayerPid = (State#state.who_revealed)#okey_player.pid,
    B = #okey_round_ended{good_shot = FinisherWinsBool,
                          reason = <<"reveal">>,
                          results = Scoring,
                          next_action = simplify_next(Next)
                         },
    message_session(Relay, PlayerPid, B),
    {ok, Next, State}.

simplify_next({done, _}) ->  done;
simplify_next({next_set, _}) -> next_set;
simplify_next({next_round, _}) -> next_round.

generate_pieces() ->
    S1 = sofs:set([element(1, #'OkeyPiece'{})]),
    S2 = sofs:set(lists:seq(1,4)),
    S3 = sofs:set(lists:seq(1,?HS-1)),
    P3 = sofs:product({S1,S2,S3}),
    R = sofs:to_external(P3),
    R ++ R.

hand_out_pieces() ->
    case nsm_db:get(config,"games/okey/debug_next_round_pieces", undefined) of
        {ok, undefined} -> generate_hand();
        {ok, [Go | L]} -> generate_hand(Go, L)
    end.

generate_hand(Gosterge, List) ->
    {Hands, Pile} = split_pile(List),
    {Gosterge, Hands, Pile}.

generate_hand() ->
    {Gosterge, List0} = kakamath:draw_random(generate_pieces()),
    List = [ ?FALSE_OKEY, ?FALSE_OKEY |  List0 ],
    generate_hand(Gosterge, kakamath:variate(List)).

split_pile(List) ->
    {A, AR} = lists:split(?HS, List),
    {B, BR} = lists:split(?HS, AR),
    {C, CR} = lists:split(?HS, BR),
    {D, Pile} = lists:split(?HS, CR),
    {[A, B, C, D], Pile}.

take_tile(0, State) -> take_tile_0(State);
take_tile(1, State) -> take_tile_1(State).

take_tile_0(State) ->
    [Tash | Rest] = State#state.pile0,
    {Tash, null, length(Rest), State#state{pile0 = Rest}}.
take_tile_1(State) ->
    Q = State#state.players,
    {{value, P}, Q2} = queue:out(Q),
    {Tash, TashA, Rest} = case P#okey_player.pile of
                              [A, B | C] ->
                                  {A, B, C};
                              [A | C] ->
                                  {A, null, C}
                          end,
    NewPile = case TashA of
                  null -> Rest;
                  _ -> [TashA | Rest]
              end,
    {Tash, TashA, length(Rest)+1, State#state{players = queue:in_r(P#okey_player{pile = NewPile}, Q2)}}.

discard_tile(Tile, State) ->
    P = get_current(State),
    PNext = get_next(State),
    Hand = P#okey_player.hand,
    {true, is_list_pile} = {is_list(Hand), is_list_pile},
    true = lists:member(Tile, Hand),
    Hand1 = lists:delete(Tile, Hand),
    P1 = P#okey_player{hand = Hand1},
    PNext1 = PNext#okey_player{pile = [Tile | PNext#okey_player.pile]},
    State1 = update_current(P1, State),
    update_next(PNext1, State1).


is_player(Pid, State) -> false /= get_player(Pid, State).
get_player(Pid, State) when is_pid(Pid) -> lists:keyfind(Pid, #okey_player.pid, queue:to_list(State#state.players));
get_player(UId, State) -> lists:keyfind(UId, #okey_player.player_id, queue:to_list(State#state.players)).
get_player_id(Pid, State) -> (get_player(Pid, State))#okey_player.player_id.
get_players_pids(State) -> [ P#okey_player.pid || P <- queue:to_list(State#state.players) ].
is_current(Pid, State) -> P = get_current(State), Pid == P#okey_player.pid.
get_current(State) -> queue:get(State#state.players).
get_next(State) -> {_, Q2} = queue:out(State#state.players), queue:get(Q2).
set_next(State) ->
    { {value, L}, Q } = queue:out(State#state.players),
    NewState = State#state{players = queue:in(L, Q)},
    NextPlayer = queue:get(NewState#state.players),
    {NextPlayer, NewState}.

update_current(Player, State) ->
    {_, Q2} = queue:out(State#state.players),
    Q3 = queue:in_r(Player, Q2),
    State#state{players = Q3}.

update_next(Player, State) ->
    {{value, Current}, Q1} = queue:out(State#state.players),
    {_Outdated, Q2} = queue:out(Q1),
    Q3 = queue:in_r(Player, Q2),
    Q4 = queue:in_r(Current, Q3),
    State#state{players = Q4}.

update_player(Old, New, State) ->
    P = queue:to_list(State#state.players),
    P1 = lists:map(fun (X) when X == Old -> New; (X) -> X end, P),
    State#state{players = queue:from_list(P1)}.

is_same_hand(Revealed0, Stored) ->
    Rev1 = lists:filter(fun(#'OkeyPiece'{}) -> true;
                           (_) -> false
                        end, lists:flatten(Revealed0)),
    A = lists:sort(Stored),
    B = lists:sort(Rev1),
    A == B.

is_winning_hand([H|_] = ReceivedHand, Gosterge) when is_list(H)->
    L = lists:foldl(fun(X, Acc) ->
                            [X, null| Acc]
                    end, [], ReceivedHand),
    is_winning_hand(lists:flatten(L), Gosterge);
is_winning_hand(ReceivedHand, Gosterge) ->
    Okey = get_okey(Gosterge),
    Rev2 = lists:map(fun(J) when J == Okey -> okey;
                        (Other) -> Other
                     end, ReceivedHand),
    Rev3 = lists:map(fun(?FALSE_OKEY) -> Okey;
                        (Other) -> Other
                     end, Rev2),
    Sets = split_by_delimiter(null, Rev3),
    ProperHand = lists:all(fun(S) ->
                                   is_set(S) orelse is_run(S)
                           end, Sets),
    PowerHand = lists:all(fun(S) ->
                                  is_pair(S)
                          end, Sets),
    ProperHand orelse PowerHand.

%%FIX: move to utils
split_by_delimiter(Delimiter, Hand) -> split_by_delimiter(Delimiter, Hand, []).
split_by_delimiter(_, [], Acc) -> lists:reverse(Acc);
split_by_delimiter(Delimiter, [Delimiter | Hand], Acc) -> split_by_delimiter(Delimiter, Hand, Acc);
split_by_delimiter(Delimiter, Hand, Acc) ->
    {L, Rest} = lists:splitwith(fun(X) when X == Delimiter ->
                                        false;
                                   (_) ->
                                        true
                                end, Hand),
    split_by_delimiter(Delimiter, Rest, [L | Acc]).

normalize_hand(Del, [H | T]) when is_list(H) -> 
    A = lists:foldl(fun(X, Acc) -> [X, Del | Acc] end, [H], T),
    lists:flatten(lists:reverse(A));
normalize_hand(_, List) ->
    List.

is_set(Set) ->
    length(Set) > 2 andalso is_set0(Set).
is_set0(Set) ->
    Normals = [ X || X <- Set, X /= okey ],
    Val = (hd(Normals))#'OkeyPiece'.value,
    NL = length(Normals),
    SameValue = lists:all(fun(#'OkeyPiece'{value = V}) when V == Val -> true;
                             (_) -> false
                          end, Normals),
    UniqueColors = length(lists:usort([X#'OkeyPiece'.color || X <- Normals])),
    case {NL, SameValue, UniqueColors} of
        {Ok, true, Ok} -> true;
        {_, _, _} -> false
    end.

is_run(Set) ->
    (length(Set) > 2) andalso is_run0(Set).
is_run0(Set) ->
    {_Okeys, Rest} = lists:partition(fun(X) when X == okey -> true;
                                        (_) -> false
                                     end, Set),
    Color = (hd(Rest))#'OkeyPiece'.color,
    SameColor = lists:all(fun(#'OkeyPiece'{color = C}) when C == Color -> true;
                             (_) -> false
                          end, Rest),
    Simple = lists:map(fun
                       (#'OkeyPiece'{value = Val}) -> Val;
                       (okey) -> okey
                      end, Set),
    LofL = kakamath:all_substitutions(Simple, 1, [1, 14]),
    RR = lists:any(fun(L) ->
                           run_run_f(L) orelse run_run_b(L)
                   end, LofL),
    SameColor andalso RR.

run_run_f(L) -> run_run_f(hd(L), tl(L)).
run_run_f(_Last, []) -> true;
run_run_f(okey, [H | T]) -> run_run_f(H, T);
run_run_f(14, [okey | _T]) -> false;
run_run_f(Last, [okey | T]) -> run_run_f(Last+1, T);
run_run_f(Last, [H | T]) when H - Last == 1 -> run_run_f(H, T);
run_run_f(_Last, [_H | _T]) -> false.

run_run_b(L) -> run_run_b(hd(L), tl(L)).
run_run_b(_Last, []) -> true;
run_run_b(okey, [H | T]) -> run_run_b(H, T);
run_run_b(1, [okey | _T]) -> false;
run_run_b(Last, [okey | T]) -> run_run_b(Last-1, T);
run_run_b(Last, [H | T]) when H - Last == -1 -> run_run_b(H, T);
run_run_b(_Last, [_H | _T]) -> false.

is_pair([_A, okey]) -> true;
is_pair([okey, _B]) -> true;
is_pair([A, A]) -> true;
is_pair(_) -> false.

get_okey(State = #state{}) -> get_okey(State#state.gosterge);
get_okey(Gosterge = #'OkeyPiece'{}) ->
    #'OkeyPiece'{color = Color, value = GValue} = Gosterge,
    Value = case GValue of ?HS - 1 -> 1; _ -> GValue + 1 end,
    #'OkeyPiece'{color = Color, value = Value}.

start_game(State) ->
    Relay = State#state.relay,
    Pile0 = State#state.pile0,
    Mode = State#state.scoring_mode,
    Gosterge = State#state.gosterge,
    Players = queue:to_list(State#state.players),
    SS = State#state.set_state,
    GameSubmode = game_okey:get_scoring_mode(Mode, Gosterge),
    lists:map(fun(#okey_player{hand = Hand, pid = Pid, player_id = UId}) ->
                      game_okey_scoring:add_event(State#state.stats, UId, started),
                      PrevChanak = game_okey_scoring:get_chanak_points(State#state.stats),
                      NewChanak = case PrevChanak of
                           0 -> game_okey_scoring:set_chanak_points(State#state.stats, 
                                        game_okey_scoring:new_chanak(GameSubmode)),
                                game_okey_scoring:get_chanak_points(State#state.stats);
                           _ -> game_okey_scoring:get_chanak_points(State#state.stats)
                      end,
                      M = #okey_game_started{tiles = Hand,
                                             gosterge = State#state.gosterge,
                                             pile_height = length(Pile0),
                                             current_round = SS#'OkeySetState'.round_cur,
                                             current_set = SS#'OkeySetState'.set_cur,
                                             game_type = State#state.game_type,
                                             game_speed = State#state.game_speed,
                                             game_submode = GameSubmode,
                                             chanak_points = NewChanak
                                            },
                      message_session(Relay, Pid, M)
              end, Players),
    #okey_player{player_id = PlayerID} = get_current(State),
    publish_ge(Relay, #okey_next_turn{player = PlayerID}).

publish_ge(Relay, MsgList) when is_list(MsgList) ->
    lists:map(fun(Msg) ->
                      publish_ge(Relay, Msg)
              end, MsgList);
publish_ge({RMod, RPid}, Msg) when is_tuple(Msg) ->
    Event = api_utils:name(Msg),
    Args = api_utils:members(Msg),
    RMod:publish(RPid, #game_event{event = Event, args = Args}).

message_session(Relay, Pid, MsgList) when is_list(MsgList) ->
    lists:map(fun(Msg) ->
                      message_session(Relay, Pid, Msg)
              end, MsgList);
message_session({RMod, RPid}, Pid, Msg) when is_tuple(Msg) ->
    Event = api_utils:name(Msg),
    Args = api_utils:members(Msg),
    RMod:to_session(RPid, Pid, #game_event{event = Event, args = Args}).

mnow() ->
    {A, B, C} = erlang:now(),
    erlang:trunc(((((A * 1000000) + B) * 1000000) + C) / 1000).

timeout_at(Value) ->
    mnow() + Value.

calc_timeout(state_created, _State) ->
    infinity;
calc_timeout(state_finished, _State) ->
    infinity;
calc_timeout(state_dead, _State) ->
    infinity;
calc_timeout(_, State) ->
    calc_timeout(State).

-spec calc_timeout(#state{}) -> integer().
calc_timeout(#state{time_mark = undefined}) ->
    infinity;
calc_timeout(#state{time_mark = TM}) ->
    Val = TM - mnow(),
    max(0, Val).

publish_okey_game_info(State) ->
    Relay = State#state.relay,
    OGI = create_okey_game_info(State),
    publish_ge(Relay, OGI).

create_game_info(State) -> create_okey_game_info(State).

create_okey_game_info(#state{set_state = SS,
                             table_name = TName,
                             mul_factor = MulFactor,
                             slang_flag = SlangFlag,
                             observer_flag = ObserverFlag
                            } = State) ->
    PlayersList = queue:to_list(State#state.players),
    PInfos = lists:map(fun(#okey_player{player_info = PI}) ->
                               PI
                       end, PlayersList),
    ?INFO("game mode is ~p", [State#state.scoring_mode]),
    #okey_game_info{table_name = TName,
                    players = PInfos,
                    timeouts = #'OkeyTimeouts'{
                      speed = proplists:get_value(speed, State#state.settings),
                      turn_timeout = State#state.turn_timeout,
                      challenge_timeout = State#state.challenge_timeout,
                      ready_timeout = State#state.ready_timeout,
                      rematch_timeout = ?REMATCH_TIMEOUT},
                    game_type = State#state.scoring_mode,
                    finish_with_gosterge = proplists:get_value(gosterge_finish, State#state.settings),
                    rounds = case SS#'OkeySetState'.round_max of
                                 infinity ->
                                     -1;
                                 RM ->
                                     RM
                             end,
                    sets = SS#'OkeySetState'.set_max,
                    set_no = SS#'OkeySetState'.set_cur,
                    mul_factor = MulFactor,
                    slang_flag = SlangFlag,
                    observer_flag = ObserverFlag
                   }.


get_scoring_mode(standard, _) ->
    standard;
get_scoring_mode(color, #'OkeyPiece'{value = Val, color = Color}) when (Val rem 2) == 0 ->
    get_scoring_mode(color, even, b2c(Color));
get_scoring_mode(color, #'OkeyPiece'{value = Val, color = Color}) when (Val rem 2) == 1 ->
    get_scoring_mode(color, odd, b2c(Color));
get_scoring_mode(evenodd, #'OkeyPiece'{value = Val}) when (Val rem 2) == 0 ->
    even;
get_scoring_mode(evenodd, #'OkeyPiece'{value = Val}) when (Val rem 2) == 1 ->
    odd;

get_scoring_mode(countdown, _) ->
    countdown.

get_scoring_mode(color, odd, C) when C == yellow; C == blue ->
    ybodd;
get_scoring_mode(color, even, C) when C == yellow; C == blue ->
    ybeven;
get_scoring_mode(color, odd, C) when C == black; C == red ->
    rbodd;
get_scoring_mode(color, even, C) when C == black; C == red ->
    rbeven.

%get_scoring_mode(color, _, _) ->
%    mixed.

b2c(1) ->
    red;
b2c(2) ->
    blue;
b2c(3) ->
    yellow;
b2c(4) ->
    black.


pid_to_uid(Pid, #state{players = Players}) ->
    #okey_player{pid = Pid,
                 player_id = UId} = lists:keyfind(Pid, #okey_player.pid, queue:to_list(Players)),
    UId.

-spec rotate_until_face(tag(), list({tag(), any()})) -> list({tag(), any()}).
rotate_until_face(FaceTag, Tuples) ->
    T = lists:keyfind(FaceTag, 1, Tuples),
    {is_member, {FaceTag, _}} = {is_member, T},
    {Sublist, InvRest} = rotate_find(FaceTag, Tuples, []),
    Sublist ++ lists:reverse(InvRest).

rotate_find(_FaceTag, [], _Tail) ->
    false;
rotate_find(FaceTag, [{Tag, _} | _] = List, Tail) when Tag == FaceTag ->
    {List, Tail};
rotate_find(FaceTag, [H | T], Tail) ->
    rotate_find(FaceTag, T, [H | Tail]).

invariables(State) ->
    Players = queue:to_list(State#state.players),
    {no_of_players, 4} = {no_of_players, length(Players)},
    PlayersTashes = [ [X#okey_player.hand ++ X#okey_player.pile] || X <- Players ],
    Tashes0 = lists:flatten([State#state.gosterge] ++ State#state.pile0 ++ PlayersTashes),
    {A, LL1, L2} = generate_hand(),
    Ideal = lists:sort([A] ++ lists:flatten(LL1) ++ L2),
    Tashes = lists:sort(Tashes0),
    % ?INFO("~nIdeal -- Tashes = ~p~nTashes -- Ideal = ~p~n", [Ideal -- Tashes, Tashes -- Ideal]),
    true = Ideal == Tashes.

stateful_test_() ->
    {foreach,
     fun() -> tests:setup() end,
     fun(State) -> tests:cleanup(State) end,
     [
      {timeout, 100, fun hand_out_t/0},
      fun() -> ok end
     ]
    }.

rotate_until_face_test() ->
    N = 10,
    A0 = [{A, a} || A <- lists:seq(1, N)],
    NewHead = crypto:rand_uniform(1, N+1),
    A = kakamath:variate(A0),
    [{OriginalHead, _} | _] = A,
    B = rotate_until_face(NewHead, A),
    [{NewHead, a} | _] = B,
    A = rotate_until_face(OriginalHead, B).

hand_out_t() ->
    {J, [A, B, C | D], Pile} = hand_out_pieces(),
    LL = lists:flatten([J, A, B, C, D, Pile]),
    ((?HS-1)*4*2+2) = length(LL).

split_by_delimiter_test() ->
    [[a,b,c], [d,e,f], [g,h,k]] = split_by_delimiter(null, [a,b,c,null,d,e,f,null,g,h,k,null]),
    [[a,b,c], [d,e,f], [g,h,k]] = split_by_delimiter(null, [null,a,b,c,null,null,d,e,f,null,g,h,k,null]),
    [[a,b], [c,d], [e,f], [g,h], [i,j], [k,l]] = split_by_delimiter(null, [a,b,null,c,d,null,e,f,null,g,h,null,i,j,null,k,l]),
    [[a,b,c, d,e,f], [g]] = split_by_delimiter(null, [null,a,b,c,d,e,f,null,g,null]),
    ok.

draw_joker_test() ->
    L0 = generate_pieces(),
    {#'OkeyPiece'{value = _Value}, L1} = kakamath:draw_random(L0),
    length(L0) == length(L1) + 1.

is_run_test() ->
    H = [
         {true, [{'OkeyPiece',1,1},
                 {'OkeyPiece',1,2},
                 {'OkeyPiece',1,3},
                 {'OkeyPiece',1,4},
                 {'OkeyPiece',1,5},
                 {'OkeyPiece',1,6},
                 {'OkeyPiece',1,7},
                 {'OkeyPiece',1,8},
                 {'OkeyPiece',1,9},
                 {'OkeyPiece',1,10},
                 {'OkeyPiece',1,11},
                 {'OkeyPiece',1,12},
                 {'OkeyPiece',1,13},
                 {'OkeyPiece',1,1}]},
         {true, [#'OkeyPiece'{color = 1, value = 2},
                 #'OkeyPiece'{color = 1, value = 3},
                 #'OkeyPiece'{color = 1, value = 4},
                 #'OkeyPiece'{color = 1, value = 5}
                ]},
         {true, [okey,
                 #'OkeyPiece'{color = 1, value = 3},
                 #'OkeyPiece'{color = 1, value = 4},
                 #'OkeyPiece'{color = 1, value = 5}
                ]},
         {true, [#'OkeyPiece'{color = 1, value = 7},
                 #'OkeyPiece'{color = 1, value = 6},
                 #'OkeyPiece'{color = 1, value = 5},
                 #'OkeyPiece'{color = 1, value = 4}
                ]},
         {true, [#'OkeyPiece'{color = 1, value = 7},
                 okey,
                 okey,
                 #'OkeyPiece'{color = 1, value = 4}
                ]},
         {true, [#'OkeyPiece'{color = 1, value = 7},
                 #'OkeyPiece'{color = 1, value = 6},
                 #'OkeyPiece'{color = 1, value = 5},
                 okey
                ]},
         {true, [#'OkeyPiece'{color = 1, value = 7},
                 okey,
                 #'OkeyPiece'{color = 1, value = 5},
                 #'OkeyPiece'{color = 1, value = 4}
                ]},
         {false, [#'OkeyPiece'{color = 1, value = 2},
                  #'OkeyPiece'{color = 1, value = 4},
                  #'OkeyPiece'{color = 1, value = 3},
                  #'OkeyPiece'{color = 1, value = 5}
                 ]},
         {false, [#'OkeyPiece'{color = 1, value = 2},
                  #'OkeyPiece'{color = 1, value = 3},
                  #'OkeyPiece'{color = 2, value = 4},
                  #'OkeyPiece'{color = 1, value = 5}
                 ]},
         {false, [#'OkeyPiece'{color = 2, value = 2},
                  #'OkeyPiece'{color = 1, value = 3},
                  #'OkeyPiece'{color = 1, value = 4},
                  #'OkeyPiece'{color = 1, value = 5}
                 ]},
         {false, [#'OkeyPiece'{color = 1, value = 4},
                  #'OkeyPiece'{color = 1, value = 2},
                  #'OkeyPiece'{color = 1, value = 3},
                  #'OkeyPiece'{color = 1, value = 5}
                 ]},
         {true , [#'OkeyPiece'{color = 1, value = 12},
                  #'OkeyPiece'{color = 1, value = 13},
                  #'OkeyPiece'{color = 1, value = 1} %% 14
                 ]},
         {true , [#'OkeyPiece'{color = 1, value = 1}, %% 14
                  #'OkeyPiece'{color = 1, value = 13},
                  #'OkeyPiece'{color = 1, value = 12}
                 ]},
         {false, [#'OkeyPiece'{color = 1, value = 12},
                  #'OkeyPiece'{color = 1, value = 1}
                 ]},
         {true, [#'OkeyPiece'{color = 1, value = 1},
                 #'OkeyPiece'{color = 1, value = 2},
                 #'OkeyPiece'{color = 1, value = 3}
                ]},
         {false, [#'OkeyPiece'{color = 1, value = 13},
                  #'OkeyPiece'{color = 1, value = 1},
                  #'OkeyPiece'{color = 1, value = 2}
                 ]},
         {false, [#'OkeyPiece'{color = 1, value = 2},
                  #'OkeyPiece'{color = 1, value = 1},
                  #'OkeyPiece'{color = 1, value = 13}
                 ]},
         {false, [#'OkeyPiece'{color = 1, value = 2},
                  okey,                               %% both 1 and 14
                  #'OkeyPiece'{color = 1, value = 13}
                 ]},
         {false, [#'OkeyPiece'{color = 1, value = 13},
                  okey,                               %% both 1 and 14
                  #'OkeyPiece'{color = 1, value = 2}
                 ]},
         {true, [#'OkeyPiece'{color = 1, value = 13},
                 okey,        %% 12
                 okey         %% 11
                ]},
         {true, [okey,
                 #'OkeyPiece'{color = 1, value = 13},
                 #'OkeyPiece'{color = 1, value = 1}
                ]},
         {false, [#'OkeyPiece'{color = 1, value = 13},
                  okey,
                  #'OkeyPiece'{color = 1, value = 1}
                 ]},
         {false, [#'OkeyPiece'{color = 1, value = 2},
                  #'OkeyPiece'{color = 1, value = 1},
                  okey
                 ]},
         {true, [#'OkeyPiece'{color = 1, value = 1},
                 okey,
                 okey
                ]},
         {true, [okey,  %% 11
                 okey,  %% 12
                 #'OkeyPiece'{color = 1, value = 13}
                ]},
         {true, [okey,  %% 3
                 okey,  %% 2
                 #'OkeyPiece'{color = 1, value = 1}
                ]},
         {true, [#'OkeyPiece'{color = 1, value = 1},
                 okey,
                 #'OkeyPiece'{color = 1, value = 3}
                ]},
         {false, [okey,
                  #'OkeyPiece'{color = 1, value = 1},
                  #'OkeyPiece'{color = 1, value = 3}
                 ]}
        ],
    HS = lists:zip(lists:seq(1, length(H)), H),
    lists:map(fun({Num, {Res, Run}}) ->
                      Z = {Num, is_run(Run), Run},
                      {Num, Res, Run} = Z
              end, HS).

is_set_test() ->
    true = is_set([#'OkeyPiece'{color = 1, value = 1},
                   #'OkeyPiece'{color = 2, value = 1},
                   #'OkeyPiece'{color = 3, value = 1}
                  ]),
    true = is_set([#'OkeyPiece'{color = 1, value = 1},
                   #'OkeyPiece'{color = 2, value = 1},
                   #'OkeyPiece'{color = 3, value = 1},
                   #'OkeyPiece'{color = 4, value = 1}
                  ]),
    false = is_set([#'OkeyPiece'{color = 1, value = 1},
                    #'OkeyPiece'{color = 2, value = 1},
                    #'OkeyPiece'{color = 2, value = 1}
                   ]),
    false = is_set([#'OkeyPiece'{color = 1, value = 1},
                    #'OkeyPiece'{color = 2, value = 2},
                    #'OkeyPiece'{color = 3, value = 1}
                   ]),
    false = is_set([#'OkeyPiece'{color = 1, value = 1},
                    #'OkeyPiece'{color = 2, value = 1}
                   ]),

    true = is_set([#'OkeyPiece'{color = 1, value = 1},
                   #'OkeyPiece'{color = 2, value = 1},
                   okey
                  ]),
    true = is_set([#'OkeyPiece'{color = 1, value = 1},
                   okey,
                   okey
                  ]),
    false = is_set([#'OkeyPiece'{color = 2, value = 1},
                    #'OkeyPiece'{color = 2, value = 1},
                    okey
                   ]),
    ok.

is_winning_hand_test() ->
    H = [
         {false, [[null,null,
                   {'OkeyPiece',1,2},
                   {'OkeyPiece',2,12},
                   {'OkeyPiece',2,13},
                   {'OkeyPiece',1,14},
                   null,null,null,
                   {'OkeyPiece',4,11},
                   {'OkeyPiece',4,12},
                   {'OkeyPiece',4,13},
                   {'OkeyPiece',3,14},
                   null],
                  [{'OkeyPiece',1,12},
                   {'OkeyPiece',1,13},
                   {'OkeyPiece',1,1},
                   null,null,null,
                   {'OkeyPiece',3,12},
                   {'OkeyPiece',3,13},
                   {'OkeyPiece',2,14},
                   null,null,null,null,null]],
          {'OkeyPiece',1,1}},
         {true, [[{'OkeyPiece',1,1},
                  {'OkeyPiece',1,2},
                  {'OkeyPiece',1,3},
                  {'OkeyPiece',1,4},
                  {'OkeyPiece',1,5},
                  {'OkeyPiece',1,6},
                  {'OkeyPiece',1,7},
                  {'OkeyPiece',1,8},
                  {'OkeyPiece',1,9},
                  {'OkeyPiece',1,10},
                  {'OkeyPiece',1,11},
                  {'OkeyPiece',1,12},
                  {'OkeyPiece',1,13},
                  {'OkeyPiece',1,1}],
                 [null,null,null,null,null,null,
                  null,null,null,null,null,null,
                  null,null]], #'OkeyPiece'{color = 2, value = 11}},
         {true, [[#'OkeyPiece'{color = 1, value = 2}, %% 1
                  #'OkeyPiece'{color = 1, value = 3},
                  #'OkeyPiece'{color = 1, value = 4},
                  #'OkeyPiece'{color = 1, value = 5},
                  null,
                  #'OkeyPiece'{color = 1, value = 5},
                  #'OkeyPiece'{color = 2, value = 5},
                  #'OkeyPiece'{color = 3, value = 5},
                  #'OkeyPiece'{color = 4, value = 5},
                  null],
                 [#'OkeyPiece'{color = 2, value = 1},
                  #'OkeyPiece'{color = 2, value = 13},
                  #'OkeyPiece'{color = 2, value = 12},
                  null,
                  #'OkeyPiece'{color = 3, value = 7},
                  #'OkeyPiece'{color = 2, value = 7},
                  #'OkeyPiece'{color = 2, value = 12}
                 ]], #'OkeyPiece'{color = 2, value = 11}},
         {false, [[#'OkeyPiece'{color = 1, value = 2},     %% 2
                   #'OkeyPiece'{color = 1, value = 3},
                   #'OkeyPiece'{color = 1, value = 4},
                   #'OkeyPiece'{color = 1, value = 5},
                   #'OkeyPiece'{color = 1, value = 5},
                   #'OkeyPiece'{color = 2, value = 5},
                   #'OkeyPiece'{color = 3, value = 5},
                   #'OkeyPiece'{color = 4, value = 5}],
                  [null,
                   #'OkeyPiece'{color = 2, value = 1},
                   #'OkeyPiece'{color = 2, value = 13},
                   #'OkeyPiece'{color = 2, value = 12},
                   null,
                   #'OkeyPiece'{color = 3, value = 7},
                   #'OkeyPiece'{color = 2, value = 7},
                   #'OkeyPiece'{color = 2, value = 12}]
                 ], #'OkeyPiece'{color = 2, value = 12}},
         {false, [#'OkeyPiece'{color = 1, value = 2},      %% 3
                  #'OkeyPiece'{color = 1, value = 3},
                  #'OkeyPiece'{color = 1, value = 4},
                  #'OkeyPiece'{color = 1, value = 6},
                  null,
                  #'OkeyPiece'{color = 1, value = 5},
                  #'OkeyPiece'{color = 2, value = 5},
                  #'OkeyPiece'{color = 3, value = 5},
                  #'OkeyPiece'{color = 4, value = 5},
                  null,
                  #'OkeyPiece'{color = 2, value = 1},
                  #'OkeyPiece'{color = 2, value = 13},
                  #'OkeyPiece'{color = 2, value = 12},
                  null,
                  #'OkeyPiece'{color = 3, value = 7},
                  #'OkeyPiece'{color = 2, value = 7},
                  #'OkeyPiece'{color = 2, value = 12}
                 ], #'OkeyPiece'{color = 2, value = 12}},
         {true, [#'OkeyPiece'{color = 1, value = 2},  %% 4
                 #'OkeyPiece'{color = 1, value = 3},
                 #'OkeyPiece'{color = 1, value = 4},
                 #'OkeyPiece'{color = 1, value = 5},
                 null,
                 #'OkeyPiece'{color = 1, value = 12},
                 ?FALSE_OKEY,
                 #'OkeyPiece'{color = 4, value = 12},
                 #'OkeyPiece'{color = 3, value = 12},
                 null,
                 #'OkeyPiece'{color = 2, value = 1},
                 #'OkeyPiece'{color = 2, value = 13},
                 ?FALSE_OKEY,
                 null,
                 #'OkeyPiece'{color = 3, value = 7},
                 #'OkeyPiece'{color = 2, value = 7},
                 #'OkeyPiece'{color = 2, value = 12}
                ], #'OkeyPiece'{color = 2, value = 11}},
         {true, [#'OkeyPiece'{color = 1, value = 2},  %% 5
                 #'OkeyPiece'{color = 1, value = 2},
                 null,
                 #'OkeyPiece'{color = 1, value = 5},
                 #'OkeyPiece'{color = 1, value = 5},
                 null,
                 #'OkeyPiece'{color = 1, value = 11},
                 #'OkeyPiece'{color = 1, value = 11},
                 null,
                 ?FALSE_OKEY,
                 ?FALSE_OKEY,
                 null,
                 #'OkeyPiece'{color = 2, value = 1},
                 #'OkeyPiece'{color = 2, value = 1},
                 null,
                 #'OkeyPiece'{color = 3, value = 7},
                 #'OkeyPiece'{color = 3, value = 7},
                 null,
                 #'OkeyPiece'{color = 1, value = 12},
                 #'OkeyPiece'{color = 3, value = 8}
                ], #'OkeyPiece'{color = 1, value = 11}},
         {true, [                                    %% 6
                                                     null,
                                                     #'OkeyPiece'{color = 3, value = 10},
                                                     #'OkeyPiece'{color = 2, value = 10},
                                                     #'OkeyPiece'{color = 1, value = 10},
                                                     null,
                                                     #'OkeyPiece'{color = 3, value = 5},
                                                     #'OkeyPiece'{color = 3, value = 6},
                                                     #'OkeyPiece'{color = 3, value = 7},
                                                     #'OkeyPiece'{color = 3, value = 8},
                                                     null,
                                                     #'OkeyPiece'{color = 4, value = 12},
                                                     #'OkeyPiece'{color = 4, value = 13},
                                                     #'OkeyPiece'{color = 4, value = 1},
                                                     null,
                                                     #'OkeyPiece'{color = 3, value = 10},
                                                     #'OkeyPiece'{color = 3, value = 11},
                                                     #'OkeyPiece'{color = 3, value = 12},
                                                     ?FALSE_OKEY,
                                                     null
                                                    ], #'OkeyPiece'{color = 3, value = 12}}
        ],
    HS = lists:zip(lists:seq(1, length(H)), H),
    lists:map(fun({Num, {Res, Hand, Gosterge}}) ->
                      Z = {Num, is_winning_hand(Hand, Gosterge), Hand, Gosterge},
                      {Num, Res, Hand, Gosterge} = Z
              end, HS).

same_hand_test() ->
    Hand = [{'OkeyPiece',2,9},
            {'OkeyPiece',1,0},
            {'OkeyPiece',1,8},
            {'OkeyPiece',1,6},
            {'OkeyPiece',2,3},
            {'OkeyPiece',2,12},
            {'OkeyPiece',2,2},
            {'OkeyPiece',4,11},
            {'OkeyPiece',4,5},
            {'OkeyPiece',3,1},
            {'OkeyPiece',1,12},
            {'OkeyPiece',2,1},
            {'OkeyPiece',2,11},
            {'OkeyPiece',3,9}],
    TashPlaces =  [{'OkeyPiece',1,0},
                   {'OkeyPiece',2,1},
                   {'OkeyPiece',3,1},
                   {'OkeyPiece',2,2},
                   {'OkeyPiece',2,3},
                   {'OkeyPiece',4,5},
                   null,
                   {'OkeyPiece',1,6},
                   {'OkeyPiece',1,8},
                   {'OkeyPiece',2,9},
                   {'OkeyPiece',3,9},
                   {'OkeyPiece',2,11},
                   {'OkeyPiece',4,11},
                   {'OkeyPiece',1,12},
                   {'OkeyPiece',2,12},
                   null,null,null,null,null,null,null],
    true = is_same_hand(TashPlaces, Hand).


statename_to_api_string(state_wait) ->
    do_okey_ready;
statename_to_api_string(state_take) ->
    do_okey_take;
statename_to_api_string(state_discard) ->
    do_okey_discard;
statename_to_api_string(state_challenge) ->
    do_okey_challenge;
statename_to_api_string(state_finished) ->
    game_finished;
statename_to_api_string(state_created) ->
    game_initializing.


statename_to_timeout(state_wait, #state{ready_timeout = Res}) -> Res;
statename_to_timeout(state_take, #state{turn_timeout = Res}) -> Res;
statename_to_timeout(state_discard, #state{turn_timeout = Res}) -> Res;
statename_to_timeout(state_challenge, #state{challenge_timeout = Res}) -> Res;
statename_to_timeout(state_finished, _) -> infinity;
statename_to_timeout(state_created, _) -> infinity;
statename_to_timeout(state_dead, _) -> infinity.

