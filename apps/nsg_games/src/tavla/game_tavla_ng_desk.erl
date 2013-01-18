%%% -------------------------------------------------------------------
%%% Author  : Sergei Polkovnikov <serge.polkovnikov@gmail.com>
%%% Description : The tavla game rules implementation
%%%
%%% Created : Jan 14, 2013
%%% -------------------------------------------------------------------
-module(game_tavla_ng_desk).
-behaviour(gen_fsm).

%% Board model schema:
%%  13 14 15 16 17 18    19 20 21 22 23 24   BO
%%                    BB
%%
%%                    WB
%%  12 11 10 09 08 07    06 05 04 03 02 01   WO

%% Parameters:
%%  home_hit_and_run - specifies is the "hit and run" allowed in the home.
%%           Type : enabled | disabled
%%  bearoff_waste_moves - specifies are "waste" moves allowed in the bear-off phase.
%%           Waste move means a normal move when a bear-off can be done.
%%           Type: enabled | disabled
%%  first_move - a color of player who should make first move.
%%           Type: black | white
%% Options:
%%  board - an initial board definition. Describes how the checkers are placed on the
%%           board. All table position must be specified in the definition. The total
%%           number of checkers must be exectly 15 per each color. The first position
%%           in the white home is 1 and the first position in the black home is 24. White
%%           checkers goes counter-clockwise, and the black ones clockwise.
%%          If the option is not defined then the regular board definition will be applyed.
%%           Type: [{Position, State}]
%%                 Position = 1-24, wb, bb, wo, bo
%%                 State = empty | {Color, CheckersNumber}
%%                   Color = black | white
%%                   CheckersNumber = 1-15

%%             Players actions       ||            Errors
%%  {move, Dice, Moves}              || not_your_order, to_many_moves,
%%      Dice = {1-6, 1-6}            || more_moves_needed, {invalid_move, Move, RestMoves},
%%      Moves = [{From, To}]         || {position_occupied, Move, RestMoves},
%%        From = wb, bb, 1-24        || {waste_move_disabled, Move, RestMoves},
%%        To = wo, bo, 1-24          || {hit_and_run_disabled, Move, RestMoves}
%%                                   || {no_checker, Move, RestMoves}

%% Outgoing events:
%%  {next_player, Color}
%%      Color = black | white
%%  {moves, Color, Moves}
%%      Moves = [{Type, From, To}]
%%        Type = move | hit
%%  {win, Color, Condition}
%%      Condition = normal | mars

%% External exports
-export([
         start/1,
         stop/1,
         player_action/3
        ]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-define(CHECKERS_NUM, 15).

-define(WHITE_OUT, wo).
-define(BLACK_OUT, bo).
-define(WHITE_BAR, wb).
-define(BLACK_BAR, bb).
-define(WHITE, white).
-define(BLACK, black).

-define(STATE_PLAYING, state_playing).
-define(STATE_FINISHED, state_finished).

-record(state,
        {home_hit_and_run_enabled    :: boolean(),
         bearoff_waste_moves_enabled :: boolean(),
         first_move                  :: black | white,
         board                       :: dict(),
         current                     :: black | white,
         finish_conditions           :: undefined | {black | white, normal | mars}
        }).

%% ====================================================================
%% External functions
%% ====================================================================
start(Params) -> gen_fsm:start(?MODULE, Params, []).

player_action(Desk, Color, Action) ->
    gen_fsm:sync_send_all_state_event(Desk, {player_action, Color, Action}).

stop(Desk) -> gen_fsm:send_all_state_event(Desk, stop).


%% ====================================================================
%% Server functions
%% ====================================================================
% --------------------------------------------------------------------
init(Params) ->
    HomeHitAndRun = get_param(home_hit_and_run, Params),
    BearoffWasteMoves = get_param(bearoff_waste_moves, Params),
    FirstMove = get_param(first_move, Params),
    BoardSpec = get_option(board, Params, undefined),
    validate_params(HomeHitAndRun, BearoffWasteMoves, FirstMove, BoardSpec),
    Board = if BoardSpec == undefined -> init_board(initial_board());
               true -> init_board(BoardSpec)
            end,
    {ok, ?STATE_PLAYING,
     #state{home_hit_and_run_enabled = HomeHitAndRun == enabled,
            bearoff_waste_moves_enabled = BearoffWasteMoves == enabled,
            first_move = FirstMove,
            current = FirstMove,
            board = Board
           }}.

%% --------------------------------------------------------------------
handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

%% --------------------------------------------------------------------
handle_sync_event({player_action, SeatNum, Action}, _From, StateName, StateData) ->
    case handle_player_action(SeatNum, Action, StateName, StateData) of
        {ok, Events, NewStateName, NewStateData} ->
            {reply, {ok, lists:reverse(Events)}, NewStateName, NewStateData};
        {error, Reason} ->
            {reply, {error, Reason}, StateName, StateData}
    end;

handle_sync_event(_Event, _From, StateName, StateData) ->
    Reply = ok,
    {reply, Reply, StateName, StateData}.

%% --------------------------------------------------------------------
handle_info(_Info, StateName, StateData) ->
    {next_state, StateName, StateData}.

%% --------------------------------------------------------------------
terminate(_Reason, _StateName, _StatData) ->
    ok.

%% --------------------------------------------------------------------
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.


%% --------------------------------------------------------------------

%% @spec handle_player_action(Color, Action, StateName, StateData) ->
%%          {ok, Events, NextStateName, NextStateData}          |
%%          {error, Reason}
%% @end

handle_player_action(PlayerId, {move, Dice, Moves}, ?STATE_PLAYING = StateName,
                     #state{current = Current} = StateData) ->
    if PlayerId == Current ->
           process_move(PlayerId, Dice, Moves, StateName, StateData);
       true ->
           {error, not_your_order}
    end;

handle_player_action(_PlayerId, _Action, _StateName, _StateData) ->
    {error, invalid_action}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

process_move(PlayerId, {Die1, Die2}, Moves, _StateName,
             #state{board = Board, bearoff_waste_moves_enabled = BearoffWasteMovesEnabled,
                    home_hit_and_run_enabled = HomeHitAndRunEnabled} = StateData) ->
    Pips = if Die1 == Die2 -> [Die1, Die1, Die1, Die1];
              true -> [Die1, Die2]
           end,
    case apply_moves(PlayerId, Pips, Moves, Board, BearoffWasteMovesEnabled, HomeHitAndRunEnabled) of
        {ok, NewBoard, Events} ->
            case is_game_finished(PlayerId, NewBoard) of
                {yes, Condition} ->
                    Events = [{win, PlayerId, Condition}, {moves, PlayerId, Moves}],
                    {ok, Events, ?STATE_FINISHED, StateData#state{board = NewBoard,
                                                                  finish_conditions = {PlayerId, Condition}}};
                no ->
                    Opponent = opponent(PlayerId),
                    Events = [{next_player, opponent(PlayerId)}, {moves, PlayerId, Moves}],
                    {ok, Events, ?STATE_PLAYING, StateData#state{board = NewBoard,
                                                                 current = Opponent}}
            end;
        {error, Reason} ->
            {error, Reason}
    end.


%% initial_board() -> BoardList
initial_board() ->
    [{01, {?BLACK, 2}},
     {02, empty},
     {03, empty},
     {04, empty},
     {05, empty},
     {06, {?WHITE, 5}},
     {07, empty},
     {08, {?WHITE, 3}},
     {09, empty},
     {10, empty},
     {11, empty},
     {12, {?BLACK, 5}},
     {13, {?WHITE, 5}},
     {14, empty},
     {15, empty},
     {16, empty},
     {17, {?BLACK, 3}},
     {18, empty},
     {19, {?BLACK, 5}},
     {20, empty},
     {21, empty},
     {22, empty},
     {23, empty},
     {24, {?WHITE, 2}},
     {?WHITE_OUT, empty},
     {?BLACK_OUT, empty},
     {?WHITE_BAR, empty},
     {?BLACK_BAR, empty}
    ].

init_board(Spec) ->
    EmptyBoard = empty_board(),
    F = fun({Pos, Value}, Acc) ->
                dict:store(Pos, Value, Acc)
        end,
    lists:foldl(F, EmptyBoard, Spec).

empty_board() ->
    dict:from_list([{Pos, empty} || Pos <- [wb, bb, wo,bo | lists:seq(1, 24)]]).

%% get_param(Id, Params) -> Value
get_param(Id, Params) ->
    {_, Value} = lists:keyfind(Id, 1, Params),
    Value.


%% get_option(Id, Params, DefaultValue) -> Value
get_option(Id, Params, DefaultValue) ->
    case lists:keyfind(Id, 1, Params) of
        {_, Value} -> Value;
        false -> DefaultValue
    end.


%% TODO: Implement the validator
validate_params(_HomeHitAndRun, _WastePipsDuringBearoff, _FirstMove, _Desk) ->
    ok.


%% opponent(Color1) -> Color2
opponent(?WHITE) -> ?BLACK;
opponent(?BLACK) -> ?WHITE.


%% is_game_finished(Color, Board) -> {yes, normal | mars} | no
is_game_finished(Color, Board) ->
    case get_checkers(out_position(Color), Board) of
        {Color, ?CHECKERS_NUM} ->
            case get_checkers(out_position(opponent(Color)), Board) of
                empty -> {yes, mars};
                _ -> {yes, normal}
            end;
        _ -> no
    end.


%% get_checkers(Pos, Board) -> {Color, Num} | empty
get_checkers(Pos, Board) -> dict:fetch(Pos, Board).

%% move_checker(From, To, Board) -> NewBoard
move_checker(From, To, Board) ->
    {Color, FromNum} = dict:fetch(From, Board),
    NewBoard = if FromNum == 1 -> dict:store(From, empty, Board);
                  true -> dict:store(From, {Color, FromNum - 1}, Board)
               end,
    case dict:fetch(To, NewBoard) of
        empty -> dict:store(To, {Color, 1}, NewBoard);
        {Color, ToNum} -> dict:store(To, {Color, ToNum + 1})
    end.


apply_moves(_Color, Pips, Moves, _Board,
            _BearoffWastePipsEnabled, _HomeHitAndRunEnabled) when
  length(Moves) > length(Pips) ->
    {error, to_many_moves};

apply_moves(Color, Pips, Moves, Board,
            BearoffWasteMoveEnabled, HomeHitAndRunEnabled) ->
    apply_moves2(Color, Pips, Moves, Board, [],
                 BearoffWasteMoveEnabled, HomeHitAndRunEnabled, []).


apply_moves2(_Color, [], [], Board, MoveEvents,
             _BearoffWasteMoveEnabled, _HomeHitAndRunEnabled, _HittedHomePositions) ->
    {ok, Board, MoveEvents};

apply_moves2(Color, Pips, [], Board, MoveEvents,
             BearoffWasteMoveEnabled, HomeHitAndRunEnabled, HittedHomePositions) ->
    case is_any_move_available(Color, Pips, Board, BearoffWasteMoveEnabled,
                               HomeHitAndRunEnabled, HittedHomePositions) of
        false -> {ok, Board, MoveEvents};
        true -> {error, more_moves_needed}
    end;

apply_moves2(Color, Pips, [{From, To} = Move | RestMoves], Board, MoveEvents,
             BearoffWasteMoveEnabled, HomeHitAndRunEnabled, HittedHomePositions) ->
    case take_pips(Color, Move, Pips, Board) of
        {ok, NewPips} ->
            case check_move_posibility(Color, From, To, Pips, Board, BearoffWasteMoveEnabled,
                                       HomeHitAndRunEnabled, HittedHomePositions) of
                ok ->
                    NewBoard = move_checker(From, To, Board),
                    NewMoveEvents = [{move, From, To} | MoveEvents],
                    apply_moves2(Color, NewPips, RestMoves, NewBoard, NewMoveEvents, BearoffWasteMoveEnabled,
                                 HomeHitAndRunEnabled, HittedHomePositions);
                hit ->
                    NewBoard1 = move_checker(To, bar_position(opponent(Color)), Board),
                    NewBoard2 = move_checker(From, To, NewBoard1),
                    {HomeMin, HomeMax} = home_range(Color),
                    NewHittedHomePositions = if To >= HomeMin andalso To =< HomeMax -> [To | HittedHomePositions];
                                               true -> HittedHomePositions
                                            end,
                    NewMoveEvents = [{hit, From, To} | MoveEvents],
                    apply_moves2(Color, NewPips, RestMoves, NewBoard2, NewMoveEvents, BearoffWasteMoveEnabled,
                                 HomeHitAndRunEnabled, NewHittedHomePositions);
                {error, occupied} -> {error, {position_occupied, Move, RestMoves}};
                {error, waste_move} -> {error, {waste_move_disabled, Move, RestMoves}};
                {error, hit_and_run} -> {error, {hit_and_run_disabled, Move, RestMoves}};
                {error, no_checker} -> {error, {no_checker, Move, RestMoves}}
            end;
        error -> {error, {invalid_move, Move, RestMoves}}
    end.


is_any_move_available(Color, Pips, Board, BearoffWasteMoveEnabled,
                      HomeHitAndRunEnabled, HitedHomePositions) ->
    Bar = bar_position(Color),
    case get_checkers(Bar, Board) of
        {Color, _} -> is_any_move_available_bar(Color, Pips, Board);
        empty -> is_any_move_available_desk(Color, Pips, Board, BearoffWasteMoveEnabled,
                                            HomeHitAndRunEnabled, HitedHomePositions)
    end.


is_any_move_available_bar(Color, Pips, Board) ->
    F = fun(P) ->
                case check_destination(Color, new_pos(Color, bar_position(Color), P), Board) of
                    ok -> true;
                    hit -> true;
                    occupied -> false
                end
        end,
    lists:any(F, Pips).


is_any_move_available_desk(Color, Pips, Board, BearoffWasteMoveEnabled,
                           HomeHitAndRunEnabled, HitedHomePositions) ->
    F = fun(Pos) ->
                F2 = fun(P) ->
                             case check_move_posibility(Color, Pos, new_pos(Color, Pos, P), Pips,
                                                        Board, BearoffWasteMoveEnabled,
                                                        HomeHitAndRunEnabled, HitedHomePositions) of
                                 ok -> true;
                                 hit -> true;
                                 {error, _} -> false
                             end
                     end,
                lists:any(F2, Pips)
        end,
    lists:any(F, route(Color)).


%% check_move_posibility/8 -> ok | hit | {error, Reason}
check_move_posibility(Color, From, To, Pips, Board, BearoffWasteMoveEnabled,
                      HomeHitAndRunEnabled, HitedHomePositions) ->
    case get_checkers(From, Board) of
        {Color, _} -> check_home_hit_and_run(Color, From, To, Pips, Board, BearoffWasteMoveEnabled,
                                             HomeHitAndRunEnabled, HitedHomePositions);
        _ -> {error, no_checker}
    end.


check_home_hit_and_run(Color, From, To, Pips, Board, BearoffWasteMoveEnabled,
                       HomeHitAndRunEnabled, HitedHomePositions) ->
    IsAllowed = if HomeHitAndRunEnabled -> true;
                   true ->
                       {_, Num} = get_checkers(From, Board),
                       if Num > 1 -> true;
                          true -> not lists:member(From, HitedHomePositions)
                       end
                end,
    if IsAllowed -> check_waste_move(Color, From, To, Pips, Board, BearoffWasteMoveEnabled,
                                     HomeHitAndRunEnabled, HitedHomePositions);
       true -> {error, hit_and_run}
    end.


check_waste_move(Color, From, To, Pips, Board, BearoffWasteMoveEnabled,
                 HomeHitAndRunEnabled, HitedHomePositions) ->
    IsAllowed = if BearoffWasteMoveEnabled -> true;
                   true ->
                       case detect_bearoff_mode(Color, Board) of
                           false -> true;
                           true ->
                               case To == out_position(Color) of
                                   true -> true;
                                   false -> true %% TODO: Check bear-off posibility
                               end
                       end
                end,
    if IsAllowed -> check_destination_pos(Color, To, Board);
       true -> {error, waste_move}
    end.


check_destination_pos(Color, To, Board) ->
    case check_destination(Color, To, Board) of
        ok -> ok;
        hit -> hit;
        occupied -> {error, occupied}
    end.


detect_bearoff_mode(Color, Board) ->
    Out = out_position(Color),
    Bar = bar_position(Color),
    {HomeMin, HomeMax} = home_range(Color),
    F = fun({Pos, {C, _}}) when C == Color, Pos == Out -> true;
           ({Pos, {C, _}}) when C == Color, Pos == Bar -> false;
           ({Pos, {C, _}}) when C == Color -> Pos >= HomeMin andalso Pos =< HomeMax;
           (_) -> true
        end,
    lists:all(F, dict:to_list(Board)).


out_position(?WHITE) -> ?WHITE_OUT;
out_position(?BLACK) -> ?BLACK_OUT.

bar_position(?WHITE) -> ?WHITE_BAR;
bar_position(?BLACK) -> ?BLACK_BAR.

home_range(?WHITE) -> {1, 6};
home_range(?BLACK) -> {19, 24}.

new_pos(?WHITE, From, Pips) ->
    if From == ?WHITE_BAR -> 25 - Pips;
       (From - Pips) > 0 -> From - Pips;
       (From - Pips) == 0 -> ?WHITE_OUT
    end;
new_pos(?BLACK, From, Pips) ->
    if From == ?BLACK_BAR -> Pips;
       (From + Pips) < 25 -> From + Pips;
       (From + Pips) == 25 -> ?BLACK_OUT
    end.

route(?WHITE) -> lists:seq(24, 1, -1);
route(?BLACK) -> lists:seq(1, 24).

check_destination(Color, To, Board) ->
    OpColor = opponent(Color),
    case get_checkers(To, Board) of
        empty -> ok;
        {Color, _} -> ok;
        {OpColor, 1} -> hit;
        {OpColor, _} -> occupied
    end.


%% take_pips(Color, {From, To}, Pips, BearoffMode) -> {ok, NewPips} | error
take_pips(Color, {From, To}, _Pips, _BearoffMode)
  when From == ?WHITE_OUT;
       From == ?BLACK_OUT;
       From == To;
       From == ?WHITE_BAR andalso To == ?WHITE_OUT;
       From == ?BLACK_BAR andalso To == ?BLACK_OUT;
       Color == ?WHITE andalso (From == ?BLACK_BAR orelse To == ?BLACK_OUT);
       Color == ?BLACK andalso (From == ?WHITE_BAR orelse To == ?WHITE_OUT)
  -> error;

take_pips(Color, {From, To}, Pips, Board) ->
    Dist = if is_integer(From), is_integer(To) -> abs(To - From);
              From == ?WHITE_BAR -> 25 - To;
              From == ?BLACK_BAR -> To;
              To == ?WHITE_OUT -> From;
              To == ?BLACK_OUT -> 25 - From
           end,
    case find_pips(Color, Dist, To, Pips, Board) of
        true -> {ok, lists:delete(Dist, Pips)};
        false -> error
    end.


%% find_pips(Color, Dist, To, Pips, Board) -> boolean()
find_pips(Color, Dist, To, Pips, Board) ->
    case lists:member(Dist, Pips) of
        true -> true;
        false ->
            BearoffMode = detect_bearoff_mode(Color, Board),
            if BearoffMode ->
                   Out = out_position(Color),
                   if To =/= Out -> false;
                      true -> more_far_checkers_exist(Color, Dist, Board)
                   end;
               true -> false
            end
    end.


%% more_far_checkers_exist(Color, Dist, Board) -> boolean()
more_far_checkers_exist(Color, Dist, Board) ->
    {HomeMin, HomeMax} = home_range(Color),
    {RangeMin, RangeMax} = if Color == ?WHITE -> {HomeMin + Dist, HomeMax};
                              Color == ?BLACK -> {HomeMin, HomeMax - Dist}
                           end,
    F = fun(Pos) ->
                case get_checkers(Pos, Board) of
                    {Color, _} -> true;
                    _ -> false
                end
        end,
    not lists:any(F, lists:seq(RangeMin, RangeMax)).

