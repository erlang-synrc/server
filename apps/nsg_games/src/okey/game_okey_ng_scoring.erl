%% Author: serge
%% Created: Oct 23, 2012
%% Description:
-module(game_okey_ng_scoring).


%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").

%%
%% Exported Functions
%%
-export([
         init/2,
         round_finished/5
        ]).

-type normal_tash() :: {integer(), integer()}.
-type tash() :: false_okey | normal_tash().

-define(MODE_STANDARD, standard).
-define(MODE_EVENODD, evenodd).
-define(MODE_COLOR, color).
-define(MODE_COUNTDOWN, countdown).


-define(COUNTDOWN10_INIT_POINTS, 10).

-define(ACH_GOSTERGE_SHOWN, 1).
-define(ACH_WIN_REVEAL, 2).
-define(ACH_WIN_REVEAL_WITH_OKEY, 3).
-define(ACH_WIN_REVEAL_WITH_PAIRS, 4).
-define(ACH_WIN_REVEAL_WITH_OKEY_PAIRS, 5).
-define(ACH_NOT_REVEAL_8_TASHES, 6).
-define(ACH_WIN_REVEAL_WITH_COLOR, 7).
-define(ACH_WIN_REVEAL_WITH_COLOR_OKEY, 8).
-define(ACH_WIN_REVEAL_WITH_COLOR_PAIRS, 9).
-define(ACH_WIN_REVEAL_WITH_COLOR_OKEY_PAIRS, 10).
-define(ACH_FAIL_REVEAL, 11).
-define(ACH_CAUGHT_BLUFF, 12).
-define(ACH_EMPTY_BOX, 13).
-define(ACH_REJECT_GOOD_HAND, 14).
-define(ACH_GOSTERGE_WINNER, 15).



-record(state,
        {mode             :: standard | evenodd | color | countdown,
         seats_num        :: integer(),
         last_round_num   :: integer(),
         rounds_scores    :: list(),    %% [{Round, [{SeatNum, DeltaPoints}]}]
         table            :: list(),    %% [{Round, [{SeatNum, Points}]}]
         chanak           :: integer()  %% Defined only for evenodd and color mode
        }).


%%
%% API Functions
%%

%% @spec init(Mode, SeatsNum) -> ScoringState
%% @doc Initialises scoring state.
%% @end
init(Mode, SeatsNum) ->
    InitPointsNum = case Mode of
                        ?MODE_COUNTDOWN -> ?COUNTDOWN10_INIT_POINTS;
                        _ -> 0
                    end,
    Table = [{0, [{SeatNum, InitPointsNum} || SeatNum <- lists:seq(1, SeatsNum)]}],
    #state{mode = Mode,
           seats_num = SeatsNum,
           last_round_num = 0,
           chanak = 0,
           rounds_scores = [],
           table = Table
          }.

%% round_finished(State, FinishReason, Hands, Gosterge, WhoHasGosterge) ->
%%                                    {NewState, RoundScores, PlayersAchsPoints}
round_finished(#state{mode = ?MODE_STANDARD = GameMode,
                      seats_num = SeatsNum,
                      last_round_num = LastRoundNum,
                      rounds_scores = RoundsScores,
                      table = Table
                     } = State,
               FinishReason,
               Hands,
               Gosterge,
               WhoHasGosterge) ->
    ScoringMode = get_scoring_mode(GameMode, Gosterge),
    PointingRules = get_pointing_rules(ScoringMode),
    Seats = lists:seq(1, SeatsNum),
    PlayersAchs = players_achivements(GameMode, Seats, FinishReason,
                                      Hands, Gosterge, WhoHasGosterge),
    PlayersAchsPoints = [{SeatNum, get_achivements_points(PointingRules, Achivements)}
                         || {SeatNum, Achivements} <- PlayersAchs],
    RoundScores = [{SeatNum, sum_achivements_points(AchPoints)}
                   || {SeatNum, AchPoints} <- PlayersAchsPoints],
    RoundNum = LastRoundNum + 1,
    NewRoundsScores = [{RoundNum, RoundScores} | RoundsScores],
    NewTable = table_add_delta(Table, RoundNum, RoundScores),
    NewState = State#state{last_round_num = RoundNum,
                           rounds_scores = NewRoundsScores,
                           table = NewTable},
    {NewState, RoundScores, PlayersAchsPoints};

%% TODO: handle other two game modes

round_finished(#state{mode = ?MODE_COUNTDOWN = GameMode,
                      seats_num = SeatsNum,
                      last_round_num = LastRoundNum,
                      rounds_scores = RoundsScores,
                      table = Table
                     } = State,
               FinishReason,
               Hands,
               Gosterge,
               WhoHasGosterge) ->
    ScoringMode = get_scoring_mode(GameMode, Gosterge),
    PointingRules = get_pointing_rules(ScoringMode),
    Seats = lists:seq(1, SeatsNum),
    PlayersAchs = players_achivements(GameMode, Seats, FinishReason, Hands, Gosterge, WhoHasGosterge),
    PlayersAchsPoints = [{SeatNum, get_achivements_points(PointingRules, Achivements)}
                         || {SeatNum, Achivements} <- PlayersAchs],
    RoundScores = [{SeatNum, sum_achivements_points(AchPoints)}
                   || {SeatNum, AchPoints} <- PlayersAchsPoints],
    RoundNum = LastRoundNum + 1,
    NewRoundsScores = [{RoundNum, RoundScores} | RoundsScores],
    NewTable = table_del_delta(Table, RoundNum, RoundScores),
    NewState = State#state{last_round_num = RoundNum,
                           rounds_scores = NewRoundsScores,
                           table = NewTable},
    {NewState, RoundScores, PlayersAchsPoints}.

%%
%% Local Functions
%%
players_achivements(GameMode,
                    Seats,
                    FinishReason,
                    Hands,
                    Gosterge,
                    WhoHasGosterge) ->
    case FinishReason of
        tashes_out ->
            [begin
                 Achivements = player_achivements_no_winner(SeatNum, WhoHasGosterge),
                 {SeatNum, Achivements}
             end || SeatNum <- Seats];
        {reveal, Revealer, Tashes, Discarded, ConfirmationList} ->
            {RightReveal, RevealWithPairs, WithColor} = check_reveal(Tashes, Gosterge),
            RevealWithColor = case GameMode of
                                  ?MODE_STANDARD -> false;
                                  ?MODE_EVENODD -> WithColor;
                                  ?MODE_COLOR -> WithColor;
                                  ?MODE_COUNTDOWN -> false
                              end,
            WinReveal = lists:all(fun({_, Response}) -> Response == true end, ConfirmationList),
            Okey = gosterge_to_okey(Gosterge),
            RevealWithOkey = Discarded == Okey,
            WrongRejects = if RightReveal ->
                                  [S || {S, Answer} <- ConfirmationList, Answer==false];
                              true -> []
                           end,
            [begin
                 {_, _Hand} = lists:keyfind(SeatNum, 1, Hands),
                 Has8Tashes = false, %% TODO:
                 Achivements = player_achivements(SeatNum, WhoHasGosterge, Has8Tashes, reveal,
                                                  Revealer, WrongRejects, WinReveal, RevealWithOkey,
                                                  RevealWithPairs, RevealWithColor),
                 {SeatNum, Achivements}
             end || SeatNum <- Seats]
    %% TODO: Gosterge finish... if needed
    end.

%% @spec get_achivements_points(PointingRules, Achivements) -> AchsPoints
%% @end
get_achivements_points(PointingRules, Achivements) ->
    [{Ach, lists:nth(Ach, PointingRules)} || Ach <- Achivements].

%% @spec sum_achivements_points(AchPoints) -> integer()
%% @end
sum_achivements_points(AchPoints) ->
    lists:foldl(fun({_, P}, Acc)-> Acc + P end, 0, AchPoints).

%% @spec table_add_delta(Table1, RoundNum, RoundScores) -> Table2
%% @end
table_add_delta(Table, RoundNum, RoundScores) ->
    {_, LastTableRecScores} = lists:keyfind(RoundNum - 1, 1, Table),
    NewTableScores = [begin
                          {_, LastScore} = lists:keyfind(SeatNum, 1, LastTableRecScores),
                          {SeatNum, LastScore + Delta}
                      end || {SeatNum, Delta} <- RoundScores],
    [{RoundNum, NewTableScores} | Table].

%% @spec table_del_delta(Table1, RoundNum, RoundScores) -> Table2
%% @end
table_del_delta(Table, RoundNum, RoundScores) ->
    {_, LastTableRecScores} = lists:keyfind(RoundNum - 1, 1, Table),
    NewTableScores = [begin
                          {_, LastScore} = lists:keyfind(SeatNum, 1, LastTableRecScores),
                          {SeatNum, LastScore - Delta}
                      end || {SeatNum, Delta} <- RoundScores],
    [{RoundNum, NewTableScores} | Table].


%% @spec gosterge_to_okey(GostergyTash) -> OkeyTash
%% @end
gosterge_to_okey({Color, Value}) ->
    if Value == 13 -> {Color, 1};
       true -> {Color, Value + 1}
    end.

%% @spec check_reveal(TashPlaces, Gosterge) -> {RightReveal, WithPairs, SameColor}
%% @end
check_reveal([TopRow, BottomRow], Gosterge) ->
    FlatList = TopRow ++ [null | BottomRow],
    Okey = gosterge_to_okey(Gosterge),
    Normalized = [case E of
                      Okey -> okey;
                      false_okey -> Okey;
                       _ -> E
                  end || E <- FlatList],
    Sets = split_by_delimiter(null, Normalized),
    ProperHand = lists:all(fun(S) -> is_set(S) orelse is_run(S) end, Sets),
    Pairs = lists:all(fun(S) -> is_pair(S) end, Sets),
    [Color | Rest] = [C || {C, _} <- Normalized],
    SameColor = lists:all(fun(C) -> C==Color end, Rest),
    {ProperHand orelse Pairs, Pairs, SameColor}.

%% @spec split_by_delimiter(Delimiter, List) -> ListOfList
%% @end
split_by_delimiter(Delimiter, Hand) -> split_by_delimiter(Delimiter, Hand, []).
split_by_delimiter(_, [], Acc) -> lists:reverse(Acc);
split_by_delimiter(Delimiter, [Delimiter | Hand], Acc) -> split_by_delimiter(Delimiter, Hand, Acc);
split_by_delimiter(Delimiter, Hand, Acc) ->
    {L, Rest} = lists:splitwith(fun(X) -> X == Delimiter end, Hand),
    split_by_delimiter(Delimiter, Rest, [L | Acc]).

%% @spec is_set(Set) -> boolean()
%% @end
is_set(Set) when
  length(Set) < 3;
  length(Set) > 4 -> false;
is_set(Set) ->
    Normals = [ X || X <- Set, X =/= okey ],
    {_, Value} = hd(Normals),
    SameValue = lists:all(fun({_, V}) -> V == Value end, Normals),
    UniqColors = length(Normals) == length(lists:usort([C || {C, _} <- Normals])),
    SameValue andalso UniqColors.


%% @spec is_run(Set) -> boolean()
%% @end
is_run(Set) when length(Set) < 3 -> false;
is_run(Set) ->
    {Okeys, Normals} = lists:partition(fun(X)-> X == okey end, Set),
    {Color, _} = hd(Normals),
    {Colors, Values} = lists:unzip(Normals),
    SameColor = lists:all(fun(C) -> C == Color end, Colors),
    SortedValues = lists:sort(Values),
    NormalizedValues = if hd(SortedValues)==1 -> tl(SortedValues) ++ [14]; true -> false end,
    OkeysNum = length(Okeys),
    Check1 = check_run(SortedValues, OkeysNum),
    Check2 = check_run(NormalizedValues, OkeysNum),
    SameColor andalso (Check1 orelse Check2).


check_run(false, _) -> false;
check_run([First | Rest], OkeysNum) ->
    check_run(First, Rest, OkeysNum).


check_run(Cur, [Cur | _], _OkeysNum) -> false;
check_run(Cur, [Next | Rest], OkeysNum) when Next == Cur + 1 ->
    check_run(Cur+1, Rest, OkeysNum);
check_run(_Cur, [_Next | _Rest], 0) -> false;
check_run(Cur, [Next | Rest], OkeysNum) ->
    check_run(Cur+1, [Next | Rest], OkeysNum - 1);
check_run(_Cur, [], _OkeysNum) -> true.

%% @spec is_pair(Set) -> boolean()
%% @end
is_pair([_A, okey]) -> true;
is_pair([okey, _B]) -> true;
is_pair([A, A]) -> true;
is_pair(_) -> false.


player_achivements_no_winner(SeatNum, WhoHasGosterge) ->
    player_achivements(SeatNum, WhoHasGosterge, undefined, no_winner, undefined, undefined,
                       undefined, undefined, undefined, undefined).

player_achivements(SeatNum, WhoHasGosterge, Has8Tashes, FinishType, Revealer, WrongRejects,
                   WinReveal, RevealWithOkey, RevealWithPairs, WithColor) ->
    L=[
%%     <<"gosterge shown">>,                            %% 1
       {?ACH_GOSTERGE_SHOWN, SeatNum == WhoHasGosterge},
%%     <<"proper reveal">>,                             %% 2
       {?ACH_WIN_REVEAL, FinishType == reveal andalso SeatNum == Revealer andalso WinReveal andalso (not RevealWithOkey) andalso (not RevealWithPairs) andalso (not WithColor)},
%%     <<"reveal with okey">>,                          %% 3
       {?ACH_WIN_REVEAL_WITH_OKEY, FinishType == reveal andalso SeatNum == Revealer andalso WinReveal andalso RevealWithOkey andalso (not RevealWithPairs) andalso (not WithColor)},
%%     <<"reveal with even tashes">>,                   %% 4
       {?ACH_WIN_REVEAL_WITH_PAIRS, FinishType == reveal andalso SeatNum == Revealer andalso WinReveal andalso (not RevealWithOkey) andalso RevealWithPairs andalso (not WithColor)},
%%     <<"reveal with even tashes and okey">>,          %% 5
       {?ACH_WIN_REVEAL_WITH_OKEY_PAIRS, FinishType == reveal andalso SeatNum == Revealer andalso WinReveal andalso RevealWithOkey andalso RevealWithPairs andalso (not WithColor)},
%%     <<"8 tashes of value in hand">>,                 %% 6
       {?ACH_NOT_REVEAL_8_TASHES, FinishType == reveal andalso SeatNum =/= Revealer andalso Has8Tashes},
%%     <<"reveal with color">>,                         %% 7
       {?ACH_WIN_REVEAL_WITH_COLOR, FinishType == reveal andalso SeatNum == Revealer andalso WinReveal andalso (not RevealWithOkey) andalso (not RevealWithPairs) andalso WithColor},
%%     <<"reveal with color and okey">>,                %% 8
       {?ACH_WIN_REVEAL_WITH_COLOR_OKEY, FinishType == reveal andalso SeatNum == Revealer andalso WinReveal andalso RevealWithOkey andalso (not RevealWithPairs) andalso WithColor},
%%     <<"reveal with color and even tashes">>,         %% 9
       {?ACH_WIN_REVEAL_WITH_COLOR_PAIRS, FinishType == reveal andalso SeatNum == Revealer andalso WinReveal andalso (not RevealWithOkey) andalso RevealWithPairs andalso WithColor},
%%     <<"reveal with color, even tashes and okey">>,   %% 10
       {?ACH_WIN_REVEAL_WITH_COLOR_OKEY_PAIRS, FinishType == reveal andalso SeatNum == Revealer andalso WinReveal andalso RevealWithOkey andalso RevealWithPairs andalso WithColor},
%%     <<"reveal with wrong hand">>,                    %% 11
       {?ACH_FAIL_REVEAL, FinishType == reveal andalso SeatNum == Revealer andalso (not WinReveal)},
%%     <<"caught bluff">>,                              %% 12 %% AKA others_on_wrong_reveal
       {?ACH_CAUGHT_BLUFF, FinishType == reveal andalso SeatNum =/= Revealer andalso (not WinReveal)},
%%     <<"deduction to empty box">>,                    %% 13
       {?ACH_EMPTY_BOX, false}, %% FIXME: what is it?
%%     <<"rejected good hand">>,                        %% 14
       {?ACH_REJECT_GOOD_HAND, FinishType = reveal andalso lists:member(SeatNum, WrongRejects)},
%%     <<"gosterge winner">>                            %% 15
       {?ACH_GOSTERGE_WINNER, false} %% FIXME: what is it?
    ],
    [Ach || {Ach, true} <- L].

get_pointing_rules(ScoringMode) ->
    {_, Rules} = lists:keyfind(ScoringMode, 1, points_matrix()),
    Rules.

%% TODO: Fix table
points_matrix() ->
    [%%          1   2   3   4   5   6   7   8   9  10  11  12  13  14, 15 <--- achievement number
     {standard, [1,  3,  6,  6, 12,  0,  0,  0,  0,  0, -9,  3,  1,  0,  1]},
     {odd,      [1,  3,  6,  6, 12, 12, 24, 48, 48, 96, -9,  3,  1,  0,  8]},
     {even,     [2,  6, 12, 12, 24, 24, 48, 96, 96,192,-18,  6,  2,  0,  8]},
     {ybodd,    [1,  3,  6,  6, 12, 12, 24, 48, 48, 96, -9,  3,  1,  0, 16]},
     {ybeven,   [2,  6, 12, 12, 24, 24, 48, 96, 96,192,-18,  6,  2,  0, 16]},
     {rbodd,    [2,  6, 12, 12, 24, 24, 48, 96, 96,192,-18,  6,  2,  0, 16]},
     {rbeven,   [4, 12, 24, 24, 48, 48, 96,192,192,384,-36, 12,  4,  0, 16]},
     {countdown,[1,  2,  4,  4,  8,  0,  0,  0,  0,  0, -2,  0,  1,  0,  1]}
    ].

%%===================================================================

%% @spec get_scoring_mode(GameMode, Gosterge) ->  ScoringMode
%% @end
get_scoring_mode(?MODE_STANDARD, _) ->  standard;
get_scoring_mode(?MODE_COUNTDOWN, _) -> countdown;
get_scoring_mode(?MODE_COLOR, {Color, Val}) when (Val rem 2) == 0 -> get_scoring_mode_c_even(b2c(Color));
get_scoring_mode(?MODE_COLOR, {Color, Val}) when (Val rem 2) == 1 -> get_scoring_mode_c_odd(b2c(Color));
get_scoring_mode(?MODE_EVENODD, {_Color, Val}) when (Val rem 2) == 0 -> even;
get_scoring_mode(?MODE_EVENODD, {_Color, Val}) when (Val rem 2) == 1 -> odd.

get_scoring_mode_c_odd(C) when C == yellow; C == blue -> ybodd;
get_scoring_mode_c_odd(C) when C == black; C == red -> rbodd.
get_scoring_mode_c_even(C) when C == yellow; C == blue -> ybeven;
get_scoring_mode_c_even(C) when C == black; C == red -> rbeven.

b2c(1) -> red;
b2c(2) -> blue;
b2c(3) -> yellow;
b2c(4) -> black.


%% Tests
test_test_() ->
    [{"is_pair",
      [?_assertEqual(true,  is_pair([{1,3}, {1,3}])),
       ?_assertEqual(true,  is_pair([{4,13}, {4,13}])),
       ?_assertEqual(false, is_pair([{4,12}, {4,13}])),
       ?_assertEqual(false, is_pair([{1,1}, {4,8}])),
       ?_assertEqual(true,  is_pair([okey, {3,8}])),
       ?_assertEqual(true,  is_pair([{2,3}, okey])),
       ?_assertEqual(true,  is_pair([okey, okey])),
       ?_assertEqual(false, is_pair([{2,4}, {4,2}, {3,3}])),
       ?_assertEqual(false, is_pair([{2,4}])),
       ?_assertEqual(false, is_pair([okey])),
       ?_assertEqual(false, is_pair([okey, okey, {2,6}]))
      ]},
     {"is_set",
      [?_assertEqual(true,  is_set([{1,3}, {3,3}, {2,3}])),
       ?_assertEqual(true,  is_set([{4,8}, okey, {2,8}])),
       ?_assertEqual(true,  is_set([{4,3}, okey, okey])),
       ?_assertEqual(true,  is_set([{4,13}, {1,13}, {3,13}, {2,13}])),
       ?_assertEqual(true,  is_set([okey, {1,13}, {3,13}, {2,13}])),
       ?_assertEqual(true,  is_set([okey, okey, {3,13}, {2,13}])),
       ?_assertEqual(false, is_set([{2,6}])),
       ?_assertEqual(false, is_set([okey])),
       ?_assertEqual(false, is_set([{3,4}, {2,6}])),
       ?_assertEqual(false, is_set([{2,3}, {4,3}])),
       ?_assertEqual(false, is_set([okey, okey])),
       ?_assertEqual(false, is_set([{3,4}, {1,4}, {3,4}])),
       ?_assertEqual(false, is_set([{3,4}, {1,4}, {2,5}])),
       ?_assertEqual(false, is_set([{3,4}, okey, {2,5}])),
       ?_assertEqual(false, is_set([{2,5}, {3,5}, {4,5}, {1,6}])),
       ?_assertEqual(false, is_set([{2,5}, {3,5}, {4,5}, {2,5}])),
       ?_assertEqual(false, is_set([{2,3}, {3,3}, {4,3}, {1,3}, {3,1}])),
       ?_assertEqual(false, is_set([{2,3}, okey, {4,3}, {1,3}, {3,3}]))
      ]},
     {"is_run",
      [?_assertEqual(false, is_run([{1,3}])),
       ?_assertEqual(false, is_run([okey])),
       ?_assertEqual(false, is_run([{2,2}, {2,3}])),
       ?_assertEqual(false, is_run([okey, {2,3}])),
       ?_assertEqual(false, is_run([{4,1}, {2,3}])),
       ?_assertEqual(true,  is_run([{4,4}, {4,6}, {4,5}])),
       ?_assertEqual(true,  is_run([okey, {4,6}, {4,5}])),
       ?_assertEqual(true,  is_run([okey, {4,6}, okey])),
       ?_assertEqual(true,  is_run([{1,12}, {1,1}, {1,13}])),
       ?_assertEqual(true,  is_run([{1,12}, {1,1}, {1,11}, {1,13}])),
       ?_assertEqual(false, is_run([{1,12}, {1,1}, {1,11}, {1,11}, {1,13}])),
       ?_assertEqual(false, is_run([{1,12}, {1,1}, {1,2}, {1,11}, {1,13}])),
       ?_assertEqual(true,  is_run([{3,6}, {3,8}, okey, {3,5}, {3,9}])),
       ?_assertEqual(false, is_run([{3,6}, {3,8}, okey, {3,5}, {3,9}, {3,2}])),
       ?_assertEqual(false, is_run([{3,6}, {3,8}, okey, {3,5}, {3,9}, {1,2}]))
      ]}
    ].
