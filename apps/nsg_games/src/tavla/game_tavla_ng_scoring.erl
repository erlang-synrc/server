%% Author: serge
%% Created: Jan 24, 2013
%% Description:
-module(game_tavla_ng_scoring).

-include_lib("eunit/include/eunit.hrl").
-include_lib("nsx_config/include/log.hrl").

%%
%% Exported Functions
%%
-export([
         init/3,
         last_round_result/1,
         round_finished/2
        ]).


-define(MODE_STANDARD, standard).

-define(ACH_WIN_NORMAL, 1).
-define(ACH_WIN_MARS, 2).


-record(state,
        {mode             :: standard,
         seats_num        :: integer(),
         rounds_num       :: undefined | pos_integer(),
         last_round_num   :: integer(),
         round_score      :: list(),    %% [{SeatNum, DeltaPoints}]
         round_achs       :: list(),    %% [{SeatNum, [{AchId, Points}]}]
         total_score      :: list(),    %% [{SeatNum, Points}]
         finish_info      :: term()     %% FinishInfo
        }).

%%
%% API Functions
%%

%% @spec init(Mode, SeatsInfo, RoundsNum) -> ScoringState
%% @doc Initialises scoring state.
%% @end
%% Types:
%%     Mode = standard
%%     SeatsInfo = [{SeatNum, Points}]
%%       SeatNum = integer()
%%       Points = integer()
%%     RoundsNum = undefined | pos_integer()

init(Mode, SeatsInfo, RoundsNum) ->
    ?INFO("TAVLA_NG_SCORING init Mode: ~p SeatsInfo = ~p RoundsNum = ~p", [Mode, SeatsInfo, RoundsNum]),
    true = lists:member(Mode, [?MODE_STANDARD]),
    true = is_integer(RoundsNum) orelse RoundsNum == undefined,
    SeatsNum = length(SeatsInfo),
    true = lists:seq(1, SeatsNum) == lists:sort([SeatNum || {SeatNum, _} <- SeatsInfo]),
    #state{mode = Mode,
           seats_num = SeatsNum,
           rounds_num = RoundsNum,
           last_round_num = 0,
           round_score = undefined,
           finish_info = undefined,
           round_achs = undefined,
           total_score = SeatsInfo
          }.


%% @spec last_round_result(ScoringState) -> {FinishInfo, RoundScore, AchsPoints, TotalScore} |
%%                                          no_rounds_played
%% @end
%% Types:
%%     FinishInfo =  timeout | set_timeout |
%%                   {win, WinnerSeatNum, Condition}
%%                      Condition = normal | mars
%%     RoundScore = [{SeatNum, DeltaPoints}]
%%     AchsPoints = [{SeatNum, [{AchId, Points}]}]
%%     TotalScore = [{SeatNum, Points}]

last_round_result(#state{last_round_num = 0}) -> no_rounds_played;

last_round_result(#state{round_score = RoundScore,
                         round_achs = RoundAchs,
                         total_score = TotalScore,
                         finish_info = FinishInfo
                        }) ->
    {FinishInfo, RoundScore, RoundAchs, TotalScore}.


%% @spec round_finished(ScoringState, FinishReason) ->
%%                                    {NewScoringState, GameIsOver}
%% @end
%% Types:
%%     FinishReason = timeout | set_timeout |
%%                    {win, SeatNum, Condition}
%%       Condition = normal | mars
%%     GameisOver = boolean() 

round_finished(#state{mode = GameMode, seats_num = SeatsNum,
                      last_round_num = LastRoundNum,
                      total_score = TotalScore} = State,
               FinishReason) ->
    ScoringMode = get_scoring_mode(GameMode),
    PointingRules = get_pointing_rules(ScoringMode),
    Seats = lists:seq(1, SeatsNum),
    FinishInfo = finish_info(GameMode, FinishReason),
    PlayersAchs = players_achivements(GameMode, Seats, FinishInfo),
    RoundAchs = [{SeatNum, get_achivements_points(PointingRules, Achivements)}
                 || {SeatNum, Achivements} <- PlayersAchs],
    RoundScore = [{SeatNum, sum_achivements_points(AchPoints)}
                   || {SeatNum, AchPoints} <- RoundAchs],
    RoundNum = LastRoundNum + 1,
    NewTotalScore = add_delta(TotalScore, RoundScore),
    NewState = State#state{last_round_num = RoundNum,
                           round_score = RoundScore,
                           total_score = NewTotalScore,
                           round_achs = RoundAchs,
                           finish_info = FinishInfo},
    {NewState, detect_game_finish(NewState)}.

%%
%% Local Functions
%%

detect_game_finish(#state{last_round_num = RoundNum, finish_info = FinishInfo,
                          rounds_num = MaxRoundNum}) ->
    if FinishInfo == set_timeout -> true;
       true -> RoundNum == MaxRoundNum
    end.


players_achivements(Mode, Seats, FinishInfo) ->
    case FinishInfo of
        timeout ->
            [{SeatNum, player_achivements_no_winner(Mode, SeatNum)} || SeatNum <- Seats];
        set_timeout ->
            [{SeatNum, player_achivements_no_winner(Mode, SeatNum)} || SeatNum <- Seats];
        {win, Winner, Condition} ->
            [{SeatNum, player_achivements_win(Mode, SeatNum, Winner, Condition)} || SeatNum <- Seats]
    end.


%% finish_info(GameMode, FinishReason) ->
%%      timeout |
%%      set_timeout |
%%      {win, Winner, Condition}
finish_info(_GameMode, FinishReason) -> FinishReason.


%% @spec get_achivements_points(PointingRules, Achivements) -> AchsPoints
%% @end
get_achivements_points(PointingRules, Achivements) ->
    [{Ach, lists:nth(Ach, PointingRules)} || Ach <- Achivements].

%% @spec sum_achivements_points(AchPoints) -> integer()
%% @end
sum_achivements_points(AchPoints) ->
    lists:foldl(fun({_, P}, Acc)-> Acc + P end, 0, AchPoints).

%% @spec add_delta(TotalScore, RoundScores) -> NewTotalScore
%% @end
add_delta(TotalScore, RoundScores) ->
    [{SeatNum, proplists:get_value(SeatNum, TotalScore) + Delta}
     || {SeatNum, Delta} <- RoundScores].


player_achivements_no_winner(Mode, SeatNum) ->
    player_achivements(Mode, SeatNum, no_winner, undefined, undefined).

player_achivements_win(Mode, SeatNum, Winner, Condition) ->
    player_achivements(Mode, SeatNum, win, Winner, Condition).


%% player_achivements(Mode, SeatNum, FinishType, Winner, Condition) -> [{AchId}]
player_achivements(_Mode, SeatNum, FinishType, Winner, Condition) ->
    L=[%% 1
       {?ACH_WIN_NORMAL, FinishType == win andalso SeatNum == Winner andalso Condition == normal},
       %% 2
       {?ACH_WIN_MARS, FinishType == win andalso SeatNum == Winner andalso Condition == mars}
      ],
    [Ach || {Ach, true} <- L].


get_pointing_rules(ScoringMode) ->
    {_, Rules} = lists:keyfind(ScoringMode, 1, points_matrix()),
    Rules.

points_matrix() ->
    [%%          1   2 <--- achievement number
     {standard, [1,  2]}
    ].

%%===================================================================

%% @spec get_scoring_mode(GameMode) ->  ScoringMode
%% @end
get_scoring_mode(?MODE_STANDARD) ->  standard.
