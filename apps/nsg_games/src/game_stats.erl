-module(game_stats).
-behaviour(gen_server).

-export([start_link/0, add_game/1, get_skill/1, get_game_points/2,
         init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3,
         assign_points/2, is_feel_lucky/2, game_info_to_ti/1, charge_quota/1]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("nsg_srv/include/basic_types.hrl").
-include_lib("nsg_srv/include/game_okey.hrl").
-include_lib("nsg_srv/include/game_tavla.hrl").
-include_lib("alog/include/alog.hrl").
-include_lib("nsg_srv/include/games.hrl").
-include_lib("nsg_srv/include/setup.hrl").
-include_lib("nsm_db/include/accounts.hrl").
-include_lib("nsm_db/include/scoring.hrl").


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_game(Game) ->
    gen_server:cast(?MODULE, {add_game, Game}).

get_skill(UserId) when is_binary(UserId) ->
    get_skill(binary_to_list(UserId));
get_skill(UserId) ->
    score_db:get_skill(UserId).

get_game_points(GameType, UserId) when is_binary(UserId) ->
    get_game_points(GameType, binary_to_list(UserId));
get_game_points(GameType, UserId) ->
    score_db:get_game_points(GameType, UserId).

init([]) ->
    {ok, no_state}.

handle_call(Request, From, State) ->
    error_logger:error_msg("unknown call ~p ~p ~n", [Request, From]),
    {noreply, State}.

handle_cast({add_game, Game}, State) % when is_record(Game, 'OkeyGameResults') 
    ->
    ?INFO("same game ~p", [Game]),
    score_db:save_game(Game),
    {noreply, State};
handle_cast(Msg, State) ->
    error_logger:error_msg("unknown cast ~p ~n", [Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    error_logger:error_msg("unknown info ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

is_feel_lucky(UId, GameInfo) ->
    UsersOPts = proplists:get_value(users_options, GameInfo),
    UO = nsx_opt:opt(UId, UsersOPts, []),
    nsx_opt:opt(feellucky, UO, false).

game_info_to_ti(GameInfo) ->
    #ti_game_event{game_name = okey,
                   game_mode = proplists:get_value(mode, GameInfo),
                   id = proplists:get_value(id, GameInfo),
                   double_points = proplists:get_value(double_points, GameInfo)
                  }.


charge_quota(GameInfo) ->
    PR0       = proplists:get_value(pointing_rules, GameInfo),
    PRLucky   = proplists:get_value(pointing_rules_lucky, GameInfo),
    Players   = proplists:get_value(initial_players, GameInfo),
    Double    = proplists:get_value(double_points, GameInfo),
    TI = game_info_to_ti(GameInfo),

    PR = rpc:call(?APPSERVER_NODE, pointing_rules, double_points, [PR0, Double]),

    [begin
         UId = binary_to_list(U#'PlayerInfo'.id),

         Amount = case is_feel_lucky(UId, GameInfo) of
                      true ->
                          PRLucky#pointing_rule.quota;
                      _ ->
                          PR#pointing_rule.quota
                  end,

        ok = rpc:call(?APPSERVER_NODE, nsm_accounts, transaction,
                           [UId, ?CURRENCY_QUOTA, -Amount, TI#ti_game_event{type = game_start}])

     end || U  <- Players].

assign_points(#'TavlaGameResults'{players = Results}, GameInfo) ->
    ConvertedResults = [ #'PlayerResults'{winner = Winner, player_id = PlayerId, score = Score}
                        || #'TavlaPlayerScore'{winner = Winner, player_id = PlayerId, score = Score} <- Results],
    assign_points(#'GameResults'{results = ConvertedResults}, GameInfo);

assign_points(#'OkeyGameResults'{series_results = Results}, GameInfo) ->
    ConvertedResults = [ #'PlayerResults'{winner = Winner, player_id = PlayerId, score = Score}
                        || #'OkeySeriesResult'{winner = Winner, player_id = PlayerId, score = Score} <- Results],
    assign_points(#'GameResults'{results = ConvertedResults}, GameInfo);

assign_points(#'GameResults'{results = Results}, GameInfo) ->

    PR0      = proplists:get_value(pointing_rules, GameInfo),
    PRLucky  = proplists:get_value(pointing_rules_lucky, GameInfo),
    Players  = proplists:get_value(initial_players, GameInfo),
    Double   = proplists:get_value(double_points, GameInfo),
    WithRobots = false, %[] /= [ robot || #'PlayerInfo'{robot = true} <- Players],
    TI = game_info_to_ti(GameInfo),

    PR1 = rpc:call(?APPSERVER_NODE, pointing_rules, double_points, [PR0, Double]),

    PlayersIds = [case U#'PlayerInfo'.robot of false -> binary_to_list(U#'PlayerInfo'.id); _ -> "(robot)" end || U <- Players],

    %% nobody get any points if playing with robots
    PR2 = case WithRobots of
             true ->
                 #pointing_rule{_ = 0};
             false ->
                 PR1
         end,

    [begin
        %% TODO: feel lucky
        UId = binary_to_list(R#'PlayerResults'.player_id),

        PR = case is_feel_lucky(UId, GameInfo) of
                 true ->
                     PRLucky;
                 false ->
                     PR2
             end,

        {Kakaush, GamePoints} =
            case R#'PlayerResults'.winner of
                <<"true">> ->
                    ?INFO("winner: ~p", [UId]),
                    {PR#pointing_rule.kakush_winner, PR#pointing_rule.game_points};
                _ ->
                    {PR#pointing_rule.kakush_winner, 0}
            end,

        case lists:member(UId, PlayersIds) of
            true -> % flesh and bones
                SR = #scoring_record{
                    game_id = proplists:get_value(id, GameInfo),
                    who = UId,
                    all_players = PlayersIds,
                    game_type = PR0#pointing_rule.game_type,
                    game_kind = PR0#pointing_rule.game,
%                   condition, % where'd I get that?
                    score_points = GamePoints,
                    score_kakaush = Kakaush,
%                   custom,    % no idea what to put here
                    timestamp = erlang:now()
                },
                Route = [feed, user, UId, scores, 0, add],  % maybe it would require separate worker for this
                nsx_util_notification:notify(Route, [SR]);
            false ->
                ok  % no statistics for robots
        end,

        ok = rpc:call(?APPSERVER_NODE, nsm_accounts, transaction,
                           [UId, ?CURRENCY_KAKUSH, Kakaush,
                            TI#ti_game_event{type = game_end}]),
        if
            GamePoints /= 0 ->
                ok = rpc:call(?APPSERVER_NODE, nsm_accounts, transaction,
                           [UId, ?CURRENCY_GAME_POINTS, GamePoints,
                            TI#ti_game_event{type = game_end}]);
            true ->
                ok
        end
     end || R  <- Results].

