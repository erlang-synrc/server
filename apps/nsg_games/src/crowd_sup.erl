%%% -------------------------------------------------------------------
%%% Author  : Sergei Polkovnikov <serge.polkovnikov@gmail.com>
%%% Description : The supervisor for serve crowd immitation games
%%% Created : Mar 21, 2013
%%% -------------------------------------------------------------------
-module(crowd_sup).
-behaviour(supervisor).
-include_lib("nsm_db/include/config.hrl").

-export([start_link/0]).
-export([init/1]).

-define(TAVLA_STANDALONE_NUM, 20).
-define(OKEY_STANDALONE_NUM, 20).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 10,
    TimeFrame = 1000,
    SupFlags = {RestartStrategy, MaxRestarts, TimeFrame},
    Specs = tavla_standalone_specs(?TAVLA_STANDALONE_NUM) ++
            okey_standalone_specs(?OKEY_STANDALONE_NUM),
    {ok, {SupFlags, Specs}}.

tavla_standalone_specs(Num) ->
    F = fun(_) ->
                GameId = id_generator:get_id(),
                GameName = "Tavla/Crowd game - " ++ erlang:integer_to_list(GameId),
                {ok, Users} = special_random_users(1),
%%%                Users = [robot],
                TableParams = [
                               {table_name, ""},
                               {mult_factor, 1},
                               {slang_allowed, false},
                               {observers_allowed, false},
                               {tournament_type, standalone},
                               {round_timeout, infinity},
                               {set_timeout, infinity},
                               {speed, fast},
                               {game_mode, standard},
                               {rounds, 3},
                               {next_series_confirmation, no_exit},
                               {pause_mode, normal},
                               {social_actions_enabled, true},
                               {tables_num, 1}
                              ],
                CommonParams = [{speed, fast},
                                {rounds, 3},
                                {double_points, 1},
                                {game_mode,standard},
                                {speed, normal},
                                {slang, false},
                                {observers, false},
                                {owner,"maxim"}
                               ],
                Params = [{game, game_tavla},
                          {game_mode, standard},
                          {game_name, GameName},
                          {seats, 2},
                          {registrants, Users},
                          {initial_points, 0},
                          {quota_per_round, 1},
                          {kakush_for_winners, 1},
                          {kakush_for_loser, 1},
                          {win_game_points, 1},
                          {mul_factor, 1},
                          {table_module, game_tavla_ng_table},
                          {bot_module, game_tavla_bot},
                          {bots_replacement_mode, enabled},
                          {table_params, TableParams},
                          {common_params, CommonParams}
                         ],
                {{tavla_standalone, GameId},
                 {nsg_trn_standalone, start_link, [GameId, Params]},
                 _Restart = permanent, _Shutdown = 2000, worker, [nsg_trn_standalone]}
        end,
    lists:map(F, lists:seq(1, Num)).


okey_standalone_specs(Num) ->
    F = fun(_) ->
                GameId = id_generator:get_id(),
                GameName = "Okey/Crowd game - " ++ erlang:integer_to_list(GameId),
                {ok, Users} = special_random_users(3),
%%%                Users = [robot, robot, robot],
                TableParams = [
                               {table_name, ""},
                               {mult_factor, 1},
                               {slang_allowed, false},
                               {observers_allowed, false},
                               {tournament_type, standalone},
                               {round_timeout, infinity},
                               {set_timeout, infinity},
                               {speed, fast},
                               {game_type, standard},
                               {rounds, 10},
                               {reveal_confirmation, true},
                               {next_series_confirmation, no_exit},
                               {pause_mode, normal},
                               {social_actions_enabled, true},
                               {gosterge_finish_allowed, undefined}
                              ],
                CommonParams = [{speed, fast},
                                {rounds,10},
                                {double_points, 1},
                                {game_mode,standard},
                                {slang, false},
                                {observers, false},
                                {owner,"maxim"}
                               ],
                Params = [{game, game_okey},
                          {game_mode, standard},
                          {game_name, GameName},
                          {seats, 4},
                          {registrants, Users},
                          {initial_points, 0},
                          {quota_per_round, 1},
                          {kakush_for_winners, 1},
                          {kakush_for_loser, 1},
                          {win_game_points, 1},
                          {mul_factor, 1},
                          {table_module, game_okey_ng_table_trn},
                          {bot_module, game_okey_bot},
                          {bots_replacement_mode, enabled},
                          {table_params, TableParams},
                          {common_params, CommonParams} %% This data will used for the gproc register
                         ],
                {{okey_standalone, GameId},
                 {nsg_trn_standalone, start_link, [GameId, Params]},
                 _Restart = permanent, _Shutdown = 2000, worker, [nsg_trn_standalone]}
        end,
    lists:map(F, lists:seq(1, Num)).


tavla_lucky_spec() ->
    TavlaTableParams = [{mult_factor, 1},
                        {slang_allowed, false},
                        {observers_allowed, false},
                        {tournament_type, lucky},
                        {round_timeout, infinity},
                        {set_timeout, infinity},
                        {speed, normal},
                        {game_mode, standard},
                        {rounds, undefined},
                        {next_series_confirmation, no},
                        {pause_mode, normal},
                        {social_actions_enabled, true}
                       ],
    TavlaGameId = id_generator:get_id(),
    TavlaGameName = "I'm filling lucky - " ++ erlang:integer_to_list(TavlaGameId),
    TavlaParams = [{game, game_tavla},
                   {game_mode, standard},
                   {game_name, TavlaGameName},
                   {mode, normal}, % Common table for several real players
                   {seats, 2},
%%%                  {quota_per_round, Quota},
                   {table_module, game_tavla_ng_table},
                   {bot_module, game_tavla_bot},
                   {table_params, TavlaTableParams}
                  ],
    {tavla_lucky, {nsg_trn_lucky, start_link, [TavlaGameId, TavlaParams]},
     _Restart = permanent, _Shutdown = 2000, worker, [nsg_trn_lucky]}.


special_random_users(Num) ->
    AllUsers = nsm_auth:imagionary_users2(),
    AllUsersNum = length(AllUsers),
    if AllUsersNum < Num -> {error, too_many};
       true -> {ok, special_random_users(Num, [], AllUsers, AllUsersNum)}
    end.

special_random_users(0, Acc, _AllUsers, _AllUsersNum) -> Acc;
special_random_users(N, Acc, AllUsers, AllUsersNum) ->
    User = list_to_binary(nsm_auth:ima_gio2(crypto:rand_uniform(1, AllUsersNum), AllUsers)),
    case lists:member(User, Acc) of
        false -> special_random_users(N - 1, [User | Acc], AllUsers, AllUsersNum);
        true -> special_random_users(N, Acc, AllUsers, AllUsersNum)
    end.
