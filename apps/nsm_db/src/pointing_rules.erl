%%----------------------------------------------------------------------
%% @author Vladimir Baranov <baranoff.vladimir@gmail.com>
%% @copyright Paynet Internet ve Bilisim Hizmetleri A.S. All Rights Reserved.
%% @doc
%%     FIXME: add description to module pointing_matrix
%% @end
%%-------------------------------------------------------------------
-module(pointing_rules).

%%
%% Include files
%%
-include("accounts.hrl").
-include_lib("nsx_config/include/log.hrl").

%%
%% Exported Functions
%%

-export([get_rules/3,
         double_points/2]).

-export([setup/0]).

%%
%% API Functions
%%

-type external_game_name() :: game_okey | game_tavla.

%% @doc Common getter for rules.
-spec get_rules(external_game_name(), game_mode(), Rounds::integer()) ->
          {ok, #pointing_rule{}, AdditionalRules::list(#pointing_rule{})} | {error, any()}.

get_rules(Game, GameType, Rounds) when Game == game_okey; Game == game_tavla ->
    {Key, AdditionalKeys} = build_keys(Game, GameType, Rounds),

    DefaultPR = #pointing_rule{game = game(Game), game_type = GameType, kakush_winner = 15,
                               kakush_other = 2, game_points = 15, quota = 20},

    MainRule = case nsm_db:get(pointing_rule, Key) of
            {ok,M} -> M;
            _ -> DefaultPR
    end,
    AdditionalRules = [case nsm_db:get(pointing_rule, K) of
                             {ok,R} -> R;
                              _ -> []
                       end || K <- AdditionalKeys],
    {ok, MainRule, lists:flatten(AdditionalRules)};

get_rules(_, _, _) ->
    {error, not_implemented}.

game(game_okey) -> okey;
game(game_tavla) -> tavla;
game(okey) -> okey;
game(tavla) -> tavla.

%% @doc Double pointing rules
-spec double_points(#pointing_rule{}, integer()) -> #pointing_rule{}.

double_points(P, C) ->
	P#pointing_rule{kakush_winner = P#pointing_rule.kakush_winner * C,
					kakush_other  = P#pointing_rule.kakush_other  * C,
					quota         = P#pointing_rule.quota         * C,
					game_points   = P#pointing_rule.game_points   * C}.

%% @doc build key to get needed rules from db
build_keys(game_okey, GameType, Rounds) ->
    MainKey = case GameType of
                  countdown ->
                      {okey, countdown};
                  _ ->
                      {okey, GameType, Rounds}
              end,
    %% additional key for feellucky
    {MainKey, [{okey, feellucky}]};
build_keys(game_tavla, GameType, Rounds) ->
    {{tavla, GameType, Rounds}, []}.

%% Initial setup of the matrix. Performs when db inits.

setup() ->

	%% OKEY

	Rounds5 = #pointing_rule{rounds = 5, game = okey,
							  kakush_winner = 15, kakush_other = 3, quota = 5},
	nsm_db:put(Rounds5#pointing_rule{id = {okey, standard, 5}, game_type = standard, game_points = 1}),
	nsm_db:put(Rounds5#pointing_rule{id = {okey, evenodd,  5}, game_type = evenodd,  game_points = 5}),
	nsm_db:put(Rounds5#pointing_rule{id = {okey, color,    5}, game_type = color,    game_points = 7}),

	Rounds10 = #pointing_rule{rounds = 10, game = okey,
							  kakush_winner = 15, kakush_other = 3, quota = 10},
	nsm_db:put(Rounds10#pointing_rule{id = {okey, standard, 10}, game_type = standard, game_points = 7}),
	nsm_db:put(Rounds10#pointing_rule{id = {okey, evenodd,  10}, game_type = evenodd,  game_points = 10}),
	nsm_db:put(Rounds10#pointing_rule{id = {okey, color,    10}, game_type = color,    game_points = 15}),

	Rounds20 = #pointing_rule{rounds = 20, game = okey,
							  kakush_winner = 15, kakush_other = 3, quota = 20},
	nsm_db:put(Rounds20#pointing_rule{id = {okey, standard, 20}, game_type = standard, game_points = 15}),
	nsm_db:put(Rounds20#pointing_rule{id = {okey, evenodd,  20}, game_type = evenodd,  game_points = 20}),
	nsm_db:put(Rounds20#pointing_rule{id = {okey, color,    20}, game_type = color,    game_points = 30}),

	Rounds40 = #pointing_rule{rounds = 40, game = okey,
							  kakush_winner = 32, kakush_other = 7, quota = 40},
	nsm_db:put(Rounds40#pointing_rule{id = {okey, standard, 40}, game_type = standard, game_points = 30}),
	nsm_db:put(Rounds40#pointing_rule{id = {okey, evenodd,  40}, game_type = evenodd,  game_points = 40}),
	nsm_db:put(Rounds40#pointing_rule{id = {okey, color,    40}, game_type = color,    game_points = 60}),

	Rounds60 = #pointing_rule{rounds = 60, game = okey,
							  kakush_winner = 32, kakush_other = 7, quota = 40},
	nsm_db:put(Rounds60#pointing_rule{id = {okey, standard, 60}, game_type = standard, game_points = 60}),
	nsm_db:put(Rounds60#pointing_rule{id = {okey, evenodd,  60}, game_type = evenodd,  game_points = 80}),
	nsm_db:put(Rounds60#pointing_rule{id = {okey, color,    60}, game_type = color,    game_points = 120}),


	Rounds80 = #pointing_rule{rounds = 80, game = okey,
							  kakush_winner = 70, kakush_other = 15, quota = 80},
	nsm_db:put(Rounds80#pointing_rule{id = {okey, standard, 80}, game_type = standard, game_points = 120}),
	nsm_db:put(Rounds80#pointing_rule{id = {okey, evenodd,  80}, game_type = evenodd,  game_points = 160}),
	nsm_db:put(Rounds80#pointing_rule{id = {okey, color,    80}, game_type = color,    game_points = 240}),

	%% Countdown 10
	nsm_db:put(#pointing_rule{id = {okey, countdown}, game = okey, game_type = countdown,
								 kakush_winner = 6, kakush_other = 1, quota = 8, game_points = 10}),

	%% Feel lucky
	nsm_db:put(#pointing_rule{id = {okey, feellucky}, game = okey, game_type = feellucky,
								 kakush_winner = 0, kakush_other = 0, quota = 0, game_points = 0}),

	%% TAVLA

	GameTypes = [standard, evenodd],
	%% here we store points in rounds field
	[nsm_db:put(#pointing_rule{id = {tavla, GT, 3}, rounds = 3, game = tavla, game_type = GT,
								  kakush_winner = 1, kakush_other = 1, quota = 3, game_points = 5})
	   || GT <- GameTypes],


	[nsm_db:put(#pointing_rule{id = {tavla, GT, 5}, rounds = 5, game = tavla, game_type = GT,
								  kakush_winner = 2, kakush_other = 1, quota = 4, game_points = 5})
	   || GT <- GameTypes],

	[nsm_db:put(#pointing_rule{id = {tavla, GT, 7}, rounds = 7, game = tavla, game_type = GT,
								  kakush_winner = 3, kakush_other = 1, quota = 5, game_points = 7})
	   || GT <- GameTypes],

	%% Kakara
	nsm_db:put(#pointing_rule{id = {tavla, kakara}, game = tavla, game_type = kakara,
								 kakush_winner = 0, kakush_other = 0, quota = 1, game_points = 0}).

%%
%% Local Functions
%%
