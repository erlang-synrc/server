-module(game_sup).
-behaviour(supervisor).
-export([start_link/0, stop/0]).
-include_lib("nsg_srv/include/conf.hrl").
-export([init/1]).
-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

stop() ->
    exit(?SERVER, shutdown).

init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    Restart = permanent,
    Shutdown = 2000,

    GameManager = {game_manager, {game_manager, start, []}, Restart, Shutdown, worker, [game_manager]},
    GameStats = {game_stats, {game_stats, start_link, []}, Restart, Shutdown, worker, [game_stats]},

    {ok, {SupFlags, [GameManager, GameStats]}}.

