-module(nsm_srv_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).
-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

%    Auth = {zealot_auth, {zealot_auth, start_link, []}, Restart, Shutdown, Type, [zealot_auth]},
    TableManager = {table_manager, {table_manager, start_link, []}, Restart, Shutdown, Type, [table_manager]},
    TopMan = {topman, {topman, start, []}, Restart, Shutdown, Type, [topman, pushsub]},
    LobbySup = {nsm_srv_tournament_lobby_sup, {nsm_srv_tournament_lobby_sup, start_link, []}, Restart, Shutdown, supervisor, [nsm_srv_tournament_lobby_sup]},

    {ok, {SupFlags, [TableManager, TopMan]}}.
