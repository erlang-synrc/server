-module(nsx_idgen_sup).
-behaviour(supervisor).
-export([start_link/0, stop/0]).
-export([init/1]).
-define(SERVER, ?MODULE).

start_link() -> supervisor:start_link({local, ?SERVER}, ?MODULE, []).
stop() -> exit(?SERVER, shutdown).

init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    Restart = permanent,
    Shutdown = 2000,
    IdGen = {id_generator, {id_generator, start_link, []},  Restart, Shutdown, worker, [id_generator]},
    {ok, {SupFlags, [IdGen]}}.
