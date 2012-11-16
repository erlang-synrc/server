-module(nsm_conn_sup).
-behaviour(supervisor).
-compile(export_all).

-include_lib("nsg_srv/include/conf.hrl").
-define(SERVER, ?MODULE).

start_link() -> supervisor:start_link({local, ?SERVER}, ?MODULE, []).
stop() -> exit(?SERVER, shutdown).
get_free_port() -> ?LISTEN_PORT.

init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 1,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,

    WorkerSup = {conn_worker_sup, {conn_worker_sup, start_link, []},
             Restart, Shutdown, worker, [conn_worker_sup]},

    Listener = {conn_listener, {conn_listener, start_link, [get_free_port()]},
                Restart, Shutdown, worker, [conn_listener]},


    {ok, {SupFlags, [WorkerSup, Listener]}}.
