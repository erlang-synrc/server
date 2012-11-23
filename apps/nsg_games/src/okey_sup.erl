-module(okey_sup).
-behaviour(supervisor).
-export([start_link/0, stop/0]).
-include_lib("nsg_srv/include/conf.hrl").
-include_lib("nsx_config/include/log.hrl").
-export([init/1, start/0, start_game/3]).
-define(SERVER, ?MODULE).

start() -> supervisor:start({local, ?SERVER}, ?MODULE, []).
start_link() -> supervisor:start_link({local, ?SERVER}, ?MODULE, []).
stop() -> exit(?SERVER, shutdown).
start_game(Mod,Par,GameId) -> 
    ?INFO("OKEY SUP START CHILD"),
    Restart = transient,
    Shutdown = 200,
    ChildSpec = {GameId, {Mod, start_link, Par}, Restart, Shutdown, worker, [Mod]},
    supervisor:start_child(?MODULE,ChildSpec).

init([]) ->
    ?INFO("OKEY SUP STARTED"),
    RestartStrategy = one_for_one,
    MaxRestarts = 1,
    MaxSecondsBetweenRestarts = 600,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    {ok, {SupFlags, []}}.

