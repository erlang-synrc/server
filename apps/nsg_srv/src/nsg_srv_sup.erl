-module(nsg_srv_sup).
-behaviour(supervisor).
-include_lib("nsm_db/include/config.hrl").
-export([start_link/0]).
-export([init/1]).
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    net_kernel:connect(?APPSERVER_NODE),
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 1,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    Restart = permanent,
    Shutdown = 2000,

    {ok, { SupFlags, []} }.

