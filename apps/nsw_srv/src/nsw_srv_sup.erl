-module(nsw_srv_sup).
-behaviour(supervisor).
-export([start_link/0,init/1]).
-include("setup.hrl").
-include("loger.hrl").

-define(CHILD(M, F, A, Type), {M, {M, F, A}, permanent, 5000, Type, [M]}).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    application:start(gproc),
    Restart = permanent,
    Shutdown = 2000,
    Type = worker,
    {ok, { {one_for_one, 5, 10}, []} }.
