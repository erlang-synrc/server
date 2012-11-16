-module(conn_worker_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

-include_lib("nsg_srv/include/logging.hrl").
-include_lib("nsg_srv/include/conf.hrl").

-define(MAX_RESTART,    60).
-define(MAX_TIME,       10).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    W = conn_handler,
    Worker = {W, {W, start_link, []}, temporary, brutal_kill, worker, [W]},
    {ok, { {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME}, [Worker]} }.

