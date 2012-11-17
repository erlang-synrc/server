-module(nsm_bg_workers_sup).
-behaviour(supervisor).
-export([init/1,start_worker/2,start_link/0]).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).
start_worker(CallbackModule, Params) -> supervisor:start_child(?MODULE, [CallbackModule, Params]).

init([]) ->
    {ok, {{simple_one_for_one, 5, 10},
          [{nsm_consumer, {nsm_consumer, start_link, []},
            transient, 3000, worker, [nsm_consumer]}]}}.

