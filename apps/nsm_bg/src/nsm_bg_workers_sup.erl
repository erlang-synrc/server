%%----------------------------------------------------------------------
%% @author Vladimir Baranov <baranoff.vladimir@gmail.com>
%% @copyright Paynet Internet ve Bilisim Hizmetleri A.S. All Rights Reserved.
%% @doc
%%    Notice application supervisor
%% @end
%%--------------------------------------------------------------------
-module(nsm_bg_workers_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

-export([start_worker/2]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_worker(CallbackModule, Params) ->
    supervisor:start_child(?MODULE, [CallbackModule, Params]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, {{simple_one_for_one, 5, 10},
          [{nsm_bg_gen_worker, {nsm_bg_gen_worker, start_link, []},
            transient, 3000, worker, [nsm_bg_gen_worker]}]}}.

