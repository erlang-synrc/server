%%%-------------------------------------------------------------------
%%% @author Dennis Novikov <dennis.novikov@gmail.com>
%%% @copyright Paynet Internet ve Bilisim Hizmetleri A.S. All Rights Reserved.
%%% @doc
%%% Tournament application top-level supervisor
%%% @end
%%%-------------------------------------------------------------------

-module(nsg_tourney_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 5,
    SupFlags = {rest_for_one, MaxRestarts, MaxSecondsBetweenRestarts},
    {ok, {SupFlags, [tinst_sup(), tmaster()]}}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

tmaster() ->
    M = nsg_tourney_master,
    {M, {M, start_link, []},
     permanent, 10000, worker, [M]
    }.

tinst_sup() ->
    M = nsg_tourney_inst_sup,
    {M, {M, start_link, []},
     permanent, infinity, supervisor, [M]
    }.
