%%%-------------------------------------------------------------------
%%% @author Dennis Novikov <dennis.novikov@gmail.com>
%%% @copyright Paynet Internet ve Bilisim Hizmetleri A.S. All Rights Reserved.
%%% @doc
%%% Tournament instances supervisor
%%% @end
%%%-------------------------------------------------------------------

-module(nsg_tourney_inst_sup).

-behaviour(supervisor).

%% API
-export([start_link/0
        ,add_tournament/3
        ]).

%% Supervisor callbacks
-export([init/1]).

-define(SUP, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec add_tournament(Client::term(), ID::term(), Params::list())
                    -> {ok, pid()}
                     | {error, Reason} when
      Reason :: {already_started, pid()}
              | term().
add_tournament(Client, ID, Params) ->
    supervisor:start_child(?SUP, tproc_sup(Client, ID, Params)).


%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SUP}, ?MODULE, []).

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
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,
    SupFlags = {one_for_one, MaxRestarts, MaxSecondsBetweenRestarts},
    {ok, {SupFlags, []}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

tproc_sup(Client, TournamentID, Params) ->
    M = nsg_tourney_proc_sup,
    {{M, TournamentID}, {M, start_link, [Client, TournamentID, Params]},
     permanent, infinity, supervisor, [M]
    }.
