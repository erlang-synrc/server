%%%-------------------------------------------------------------------
%%% @author Dennis Novikov <dennis.novikov@gmail.com>
%%% @copyright Paynet Internet ve Bilisim Hizmetleri A.S. All Rights Reserved.
%%% @doc
%%% An single tournament processes supervisor
%%% @end
%%%-------------------------------------------------------------------

-module(nsg_tourney_proc_sup).

-behaviour(supervisor).


-include("tournament.hrl").


%% API
-export([start_link/3
        ]).

%% Supervisor callbacks
-export([init/1]).


%%%===================================================================
%%% API functions
%%%===================================================================


-spec start_link(Client::term(), ToutnamentID::term(), Params::list()) ->
                        {ok, PID::pid()}
                       |{error, Reason::term()}.
%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Client, TournamentID, Params) ->
    supervisor:start_link(?MODULE, [Client, TournamentID, Params]).


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
init([Client, TournamentID, Params]) ->
    MaxRestarts = 100,
    MaxSecondsBetweenRestarts = 5,
    SupFlags = {rest_for_one, MaxRestarts, MaxSecondsBetweenRestarts},
    {ok, {SupFlags
         ,[tblctl_sup(TournamentID), tmgr(Client, TournamentID, Params)]
         }}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


%% Tournament manager
tmgr(Client, TournamentID, Params) ->
    M = nsg_tourney_mgr,
    {{M, TournamentID}, {M, start_link, [Client, TournamentID, Params]},
     permanent, 10000, worker, [M]
    }.


%% Table controllers supervisor
tblctl_sup(TournamentID) ->
    M = nsg_tourney_tblctl_sup,
    {{M, TournamentID}, {M, start_link, [TournamentID]},
     permanent, infinity, supervisor, [M]
    }.
