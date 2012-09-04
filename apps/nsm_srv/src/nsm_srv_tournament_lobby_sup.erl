%%----------------------------------------------------------------------
%% @author Vladimir Baranov <baranoff.vladimir@gmail.com>
%% @copyright Paynet Internet ve Bilisim Hizmetleri A.S. All Rights Reserved.
%% @doc
%%   Tournaments lobby supervisor
%% @end
%%--------------------------------------------------------------------
-module(nsm_srv_tournament_lobby_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([start_lobby/1]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_lobby(TID) ->
    supervisor:start_child(?MODULE, [TID]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	{ok, {{simple_one_for_one, 1000, 3600},
		  [{nsm_srv_tournament_lobby, {nsm_srv_tournament_lobby, start_link, []},
			transient, 5000, worker, [nsm_srv_tournament_lobby]}]}}.
