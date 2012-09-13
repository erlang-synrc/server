-module(nsm_srv_tournament_lobby_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([start_lobby/1]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_lobby(TID) ->
    supervisor:start_child(?MODULE, [TID]).

init([]) ->
    {ok, {{simple_one_for_one, 1000, 3600},
          [{nsm_srv_tournament_lobby, {nsm_srv_tournament_lobby, start_link, []},
            transient, 5000, worker, [nsm_srv_tournament_lobby]}]}}.
