-module(nsm_db_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->

  RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    % you can put zealot_auth here if it is not working with start_link

    Auth = {zealot_auth, {zealot_auth, start_link, []}, Restart, Shutdown, Type, [zealot_auth]},
    TableManager = {table_manager, {table_manager, start_link, []}, Restart, Shutdown, Type, [table_manager]},
    TopMan = {topman, {topman, start, []}, Restart, Shutdown, Type, [topman, pushsub]},

    {ok, { {one_for_one, 5, 10}, [TableManager,TopMan, % Auth
                                        ]} }.

