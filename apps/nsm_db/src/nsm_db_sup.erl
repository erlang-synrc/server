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

 case nsx_opt:get_env(nsp_srv,riak_srv_node,0) of
    0 ->

    error_logger:info_msg("Waiting for Riak to Start...."),
    nsm_db:start(),
    error_logger:info_msg("Waiting for Riak to Initialize...."),
    nsm_db_app:wait_vnodes();
    _ -> skip end,

    nsm_db:initialize(),

 case nsx_opt:get_env(nsp_srv,riak_srv_node,0) of
    0 ->

    nsm_db:init_indexes(),
    case nsx_opt:get_env(nsm_db,sync_nodes,false) of
         true -> [ error_logger:info_msg("Joined: ~p ~p~n", [N, riak_core:join(N)]) || N <- nsx_opt:get_env(nsm_db, nodes, []) -- [node()] ];
         false -> skip
    end,
    case  nsx_opt:get_env(nsm_db,pass_init_db, true) of 
         false -> nsm_db:init_db();
         true -> pass
    end;
    _ -> application:stop(riak_kv), 
         application:stop(riak_pipe),
         application:stop(eleveldb),
         application:stop(erlang_js),
         application:stop(webmachine),
         application:stop(mochiweb),
         application:stop(bitcask)
          end,

    % you can put zealot_auth here if it is not working with start_link

%    Auth = {zealot_auth, {zealot_auth, start_link, []}, Restart, Shutdown, Type, [zealot_auth]},
    TableManager = {table_manager, {table_manager, start_link, []}, Restart, Shutdown, Type, [table_manager]},
%    TopMan = {topman, {topman, start, []}, Restart, Shutdown, Type, [topman, pushsub]},

    {ok, { {one_for_one, 5, 10}, [TableManager]} }.

