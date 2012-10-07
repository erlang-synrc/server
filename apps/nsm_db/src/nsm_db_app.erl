-module(nsm_db_app).
-include_lib("nsx_config/include/log.hrl").
-behaviour(application).
-export([start/2, stop/1]).

wait_riak() ->
    case nsm_db:put({test,ok}) of
         ok -> stop;
         _ -> wait_riak()
    end.

wait_vnodes() ->
    {ok,C} = riak:client_connect(node()),
    case C:get(<<"test">>,<<"ok">>,[]) of
         {error,{insufficient_vnodes,_,_,_}} -> wait_vnodes();
         _ -> stop
    end.

start(_StartType, _StartArgs) ->
    nsm_db:start(),
    nsm_db:initialize(),
    ?INFO("Waiting for Riak to Initialize...."),
    wait_vnodes(),
    nsm_db:init_indexes(),
    case db_opt:get_pass_init_db() of 
         false -> nsm_db:init_db();
         true -> pass
    end,
    case nsx_opt:get_env(nsm_db,sync_nodes,false) of
         true -> [ ?INFO("Joined: ~p ~p~n", [N, riak_core:join(N)]) || N <- nsx_opt:get_env(nsm_db, nodes, []) -- [node()] ];
         false -> skip
    end,
    zealot_auth:start_link(),
    nsm_db_sup:start_link().

stop(_State) ->
    ok.
