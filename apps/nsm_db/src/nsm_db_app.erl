-module(nsm_db_app).
-include_lib("alog/include/alog.hrl").
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
    application:start(nsx_utils),
    application:start(nsx_config),
    application:start(nsm_mq),
    nsm_db:start(),
    nsm_db:initialize(),
    ?INFO("Waiting for Riak to Initialize...."),
    wait_vnodes(),
    nsm_db:init_db(),
    nsm_db_sup:start_link().

stop(_State) ->
    ok.
