-module(nsm_db_app).
-include_lib("nsx_config/include/log.hrl").
-behaviour(application).
-export([start/2, stop/1,wait_vnodes/0]).

wait_riak() ->
    case nsm_db:put({test,ok}) of
         ok -> stop;
         _ -> wait_riak()
    end.

wait_vnodes() ->
    case riak:client_connect(node()) of
    {ok,C} ->
    case C:get(<<"test">>,<<"ok">>,[]) of
         {error,{insufficient_vnodes,_,_,_}} -> wait_vnodes();
         _ -> stop
    end;
    _ -> error end.

start(_StartType, _StartArgs) ->
    nsm_db_sup:start_link().

stop(_State) ->
    ok.
