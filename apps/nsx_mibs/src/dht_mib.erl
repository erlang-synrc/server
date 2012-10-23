-module(dht_mib).
-include("DHT-MIB.hrl").

-export([dht_table/1, dht_table/3]).
-export([update_dht_table/0]).

-record(dhtTable, {dName, nodeGets, nodePuts}).
-define(dhtShadowArgs, {dhtTable, string, record_info(fields, dhtTable), 5000, fun dht_mib:update_dht_table/0}).

dht_table(Op)->
    snmp_shadow_table:table_func(Op, ?dhtShadowArgs).

dht_table(Op, RowIndex, Cols) ->
    snmp_shadow_table:table_func(Op, RowIndex, Cols, ?dhtShadowArgs).

update_dht_table()->
    [{count,_}, {one, NodeGets}] = folsom_metrics:get_metric_value({riak_kv, node_gets}),
    [{count,_}, {one, NodePuts}] = folsom_metrics:get_metric_value({riak_kv, node_puts}),

    error_logger:info_msg("DHT status: ~p:~p~n", [NodeGets, NodePuts]),
    ok = mnesia:dirty_write(#dhtTable{dName = "srv0", nodeGets = NodeGets, nodePuts = NodePuts}).