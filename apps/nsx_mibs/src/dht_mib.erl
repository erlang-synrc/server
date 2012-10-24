-module(dht_mib).
-include("DHT-MIB.hrl").

-export([dht_table/1, dht_table/3]).
-export([update_dht_table/0]).

-record(dhtTable, {nodeName, handoffTimeouts, nodeGetsTotal, nodePutsTotal, cpuAvg15,
    nodeGetFsmTimeMedian, nodePutFsmTimeMedian}).
-define(dhtShadowArgs, {dhtTable, string, record_info(fields, dhtTable), 5000, fun dht_mib:update_dht_table/0}).

dht_table(Op)->
    snmp_shadow_table:table_func(Op, ?dhtShadowArgs).

dht_table(Op, RowIndex, Cols) ->
    snmp_shadow_table:table_func(Op, RowIndex, Cols, ?dhtShadowArgs).

update_dht_table()->
    ok = mnesia:dirty_write(#dhtTable{
	nodeName= "node",
	handoffTimeouts = folsom_metrics:get_metric_value({riak_core, handoff_timeouts}),
	nodeGetsTotal = get_count(node_gets),
	nodePutsTotal = get_count(node_puts),
	cpuAvg15 = cpu_sup:avg15(),
	nodeGetFsmTimeMedian= get_median(node_get_fsm_time),
	nodePutFsmTimeMedian= get_median(node_put_fsm_time) }).

get_median(Metric) ->
    [{min, _}, {max, _},
    {arithmetic_mean, _}, {geometric_mean, _}, {harmonic_mean, _},
    {median, Median},
    {variance, _}, {standard_deviation, _},
    {skewness, _}, {kurtosis, _}, {percentile, _},
    {histogram, _}] = folsom_metrics:get_histogram_statistics({riak_kv, Metric}),
    lists:nth(1, io_lib:format("~w", [Median])).

get_count(Metric) ->
    [{count,Count}, {one, _}] = folsom_metrics:get_metric_value({riak_kv, Metric}),
    Count.