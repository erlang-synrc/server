-module(dht_mib).
-compile(export_all).

-record(dhtTable,      {dhtIndex, handoffTimeouts, nodeGetsTotal, nodePutsTotal,
                        cpuAvg15, nodeGetFsmTimeMedian, nodePutFsmTimeMedian, dhtStatus}).
-define(dhtShadowArgs, {dhtTable, integer, record_info(fields, dhtTable), 5000, fun dht_mib:update_dht_table/0}).

-record(dhtArray, {arrayIndex, value, dhtStatus2}).
-define(dhtArrayShadowArgs, {dhtArray, integer, record_info(fields, dhtArray), 5000, fun dht_mib:update_dht_array/0}).

dht_table(Op)->  snmp_shadow_table:table_func(Op, ?dhtShadowArgs).
dht_table(Op, RowIndex, Cols) ->  snmp_shadow_table:table_func(Op, RowIndex, Cols, ?dhtShadowArgs).
dht_array(Op) -> snmp_shadow_table:table_func(Op, ?dhtArrayShadowArgs).
dht_array(Op, RowIndex, Cols) -> snmp_shadow_table:table_func(Op, RowIndex, Cols, ?dhtArrayShadowArgs).

update_dht_table()->
    ok = mnesia:dirty_write(#dhtTable{
	dhtIndex=1,
	handoffTimeouts = folsom_metrics:get_metric_value({riak_core, handoff_timeouts}),
	nodeGetsTotal = get_count(node_gets),
	nodePutsTotal = get_count(node_puts),
	cpuAvg15 = cpu_sup:avg15(),
	nodeGetFsmTimeMedian= get_median(node_get_fsm_time),
	nodePutFsmTimeMedian= get_median(node_put_fsm_time),
	dhtStatus=1 }).

update_dht_array()->
    error_logger:info_msg("Update DHT array", []),
    Table = folsom_metrics:get_metric_value({riak_kv,node_get_fsm_time}),
    [mnesia:dirty_write(#dhtArray{arrayIndex = VAL, value = VAL, dhtStatus2=1})||VAL <- Table ].

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