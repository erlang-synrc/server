-module(mem_mib).

-export([mem_table/1, mem_table/3]).
-export([update_mem_table/0]).

-record(memTable, {id, total, used, memStatus}).
-define(memShadowArgs, {memTable, integer, record_info(fields, memTable), 5000, fun mem_mib:update_mem_table/0}).

mem_table(Op)->
    snmp_shadow_table:table_func(Op, ?memShadowArgs).

mem_table(Op, RowIndex, Cols) ->
    snmp_shadow_table:table_func(Op, RowIndex, Cols, ?memShadowArgs).

update_mem_table()->
    {Total, Allocated, _Worst} = memsup:get_memory_data(),
    ok = mnesia:dirty_write(#memTable{id = 1, total = Total, used =  Allocated}).