-module(db).

-export([start/0,
    stop/0,
    start_kakaserver/0,
         create_schema/0,
         delete_schema/0,
    sharing_mnesia/1,

    transaction/1,
    create_table/3,
    add_record/1,
    multi_get/1,
    multi_get_match/1,
    get_match/1,
    get_record/1,
    get_records/1,
    get_record_index/3,
    get_next/1,
    get_next/2,
    delete_object/1,
    get_all_records/1,
    get_all_keys/1,
    add_table_index/2]).

-include("types.hrl").

start() ->
    mnesia:start(),
    mnesia:change_table_copy_type(schema, node(), disc_copies).

stop() ->
    mnesia:stop().

start_kakaserver() ->
    create_schema(),
    mnesia:start().

create_schema() ->
    mnesia:create_schema([node()]).
delete_schema() ->
    mnesia:delete_schema([node()]).

sharing_mnesia(MasterNode) ->
    SelfNode = node(),
    mnesia:start(),
    mnesia:change_config(extra_db_nodes, [MasterNode]),
    mnesia:change_table_copy_type(schema, SelfNode, disc_copies),
    Tabs = mnesia:system_info(tables) -- [schema],
    [mnesia:add_table_copy(Tab, SelfNode, disc_copies) || Tab <- Tabs].

transaction(F) ->
    case mnesia:transaction(F) of
        {atomic, R} ->
            {ok, R};
        Other ->
            Other
    end.


-spec create_table(atom(), list(), proplist() ) -> ok | {error, any()}.
create_table(Record, RecordInfo,  Opts0) ->
    Attr = [{attributes, RecordInfo}],
    Opts = transform_opts(Opts0),
    AllOpts = lists:concat([Opts, Attr]),
    io:fwrite("create_table: ~p, ~p",[Record, lists:flatten(AllOpts)]),
    case mnesia:create_table(Record, [{record_name,config}|lists:flatten(AllOpts)]) of
        {atomic,ok}                          -> ok;
        {aborted, {already_exists, Record}}  -> ok;
        {aborted, Err}                       -> {error, Err}
    end.

add_table_index(Record, Field) ->
    case mnesia:add_table_index(Record, Field) of
        {atomic, ok}                        -> ok;
        {aborted,{already_exists,Record,_}} -> ok;
        {aborted, Err}                       -> {error, Err}
    end.


%%FIX: let it return ok or {error, ErrorDetails}
add_record(Record) ->
    case mnesia:transaction(fun() ->
                                    mnesia:write(Record)
                            end) of
        {atomic, ok} ->
            ok;
        {aborted, Error} ->
            {error, Error}
    end.


%%FIX: let it return just a list of results and crash on failed transaction
multi_get(Query) ->
    case mnesia:transaction(fun() ->
                                    [ mnesia:read(Q) || Q <- Query ]
                            end) of
        {atomic, R} ->
            lists:flatten(R);
        _ ->
            not_found
    end.

%%FIX: let it return just a list of results and crash on failed transaction
multi_get_match(Query) ->
    case mnesia:transaction(fun() ->
                                    [ mnesia:match_object(Q) || Q <- Query ]
                            end) of
        {atomic, R} ->
            lists:flatten(R);
        _ ->
            not_found
    end.

get_match(Query) ->
    {atomic, R} = mnesia:transaction(fun() ->
                                             mnesia:match_object(Query)
                                     end),
    R.

get_record(Query) ->
    case mnesia:transaction(fun() -> mnesia:read(Query) end) of
        {atomic, []} ->
            {error, not_found};
        {atomic, [R]} ->
            {ok, R}
    end.

get_records(Query) ->
    case mnesia:transaction(fun() -> mnesia:read(Query) end) of
        {atomic, R} ->
            {ok, R};
        {aborted, Er} ->
            {error, Er}
    end.


-spec get_record_index(atom(), any(), non_neg_integer() | atom()) -> 'not_found' | list().
get_record_index(Table, Value, Index) ->
    case mnesia:transaction(fun() -> mnesia:index_read(Table, Value, Index) end) of
        {atopic, []} ->
            not_found;
        {atomic, R} ->
            R
    end.



%%FIX: add get_records/1 which returns a list (may be an empty list)
%%get_records(Query) -> list()

get_next(Thing) ->
    get_next(Thing, 1).
get_next(Thing, Incr) ->
    {atomic, I} = mnesia:sync_transaction(fun() ->
                                                  mnesia:dirty_update_counter({id_seq, Thing}, Incr)
                                          end),
    I.

%%FIX: "not_found" return value is misleading. It should just return
%%'ok' if transaction is successful and raise an exception otherwise
delete_object(Query) ->
    case mnesia:transaction(fun() -> mnesia:delete_object(Query) end) of
        {atomic, ok} ->
            ok;
        _ ->
            not_found
    end.

get_all_records(Table) ->
    {atomic, All} =
        mnesia:transaction(fun() ->
                                   Lists = mnesia:all_keys(Table),
                                   [ mnesia:read({Table, G}) || G <- Lists ]
                       end),
    {ok, lists:flatten(All)}.

get_all_keys(Table) ->
    {atomic, All} =
    mnesia:transaction(fun() ->
                                   mnesia:all_keys(Table)
                           end),
    {ok, All}.

%%%%
%% Internal function
%%%%
transform_opts(Opts) ->
    transform_opts(Opts, []).
transform_opts([], Acc) ->
    lists:reverse(Acc);
transform_opts([{storage, Value} | Rest], Acc0) ->
    NewOpts = storage_to_mnesia_type(Value),
    Acc = [NewOpts | Acc0],
    transform_opts(Rest, Acc);
transform_opts([Other | Rest], Acc0) ->
    Acc = [Other | Acc0],
    transform_opts(Rest, Acc).

storage_to_mnesia_type(permanent) ->
    {disc_copies, [node()]};
storage_to_mnesia_type(temporary) ->
    {ram_copies, [node()]};
storage_to_mnesia_type(ondisk) ->
    {disc_only_copies, [node()]}.


