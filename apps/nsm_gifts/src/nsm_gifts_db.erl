%%----------------------------------------------------------------------
%% @author Serge Polkovnikov <serge.polkovnikov@gmail.com>
%% @copyright Paynet Internet ve Bilisim Hizmetleri A.S. All Rights Reserved.
%% @doc
%% Interfaces for working with the gifts database
%% @end
%% Created: Sep 7, 2012
%%----------------------------------------------------------------------
-module(nsm_gifts_db).

%%
%% Include files
%%
-include_lib("alog/include/alog.hrl").
-include("db.hrl").
-include("common.hrl").

%%
%% Exported Functions
%%
-export([
         init_db/0
        ]).

-export([
         create_category/2,
         create_category/3,
         create_gift/1,
         create_gift/2,
         get_gifts/1,
         get_gifts/2
        ]).

-type db_handler() :: any().

%% Buckets
-define(GIFTS_BUCKET, <<"gifts">>).
-define(COUNTERS_BUCKET, <<"gifts_counters">>).
-define(CATEGORIES_BUCKET, <<"gifts_categories">>).

%% Counters
-define(GIFTS_COUNTER, <<"gift_id">>).
-define(CATEGORIES_COUNTER, <<"category_id">>).

%% Indices
-define(BUCKET_INDEX, "bucket_bin").
-define(CATEGORY_INDEX, "categoty_bin").

%% Meta Id
-define(MD_INDEX, <<"index">>).

%%
%% API Functions
%%
-spec init_db() -> ok.
%% @spec init_db() -> ok
%% @doc Initialize the data storage
%% @end

init_db() ->
    ?INFO("~w:init_db/0: started", [?MODULE]),
    C = start_riak_client(),
    ok = C:set_bucket(?GIFTS_BUCKET, [{backend, leveldb_backend}]),
    ok = init_counter(C, ?GIFTS_COUNTER, 1, []),
    ok = init_counter(C, ?CATEGORIES_COUNTER, 1, []),
    stop_riak_client(C),
    ?INFO("~w:init_db/0: done", [?MODULE]),
    ok.

%% @spec create_category(Name, Description) -> ok | {error, Reason}
%% @doc
%% Types:
%%     Name = Description = binary()
%%     Reason = term()
%% @end

create_category(Name, Description) ->
    Handler = start_riak_client(),
    Res = create_category(Handler, Name, Description),
    stop_riak_client(Handler),
    Res.

%% @spec create_category(Handler, Name, Description) -> ok | {error, Reason}
%% @doc
%% Types:
%%     Handler = db_handler()
%%     Name = Description = binary()
%%     Reason = term()
%% @end

create_category(Handler, Name, Description) ->
    create_new_category_record(Handler, Name, Description).


%% @spec create_gift(GiftRec) -> ok | {error, Reason}
%% @doc
%% Types:
%%     GiftRec = #ext_product_info{}
%%     Reason = term()
%% @end

create_gift(GiftRec) ->
    Handler = start_riak_client(),
    Res = create_gift(Handler, GiftRec),
    stop_riak_client(Handler),
    Res.

%% @spec create_gift(Handler, GiftRec) -> ok | {error, Reason}
%% @doc
%% Types:
%%     Handler = db_handler()
%%     GiftRec = #ext_product_info{}
%%     Reason = term()
%% @end

create_gift(Handler, GiftRec) ->
    create_new_gift_record(Handler, GiftRec).

%% @spec get_gifts(Category) -> List
%% @doc
%% Types:
%%     Category = integer()
%%     List = list(#gift{})
%% @end

get_gifts(Category) ->
    Handler = start_riak_client(),
    Res = get_gifts(Handler, Category),
    stop_riak_client(Handler),
    Res.

%% @spec get_gifts(Handler, Category) -> List
%% @doc
%% Type:
%%     Handler = db_handler()
%%     Category = integer()
%%     List = list(#gift{})
%% @end

get_gifts(Handler, Category) ->
    Keys = get_gifts_keys_by_cat(Handler, Category),
    Objects = read_gifts_objects(Handler, Keys),
    gifts_objects_to_data(Objects).


-spec gifts_objects_to_data(list()) -> list(#gift{}).
%% @private

gifts_objects_to_data(Objects) ->
    [begin
         riak_object:get_value(Obj)
     end
     || Obj <- Objects].


%%
%% Local Functions
%%

%% @private
-spec start_riak_client() -> any().

%start_riak_client() ->
%    {ok, Cl} = riak:local_client(),
%    Cl.

start_riak_client() ->
    [{_,_,{_,Cl}}] = ets:lookup(config, "riak_client"),
    Cl.


%% @private
-spec stop_riak_client(any()) -> ok.

stop_riak_client(_Cl) ->
    ok.



-spec init_counter(any(), binary(), integer(), list()) -> ok | {error, any()}.
%% @private
%% @spec init_counter(Cl, CounterId, InitVal, Options) -> ok | {error, Error}
%% @end

init_counter(Cl, CounterId, InitVal, Options) ->
    Force = proplists:get_value(force, Options, false),
    if Force ->
           Object = create_counter_object(CounterId, InitVal),
           Cl:put(Object, []);
       true ->
           case Cl:get(?COUNTERS_BUCKET, CounterId, []) of
               {ok, _Object} ->
                   ok;
               {error, notfound} ->
                   Object = create_counter_object(CounterId, InitVal),
                   Cl:put(Object, [])
           end
    end.

%% @private

create_counter_object(CounterId, Val) ->
    Obj1 = riak_object:new(?COUNTERS_BUCKET, CounterId, Val),
    Index = [{?BUCKET_INDEX, ?COUNTERS_BUCKET}],
    Meta = dict:store(?MD_INDEX, Index, dict:new()),
    Obj2 = riak_object:update_metadata(Obj1, Meta),
    Obj2.


create_new_category_record(Cl, Name, Description) ->
    Id = category_id(Cl),
    Record=#gifts_category{id = Id,
                           name = Name,
                           description = Description},
    write_category_record(Cl, Record),
    ok.

create_new_gift_record(Cl, Record) ->
    Id = gift_id(Cl),
    Record2 = Record#gift{id = Id},
    write_gift_record(Cl, Record2),
    ok.

category_id(Handler) ->
    next_id(Handler, ?CATEGORIES_COUNTER).

gift_id(Handler) ->
    next_id(Handler, ?GIFTS_COUNTER).


%% @private

next_id(Cl, CounterId) ->
    {ok, Object} = Cl:get(?COUNTERS_BUCKET, CounterId, []),
    CurValue = riak_object:get_value(Object),
    Object2 = riak_object:update_value(Object, CurValue+1),
    case Cl:put(Object2, [if_not_modified]) of
        ok ->
            CurValue;
        {error, _} ->
            next_id(Cl, CounterId)
    end.



-spec write_category_record(any(), #gifts_category{}) -> ok.
%% @private

write_category_record(Cl, #gifts_category{id = CategoryId} = Record) ->
    Obj = riak_object:new(?CATEGORIES_BUCKET,
                          term_to_binary(CategoryId), Record),
    ok = Cl:put(Obj, []).

-spec write_gift_record(any(), #gift{}) -> ok.
%% @private

write_gift_record(Cl, #gift{id = Id,
                            category_id = CatId
                           } = Record) ->
    Obj1 = riak_object:new(?GIFTS_BUCKET,
                           term_to_binary(Id), Record),
    Indecies = [{?BUCKET_INDEX, ?GIFTS_BUCKET},
                {?CATEGORY_INDEX, term_to_binary(CatId)}],
    Meta = dict:store(?MD_INDEX, Indecies, dict:new()),
    Obj2 = riak_object:update_metadata(Obj1, Meta),
    ok = Cl:put(Obj2, []).

%% @private
-spec get_gifts_keys_by_cat(any(), integer()) -> list(binary()).

get_gifts_keys_by_cat(Cl, CatId) ->
    {ok, Keys} =
        Cl:get_index(?GIFTS_BUCKET,
                     {eq, <<?CATEGORY_INDEX>>, term_to_binary(CatId)}),
    Keys.


-spec read_gifts_objects(any(), binary()) -> list().
%% @private
%% @doc Reads objects from a database.
%% @spec read_gifts_objects(Cl, Key) -> Objects
%% @end

read_gifts_objects(Cl, Key) ->
    read_objects(Cl, ?GIFTS_BUCKET, Key, []).


-spec read_objects(any(), binary(), list(), timeout() | any()) -> list().
%% @private
%% @spec read_objects(Cl, Bucket, Keys, Options) -> Objects
%% @doc Reads objects from a database. Beware! If an object is absent
%%      it will not be included to a result list.
%% @end

read_objects(Cl, Bucket, Keys, Options) ->
    F = fun(Key, Acc) ->
              case read_object(Cl, Bucket, Key, Options) of
                  {ok, Object} ->
                      [Object | Acc];
                  {error, notfound} ->
                      Acc
              end
        end,
    lists:foldl(F, [], Keys).

%% @private
%% @doc Reads an object from the database.
%% @spec read_object(Cl, Bucket, Key, Options) -> {ok, Object} | {error, Error}
%% @end

read_object(Cl, Bucket, Key, Options) ->
    Cl:get(Bucket, Key, Options).

