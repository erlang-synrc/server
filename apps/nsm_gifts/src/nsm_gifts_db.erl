%%----------------------------------------------------------------------
%% @author Serge Polkovnikov <serge.polkovnikov@gmail.com>
%% @copyright Paynet Internet ve Bilisim Hizmetleri A.S. All Rights Reserved.
%% @doc
%% Interfaces for working with the gifts database.
%% @end
%% Created: Sep 7, 2012
%%----------------------------------------------------------------------
-module(nsm_gifts_db).

%%
%% Include files
%%
-include_lib("nsx_config/include/log.hrl").
-include("db.hrl").
-include("common.hrl").

-record(config,{key,value}).

%%
%% Exported Functions
%%
-export([
         init_db/0, init_indexes/0
        ]).

-export([
         start_client/0,
         stop_client/1,
         set_factors/4,
         set_factors/5,
         get_factors/0,
         get_factors/1,
         create_category/3,
         create_category/4,
         create_gift/1,
         create_gift/2,
         get_all_categories/0,
         get_all_categories/1,
         get_all_gifts/0,
         get_all_gifts/1,
         get_categories/1,
         get_categories/2,
         get_gifts/1,
         get_gifts/2,
         get_category/1,
         get_category/2,
         get_gift/1,
         get_gift/2,
         update_category/2,
         update_category/3,
         update_gift/2,
         update_gift/3
        ]).

-export([
         clear_gifts/0,
         clear_gifts/1
        ]).

-type db_handler() :: any().

%% Buckets
-define(GIFTS_BUCKET, <<"gifts">>).
-define(CATEGORIES_BUCKET, <<"gifts_categories">>).

%% Counters
-define(GIFTS_COUNTER, <<"gift_id">>).
-define(CATEGORIES_COUNTER, <<"category_id">>).

%% Indices
-define(BUCKET_INDEX, "bucket_bin").
-define(CATEGORY_INDEX, "categoty_bin").
-define(CATPARENT_INDEX, "catparent_bin").

%% Meta Id
-define(MD_INDEX, <<"index">>).

%% Conf parameters
-define(CONF_FACTOR_A, "gifts/factor_a").
-define(CONF_FACTOR_B, "gifts/factor_b").
-define(CONF_FACTOR_C, "gifts/factor_c").
-define(CONF_FACTOR_D, "gifts/factor_d").

%%
%% API Functions
%%
-spec init_db() -> ok.
%% @spec init_db() -> ok
%% @doc Initialize the data storage
%% @end

init_indexes() ->
    C = start_riak_client(),
    ok = C:set_bucket(?GIFTS_BUCKET, [{backend, leveldb_backend}]),
    ok = C:set_bucket(?CATEGORIES_BUCKET, [{backend, leveldb_backend}]).

init_db() ->
%    ?INFO("~w:init_db/0: started", [?MODULE]),
    C = start_riak_client(),
    ok = init_conf(C, ?CONF_FACTOR_A, 1.15, []),
    ok = init_conf(C, ?CONF_FACTOR_B, 4, []),
    ok = init_conf(C, ?CONF_FACTOR_C, 100, []),
    ok = init_conf(C, ?CONF_FACTOR_D, 0.2, []),
    stop_riak_client(C),
%    ?INFO("~w:init_db/0: done", [?MODULE]),
    ok.

-spec start_client() -> db_handler().
%% @spec start_client() -> Handler
%% @doc
%% Types:
%%     Handler = db_handler()
%% Creates a handler to the database.
%% @end

start_client() ->
    start_riak_client().



-spec stop_client(db_handler()) -> ok.
%% @spec start_client(Handler) -> ok
%% @doc
%% Types:
%%     Handler = db_handler()
%% Releases a handler to the database.
%% @end

stop_client(Handler) ->
    stop_riak_client(Handler).


%% @spec set_factors(A, B, C, D) -> ok
%% @doc
%% Types:
%%     A = B = C = D = number()
%% Sets values of factors that used in gifts prices calculations.
%% @end

set_factors(A, B, C, D) ->
    Handler = start_riak_client(),
    Res = set_factors(Handler, A, B, C, D),
    stop_riak_client(Handler),
    Res.

set_factors(Handler, A, B, C, D) ->
    ok = set_conf_val(Handler, ?CONF_FACTOR_A, A),
    ok = set_conf_val(Handler, ?CONF_FACTOR_B, B),
    ok = set_conf_val(Handler, ?CONF_FACTOR_C, C),
    ok = set_conf_val(Handler, ?CONF_FACTOR_D, D).


%% @spec get_factors() -> {A, B, C, D}
%% @doc
%% Types:
%%     A = B = C = D = number()
%% Gets values of factors that used in gifts prices calculations.
%% @end

get_factors() ->
    Handler = start_riak_client(),
    Res = get_factors(Handler),
    stop_riak_client(Handler),
    Res.

get_factors(Handler) ->
    {ok, A} = case get_conf_val(Handler, ?CONF_FACTOR_A)  of {ok,_A} -> {ok,_A}; _ -> {ok, 1.15} end,
    {ok, B} = case get_conf_val(Handler, ?CONF_FACTOR_B)  of {ok,_B} -> {ok,_B}; _ -> {ok, 4} end,
    {ok, C} = case get_conf_val(Handler, ?CONF_FACTOR_C)  of {ok,_C} -> {ok,_C}; _ -> {ok, 100} end,
    {ok, D} = case get_conf_val(Handler, ?CONF_FACTOR_D)  of {ok,_D} -> {ok,_D}; _ -> {ok, 0.2} end,
    {A, B, C, D}.

%% @spec create_category(Name, Description, ParentId) -> ok | {error, Reason}
%% @doc
%% Types:
%%     Name = Description = binary()
%%     Parentid = undefined | integer()
%%     Reason = term()
%% @end

create_category(Name, Description, ParentId) ->
    Handler = start_riak_client(),
    Res = create_category(Handler, Name, Description, ParentId),
    stop_riak_client(Handler),
    Res.

%% @spec create_category(Handler, Name, Description, ParentId) ->
%%                                              ok | {error, Reason}
%% @doc
%% Types:
%%     Handler = db_handler()
%%     Name = Description = binary()
%%     Parentid = undefined | integer()
%%     Reason = term()
%% @end

create_category(Cl, Name, Description, ParentId) ->
    Id = category_id(),
    Record=#gifts_category{id = Id,
                           name = Name,
                           description = Description,
                           parent = ParentId},
    Obj1 = riak_object:new(?CATEGORIES_BUCKET, term_to_binary(Id), Record),
    Indices = [{?BUCKET_INDEX, ?CATEGORIES_BUCKET},
               {?CATPARENT_INDEX, term_to_binary(ParentId)}],
    Meta = dict:store(?MD_INDEX, Indices, dict:new()),
    Obj2 = riak_object:update_metadata(Obj1, Meta),
    ok = Cl:put(Obj2, []).


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

create_gift(Cl, #gift{categories = Cats} = Record) ->
    Id = gift_id(),
    Record2 = Record#gift{id = Id},
    Obj1 = riak_object:new(?GIFTS_BUCKET, term_to_binary(Id), Record2),
    Indices = [{?BUCKET_INDEX, ?GIFTS_BUCKET} |
                 [{?CATEGORY_INDEX, term_to_binary(CatId)} ||
                  CatId <- lists:usort(Cats)]],
    Meta = dict:store(?MD_INDEX, Indices, dict:new()),
    Obj2 = riak_object:update_metadata(Obj1, Meta),
    ok = Cl:put(Obj2, []).

%% @spec get_all_categories() -> List
%% @doc
%% Types:
%%     List = list({CatRec, Obj})
%%       CatRec = #gifts_category{}
%%       Obj = term()
%% Returns a list of categories.
%% The Obj term is opaque value and should not be analysed. The Obj
%% is used for performing update_category/2,3 calls.
%% @end

get_all_categories() ->
    Handler = start_riak_client(),
    Res = get_all_categories(Handler),
    stop_riak_client(Handler),
    Res.

%% @spec get_all_categories(Handler) -> List
%% @doc
%% Type:
%%     Handler = db_handler()
%%     List = list({CatRec, Obj})
%%       CatRec = #gifts_category{}
%%       Obj = term()
%% Returns a list of categories.
%% The Obj term is opaque value and should not be analysed. The Obj
%% is used for performing update_category/2,3 calls.
%% @end

get_all_categories(Handler) ->
    Keys = get_all_categories_keys(Handler),
    Objects = read_objects(Handler, ?CATEGORIES_BUCKET, Keys, []),
    categories_objects_to_data(Objects).


%% @spec get_all_gifts() -> List
%% @doc
%% Types:
%%     List = list({GiftRec, Obj})
%%       GiftRec = #gift{}
%%       Obj = term()
%% Returns a list of gifts.
%% The Obj term is opaque value and should not be analysed. The Obj
%% is used for performing update_gift/2,3 calls.
%% @end

get_all_gifts() ->
    Handler = start_riak_client(),
    Res = get_all_gifts(Handler),
    stop_riak_client(Handler),
    Res.

%% @spec get_all_gifts(Handler) -> List
%% @doc
%% Type:
%%     Handler = db_handler()
%%     List = list({GiftRec, Obj})
%%       GiftRec = #gift{}
%%       Obj = term()
%% Returns a list of gifts.
%% The Obj term is opaque value and should not be analysed. The Obj
%% is used for performing update_gift/2,3 calls.
%% @end

get_all_gifts(Handler) ->
    Keys = get_all_gifts_keys(Handler),
    Objects = read_objects(Handler, ?GIFTS_BUCKET, Keys, []),
    gifts_objects_to_data(Objects).

%% @spec get_categories(ParentId) -> List
%% @doc
%% Types:
%%     ParentId = undefined | integer()
%%     List = list({CatRec, Obj})
%%       CatRec = #gifts_category{}
%%       Obj = term()
%% Returns a list of categories which belong to the specified parent category.
%% To get top level categories pass atom 'undefined' as a ParentId.
%% The Obj term is opaque value and should not be analysed. The Obj
%% is used for performing update_category/2,3 calls.
%% @end

get_categories(ParentId) ->
    Handler = start_riak_client(),
    Res = get_categories(Handler, ParentId),
    stop_riak_client(Handler),
    Res.

%% @spec get_categories(Handler, ParentId) -> List
%% @doc
%% Types:
%%     Handler = db_handler()
%%     ParentId = undefined | integer()
%%     List = list({CatRec, Obj})
%%       CatRec = #gifts_category{}
%%       Obj = term()
%% Returns a list of categories which belong to the specified parent category.
%% To get top level categories pass atom 'undefined' as a ParentId.
%% The Obj term is opaque value and should not be analysed. The Obj
%% is used for performing update_category/2,3 calls.
%% @end

get_categories(Handler, ParentId) ->
    Keys = get_cats_keys_by_parent(Handler, ParentId),
    Objects = read_objects(Handler, ?CATEGORIES_BUCKET, Keys, []),
    categories_objects_to_data(Objects).


-spec categories_objects_to_data(list()) -> list({#gifts_category{}, term()}).
%% @private

categories_objects_to_data(Objects) ->
    [begin
         {riak_object:get_value(Obj), Obj}
     end
     || Obj <- Objects].


%% @spec get_gifts(Category) -> List
%% @doc
%% Types:
%%     Category = integer()
%%     List = list({GiftRec, Obj})
%%       GiftRec = #gift{}
%%       Obj = term()
%% Returns a list of gifts which belong to the specified category.
%% The Obj term is opaque value and should not be analysed. The Obj
%% is used for performing update_gift/2,3 calls.
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
%%     List = list({GiftRec, Obj})
%%       GiftRec = #gift{}
%%       Obj = term()
%% Returns a list of gifts which belong to the specified category.
%% The Obj term is opaque value and should not be analysed. The Obj
%% is used for performing update_gift/2,3 calls.
%% @end

get_gifts(Handler, Category) ->
    Keys = get_gifts_keys_by_cat(Handler, Category),
    Objects = read_objects(Handler, ?GIFTS_BUCKET, Keys, []),
    gifts_objects_to_data(Objects).


-spec gifts_objects_to_data(list()) -> list({#gift{}, term()}).
%% @private

gifts_objects_to_data(Objects) ->
    [begin
         {riak_object:get_value(Obj), Obj}
     end
     || Obj <- Objects].

%% @spec get_category(CatId) -> {ok, {CatRec, Obj}} |
%%                           {error, notfound}
%% @doc
%% Types:
%%     CatId = integer()
%%     CatRec = #gifts_category{}
%%     Obj = term()
%% @end

get_category(CatId) ->
    Handler = start_riak_client(),
    Res = get_category(Handler,CatId),
    stop_riak_client(Handler),
    Res.

%% @spec get_category(Handler, CatId) -> {ok, {CatRec, Obj}} |
%%                                       {error, notfound}
%% @doc
%% Type:
%%     Handler = db_handler()
%%     CatId = integer()
%%     CatRec = #gifts_category{}
%%     Obj = term()
%% @end

get_category(Handler, CatId) ->
    case read_object(Handler, ?CATEGORIES_BUCKET, term_to_binary(CatId), []) of
        {ok, Object} ->
            Rec = riak_object:get_value(Object),
            {ok, {Rec, Object}};
        {error, notfound} ->
            {error, notfound}
    end.

%% @spec get_gift(GiftId) -> {ok, {GiftRec, Obj}} |
%%                           {error, notfound}
%% @doc
%% Types:
%%     GiftId = integer()
%%     GiftRec = #gift{}
%%     Obj = term()
%% @end

get_gift(GiftId) ->
    Handler = start_riak_client(),
    Res = get_gift(Handler, GiftId),
    stop_riak_client(Handler),
    Res.

%% @spec get_gift(Handler, GiftId) -> {ok, {GiftRec, Obj}} |
%%                                    {error, notfound}
%% @doc
%% Type:
%%     Handler = db_handler()
%%     GiftId = integer()
%%     GiftRec = #gift{}
%%     Obj = term()
%% @end

get_gift(Handler, GiftId) ->
    case read_object(Handler, ?GIFTS_BUCKET, term_to_binary(GiftId), []) of
        {ok, Object} ->
            Rec = riak_object:get_value(Object),
            {ok, {Rec, Object}};
        {error, notfound} ->
            {error, notfound}
    end.



%% @spec update_gift(GiftRec, Object) -> ok | {error, Reason}
%% @doc
%% Types:
%%     GiftRec = #gift{}
%%     Object = term()
%% @end

update_gift(GiftRec, Object) ->
    Handler = start_riak_client(),
    Res = update_gift(Handler, GiftRec, Object),
    stop_riak_client(Handler),
    Res.

%% @spec update_gift(Handler, GiftRec, Object) -> ok | {error, Reason}
%% @doc
%% Types:
%%     Handler = db_handler()
%%     GiftRec = #gift{}
%%     Object = term()
%% @end

update_gift(Handler, #gift{categories = Cats} = GiftRec, Object) ->
    Obj1 = riak_object:update_value(Object, GiftRec),
    Indices = [{?BUCKET_INDEX, ?GIFTS_BUCKET} |
                   [{?CATEGORY_INDEX, term_to_binary(CatId)} ||
                    CatId <- lists:usort(Cats)]],
    Meta = dict:store(?MD_INDEX, Indices, dict:new()),
    Obj2 = riak_object:update_metadata(Obj1, Meta),

    case write_object(Handler, Obj2, [if_not_modified]) of
        ok ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%% @spec update_category(CatRec, Object) -> ok | {error, Reason}
%% @doc
%% Types:
%%     CatRec = #gifts_category{}
%%     Object = term()
%% @end

update_category(CatRec, Object) ->
    Handler = start_riak_client(),
    Res = update_category(Handler, CatRec, Object),
    stop_riak_client(Handler),
    Res.

%% @spec update_category(Handler, CatRec, Object) -> ok | {error, Reason}
%% @doc
%% Types:
%%     Handler = db_handler()
%%     CatRec = #gifts_category{}
%%     Object = term()
%% @end

update_category(Handler,
                #gifts_category{parent = ParentId
                               } = CatRec,
                Object) ->
    Obj1 = riak_object:update_value(Object, CatRec),
    Indices = [{?BUCKET_INDEX, ?CATEGORIES_BUCKET},
               {?CATPARENT_INDEX, term_to_binary(ParentId)}],
    Meta = dict:store(?MD_INDEX, Indices, dict:new()),
    Obj2 = riak_object:update_metadata(Obj1, Meta),

    case write_object(Handler, Obj2, [if_not_modified]) of
        ok ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.


%% @spec clear_gifts() -> ok
%% @doc
%% @end

clear_gifts() ->
    Handler = start_riak_client(),
    Res = clear_gifts(Handler),
    stop_riak_client(Handler),
    Res.

%% @spec clear_gifts(Handler) -> ok
%% @doc
%% Type:
%%     Handler = db_handler()
%% @end

clear_gifts(Handler) ->
    Keys = get_all_gifts_keys(Handler),
    ok = delete_objects(Handler, ?GIFTS_BUCKET, Keys, []).



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

-spec init_conf(any(), binary(), term(), list()) -> ok.
%% @private
%% @spec init_conf(Cl, Key, InitVal, Options) -> ok
%% @end

init_conf(Cl, Key, InitVal, Options) ->
    Force = proplists:get_value(force, Options, false),
    if Force ->
           ok = set_conf_val(Cl, Key, InitVal);
       true ->
           case get_conf_val(Cl, Key) of
               {ok, _} ->
                   ok;
               {error, notfound} ->
                   ok = set_conf_val(Cl, Key, InitVal)
           end
    end.


category_id() ->
    nsm_db:next_id(?CATEGORIES_COUNTER).

gift_id() ->
    nsm_db:next_id(?GIFTS_COUNTER).


%% @private
get_conf_val(_Cl, Key) ->
    case nsm_db:get(config, Key) of
        {ok, {_, _, Val}} ->
            {ok, Val};
        {error, notfound} ->
            {error, notfound}
    end.


%% @private
set_conf_val(_Cl, Key, Value) ->
    Rec = #config{key=Key, value=Value},
    nsm_db:put(Rec).

%% @private
-spec get_gifts_keys_by_cat(any(), integer()) -> list(binary()).

get_gifts_keys_by_cat(Cl, CatId) ->
    {ok, Keys} =
        Cl:get_index(?GIFTS_BUCKET,
                     {eq, <<?CATEGORY_INDEX>>, term_to_binary(CatId)}),
    Keys.

%% @private
-spec get_cats_keys_by_parent(any(), integer()) -> list(binary()).

get_cats_keys_by_parent(Cl, ParentId) ->
    {ok, Keys} =
        Cl:get_index(?CATEGORIES_BUCKET,
                     {eq, <<?CATPARENT_INDEX>>, term_to_binary(ParentId)}),
    Keys.

%% @private
-spec get_all_gifts_keys(any()) -> list(binary()).

get_all_gifts_keys(Cl) ->
    {ok, Keys} =
        Cl:get_index(?GIFTS_BUCKET, {eq, <<?BUCKET_INDEX>>, ?GIFTS_BUCKET}),
    Keys.

%% @private
-spec get_all_categories_keys(any()) -> list(binary()).

get_all_categories_keys(Cl) ->
    {ok, Keys} =
        Cl:get_index(?CATEGORIES_BUCKET,
                     {eq, <<?BUCKET_INDEX>>, ?CATEGORIES_BUCKET}),
    Keys.


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


-spec delete_objects(any(), binary(), list(), timeout() | any()) -> ok.
%% @private
%% @spec delete_objects(Cl, Bucket, Keys, Options) -> Objects
%% @doc Delete objects from a database.
%% @end

delete_objects(Cl, Bucket, Keys, Options) ->
    [delete_object(Cl, Bucket, Key, Options) ||
       Key <- Keys],
    ok.

%% @private
%% @doc Reads an object from the database.
%% @spec read_object(Cl, Bucket, Key, Options) -> {ok, Object} | {error, Error}
%% @end

read_object(Cl, Bucket, Key, Options) ->
    Cl:get(Bucket, Key, Options).


%% @private
%% @doc Writes an object to the database.
%% @spec write_object(Cl, Object, Options) -> ok | {error, Error}
%% @end

write_object(Cl, Object, Options) ->
    Cl:put(Object, Options).


%% @private
%% @doc Deletes an object from the database.
%% @spec delete_object(Cl, Bucket, Key, Options) -> ok | {error, Error}
%% @end

delete_object(Cl, Bucket, Key, Options) ->
    Cl:delete(Bucket, Key, Options).

