%%----------------------------------------------------------------------
%% @author Serge Polkovnikov <serge.polkovnikov@gmail.com>
%% @copyright Paynet Internet ve Bilisim Hizmetleri A.S. All Rights Reserved.
%% @doc
%% Interfaces for working with affiliates ficility
%% @end
%% Created: Aug 9, 2012
%%----------------------------------------------------------------------
-module(nsm_affiliates).
%% TODO: Move the lowlevel abstaction API to another module.
%% TODO: Create a separate application for affiliates fuctionality.
%% TODO: Fix functions specs according to edoc requirements.

%%
%% Include files
%%

-include("affiliates.hrl").
-include("membership_packages.hrl").
-include_lib("nsx_config/include/log.hrl").

%%
%% Exported Functions
%%

%% Service API
-export([
         init_db/0
        ]).

%% Client API
-export([
         start_client/0,
         stop_client/1,
         create_affiliate/1,
         create_affiliate/2,
         delete_affiliate/1,
         delete_affiliate/2,
         is_existing_affiliate/1,
         is_existing_affiliate/2,
         enable_to_look_details/1,
         enable_to_look_details/2,
         disable_to_look_details/1,
         disable_to_look_details/2,
         is_able_to_look_details/1,
         is_able_to_look_details/2,
         get_user_affiliate/1,
         get_user_affiliate/2,
         reg_follower/3,
         reg_follower/4,
         get_followers/1,
         get_followers/2,
         affiliates/0,
         affiliates/1,
         get_purchases_details/1,
         get_purchases_details/2,
         check_contract/6,
         check_contract/7,
         create_contract/6,
         create_contract/7,
         get_contracts/1,
         get_contracts/2,
         get_contract_types/0,
         get_contract_types/1,
         create_contract_type/4,
         create_contract_type/5,
         disable_contract_type/1,
         disable_contract_type/2,
         invitation_hook/2,
         purchase_hook/1
        ]).

%% For test proposes
-export([
         check_contracts_in_period/4,
         find_contract/3,
         find_contract_by_time/3,
         write_affiliate_rel_record/2,
         write_contract_type_record/2,
         write_contract_record/2,
         write_object/3,
         new_object/4,
         is_date_in_range/3,
         do_purchase/2
        ]).

%% Types specs

-type user_id() :: any().
-type contract_id() :: string().
-type contract_type_id() :: string().
-type db_handler() :: any().



-define(OWNER_INDEX, "owner_bin").
-define(CONTRACT_INDEX, "contract_bin").
-define(AFFILIATE_INDEX, "affiliate_bin").
-define(BUCKET_INDEX, "bucket_bin").

-define(CONTRACTS_COUNTER, <<"contract_id">>).
-define(CONTRACT_TYPES_COUNTER, <<"contract_type_id">>).

-define(MD_INDEX, <<"index">>).

%%
%% API Functions
%%

-spec init_db() -> ok.
%% @spec init_db() -> ok
%% @doc Initialize the data storage
%% @end

init_db() ->
%    ?INFO("~w:init_db/0: started", [?MODULE]),
%    C = start_riak_client(),
%    stop_riak_client(C),
%    ?INFO("~w:init_db/0: done", [?MODULE]),
    ok.


%% @doc Creates a handler to the database
%% @spec start_client() -> Handler
%% Types:
%%     Handler = db_handler()
%% @end
-spec start_client() -> db_handler().

start_client() ->
    start_riak_client().



%% @doc Releases a handler to the database
%% @spec start_client(Handler) -> ok
%% Types:
%%     Handler = db_handler()
%% @end
-spec stop_client(db_handler()) -> ok.

stop_client(Handler) ->
    stop_riak_client(Handler).



-spec create_affiliate(user_id()) -> ok.
%% @spec create_affiliate(OwnerId) -> ok
%% Types:
%%     OwnerId = user_id()
%% @doc Creates an affiliate for the user. The user become an owner
%%      of the affiliate. The function doesn't check is the passed
%%      user really registered in the base.
%% @end

create_affiliate(OwnerId) ->
    Handler = start_client(),
    create_affiliate(Handler, OwnerId),
    stop_client(Handler),
    ok.


-spec create_affiliate(db_handler, user_id()) -> ok.
%% @spec create_affiliate(Handler, OwnerId) -> ok
%% Types:
%%     Handler = db_handler()
%%     OwnerId = user_id()
%% @doc Creates an affiliate for the user. The user become an owner
%%      of the affiliate. The function doesn't check is the passed
%%      user really registered in the base.
%% @end

create_affiliate(Handler, OwnerId) ->
    create_new_affiliate_rel_record(Handler, OwnerId).


-spec delete_affiliate(user_id()) -> ok.
%% @spec delete_affiliate(OwnerId) -> ok
%% Types:
%%     OwnerId = user_id()
%% @doc Deletes an affiliate. The specified user should be an owner
%%      of the affiliate.
%%      Beware! Deletion executed in non-transactional way,
%%      so ghost records can appear.
%% @end

delete_affiliate(OwnerId) ->
    Handler = start_client(),
    Result = delete_affiliate(Handler, OwnerId),
    stop_client(Handler),
    Result.


-spec delete_affiliate(db_handler(), user_id()) -> ok.
%% @spec delete_affiliate(Handler, OwnerId) -> ok
%% Types:
%%     Handler = db_handler()
%%     OwnerId = user_id()
%% @doc Deletes an affiliate. The specified user should be an owner
%%      of the affiliate.
%%      Beware! Deletion executed in non-transactional way,
%%      so ghost records can appear.
%% @end

delete_affiliate(Handler, OwnerId) ->
    Keys = get_affiliate_followers(Handler, OwnerId),
    delete_rels_objects(Handler, Keys, []),
    ok.


-spec is_existing_affiliate(user_id()) -> boolean().
%% @spec is_existing_affiliate(OwnerId) -> boolean()
%% Types:
%%     OwnerId = user_id()
%% @doc Returns true if the specified affiliate exists.
%% @end

is_existing_affiliate(OwnerId) ->
    Handler = start_client(),
    Response = is_existing_affiliate(Handler, OwnerId),
    stop_riak_client(Handler),
    Response.


-spec is_existing_affiliate(db_handler(), user_id()) -> boolean().
%% @spec is_existing_affiliate(Handler, OwnerId) -> boolean()
%% Types:
%%     Handler = db_handler()
%%     OwnerId = user_id()
%% @doc Returns true if the specified affiliate exists.
%% @end

is_existing_affiliate(Handler, OwnerId) ->
    % To detect is the affiliate exists we use inderect sign:
    % if the user's record exists and his affiliate owner is
    % the user itself.
    case read_affiliate_record(Handler, term_to_binary(OwnerId), []) of
        {ok, #affiliates_rels{affiliate=OwnerId}} ->
            true;
        {ok, #affiliates_rels{}} ->
            false;
        {error, notfound} ->
            false
    end.


-spec enable_to_look_details(user_id()) -> ok | {error, any()}.
%% @spec enable_to_look_details(UserId) -> ok
%% Types:
%%     UserId = user_id()
%% @doc Enables the affilate owner to look details about
%%      the affilate state.
%% @end

enable_to_look_details(UserId) ->
    Handler = start_client(),
    Result = enable_to_look_details(Handler, UserId),
    stop_client(Handler),
    Result.

-spec enable_to_look_details(db_handler(), user_id()) -> ok | {error, any()}.
%% @spec enable_to_look_details(Handler, UserId) -> ok
%% Types:
%%     Handler = db_handler()
%%     UserId = user_id()
%% @doc Enables the affilate owner to look details about
%%      the affilate state.
%% @end

enable_to_look_details(Handler, UserId) ->
    Record = #affiliates_look_perms{user_id=UserId, enabled=true},
    ok = write_look_perms_record(Handler, Record).



-spec disable_to_look_details(user_id()) -> ok.
%% @spec disable_to_look_details(UserId) -> ok
%% Types:
%%     UserId = user_id()
%% @doc Disables the affilate owner to look details about
%%      the affilate state.
%% @end

disable_to_look_details(UserId) ->
    Handler = start_client(),
    Result = disable_to_look_details(Handler, UserId),
    stop_client(Handler),
    Result.


-spec disable_to_look_details(db_handler(), user_id()) -> ok.
%% @spec disable_to_look_details(Handler, UserId) -> ok
%% Types:
%%     Handler = db_handler()
%%     UserId = user_id()
%% @doc Disables the affilate owner to look details about
%%      the affilate state.
%% @end

disable_to_look_details(Handler, UserId) ->
    delete_look_perms_record(Handler, term_to_binary(UserId)).


-spec is_able_to_look_details(user_id()) -> boolean().
%% @spec is_able_to_look_details(UserId) -> boolean()
%% Types:
%%     UserId = user_id()
%% @doc Returns true if the user can look his affilate state.
%%      Before using, be sure that the user is the owner of an
%%      affiliate.
%% @end

is_able_to_look_details(UserId) ->
    Handler = start_client(),
    Result = is_able_to_look_details(Handler, UserId),
    stop_client(Handler),
    Result.

-spec is_able_to_look_details(db_handler(), user_id()) -> boolean().
%% @spec is_able_to_look_details(Handler, UserId) -> boolean()
%% Types:
%%     Handler = db_handler()
%%     UserId = user_id()
%% @doc Returns true if the user can look his affilate state.
%%      Before using, be sure that the user is the owner of an
%%      affiliate.
%% @end

is_able_to_look_details(Handler, UserId) ->
    case read_look_perms_record(Handler, term_to_binary(UserId)) of
        {ok, #affiliates_look_perms{enabled=true}} ->
            true;
        {ok, #affiliates_look_perms{enabled=false}} ->
            false;
        {error, notfound} ->
            false
    end.



-spec get_user_affiliate(user_id()) ->
          {ok, user_id(), non_neg_integer()} | {error, not_in_affiliate}.
%% @spec get_user_affiliate(UserId) -> {ok, Affiliate, Depth} |
%%                                     {error, not_in_affiliate}
%% Types:
%%     UserId = Affiliate = user_id()
%%     Depth = non_neg_integer()
%% @doc Return user's affiliate (and a position in the affiliate tree)
%%      if exists one.
%% @end

get_user_affiliate(UserId) ->
    Handler = start_client(),
    Response = get_user_affiliate(Handler, UserId),
    stop_client(Handler),
    Response.


-spec get_user_affiliate(db_handler(), user_id()) ->
          {ok, user_id(), non_neg_integer()} | {error, not_in_affiliate}.
%% @spec get_user_affiliate(Handler, UserId) -> {ok, Affiliate, Depth} |
%%                                              {error, not_in_affiliate}
%% Types:
%%     Handler = db_handler()
%%     UserId = Affiliate = user_id()
%%     Depth = non_neg_integer()
%% @doc Return user's affiliate (and a position in the affiliate tree)
%%      if exists one.
%% @end

get_user_affiliate(Handler, UserId) ->
    case read_affiliate_record(Handler, term_to_binary(UserId), []) of
        {ok, #affiliates_rels{affiliate=Affiliate,
                              depth=Depth}} ->
            {ok, Affiliate, Depth};
        {error, notfound} ->
            {error, not_in_affiliate}
    end.



-spec reg_follower(user_id(), user_id(), non_neg_integer()) -> ok | {error, any()}.
%% @spec reg_follower(UserId, OwnerId, Depth) -> ok | {error, Reason}
%% Types:
%%     UserId = OwnerId = user_id()
%%     Depth = non_neg_integer()
%%     Reason = no_such_affilate
%% @doc Registers passed user as a member of the affiliate.
%%      The function doesn't check is the user really registered in the base.
%% @end

reg_follower(UserId, OwnerId, Depth) ->
    Handler = start_client(),
    Response = reg_follower(Handler, UserId, OwnerId, Depth),
    stop_client(Handler),
    Response.

-spec reg_follower(db_handler(), user_id(), user_id(), non_neg_integer()) -> ok | {error, any()}.
%% @spec reg_follower(Handler, UserId, OwnerId, Depth) -> ok | {error, Reason}
%% Types:
%%     Handler = db_handler()
%%     UserId = OwnerId = user_id()
%%     Depth = non_neg_integer()
%%     Reason = no_such_affilate
%% @doc Registers passed user as a follower of the affiliate.
%%      The function doesn't check is the user really registered in the base.
%% @end

reg_follower(Handler, UserId, OwnerId, Depth) ->
    case is_existing_affiliate(OwnerId) of
        true ->
            Record = #affiliates_rels{user = UserId,
                                      affiliate = OwnerId,
                                      depth = Depth},
            write_affiliate_rel_record(Handler, Record);
        false ->
            {error, no_such_affilate}
    end.



-spec get_followers(user_id()) ->
          list({user_id(), non_neg_integer()}).
%% @spec get_followers(OwnerId) -> List
%% Types:
%%     OwnerId = user_id()
%%     List = [{user_id(), Depth}]
%%       Depth = non_neg_integer()
%% @doc Returns a list with information about members of the affiliate.
%% @end

get_followers(OwnerId) ->
    Handler = start_client(),
    Response = get_followers(Handler, OwnerId),
    stop_client(Handler),
    Response.


-spec get_followers(db_handler(), user_id()) ->
          list({user_id(), non_neg_integer()}).
%% @spec get_followers(Handler, OwnerId) -> List
%% Types:
%%     Handler = db_handler()
%%     OwnerId = user_id()
%%     List = [{user_id(), Depth}]
%%       Depth = non_neg_integer()
%% @doc Returns a list with information about members of the affiliate.
%% @end

get_followers(Handler, OwnerId) ->
    Keys = get_affiliate_followers(Handler, OwnerId),
    Objects = read_rels_objects(Handler, Keys, []),
    rels_objects_to_data(Objects).


-spec rels_objects_to_data(list()) -> list({user_id(), non_neg_integer()}).
%% @private

rels_objects_to_data(Objects) ->
    [begin
         #affiliates_rels{user = UserId,
                          depth = Depth} =
                             riak_object:get_value(Obj),
         {UserId, Depth}
     end
     || Obj <- Objects].




-spec affiliates() -> list().
%% @spec affiliates() -> List
%% Types:
%%     List = list(user_id())
%% @doc Returns a list of all affiliates.
%% @end

affiliates() ->
    Handler = start_client(),
    Response = affiliates(Handler),
    stop_client(Handler),
    Response.

-spec affiliates(db_handler()) -> list().
%% @spec affiliates(Handler) -> List
%% Types:
%%     Handler = db_handler()
%%     List = list(user_id())
%% @doc Returns a list of all affiliates.
%% @end

affiliates(Handler) ->
    Keys = get_affiliates_keys(Handler),
    [binary_to_term(Key) || Key <- Keys].


-spec get_purchases_details(contract_id()) ->
          list({user_id(), pos_integer(), non_neg_integer(), non_neg_integer(), list()}).
%% @spec get_purchases_details(ContractId) ->
%% @doc Gets a list of purchases assigned to the contract. See get_packages_details/2.
%% @end

get_purchases_details(ContractId) ->
    Handler = start_client(),
    Res = get_purchases_details(Handler, ContractId),
    stop_client(Handler),
    Res.

-spec get_purchases_details(db_handler(), contract_id()) ->
          list({user_id(), pos_integer(), non_neg_integer(), non_neg_integer(), list()}).
%% @spec get_purchases_details(Handler, ContractId) -> List
%% Types:
%%     ContractId = contract_id()
%%     Handler = db_handler()
%%     List = [{UserId, PurchasesNum, PurchasesSum, CommissionSum, PurchasesList}]
%%       UserId = user_id()
%%       PurchasesNum = pos_integer()
%%       PurchasesSum = non_neg_integer()  % Tl
%%       CommissionSum = non_neg_integer() % Tl * 100
%%       PurchasesList = [{PurchaseId, Date, PackageId, PackageNumber, PaymentType, Price, Commission}]
%%         Date = calendar:datetime()
%%         PurchaseId = any()
%%         PackageId = any()
%%         PackageNumber = integer()
%%         PaymentType = membership_packages:payment_type()
%%         Price = non_neg_integer()      % Tl
%%         Commission = non_neg_integer() % Tl*100
%% @doc Gets a list of purchases assigned to the contract.
%% @end

get_purchases_details(Handler, ContractId) ->
    Keys = get_purchases_keys_by_contract(Handler, ContractId),
    Objects = read_purchase_objects(Handler, Keys, []),
    purchase_objects_to_data(Objects).

-spec purchase_objects_to_data(list()) -> list().
%% @private

purchase_objects_to_data(Objects) ->
    [begin
         #affiliates_purchases{
                               user_id = UserId,
                               purchases_num = PurchasesNum,
                               purchases_sum = PurchasesSum,
                               commission_sum = CommissionSum,
                               purchases = PurchasesList
                              } = riak_object:get_value(Obj),
         {UserId, PurchasesNum, PurchasesSum, CommissionSum, PurchasesList}
     end
     || Obj <- Objects].



-spec create_contract(user_id(), string(), calendar:date(), calendar:date(),
                      all | pos_integer(), number()) ->
          ok | {error, {contracts_conflict, contract_id()}}.
%% @spec create_contract(UserId, Name, StartDate, FinishDate,
%%                       PurchaseLimit, CommissionRate) ->
%%                           ok | {error, {contracts_confilict, ContractId}}
%% Types:
%%     UserId = user_id()
%%     Name = string()
%%     StartDate = FinishDate = calendar:date()
%%     PurchaseLimit = all | pos_integer()
%%     CommissionRate = number()
%%     ContractId = contract_id()
%% @doc Creates new contract.
%% @end

create_contract(UserId, Name, StartDate, FinishDate,
                PurchaseLimit, CommissionRate) ->
    Handler = start_client(),
    Res = create_contract(Handler, UserId, Name, StartDate, FinishDate,
                          PurchaseLimit, CommissionRate),
    stop_client(Handler),
    Res.

-spec create_contract(db_handler(), user_id(), string(), calendar:date(),
                      calendar:date(), all | pos_integer(), number()) ->
          ok | {error, {contracts_conflict, contract_id()}}.
%% @spec create_contract(Handler, UserId, Name, StartDate, FinishDate,
%%                       PurchaseLimit, CommissionRate) ->
%%                           ok | {error, {contracts_confilict, ContractId}}
%% Types:
%%     Handler = db_handler()
%%     UserId = user_id()
%%     Name = string()
%%     StartDate = FinishDate = calendar:date()
%%     PurchaseLimit = all | pos_integer()
%%     CommissionRate = number()
%%     ContractId = contract_id()
%% @doc Creates new contract.
%% @end

create_contract(Handler, UserId, Name, {_,_,_}=StartDate, {_,_,_}=FinishDate,
                PurchaseLimit, CommissionPercent)
  when
  StartDate =< FinishDate,
  PurchaseLimit == all orelse
      (is_integer(PurchaseLimit) andalso PurchaseLimit > 0),
  CommissionPercent >= 0 ->
    case check_contracts_in_period(Handler, StartDate, FinishDate, UserId) of
        ok ->
            ContractId = contract_id(),
            Contract = #affiliates_contracts{id = ContractId,
                                             owner = UserId,
                                             name = Name,
                                             start_date = StartDate,
                                             finish_date = FinishDate,
                                             purchases_limit = PurchaseLimit,
                                             commission = CommissionPercent
                                             },
            write_contract_record(Handler, Contract);
        {error, ExContractId} ->
            {error, {contracts_conflict, ExContractId}}
    end.

%%akalenuk: I need this to deparate check from real data writing. Though this piece is poorly written and may need reengineering
check_contract(UserId, Name, StartDate, FinishDate, PurchaseLimit, CommissionRate) ->
    Handler = start_client(),
    Res = check_contract(Handler, UserId, Name, StartDate, FinishDate, PurchaseLimit, CommissionRate),
    stop_client(Handler),
    Res.

check_contract(Handler, UserId, Name, {_,_,_}=StartDate, {_,_,_}=FinishDate, PurchaseLimit, CommissionPercent)
  when
  StartDate =< FinishDate,
  PurchaseLimit == all orelse
      (is_integer(PurchaseLimit) andalso PurchaseLimit > 0),
  CommissionPercent >= 0 ->
    case check_contracts_in_period(Handler, StartDate, FinishDate, UserId) of
        ok ->
            ok;
        {error, ExContractId} ->
            {error, {contracts_conflict, ExContractId}}
    end.
%%


%% @spec get_contracts(UserId) -> List
%% Types:
%%     UserId = user_id()
%%     List = [{ContractId, ContractName, StartDate, FinishDate, Limit, Commission}]
%%       ContractId = contract_id(),
%%       ContractName = string(),
%%       StartDate = calendar:datetime(),
%%       FinishDate = calendar:datetime(),
%%       Limit = all | pos_integer()
%%       Commission = number()
%% @doc Returns a list of contracts for the user
%% @end

get_contracts(UserId) ->
    Handler = start_client(),
    Res = get_contracts(Handler, UserId),
    stop_client(Handler),
    Res.

%% @spec get_contracts(Handler, UserId) -> List
%% Types:
%%     Handler = db_handler()
%%     UserId = user_id()
%%     List = [{ContractId, ContractName, StartDate, FinishDate, Limit, Commission}]
%%       ContractId = contract_id(),
%%       ContractName = string(),
%%       StartDate = calendar:datetime(),
%%       FinishDate = calendar:datetime(),
%%       Limit = all | pos_integer()
%%       Commission = number()
%% @doc Returns a list of contracts for the user
%% @end

get_contracts(Handler, UserId) ->
    Keys = get_user_contracts_keys(Handler, UserId),
    Objects = read_contract_objects(Handler, Keys, []),
    contract_objects_to_data(Objects).


-spec contract_objects_to_data(list()) -> list().
%% @private

contract_objects_to_data(Objects) ->
    [begin
         #affiliates_contracts{id = ContractId,
                               name = Name,
                               owner = _UserId,
                               start_date = StartDate,
                               finish_date = FinishDate,
                               purchases_limit = Limit,
                               commission = Commision
                              } = riak_object:get_value(Obj),
         {ContractId, Name, StartDate, FinishDate, Limit, Commision}
     end
     || Obj <- Objects].


-spec get_contract_types() -> list().
%% @spec get_contract_types() -> List
%% Types:
%%     List = [{CTypeId, Name, Duration, Limit, Commission, Disabled}]
%%       CTypeId = contract_type_id()
%%       Name = string()
%%       Duration = pos_integer()
%%       Limit = all | pos_integer()
%%       Commission = number(),
%%       Disabled = boolean()
%% @doc Returns a list of contract types
%% @end

get_contract_types() ->
    Handler = start_client(),
    Res = get_contract_types(Handler),
    stop_client(Handler),
    Res.

-spec get_contract_types(db_handler()) -> list().
%% @spec get_contract_types(Handler) -> List
%% Types:
%%     Handler = db_handler()
%%     List = [{CTypeId, Name, Duration, Limit, Commission, Disabled}]
%%       CTypeId = contract_type_id()
%%       Name = string()
%%       Duration = pos_integer()
%%       Limit = all | pos_integer()
%%       Commission = number(),
%%       Disabled = boolean()
%% @doc Returns a list of contract types
%% @end

get_contract_types(Handler) ->
    Keys = get_ctypes_keys(Handler),
    Objects = read_ctypes_objects(Handler, Keys, []),
    ctypes_objects_to_data(Objects).


-spec ctypes_objects_to_data(list()) -> list().
%% @private

ctypes_objects_to_data(Objects) ->
    [begin
         #affiliates_contract_types
         {id = CTypeId,
          name = Name,
          duration = Duration,
          purchases_limit = Limit,
          commission = Commission,
          disabled = Disabled
         } = riak_object:get_value(Obj),
         {CTypeId, Name, Duration, Limit, Commission, Disabled}
     end
     || Obj <- Objects].


-spec create_contract_type(string(), non_neg_integer(), all | pos_integer(), number()) -> ok.
%% @spec create_contract_type(Name, Duration, PurchasesLimit, CommissionRate) -> ok
%% Types:
%%     Name = string()
%%     Duration = non_neg_integer()
%%     PurchaseLimit = all | pos_integer()
%%     Commission = number()
%% @doc Creates a new contract type.
%% @end

create_contract_type(Name, Duration, PurchasesLimit, CommissionRate) ->
    Handler = start_client(),
    Result = create_contract_type(Handler, Name, Duration,
                                  PurchasesLimit, CommissionRate),
    stop_client(Handler),
    Result.

-spec create_contract_type(db_handler(), string(), non_neg_integer(), all | pos_integer(), number()) -> ok.
%% @spec create_contract_type(Handler, Name, Duration, PurchasesLimit, CommissionRate) -> ok
%% Types:
%%     Handler = db_handler()
%%     Name = string()
%%     Duration = non_neg_integer()
%%     PurchaseLimit = all | pos_integer()
%%     Commission = number()
%% @doc Creates a new contract type.
%% @end

create_contract_type(Handler, Name, Duration, PurchasesLimit, Commission) ->
    TypeId = contract_type_id(),
    Record = #affiliates_contract_types{id = TypeId,
                                        name = Name,
                                        duration = Duration,
                                        purchases_limit = PurchasesLimit,
                                        commission = Commission,
                                        disabled = false},
    write_contract_type_record(Handler, Record).

-spec disable_contract_type(contract_type_id()) -> ok.
%% @spec disable_contract_type(TypeId) -> ok
%% Types:
%%     TypeId = contract_type_id()
%% @doc Disables the contract type.
%% @end

disable_contract_type(TypeId) ->
    Handler = start_client(),
    Result = disable_contract_type(Handler, TypeId),
    stop_client(Handler),
    Result.

-spec disable_contract_type(db_handler(), contract_type_id()) -> ok.
%% @spec disable_contract_type(Handler, TypeId) -> ok
%% Types:
%%     Handler = db_handler()
%%     TypeId = contract_type_id()
%% @doc Disables the contract type.
%% @end

disable_contract_type(Handler, TypeId) ->
    case read_contract_type_object(Handler, term_to_binary(TypeId)) of
        {ok, Object} ->
            Record = get_value(Object),
            NewRecord = Record#affiliates_contract_types{disabled = true},
            NewObject = update_value(Object, NewRecord),
            ok = write_object(Handler, NewObject, []);
        {error, notfound} ->
            ok
    end.


-spec invitation_hook(user_id(), user_id()) -> none().
%% @spec invitation_hook(Issuer, User) -> none()
%% Types:
%%     Issuer = User = user_id()
%% @doc This is a callback function that is invoked on
%% an invitation confirmation

invitation_hook(Issuer, UserId) ->
    ?INFO("~w:invitation_hook/1 Started. Issure: ~p User:~p",
          [?MODULE, Issuer, UserId]),
    Cl = start_client(),
    case get_user_affiliate(Cl, Issuer) of
        {ok, OwnerId, Depth} ->
            ?INFO("~w:invitation_hook/1 Assigning a user to an affiliate. "
                  "User:~p Affiliate:~p",
                  [?MODULE, UserId, OwnerId]),
            reg_follower(Cl, UserId, OwnerId, Depth+1);
        {error, not_in_affiliate} ->
            ?INFO("~w:invitation_hook/1 User did not assigned to any "
                  "affiliate. User:~p",
                  [?MODULE, UserId]),
            do_nothing
    end,
    stop_client(Cl).





purchase_hook(#membership_purchase{user_id = UserId,
                                   id = PurchaseId,
                                   state = done,
                                   end_time = Time1
                                  } = MP) ->
    ?INFO("~w:purchase_hook/1 Started. User: ~p PurchaseId: ~p",
          [?MODULE, UserId, PurchaseId]),
    Handler = start_client(),
    case get_user_affiliate(Handler, UserId) of
        {ok, Affiliate, _} ->
            {Date, _Time} = calendar:now_to_local_time(Time1),
            case find_contract(Handler, Affiliate, Date) of
                {ok, Contract} ->
                    store_purchase(Handler, Contract, UserId, MP);
                {error, notfound} ->
                    do_nothing
            end;
        {error, not_in_affiliate} ->
            do_nothing
    end,
    stop_client(Handler);

purchase_hook(_) ->
    do_nothing.


%%
%% Local Functions
%%

%% @private

do_purchase(UserId, PackageId) ->
    {ok, Package} = nsm_membership_packages:get_package(PackageId),
    {ok, MPId} = nsm_membership_packages:add_purchase(#membership_purchase{user_id=UserId,
                                                                               membership_package=Package
                                                                              }),
    ok = nsm_membership_packages:set_purchase_state(MPId, ?MP_STATE_DONE, undefined).


-spec contract_type_id() -> contract_type_id().
%% @private
%% @doc generate contract type id

contract_type_id() ->
    NextId = nsm_db:next_id(?CONTRACT_TYPES_COUNTER),
    lists:concat([timestamp(), "_", NextId]).

-spec contract_id() -> contract_id().
%% @private
%% @doc generate contract id

contract_id() ->
    NextId = nsm_db:next_id(?CONTRACTS_COUNTER),
    lists:concat([timestamp(), "_", NextId]).


%% @private

timestamp()->
    {Y, Mn, D} = erlang:date(),
    {H, M, S} = erlang:time(),
    lists:flatten(
      io_lib:format("~b~2..0b~2..0b_~2..0b~2..0b~2..0b", [Y, Mn, D, H, M, S])).


%% @private

check_contracts_in_period(Handler, StartDate, FinishDate, UserId) ->
    check_contracts_in_period2(Handler,
                               calendar:date_to_gregorian_days(StartDate),
                               calendar:date_to_gregorian_days(FinishDate),
                               UserId).


%% @private

check_contracts_in_period2(_Handler, Date, FinishDate, _UserId) 
  when Date > FinishDate ->
    ok;

check_contracts_in_period2(Handler, Date, FinishDate, UserId) ->
    GrDate = calendar:gregorian_days_to_date(Date),
    case find_contract(Handler, UserId, GrDate) of
        {ok, #affiliates_contracts{id = ContractId}} ->
            {error, ContractId};
        {error, notfound} ->
            check_contracts_in_period2(Handler, Date+1, FinishDate, UserId)
    end.


%% @private

store_purchase(Handler, Contract, UserId, MP) ->
    case get_user_purchases_data(Handler, Contract, UserId) of
        {ok, Object} ->
            add_contract_purchase(Handler, Contract, MP, Object);
        {error, notfound} ->
            create_contract_purchase(Handler, Contract, MP)
    end.



%% @private

add_contract_purchase(Handler, Contract, MPurchase, Object) ->
    add_contract_purchase(Handler, Contract, MPurchase, Object, 10).


%% @private

add_contract_purchase(_, _, #membership_purchase{id=PurchaseId}, _, 0) ->
    ?ERROR("~w:add_contract_purchase/5 Can't store a purchase info to a "
           "contract story. Purchase id:",
           [?MODULE, PurchaseId]),
    do_nothing;

add_contract_purchase(Handler, Contract, MPurchase, Object, Counter) ->
    ?INFO("~w:add_contract_purchase/5 Started. Counter:~w", [?MODULE, Counter]),
    #membership_purchase
    {
     id = PurchaseId,
     membership_package = Package,
     end_time = Time1
    } = MPurchase,

    #membership_package
    {
     id = PackageId,
     payment_type = PaymentType,
     no = PackageNumber,
     amount = Price
    } = Package,

    #affiliates_contracts
    {
     purchases_limit = PurchasesLimit,
     commission = CommissionPercent
    } = Contract,

    Purchases = riak_object:get_value(Object),
    #affiliates_purchases
    {
     purchases_num = CurPurchasesNum,
     purchases_sum = CurPurchasesSum,
     purchases = CurPurchasesList,
     commission_sum = CurCommissionSum
    } = Purchases,

    if PurchasesLimit == all;
       CurPurchasesNum < PurchasesLimit ->
           Time = calendar:now_to_local_time(Time1),
           Commission = round(Price*CommissionPercent),
           Purchase = {PurchaseId, Time, PackageId, PackageNumber, PaymentType, Price, Commission},
           NewPurchases =
               Purchases#affiliates_purchases
               {
                purchases_num = CurPurchasesNum + 1,
                purchases_sum = CurPurchasesSum + Price,
                commission_sum = CurCommissionSum + Commission,
                purchases = [Purchase | CurPurchasesList]
               },
           Obj1 = update_value(Object, NewPurchases),
           case write_object(Handler, Obj1, [if_not_modified]) of
               ok ->
                   do_nothing;
               {error, _} ->
                   add_contract_purchase(Handler, Contract, MPurchase, Object, Counter - 1)
           end;
       true ->
           do_nothing
    end.


%% @private

create_contract_purchase(Handler, Contract, MPurchase) ->
    #membership_purchase
    {
     id = PurchaseId,
     user_id = UserId,
     membership_package = Package,
     end_time = Time1
    } = MPurchase,

    #membership_package
    {
     id = PackageId,
     payment_type = PaymentType,
     no = PackageNumber,
     amount = Price
    } = Package,

    #affiliates_contracts
    {
     id = ContractId,
     owner = OwnerId,
     purchases_limit = PurchasesLimit,
     commission = CommissionPercent
    } = Contract,

    if PurchasesLimit == all;
       PurchasesLimit > 0 ->
           Time = calendar:now_to_local_time(Time1),
           Commission = round(Price*CommissionPercent),
           Purchase = {PurchaseId, Time, PackageId, PackageNumber,
                       PaymentType, Price, Commission},
           NewPurchases =
               #affiliates_purchases
               {
                user_id = UserId,
                contract_id = ContractId,
                purchases_num = 1,
                purchases_sum = Price,
                commission_sum = Commission,
                purchases = [Purchase]
               },
           Index = [{?OWNER_INDEX, term_to_binary(OwnerId)},
                    {?CONTRACT_INDEX, term_to_binary(ContractId)}
                   ],
           Object = new_object(?PURCHASES_BUCKET,
                               term_to_binary({ContractId, UserId}),
                               NewPurchases,
                               Index),
           case write_object(Handler, Object, [if_none_match]) of
               ok ->
                   do_nothing;
               {error, _} ->
                   store_purchase(Handler, Contract, UserId, MPurchase)
           end;
       true ->
           do_nothing
    end.


%% @private

find_contract(Handler, UserId, Date) ->
    ContractsKeys = get_user_contracts_keys(Handler, UserId),
    find_contract_by_time(Handler, ContractsKeys, Date).


%% @private

find_contract_by_time(_, [], _) ->
    {error, notfound};

find_contract_by_time(Handler, [ContractKey | Rest], Date) ->
    case read_contract_record(Handler, ContractKey, []) of
        {ok, #affiliates_contracts{start_date = StartDate,
                                   finish_date = FinishDate} = Contract} ->
            case is_date_in_range(Date, StartDate, FinishDate) of
                true ->
                    {ok, Contract};
                false ->
                    find_contract_by_time(Handler, Rest, Date)
            end;
        {error, notfound} ->
            find_contract_by_time(Handler, Rest, Date)
    end.


%% @private

is_date_in_range(Date, StartDate, FinishDate) ->
        Date >= StartDate andalso Date =< FinishDate.

-spec new_object(binary(), binary(), term(), list()) -> Object::term().
%% @private

new_object(Bucket, Key, Value, Index) ->
    Obj1 = riak_object:new(Bucket, Key, Value),
    Meta = dict:store(?MD_INDEX, Index, dict:new()),
    Obj2 = update_metadata(Obj1, Meta),
    Obj2.

%% @private

get_value(Object) ->
    riak_object:get_value(Object).

%% @private

update_value(Object, NewValue) ->
    riak_object:update_value(Object, NewValue).

%% @private

update_metadata(Object, Meta) ->
    riak_object:update_metadata(Object, Meta).

%% @private

get_user_purchases_data(Handler, Contract, UserId) ->
    ContractId = Contract#affiliates_contracts.id,
    case read_object(Handler,
                     ?PURCHASES_BUCKET,
                     term_to_binary({ContractId, UserId}),
                     []) of
        {ok, Object} ->
            {ok, Object};
        {error, notfound} ->
            {error, notfound}
    end.



-spec read_look_perms_record(any(), binary()) -> {ok, #affiliates_look_perms{}} | {error, notfound}.
%% @private

read_look_perms_record(Cl, Key) ->
    read_record(Cl, ?PERMS_BUCKET, Key, []).


-spec read_affiliate_record(any(), binary(), any()) -> {ok, #affiliates_rels{}} | {error, notfound}.
%% @private

read_affiliate_record(Cl, Key, Options) ->
    read_record(Cl, ?RELS_BUCKET, Key, Options).


-spec read_contract_record(any(), binary(), any()) -> {ok, #affiliates_contracts{}} | {error, notfound}.
%% @private

read_contract_record(Cl, Key, Options) ->
    read_record(Cl, ?CONTRACTS_BUCKET, Key, Options).

-spec read_record(any(), binary(), binary(), any()) -> {ok, any()} | {error, any()}.
%% @private

read_record(Cl, Bucket, Key, Options) ->
    case Cl:get(Bucket, Key, Options) of
        {ok, Object} ->
            Record = riak_object:get_value(Object),
            {ok, Record};
        {error, Error} ->
            {error, Error}
    end.



-spec write_affiliate_rel_record(any(), #affiliates_rels{}) -> ok.
%% @private

write_affiliate_rel_record(Cl,
                           #affiliates_rels{user = UserId,
                                            affiliate = OwnerId} = Record
                          ) ->
    Obj1 = riak_object:new(?RELS_BUCKET,
                           term_to_binary(UserId), Record),
    Indexes1 = [{?OWNER_INDEX, term_to_binary(OwnerId)}],
    Indexes2 = if UserId==OwnerId ->
                      [{?AFFILIATE_INDEX, <<1>>} | Indexes1];
                  true ->
                      Indexes1
               end,
    Meta = dict:store(?MD_INDEX, Indexes2, dict:new()),
    Obj2 = riak_object:update_metadata(Obj1, Meta),
    ok = Cl:put(Obj2, []).



-spec write_contract_record(any(), #affiliates_contracts{}) -> ok.
%% @private

write_contract_record(Cl,
                      #affiliates_contracts{id = ContractId,
                                            owner = OwnerId} = Record
                      ) ->
    Obj1 = riak_object:new(?CONTRACTS_BUCKET,
                           term_to_binary(ContractId), Record),
    Indexes = [{?OWNER_INDEX, term_to_binary(OwnerId)}],
    Meta = dict:store(?MD_INDEX, Indexes, dict:new()),
    Obj2 = riak_object:update_metadata(Obj1, Meta),
    ok = Cl:put(Obj2, []).



-spec write_contract_type_record(any(), #affiliates_contract_types{}) -> ok.
%% @private

write_contract_type_record(Cl,
                           #affiliates_contract_types{id = TypeId} = Record
                          ) ->
    Obj1 = riak_object:new(?CTYPES_BUCKET,
                           term_to_binary(TypeId), Record),
    Indexes = [{?BUCKET_INDEX, ?CTYPES_BUCKET}],
    Meta = dict:store(?MD_INDEX, Indexes, dict:new()),
    Obj2 = riak_object:update_metadata(Obj1, Meta),
    ok = Cl:put(Obj2, []).




-spec write_look_perms_record(any(), #affiliates_look_perms{}) -> ok.
%% @private

write_look_perms_record(Cl,
                        #affiliates_look_perms{user_id = UserId} = Record
                       ) ->
    Obj = riak_object:new(?PERMS_BUCKET,
                          term_to_binary(UserId), Record),
    ok = Cl:put(Obj, []).



-spec create_new_affiliate_rel_record(db_handler(), user_id()) -> ok.
%% @private

create_new_affiliate_rel_record(Cl, OwnerId) ->
    Record=#affiliates_rels{user=OwnerId,
                            affiliate=OwnerId,
                            depth=0},
    write_affiliate_rel_record(Cl, Record),
    ok.


-spec delete_look_perms_record(any(), binary()) -> ok.
%% @private

delete_look_perms_record(Cl, Key) ->
    ok = Cl:delete(?PERMS_BUCKET, Key, []).


-spec delete_rels_objects(any(), list(binary()), timeout() | any()) -> list({binary(), term()}).
%% @private

delete_rels_objects(Cl, Keys, Options) ->
    [{Key, Cl:delete(?RELS_BUCKET, Key, Options)}
       || Key <- Keys].


-spec read_rels_objects(any(), list(), timeout() | any()) -> list().
%% @private
%% @spec read_rels_objects(Cl, Keys, Options) -> Objects
%% @doc Reads objects from a database. Beware! If an object is absent
%%      it will not be included to a result list.
%% @end

read_rels_objects(Cl, Keys, Options) ->
    read_objects(Cl, ?RELS_BUCKET, Keys, Options).


-spec read_contract_objects(any(), list(), timeout() | any()) -> list().
%% @private
%% @spec read_contract_objects(Cl, Keys, Options) -> Objects
%% @doc Reads objects from a database. Beware! If an object is absent
%%      it will not be included to a result list.
%% @end

read_contract_objects(Cl, Keys, Options) ->
    read_objects(Cl, ?CONTRACTS_BUCKET, Keys, Options).

-spec read_ctypes_objects(any(), list(), timeout() | any()) -> list().
%% @private
%% @spec read_ctypes_objects(Cl, Keys, Options) -> Objects
%% @doc Reads objects from a database. Beware! If an object is absent
%%      it will not be included to a result list.
%% @end

read_ctypes_objects(Cl, Keys, Options) ->
    read_objects(Cl, ?CTYPES_BUCKET, Keys, Options).

-spec read_purchase_objects(any(), list(), timeout() | any()) -> list().
%% @private
%% @spec read_purchase_objects(Cl, Keys, Options) -> Objects
%% @doc Reads objects from a database. Beware! If an object is absent
%%      it will not be included to a result list.
%% @end

read_purchase_objects(Cl, Keys, Options) ->
    read_objects(Cl, ?PURCHASES_BUCKET, Keys, Options).


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


-spec read_contract_type_object(any(), binary()) ->
          {ok, any()} | {error, any()}.
%% @private
%% @doc Reads object from a database.
%% @spec read_contract_type_object(Cl, Key) -> {ok, Object} | {error, Error}
%% @end

read_contract_type_object(Cl, Key) ->
    read_object(Cl, ?CTYPES_BUCKET, Key, []).


-spec read_object(any(), binary(), binary(), timeout() | any()) ->
          {ok, any()} | {error, any()}.
%% @private
%% @doc Reads an object from the database.
%% @spec read_object(Cl, Bucket, Key, Options) -> {ok, Object} | {error, Error}
%% @end

read_object(Cl, Bucket, Key, Options) ->
    Cl:get(Bucket, Key, Options).


-spec write_object(any(), binary(), timeout() | any()) -> ok | {error, any()}.
%% @private
%% @doc Writes an object to the database.
%% @spec write_object(Cl, Object, Options) -> ok | {error, Error}
%% @end

write_object(Cl, Object, Options) ->
    Cl:put(Object, Options).



%% @private
-spec get_affiliate_followers(any(), user_id()) -> list(binary()).

get_affiliate_followers(Cl, OwnerId) ->
    {ok, Keys} =
        Cl:get_index(?RELS_BUCKET,
                     {eq, <<?OWNER_INDEX>>, term_to_binary(OwnerId)}),
    Keys.


%% @private
-spec get_affiliates_keys(any()) -> list(binary()).

get_affiliates_keys(Cl) ->
    {ok, Keys} =
        Cl:get_index(?RELS_BUCKET, {eq, <<?AFFILIATE_INDEX>>, <<1>>}),
    Keys.


%% @private
-spec get_user_contracts_keys(any(), user_id()) -> list(binary()).

get_user_contracts_keys(Cl, OwnerId) ->
    {ok, Keys} =
        Cl:get_index(?CONTRACTS_BUCKET,
                     {eq, <<?OWNER_INDEX>>, term_to_binary(OwnerId)}),
    Keys.


%% @private
-spec get_ctypes_keys(any()) -> list(binary()).

get_ctypes_keys(Cl) ->
    {ok, Keys} =
        Cl:get_index(?CTYPES_BUCKET, {eq, <<?BUCKET_INDEX>>, ?CTYPES_BUCKET}),
    Keys.



%% @private
-spec get_purchases_keys_by_contract(any(), any()) -> list(binary()).

get_purchases_keys_by_contract(Cl, ContractId) ->
    {ok, Keys} =
        Cl:get_index(?PURCHASES_BUCKET,
                     {eq, <<?CONTRACT_INDEX>>, term_to_binary(ContractId)}),
    Keys.


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






