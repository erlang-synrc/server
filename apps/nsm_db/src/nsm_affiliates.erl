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
         create_affiliate/1,
         delete_affiliate/1,
         is_existing_affiliate/1,
         enable_to_look_details/1,
         disable_to_look_details/1,
         is_able_to_look_details/1,
         get_user_affiliate/1,
         reg_follower/3,
         get_followers/1,
         affiliates/0,
         get_purchases_details/1,
         check_contract/6,
         create_contract/6,
         get_contracts/1,
         find_contract/2,
         get_contract_types/0,
         create_contract_type/4,
         disable_contract_type/1,
         invitation_hook/2,
         purchase_hook/1
        ]).

%% For test proposes
-export([
         check_contracts_in_period/3,
%%         find_contract/3,
%%         find_contract_by_time/3,
%%         write_affiliate_rel_record/2,
%%         write_contract_type_record/2,
%%         write_contract_record/2,
%%         write_object/3,
%%         new_object/4,
         is_date_in_range/3,
         do_purchase/2
        ]).

%% Types specs

-type user_id() :: any().
-type contract_id() :: string().
-type contract_type_id() :: string().


-define(RELS_TABLE, affiliates_rels).
-define(CONTRACTS_TABLE, affiliates_contracts).
-define(CTYPES_TABLE, affiliates_contract_types).
-define(PERMS_TABLE, affiliates_look_pems).
-define(PURCHASES_TABLE, affiliates_purchases).

-define(CTYPES_BUCKET, <<"affiliates_contract_types">>).

-define(OWNER_INDEX, <<"owner_bin">>).
-define(CONTRACT_INDEX, <<"contract_bin">>).
-define(AFFILIATE_INDEX, <<"affiliate_bin">>).
-define(BUCKET_INDEX, <<"bucket_bin">>).

-define(CONTRACTS_COUNTER, <<"contract_id">>).
-define(CONTRACT_TYPES_COUNTER, <<"contract_type_id">>).

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


-spec create_affiliate(user_id()) -> ok.
%% @spec create_affiliate(OwnerId) -> ok
%% Types:
%%     OwnerId = user_id()
%% @doc Creates an affiliate for the user. The user become an owner
%%      of the affiliate. The function doesn't check is the passed
%%      user really registered in the base.
%% @end

create_affiliate(OwnerId) ->
    Record=#affiliates_rels{user=OwnerId,
                            affiliate=OwnerId,
                            depth=0},
    ok = nsm_db:put(Record).


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
    nsm_db:delete_by_index(?RELS_TABLE, ?OWNER_INDEX, OwnerId),
    ok.


-spec is_existing_affiliate(user_id()) -> boolean().
%% @spec is_existing_affiliate(OwnerId) -> boolean()
%% Types:
%%     OwnerId = user_id()
%% @doc Returns true if the specified affiliate exists.
%% @end

is_existing_affiliate(OwnerId) ->
    % To detect is the affiliate exists we use inderect sign:
    % if the user's record exists and his affiliate owner is
    % the user itself.
    case nsm_db:get(?RELS_TABLE, OwnerId) of
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
    Record = #affiliates_look_perms{user_id=UserId, enabled=true},
    ok = nsm_db:put(Record).



-spec disable_to_look_details(user_id()) -> ok.
%% @spec disable_to_look_details(UserId) -> ok
%% Types:
%%     UserId = user_id()
%% @doc Disables the affilate owner to look details about
%%      the affilate state.
%% @end

disable_to_look_details(UserId) ->
    ok = nsm_db:delete(?CTYPES_TABLE, UserId),
    ok.


-spec is_able_to_look_details(user_id()) -> boolean().
%% @spec is_able_to_look_details(UserId) -> boolean()
%% Types:
%%     UserId = user_id()
%% @doc Returns true if the user can look his affilate state.
%%      Before using, be sure that the user is the owner of an
%%      affiliate.
%% @end

is_able_to_look_details(UserId) ->
    case nsm_db:get(?PERMS_TABLE, UserId) of
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
    case nsm_db:get(?RELS_TABLE, UserId) of
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
%%      Beware: The function doesn't check is the user really registered in the base.
%% @end

reg_follower(UserId, OwnerId, Depth) ->
    case is_existing_affiliate(OwnerId) of
        true ->
            Record = #affiliates_rels{user = UserId,
                                      affiliate = OwnerId,
                                      depth = Depth},
            ok = nsm_db:put(Record);
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
    Records = nsm_db:all_by_index(?RELS_TABLE, ?OWNER_INDEX, OwnerId),
    [{UserId, Depth} || #affiliates_rels{user = UserId, depth = Depth} <- Records].


-spec affiliates() -> list().
%% @spec affiliates() -> List
%% Types:
%%     List = list(user_id())
%% @doc Returns a list of all affiliates.
%% @end

affiliates() ->
    Records = nsm_db:all_by_index(?RELS_TABLE, ?AFFILIATE_INDEX, 1),
    [Affiliate || #affiliates_rels{affiliate = Affiliate} <- Records].


-spec get_purchases_details(contract_id()) ->
          list({user_id(), pos_integer(), non_neg_integer(), non_neg_integer(), list()}).
%% @spec get_purchases_details(ContractId) -> List
%% Types:
%%     ContractId = contract_id()
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
%%         Price = non_neg_integer()      % Tl*100
%%         Commission = non_neg_integer() % Tl*100
%% @doc Gets a list of purchases assigned to the contract.
%% @end

get_purchases_details(ContractId) ->
    PurchasesList = nsm_db:all_by_index(?PURCHASES_TABLE, ?CONTRACT_INDEX, ContractId),
    purchases_list_to_data(PurchasesList).

-spec purchases_list_to_data(list()) -> list().
%% @private
purchases_list_to_data(List) ->
    [{UserId, PurchasesNum, PurchasesSum, CommissionSum, PurchasesList} ||
     #affiliates_purchases{user_id = UserId,
                           purchases_num = PurchasesNum,
                           purchases_sum = PurchasesSum,
                           commission_sum = CommissionSum,
                           purchases = PurchasesList
                          } <- List].



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

create_contract(UserId, Name, {_,_,_}=StartDate, {_,_,_}=FinishDate,
                PurchaseLimit, CommissionRate)
  when
  StartDate =< FinishDate,
  PurchaseLimit == all orelse
      (is_integer(PurchaseLimit) andalso PurchaseLimit > 0),
  CommissionRate >= 0 ->
    case check_contracts_in_period(StartDate, FinishDate, UserId) of
        ok ->
            Contract = #affiliates_contracts{id = contract_id(),
                                             owner = UserId,
                                             name = Name,
                                             start_date = StartDate,
                                             finish_date = FinishDate,
                                             purchases_limit = PurchaseLimit,
                                             commission = CommissionRate
                                             },
            nsm_db:put(Contract);
        {error, ExContractId} ->
            {error, {contracts_conflict, ExContractId}}
    end.

%%akalenuk: I need this to deparate check from real data writing. Though this piece is poorly written and may need reengineering
check_contract(UserId, _Name, {_,_,_}=StartDate, {_,_,_}=FinishDate, PurchaseLimit, CommissionRate)
  when
  StartDate =< FinishDate,
  PurchaseLimit == all orelse
      (is_integer(PurchaseLimit) andalso PurchaseLimit > 0),
  CommissionRate >= 0 ->
    case check_contracts_in_period(StartDate, FinishDate, UserId) of
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
    Contracts = nsm_db:all_by_index(?CONTRACTS_TABLE, ?OWNER_INDEX, UserId),
    [contract_to_data(C) || C <- Contracts].



%% @spec find_contract(UserId, Date) -> {ok, Contract} | {error, notfound}
%% Types:
%%     UserId = user_id()
%%     Date = calendar:datetime()
%%     Contract = {ContractId, ContractName, StartDate, FinishDate, Limit, Commission}
%%       ContractId = contract_id(),
%%       ContractName = string(),
%%       StartDate = calendar:datetime(),
%%       FinishDate = calendar:datetime(),
%%       Limit = all | pos_integer()
%%       Commission = number()
%% @doc Returns information about the user contract if one exits for specified date.
%% @end
find_contract(UserId, Date) ->
    Contracts = nsm_db:all_by_index(?CONTRACTS_TABLE, ?OWNER_INDEX, UserId),
    case find_contract_by_time(Contracts, Date) of
        {ok, Contract} ->
            {ok, contract_to_data(Contract)};
        {error,  notfound} ->
            {error, notfound}
    end.



-spec contract_to_data(#affiliates_contract_types{}) -> tuple().
%% @private

contract_to_data(#affiliates_contracts{id = ContractId,
                                       name = Name,
                                       owner = _UserId,
                                       start_date = StartDate,
                                       finish_date = FinishDate,
                                       purchases_limit = Limit,
                                       commission = Commision
                                      }) ->
    {ContractId, Name, StartDate, FinishDate, Limit, Commision}.




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
    Types = nsm_db:all_by_index(?CTYPES_TABLE, ?BUCKET_INDEX, ?CTYPES_BUCKET),
    ctypes_to_data(Types).


-spec ctypes_to_data(list()) -> list().
%% @private

ctypes_to_data(Types) ->
    [{CTypeId, Name, Duration, Limit, Commission, Disabled}
     || #affiliates_contract_types
        {id = CTypeId,
         name = Name,
         duration = Duration,
         purchases_limit = Limit,
         commission = Commission,
         disabled = Disabled
        } <- Types].


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
    TypeId = contract_type_id(),
    Record = #affiliates_contract_types{id = TypeId,
                                        name = Name,
                                        duration = Duration,
                                        purchases_limit = PurchasesLimit,
                                        commission = CommissionRate,
                                        disabled = false},
    ok = nsm_db:put(Record).

-spec disable_contract_type(contract_type_id()) -> ok.
%% @spec disable_contract_type(TypeId) -> ok
%% Types:
%%     TypeId = contract_type_id()
%% @doc Disables the contract type.
%% @end

disable_contract_type(TypeId) ->
    case nsm_db:get(?CTYPES_TABLE, TypeId) of
        {ok, Record} ->
            NewRecord = Record#affiliates_contract_types{disabled = true},
            ok = nsm_db:put(NewRecord);
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
    case nsm_affiliates:get_user_affiliate(Issuer) of
        {ok, OwnerId, Depth} ->
            ?INFO("~w:invitation_hook/1 Assigning a user to an affiliate. "
                  "User:~p Affiliate:~p",
                  [?MODULE, UserId, OwnerId]),
            nsm_affiliates:reg_follower(UserId, OwnerId, Depth+1);
        {error, not_in_affiliate} ->
            ?INFO("~w:invitation_hook/1 User did not assigned to any "
                  "affiliate. User:~p",
                  [?MODULE, UserId]),
            do_nothing
    end.





purchase_hook(#membership_purchase{user_id = UserId,
                                   id = PurchaseId,
                                   state = done,
                                   end_time = Time1
                                  } = MP) ->
    ?INFO("~w:purchase_hook/1 Started. User: ~p PurchaseId: ~p",
          [?MODULE, UserId, PurchaseId]),
    case nsm_affiliates:get_user_affiliate(UserId) of
        {ok, Affiliate, _} ->
            {Date, _Time} = calendar:now_to_local_time(Time1),
            case find_contract_record(Affiliate, Date) of
                {ok, Contract} ->
                    store_purchase(Contract, UserId, MP);
                {error, notfound} ->
                    do_nothing
            end;
        {error, not_in_affiliate} ->
            do_nothing
    end;

purchase_hook(_) ->
    do_nothing.


%%
%% Local Functions
%%

find_contract_record(UserId, Date) ->
    Contracts = nsm_db:all_by_index(?CONTRACTS_TABLE, ?OWNER_INDEX, UserId),
    case find_contract_by_time(Contracts, Date) of
        {ok, Contract} ->
            {ok, Contract};
        {error,  notfound} ->
            {error, notfound}
    end.


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

check_contracts_in_period(StartDate, FinishDate, UserId) ->
    check_contracts_in_period2(calendar:date_to_gregorian_days(StartDate),
                               calendar:date_to_gregorian_days(FinishDate),
                               UserId).


%% @private

check_contracts_in_period2(Date, FinishDate, _UserId) 
  when Date > FinishDate ->
    ok;

check_contracts_in_period2(Date, FinishDate, UserId) ->
    GrDate = calendar:gregorian_days_to_date(Date),
    case find_contract_record(UserId, GrDate) of
        {ok, #affiliates_contracts{id = ContractId}} ->
            {error, ContractId};
        {error, notfound} ->
            check_contracts_in_period2(Date+1, FinishDate, UserId)
    end.


%% @private

store_purchase(Contract, UserId, MP) ->
    store_purchase(Contract, UserId, MP, 10).


store_purchase(Contract, UserId, #membership_purchase{id=PurchaseId}, 0) ->
    ?ERROR("~w:store_purchase/4 Can't store a purchase info to a "
           "contract story. Purchase id: ~p. Contract id: ~p. User id: ~p.",
           [?MODULE, PurchaseId, Contract, UserId]),
    {error, cant_store_purchase};

store_purchase(Contract, UserId, MP, Counter) ->
    case nsm_db:get_for_update(?PURCHASES_TABLE, {Contract, UserId}) of
        {ok, Purchases, Meta} ->
            add_contract_purchase(Contract, UserId, MP, Purchases, Meta, Counter);
        {error, notfound} ->
            create_contract_purchase(Contract, UserId, MP, Counter)
    end.


add_contract_purchase(Contract, UserId, MPurchase, Purchases, Meta, Counter) ->
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
     commission = CommissionRate
    } = Contract,

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
           Commission = round(Price*CommissionRate), %% FIXME: /100 ?
           Purchase = {PurchaseId, Time, PackageId, PackageNumber, PaymentType, Price, Commission},
           NewPurchases =
               Purchases#affiliates_purchases
               {
                purchases_num = CurPurchasesNum + 1,
                purchases_sum = CurPurchasesSum + Price,
                commission_sum = CurCommissionSum + Commission,
                purchases = [Purchase | CurPurchasesList]
               },
           case nsm_db:update(NewPurchases, Meta) of
               ok ->
                   ok;
               {error, _} ->
                   store_purchase(Contract, UserId, MPurchase, Counter - 1)
           end;
       true ->
           ignoring
    end.


%% @private

create_contract_purchase(Contract, UserId, MPurchase, Counter) ->
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
           Commission = round(Price*CommissionPercent), %% FIXME: /100 ?
           Purchase = {PurchaseId, Time, PackageId, PackageNumber,
                       PaymentType, Price, Commission},
           NewPurchases =
               #affiliates_purchases
               {
                user_id = UserId,
                contract_id = ContractId,
                owner_id = OwnerId,
                purchases_num = 1,
                purchases_sum = Price,
                commission_sum = Commission,
                purchases = [Purchase]
               },
           case nsm_db:put_if_none_match(NewPurchases) of
               ok ->
                   ok;
               {error, _} ->
                   store_purchase(Contract, UserId, MPurchase, Counter - 1)
           end;
       true ->
           ignoring
    end.

%% @private

find_contract_by_time([], _) ->
    {error, notfound};

find_contract_by_time([Contract | Rest], Date) ->
    #affiliates_contracts{start_date = StartDate,
                          finish_date = FinishDate} = Contract,
    case is_date_in_range(Date, StartDate, FinishDate) of
        true ->
            {ok, Contract};
        false ->
            find_contract_by_time(Rest, Date)
    end.


%% @private

is_date_in_range(Date, StartDate, FinishDate) ->
        Date >= StartDate andalso Date =< FinishDate.

