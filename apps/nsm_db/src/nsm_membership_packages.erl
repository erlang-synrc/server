-module(nsm_membership_packages).
-author('Vladimir Baranov <baranoff.vladimir@gmail.com>').

-include("membership_packages.hrl").
-include_lib("nsx_config/include/log.hrl").
-include("accounts.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([create_storage/0, add_sample_data/0]).
-export([add_package/1, list_packages/0, list_packages/1, get_package/1, available_for_sale/2]).
-export([add_purchase/1, add_purchase/3, get_purchase/1, set_purchase_external_id/2,
         set_purchase_state/3, set_purchase_info/2, list_purchases/0, list_purchases/1,
         purchase_id/0]).
-export([get_monthly_purchase_limit/0, check_limit_over/2]).

-type package_id() :: integer().
-type list_options()::[{payment_type, payment_type()}|{available_for_sale, boolean()}].

-spec add_package(#membership_package{})->{ok, Id::package_id()}|{error, Reason::any()}.
add_package(#membership_package{}=Package)->
    Id = generate_id(),
    save_package(Package#membership_package{id = Id}).

-spec get_package(any())-> {ok, #membership_package{}} | {error, Reason::any()}.
get_package(PackageId)->
    case nsm_db:get(membership_package, PackageId) of
        {ok, #membership_package{} = Package}->
            {ok, Package};
        {error, Reason}->
            {error, Reason}
    end.

-spec list_packages(SelectOptions::list_options())->[#membership_package{}].
list_packages(Options) ->
    Predicate =
        fun(MP = #membership_package{}) ->
                check_conditions(Options, MP, true)
        end,

    select(membership_package, Predicate).

-spec list_packages()->[#membership_package{}].
list_packages()->
     nsm_db:all(membership_package).

-spec available_for_sale(package_id(), boolean()) -> ok | {error, any()}.
available_for_sale(PackageId, State) ->
    case get_package(PackageId) of
        {ok, Package} ->
            case save_package(
                   Package#membership_package{available_for_sale = State}) of
                {ok, _} ->
                    ok;
                Error ->
                    Error
            end;
        {error, Reason}->
            {error, Reason}
    end.

-spec add_purchase(#membership_purchase{}) -> {ok, PurchaseId::string()}.
add_purchase(#membership_purchase{} = MP) ->
    add_purchase(#membership_purchase{} = MP, undefined, undefined).

-spec add_purchase(#membership_purchase{}, purchase_state(), StateInfo::any()) -> {ok, PurchaseId::string()}.
add_purchase(#membership_purchase{} = MP, State0, Info) ->
    case nsm_db:get(membership_purchase, MP#membership_purchase.id) of
        {ok, _} -> {error, already_bought_that_one};
        {error, notfound} ->
            %% fill needed fields
            Start = now(),
            State = default_if_undefined(State0, undefined, ?MP_STATE_ADDED),
            %% FIXME: uniform info field if needed
            StateLog = case Info of
                           undefined ->
                               [#state_change{time = Start, state = State,
                                              info = system_change}];
                           _ ->
                               [#state_change{time = Start, state = State,
                                              info = Info}]
                       end,

            %% TODO: add check for duplicate purches if id given by external module
            Id = default_if_undefined(MP#membership_purchase.id, undefined, purchase_id()),

            Purchase = MP#membership_purchase{id = Id,
                                              state = State,
                                              start_time = Start,
                                              state_log = StateLog},

            %% notify about purchase added
%            nsx_msg:notify_purchase(Purchase),

            ?INFO("Purchase added ~p ~p",[Purchase#membership_purchase.user_id, Purchase]),

            nsm_riak:add_purchase_to_user(Purchase#membership_purchase.user_id, Purchase)
    end.

-spec get_purchase(string())-> {ok, #membership_purchase{}} | {error, Reason::any()}.
get_purchase(PurchaseId)->
    case nsm_db:get(membership_purchase, PurchaseId) of
        {ok, #membership_purchase{} = Package}->
            {ok, Package};
        {error, Reason}->
            {error, Reason}
    end.

-spec set_purchase_state(term(), purchase_state(), term()) -> ok.
set_purchase_state(MPId, NewState, Info) ->
    case nsm_db:get(membership_purchase, MPId) of 
      {ok, MP} ->

    Time = now(),
    StateLog = MP#membership_purchase.state_log,
    %% add new state to head of state log
    NewStateLog = [#state_change{time = Time, state = NewState,
                                 info = Info}|StateLog],
    EndTime = case NewState of
                  ?MP_STATE_DONE -> now();
                  ?MP_STATE_CANCELLED -> now();
                  ?MP_STATE_FAILED -> now();
                  _ -> MP#membership_purchase.end_time
              end,
    Purchase = MP#membership_purchase{state = NewState,
                                      end_time = EndTime,
                                      state_log = NewStateLog},

    %% notify aboput state change
%    nsx_msg:notify_purchase(Purchase),
    NewMP=MP#membership_purchase{state = NewState,
                                         end_time = EndTime,
                                         state_log = NewStateLog},
    nsm_db:put(NewMP),

    if
        NewState == ?MP_STATE_DONE ->
            charge_user_account(MP),
            nsm_affiliates:purchase_hook(NewMP);
        true ->
            ok
    end,
    ok;
  
    Error -> ?INFO("Can't set purchase state, not yet in db"), Error
    end.


-spec set_purchase_info(term(), term()) -> ok | {error, not_found}.
set_purchase_info(MPId, Info) ->
    {ok, MP} = nsm_db:get(membership_purchase, MPId),
    nsm_db:put(MP#membership_purchase{info = Info}).

set_purchase_external_id(MPId, ExternalId) ->
    {ok, MP} = nsm_db:get(membership_purchase, MPId),
    case MP#membership_purchase.external_id of
        ExternalId ->
            ok;
        _ ->
            nsm_db:put(MP#membership_purchase{external_id = ExternalId})
    end.

-spec create_storage()-> ok.
create_storage()->
    %% FIXME: usage of direct mnesia calls
    ok = nsm_mnesia:create_table(membership_package,
                                    record_info(fields, membership_package),
                                    [{storage, permanent}]),
    ok = nsm_mnesia:create_table(membership_purchase,
                                    record_info(fields, membership_purchase),
                                    [{storage, permanent}]).

-spec list_purchases() -> list(#membership_purchase{}).
list_purchases() ->
    nsm_db:all(membership_purchase).

-spec list_purchases(SelectOptions::list()) -> list(#membership_purchase{}).
list_purchases(SelectOptions) ->
    Predicate =
        fun(MP = #membership_purchase{}) ->
                check_conditions(SelectOptions, MP, true)
        end,

    select(membership_purchase, Predicate).

-spec purchase_id() -> string().
purchase_id() ->
    %% get next generated id for membership purchase
    NextId = nsm_db:next_id("membership_purchase"),
    lists:concat([timestamp(), "_", NextId]).


%% FIXME: temporary! Delete in production.
-spec add_sample_data()-> [ok|{error, any()}].
add_sample_data()->
    SamplePackages = [#membership_package{
                         no = 1,
                         amount = 7,
                         deducted_for_gifts = 0,
                         quota = 7,
                         net_membership = 7},
     #membership_package{
                         no = 2,
                         amount = 12,
                         deducted_for_gifts = 5,
                         quota = 15,
                         net_membership = 7},
     #membership_package{
                         no = 3,
                         amount = 12,
                         deducted_for_gifts = 0,
                         quota = 15,
                         net_membership = 12},
     #membership_package{
                         no = 4,
                         amount = 25,
                         deducted_for_gifts = 10,
                         quota = 30,
                         net_membership = 15},
     #membership_package{
                         no = 5,
                         amount = 30,
                         deducted_for_gifts = 0,
                         quota = 60,
                         net_membership = 30},
     #membership_package{
                         no = 6,
                         amount = 50,
                         deducted_for_gifts = 20,
                         quota = 60,
                         net_membership = 30},
     #membership_package{
                         no = 7,
                         amount = 50,
                         deducted_for_gifts = 0,
                         quota = 90,
                         net_membership = 50},
     #membership_package{
                         no = 8,
                         amount = 100,
                         deducted_for_gifts = 40,
                         quota = 120,
                         net_membership = 60}],

    WithPaymentTypes =
        [Package#membership_package{id = generate_id(),
                                    payment_type=Payment} ||
            Payment <- [facebook, credit_card, wire_transfer, paypal, mobile],
            Package <- SamplePackages],

	%%make all packages enable for sale
    Enabled = [P#membership_package{available_for_sale = true} ||
                  P <- WithPaymentTypes],

    nsm_db:put(Enabled).

generate_id()->
    Id = nsm_db:next_id("membership_package"),
    integer_to_list(Id).

%% return default value if value match Undefined spec
default_if_undefined(Value, Undefined, Default) ->
    case Value of
        Undefined ->
            Default;
        _ ->
            Value
    end.

%% charge user account with kakush and quota.
charge_user_account(MP) ->
    %% Charge user account
    OrderId = MP#membership_purchase.id,
    Package = MP#membership_purchase.membership_package,
    Kakush = Package#membership_package.deducted_for_gifts,
    Quota = Package#membership_package.quota,
    UserId = MP#membership_purchase.user_id,

    PaymentTransactionInfo = #ti_payment{id=MP#membership_purchase.id},

    try
        ?INFO("charge user account. OrderId: ~p, User: ~p, Kakush:~p, Quota:~p",
              [OrderId, UserId, Kakush, Quota]),

        nsm_accounts:transaction(UserId, ?CURRENCY_KAKUSH, Kakush, PaymentTransactionInfo),
        nsm_accounts:transaction(UserId, ?CURRENCY_QUOTA, Quota, PaymentTransactionInfo)
    catch
        _:E ->
            ?ERROR("unable to charge user account. User=~p, OrderId=~p. Error: ~p",
                   [UserId, OrderId, E])
    end.


%% get all records from database, filter them with predicate.
%% FIXME: temporary hack to provide mnesias select functionality with riak
select(RecordType, Predicate) ->
    All = nsm_db:all(RecordType),
	lists:filter(Predicate, All).


save_package(Package) ->
    case nsm_db:put([Package]) of
        ok ->
            {ok, Package#membership_package.id};
        {error, Reason}->
            {error, Reason}
    end.

timestamp()->
    {Y, Mn, D} = erlang:date(),
    {H, M, S} = erlang:time(),
    lists:flatten(
      io_lib:format("~b~2..0b~2..0b_~2..0b~2..0b~2..0b", [Y, Mn, D, H, M, S])).

%% Packages conditions
check_conditions(_, _, false) -> false;
check_conditions([{available_for_sale, AS}|T],
                 MP = #membership_package{available_for_sale = AS1}, _) ->
    check_conditions(T, MP, AS == AS1);
check_conditions([{payment_type, PT}|T],
                 MP = #membership_package{payment_type = PT1}, _) ->
    check_conditions(T, MP, PT == PT1);

%%TODO: Add purchase conditions
check_conditions([], _, true) -> true.

%% @private
delete_package(PackageId) ->
    nsm_db:delete(membership_package, PackageId).

%%
%% Tests
%%
%-ifdef(TEST).

packages_test_()->
    {setup, fun setup/0, fun cleanup/1,
     {with, [fun list_/1,
             fun get_/1,
             fun change_availability_/1]}
    }.


setup() ->
    % Uncomment to run tests with dbg:
    % dbg:tracer(),
    % dbg:p(all, call),
    % dbg:tpl(membership_packages, []),
    TestPackages =
        [#membership_package{no=1,
                             payment_type=test_payment_x,
                             amount = 100,
                             deducted_for_gifts = 5,
                             quota = 300,
                             net_membership = 7,
                             available_for_sale = true
                            },
         #membership_package{no=2,
                             payment_type=test_payment_y,
                             amount = 100,
                             deducted_for_gifts = 5,
                             quota = 300,
                             net_membership = 7
                            },
         #membership_package{no=3,
                             payment_type=test_payment_y,
                             amount = 100,
                             deducted_for_gifts = 5,
                             quota = 300,
                             net_membership = 7
                            }],

     [Id || {ok, Id} <-
                [{ok, _} = ?MODULE:add_package(Package) || Package <- TestPackages]].

cleanup(Ids) ->
    [ok = delete_package(PackageId) || PackageId <- Ids].

list_(_Packages)->
    ?assertMatch([#membership_package{no=1}], ?MODULE:list_packages([{payment_type, test_payment_x}])),

    ?assertEqual([], ?MODULE:list_packages([{payment_type, test_payment_y},
                                            {available_for_sale, true}])),
    YPaymentTypePackages = ?MODULE:list_packages([{payment_type, test_payment_y}]),
    YPaymentTypePackagesNumbers = lists:sort([No || #membership_package{no=No} <- YPaymentTypePackages]),
    ?assertMatch([2,3], YPaymentTypePackagesNumbers).

get_([Id|_])->
    ?assertMatch({ok, #membership_package{id=Id}}, ?MODULE:get_package(Id)),
    ?assertMatch({error, _}, ?MODULE:get_package(-1)).

change_availability_([Id | _]) ->
    [begin
         ?assertEqual(ok, ?MODULE:available_for_sale(Id, State)),
         ?assertMatch({ok, #membership_package{id=Id, available_for_sale = State}},
                      ?MODULE:get_package(Id))
     end || State <- [true, false]].


get_monthly_purchase_limit() ->
    MostExpencivePackageWorth = lists:max([P#membership_package.amount || P <- nsm_db:all(membership_package), P#membership_package.available_for_sale]),
    ?MP_MONTHLY_LIMIT_MULTIPLIER * MostExpencivePackageWorth.

check_limit_over(UId, PackageId) ->
    Limit = ?MP_MONTHLY_LIMIT_MULTIPLIER, %get_monthly_purchase_limit(),
    {ok, Package} = nsm_membership_packages:get_package(PackageId),
    PackagePrice = Package#membership_package.amount,
    {{CurYear, CurMonth, _}, _} = calendar:now_to_datetime(now()),
    UserMonthlyPurchases = [begin
        {{Year, Month, _}, _} = calendar:now_to_datetime(P#membership_purchase.start_time),
        {Year, Month, P}
    end || P <- nsm_db:purchases(UId)],
    ThisMonthPurchases = [P || {Y, M, P} <- UserMonthlyPurchases, Y == CurYear, M == CurMonth],
    ThisMonthTotal = lists:sum([(P#membership_purchase.membership_package)#membership_package.amount || P <- ThisMonthPurchases]),
    (ThisMonthTotal + PackagePrice) > Limit.
