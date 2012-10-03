%%----------------------------------------------------------------------
%% @author Vladimir Baranov <baranoff.vladimir@gmail.com>
%% @copyright Paynet Internet ve Bilisim Hizmetleri A.S. All Rights Reserved.
%% @doc
%%   Deal with accounts (points, money) and transactions. All account operations
%%   should be performing using this module.
%% @end
%%--------------------------------------------------------------------
-module(nsm_accounts).

%%
%% Include files
%%

-include("accounts.hrl").
-include_lib("nsx_config/include/log.hrl").

%%
%% Exported Functions
%%
-export([debet/2, credit/2, balance/2,
		 create_account/1, create_account/2]).

-export([transaction/4, transaction/5]).

%% High level API
-export([check_quota/1, check_quota/2]).

%%
%% API Functions
%%

%% @doc Simple api for transactions. Account - username. If Amount > 0 we will
%% increase balance and vice versa.

-spec transaction(string(), currency(), integer(), transaction_info()) ->
		  {ok, transaction_id()} | {error, term()}.


transaction(Account, Currency, 0, TransactionInfo) ->
    ?WARNING("zero transaction: Account=~p, Currency=~p,"
    " TransactionInfo=~p", [Account, Currency, TransactionInfo]),
    ok;
transaction(Account, Currency, Amount, TransactionInfo) when Amount /= 0->
	{Remitter, Acceptor} = if Amount > 0 ->
								  {?SYSTEM_ACCOUNT_ID, Account};
							  true ->
								  {Account, ?SYSTEM_ACCOUNT_ID}
						   end,

	transaction(Remitter, Acceptor, Currency, abs(Amount), TransactionInfo).


%% @doc Add new transaction, will change state of accouts. All transactions
%%      are between user account and system account. So  moving of the funds
%%      (money, kaush, game_points, etc.) should be performed with transactions.

-spec transaction(Remitter::string(),
				  Acceptor::string(),
				  Currency::currency(),
				  Amount::integer(), TransactionInfo::transaction_info())->
                  {ok, transaction_id()} | {error, any()}.

transaction(Remitter, Acceptor, Currency, Amount, TransactionInfo) ->
	TX = #transaction{id = generate_id(),
					  acceptor = Acceptor,
					  remitter = Remitter,
					  amount = Amount,
					  commit_time = now(),
					  currency = Currency,
					  info = TransactionInfo},
	commit_transaction(TX).


%% @doc How many funds got from the start of the time.

-spec debet(account_id(), currency()) -> {ok, integer()} | {error, term()}.

debet(Account, Currency) ->
	case get_account(Account, Currency) of
		#account{debet = Debet} ->
			{ok, Debet};
		Error ->
			Error
	end.

%% @doc How many funds spent  from the start of the time.

-spec credit(account_id(), currency()) -> {ok, integer()} | {error, term()}.

credit(AccountId, Currency) ->
	case get_account(AccountId, Currency) of
		#account{credit = Credit} ->
			{ok, Credit};
		Error ->
			Error
	end.

%% @doc balance of the given account by given currency

-spec balance(account_id(), currency()) -> {ok, integer()} | {error, term()}.

balance(AccountId, Currency) ->
	case get_account(AccountId, Currency) of
		#account{debet = Debet, credit = Credit} ->
			{ok, Debet - Credit};
		Error ->
			Error
	end.


%% @doc Create account with for all supported currencies.

-spec create_account(Username::string()|system) -> ok | {error, term()}.

create_account(AccountId) ->
	Currencies = get_currencies(),
	try
		[{ok, Currency} = {create_account(AccountId, Currency), Currency} ||
						  Currency <- Currencies]
	catch
		_:_ ->
			{error, unable_create_account}
	end.


%% @doc Create account for specified currency.

create_account(AccountId, Currency) ->
	Account = #account{id = ?ACC_ID(AccountId, Currency),
					   credit = 0,
					   debet = 0,
					   last_change = 0},

	case nsm_db:put(Account) of
		ok ->
			ok;
		Error ->
			?ERROR("create_account: put to db error: ~p", [Error]),
			{error, unable_to_store_account}
	end.

%% @doc Check quota balance against hard and soft limits
-spec check_quota(Username::string()) -> ok | {error, soft_limit} | {error, hard_limit}.

check_quota(User) ->
    check_quota(User, 0).

%% @doc Check quota balance against hard and soft limit. Second argument is
%%      Amount planning to be cherged from user.
-spec check_quota(User::string(), Amount::integer()) -> ok | {error, soft_limit} | {error, hard_limit}.

check_quota(User, Amount) ->
    SoftLimit = app_opt:get_quota_limit_soft(),
    {ok, Balance} = balance(User, quota),
    BalanceAfterChange = Balance - Amount,
    if
        BalanceAfterChange > SoftLimit ->
            ok;
        true ->
            HardLimit = app_opt:get_quota_limit_hard(),
            if
                BalanceAfterChange =< HardLimit ->
                    {error, hard_limit};
                true ->
                    {error, soft_limit}
            end
    end.

%%
%% Local Functions
%%
%% wrapper to construct account id

commit_transaction(#transaction{remitter = R, acceptor = A,  currency = Currency,
								amount = Amount} = TX) ->
	case change_accounts(R, A, Currency, Amount) of
		ok ->
			nsm_db:put(TX);
		Error ->
            %% in case of game events it is possible to assign points to undefined
            %% accounts when robots play game. System account will not be changed
            case TX#transaction.info of
                #ti_game_event{} ->
                    nsm_db:put(TX);
                _ ->
                    ?ERROR("commit transaction error: change accounts ~p", Error),
                    Error
            end
	end.

change_accounts(Remitter, Acceptor, Currency, Amount) ->
	case {get_account(Remitter, Currency), get_account(Acceptor, Currency)} of
		{RA = #account{}, AA = #account{}}  ->
			?INFO("transacrion: RemitterAccount ~p, AcceptorAccount: ~p", [RA, AA]),
			%% check balance for remitter according to currency and amount
			case check_remitter_balance(RA, Amount) of
				%% all ok write changes
				ok ->
					%% increase credit of remmitter, last change is less then zero
					RA1 = RA#account{credit = RA#account.credit + Amount,
								   last_change = -Amount },

					%% increase debet of acceptor, last change is positive

					AA1 = AA#account{debet = AA#account.debet + Amount,
								   last_change = Amount},

					nsm_db:put([AA1, RA1]);
				{error, Reason} ->
					{error, {remitter_balance, Reason}}
			end;

		{{error, Reason}, #account{}} ->
			{error, {remitter_account_unavailable, Reason}};

		{#account{}, {error, Reason}} ->
			{error, {acceptor_account_unavailable, Reason}};

		{RE, AE} ->
			{error, both_accounts_unavailable}
	end.


%% for system account we don't need to check balance, this account only for
%% statistic
check_remitter_balance(#account{id = ?ACC_ID(?SYSTEM_ACCOUNT_ID, _)}, _) ->
	ok;
%% not apply check for now. This is for future use, if any
check_remitter_balance(_Account, _Amount) ->
    ok.


get_account(Account, Currency) ->
	case nsm_db:get(account, ?ACC_ID(Account, Currency)) of
		{ok, #account{} = AR} ->
			AR;
		_ ->
			{error, account_not_found}
	end.

get_currencies() ->
	[?CURRENCY_GAME_POINTS,
	 ?CURRENCY_KAKUSH_CURRENCY,
	 ?CURRENCY_KAKUSH,
	 ?CURRENCY_QUOTA,
	 ?CURRENCY_MONEY].

generate_id() ->
	{MegSec, Sec, MicroSec} = now(),
	H = erlang:phash2(make_ref()),
	lists:concat([MegSec*1000000000000, Sec*1000000, MicroSec, "-", H]).
