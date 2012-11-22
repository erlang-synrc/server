%%%-------------------------------------------------------------------
%%% @author JLarky <jlarky@punklan.net>
%%% @copyright Paynet Internet ve Bilisim Hizmetleri A.S. All Rights Reserved.
%%% @doc
%%% Server collecting number of users using site.
%%% @end
%%% Created :  9 Jul 2011 by JLarky <jlarky@punklan.net>
%%%-------------------------------------------------------------------
-module(user_counter).

-define(CACHE_LIFETIME, 3).

-behaviour(gen_server).

-include("setup.hrl").
-include("common.hrl").
-include_lib("nsm_db/include/config.hrl").
-include_lib("nsm_db/include/user_counter.hrl").

%% API
-export([start_link/0,
	 wf_update_me/0,
         wf_update_me/1,
	 user_count/0,
         user_count/1,
	 now_minutes/0
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%% id is browser id and munute is time in minutes when user last time
%% was accessing to site. Mnesia table type is bag so it will contain
%% unique pars of {id, munute} that will be counted later.
-record(state, {timer, % timer ref
		cache_time = 0,  % last time when transaction was done
		cache_value = 0, % last value
                cache_table      % ets table to store counters
	       }).

%%%===================================================================
%%% API
%%%===================================================================

-spec wf_update_me() -> ok.
%% @doc used from nitrogen to mark browser as active.
wf_update_me() ->
    update_me(undefined).

-spec wf_update_me(atom()) -> ok.
wf_update_me(Game) ->
	update_me(Game).

-spec update_me(atom()) -> ok.
update_me(Game) ->
    ok.
%	case wf:cookie("newcookie") of %% see nitrogen/src/handlers/session/simple_session_handler.erl
%	undefined -> %% cookies disabled. user can't login with disabled cookies, no need to count that
%	    ignore_cookies_disabled;
%	Cookie ->
%	    case wf:depickle(Cookie) of
%		{state, BinID, _} ->
%        ok = gen_server:call(?SERVER, {update_me, BinID, Game});
%		_wrong_cookie ->
%		    ignore_wrong_cookie %% this user can't play either
%	    end
%    end,
%    ok.

-spec user_count() -> integer().
%% @doc counts how many browsers were active in last minutes.
user_count() ->
    gen_server:call(?SERVER, {user_count,undefined}).

-spec user_count(atom) -> integer().
user_count(Game) ->
    gen_server:call(?SERVER, {user_count,Game}).

-spec now_sec() -> integer().
now_sec() ->
    {Megasec, Sec, _} = now(),
    Megasec*1000000+Sec.
-spec now_minutes() -> integer().
%% @doc return current time in minutes.
now_minutes() ->
    now_sec() div 60.


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, TRef} = timer:send_interval(30000, remove_old),
    CacheTab = ets:new(browser_counter, [{keypos, 2}]),
    {ok, #state{timer=TRef, cache_table = CacheTab}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({update_me, BinID,Game}, _From, #state{cache_table = CT} = State) ->
    ets:insert(CT, #browser_counter{id=BinID, minute=now_minutes(), game=Game}),
    {reply, ok, State};

handle_call({user_count,undefined}, _From, #state{cache_table = CT} = State) ->
    Count = ets:info(CT, size),
    ?INFO("user_count: undefined, Res: ~p", [Count]),
    {reply, Count, State};

handle_call({user_count, Game}, _From, #state{cache_table=CT} = State) ->
    GameH = case Game of tavla -> game_tavla; okey -> game_okey end,
    GameCounts = nsm_queries:map_reduce(game_manager,counter,[GameH]),
    ?INFO("user count: ~p",[{GameCounts,GameH}]),
    {reply, GameCounts, State};
    %% TODO: cache result
%    GameRecords = ets:match(CT, #browser_counter{game = Game, _ = '_'}),
 %   {reply, length(GameRecords), State};

handle_call(_Request, _From, State) ->
    Reply = unknown,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(remove_old, #state{cache_table = CT} = State) ->
    RemainMinutes = 2,
    Minutes = now_minutes(),
    MinTS = Minutes-RemainMinutes+1,

    MatchHead = #browser_counter{minute='$1', _ = '_'},
    Guard = {'<', '$1', MinTS},
    Result = '$_',
    OldList = ets:select(CT, [{MatchHead, [Guard], [Result]}]),
    lists:foreach(fun(#browser_counter{id = Id}) ->
                          ets:delete(CT, Id)
                  end, OldList),

    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    timer:cancel(State#state.timer),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
