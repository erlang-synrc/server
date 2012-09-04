%%----------------------------------------------------------------------
%% @author Paul Peregud <paulperegud@gmail.com>
%% @copyright Paynet Internet ve Bilisim Hizmetleri A.S. All Rights Reserved.
%% @doc
%% Manages queues of requests for matchmaking. Pairing is done in ?LIB
%% module.
%% LIB modules should export methods prepare_request/2 and match/3
%% @end
%%-------------------------------------------------------------------
-module(match_maker).

-behaviour(gen_server).


-include_lib("nsg_srv/include/requests.hrl").
-include_lib("alog/include/alog.hrl").
-include_lib("nsg_srv/include/settings.hrl").
-include_lib("nsg_srv/include/setup.hrl").

-include_lib("matchmaking.hrl").

%% API
-export([start_link/0]).

-export([match_me/1, stop_matching_me/1]).
-export([get_replacement/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(LIB, skill).

-record(state, {
          sleep :: integer(),
          requests :: any() %% handle to ets table, that contains
                            %% #request records, defined in matchmaking.hrl
         }).

%%%===================================================================
%%% API
%%%===================================================================

match_me(Game) when is_atom(Game) ->
    Ref = id_generator:get_id(),
    Session = self(),
    true = is_pid(whereis(?SERVER)),
    ok = gen_server:cast(?SERVER, {match_me, Session, Game, Ref}),
    Ref.

stop_matching_me(Ref) ->
    gen_server:cast(?SERVER, {stop_matching_me, Ref}).

get_replacement(GameId, Game) when is_atom(Game) ->
    true = is_pid(whereis(?SERVER)),
    gen_server:call(?SERVER, {get_replacement, GameId, Game}).

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
%% Initiates the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    Tab = ets:new(requests, [public, ordered_set, {keypos, #request.ref}]), %matchmaker_state:get_tab(),
    Sleep = case catch ?IS_TEST of
                true ->
                    500;
                _ ->
                    0
            end,
    {ok, #state{requests = Tab, sleep = Sleep}}.

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
handle_call({get_replacement, GameId, Game}, _From, State) ->
    {reply, get_replacement(State, GameId, Game), State};
handle_call(Request, _From, State) ->
    {stop, {unrecognized_call, Request}, State}.

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
handle_cast({match_me, Session, Game, Ref}, #state{requests = Tab} = State) ->
    MonRef = erlang:monitor(process, Session),
    Rec = #request{ref = Ref, pid = Session, game = Game, monref = MonRef,
                   data = ?LIB:prepare_request(Session, Game)},
    ets:insert(Tab, Rec),
    perform_matching(State, Game),
    {noreply, State};
handle_cast({stop_matching_me, Ref}, #state{requests = Tab} = State) ->
    ?INFO("got {stop_matching_me, Ref}: ~p", [{stop_matching_me, Ref}]),
    Game = stop_matching(Tab, Ref),
    perform_matching(State, Game),
    {reply, ok, State};
handle_cast(Msg, State) ->
    {stop, {unrecognized_cast, Msg}, State}.

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
handle_info({'DOWN', MonitorRef, _Type, _Object, _Info}, #state{requests = Tab} = State) ->
    ?INFO("demonitoring ~p", [_Object]),
    MatchSpec = [{{request,'_','_','_',MonitorRef}, [], ['$_']}],
    Queue = ets:select(Tab, MatchSpec),
    [ begin stop_matching(Tab, X) end || X <- Queue ],
    {noreply, State};

handle_info(Info, State) ->
    {stop, {unrecognized_info, Info}, State}.

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
terminate(_Reason, _State) ->
    ?INFO("terminating: ~p", [_Reason]),
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

get_replacement(State, GameId, Game) ->
    Tab = State#state.requests,
    Queue = get_queue(Tab, Game),
    ?INFO("get human replacement material: ~p", [Queue]),
    case ?LIB:get_replacement(Queue) of
        {ok, Rep} ->
            Rep#request.pid ! #match_found{ref = Rep#request.ref, game_id = GameId, is_replacing = true},
            stop_matching(Tab, Rep#request.ref),
            {ok, Rep#request.pid};
        _ ->
            ?INFO("no player replacement in matchmaker found", []),
            false
    end.

perform_matching(State, Game) ->
    Tab = State#state.requests,
    Reqs = Game:get_requirements(),
    Queue = get_queue(Tab, Game),
    case ?LIB:match(Queue, #request.data, Reqs) of
        {ok, Party} ->
            Sessions = [ R#request.pid || #request{} = R <- Party ],
            timer:sleep(State#state.sleep),
            %%FIX: this generates okey_game_info sent to users
            A = game_manager:create_game(Game, Sessions),
            ?INFO("game_manager:create_game -> ~p", [A]),
            case A of
                {ok, GameId} ->
                    %%FIX: this generates game_matched sent to users
                    %% order should be different, first game_matched,
                    %% then okey_game_info
                    [ begin
                          Request#request.pid ! #match_found{ref = Request#request.ref, game_id = GameId},
                          stop_matching(Tab, Request#request.ref)
                      end || #request{} = Request <- Party];

                {error, {bad_return_value, {error, {_, {gen_server, call, [DeadPid, get_player_id]}}}}} ->
                    stop_matching(Tab, DeadPid),
                    perform_matching(State, Game);
                {error, {noproc, {gen_server,call, [DeadPid, _]}}} ->
                    stop_matching(Tab, DeadPid),
                    perform_matching(State, Game)
            end;
        _ ->
            ?INFO("matchmaking failed - not enough requests in queue", []),
            ok
    end.


stop_matching(Tab, Pid) when is_pid(Pid) ->
    Refs = get_ref_by_pid(Tab, Pid),
    Games = [ stop_matching(Tab, X) || X <- Refs ],
    hd(Games);
stop_matching(Tab, #request{ref = Ref}) ->
    stop_matching(Tab, Ref);
stop_matching(Tab, Ref) ->
    Res = ets:lookup(Tab, Ref),
    case Res of
        [#request{monref = MonRef, game = Game}] ->
            ?INFO("stopping matching ref ~p", [Ref]),
            erlang:demonitor(MonRef),
            ets:delete(Tab, Ref),
            Game;
        [] ->
            erlang:error(demonitoring_failed)
    end.

get_ref_by_pid(Tab, Pid) ->
    % #record{id=10, _='_'}
    MatchSpec = [{#request{pid = Pid, _='_'}, [], ['$_']}],
    ets:select(Tab, MatchSpec).

get_queue(Tab, Game) ->
    %% select all requests with atom Game as 3d element
    MatchSpec = [{#request{game = Game, _='_'}, [], ['$_']}],
    ets:select(Tab, MatchSpec).

%%%===================================================================
%%% Tests
%%%===================================================================
