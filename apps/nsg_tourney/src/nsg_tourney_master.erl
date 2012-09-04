%%%-------------------------------------------------------------------
%%% @author Dennis Novikov <dennis.novikov@gmail.com>
%%% @copyright Paynet Internet ve Bilisim Hizmetleri A.S. All Rights Reserved.
%%% @doc
%%% Tournament application main server
%%% @end
%%%-------------------------------------------------------------------

-module(nsg_tourney_master).

-behaviour(gen_server).


-include("tournament.hrl").


%% API
-export([start_link/0
        ,start_tournament/3        % Start 'static' tournament processes
        ,tournament_started/4         % Report tpurnament procs started
        ,tournament_failed/3       % Report tournament procs startup prolem
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SRV, ?MODULE).


%% tournament data
-record(t_info, {manager            :: pid()|reference()
                ,proc_sup           :: pid()|undefined
                ,params             :: list()
                ,games = dict:new() :: dict()
                }).


-record(state, {tournaments = dict:new() :: dict() % ID::term() -> #t_info{},
                                                   % ProcSup::pid()->ID::term()
               }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SRV}, ?MODULE, [], []).


-spec start_tournament(ID::term(), Game::atom(), Params::list())
                      -> ok
                       | {error, Reason::term()}.
%% @doc Start tournament
%% @end
start_tournament(ID, Game, Params) ->
    gen_server:call(?SRV, {start_tournament, ID, Game, Params}, infinity).


-spec tournament_started(Client::term(), ID::term(), Mgr::pid(), ProcSup::pid())
                     -> ok.
%% manager reports on successful start
tournament_started(Client, ID, Mgr, ProcSup) ->
    gen_server:cast(?SRV, {manager_started, Client, ID, Mgr, ProcSup}).


-spec tournament_failed(Client::term(), ID::term(), Reason::term()) -> ok.
%% tournament starter reports problem
tournament_failed(Client, ID, Reason) ->
    gen_server:cast(?SRV, {tournament_failed, Client, ID, Reason}).


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
    lager:warning([], "Tournament server ~w is starting", [self() ]),
    {ok, #state{}}.


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
handle_call({start_tournament, ID, Game, Params}, From, State) ->
    case get_t_info(ID, State) of
        undefined ->
            TInfo = #t_info{params = Params},
            lager:info([{tournament, ID}],
                       "Tournament start request: game ~w, params ~w",
                       [Game, Params]),
            start_t_procs_async(From, ID, Params),
            {noreply, save_t_info(ID, TInfo, State)};
        #t_info{manager = undefined} ->
            lager:error([{tournament, ID}],
                        "Tournament start requested for tournament"
                        " already being started: game ~w, params ~w",
                        [Game, Params]),
            {reply, {error, {starting, ID}}, State};
        #t_info{manager = Mgr} ->
            lager:error([{tournament, ID}],
                        "Tournament start requested for tournament"
                        " already running (~w): game ~w, params ~w",
                        [Mgr, Game, Params]),
            {reply, {error, {already_started, ID}}, State}
    end.


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
handle_cast({tournament_started, Client, ID, Mgr1, ProcSup1}, State) ->
    case get_t_info(ID, State) of
        #t_info{manager = OldMgr
               ,proc_sup = OldProcSup
               } = TInfo ->
            lager:info([{tournament, ID}],
                       "tournament restarted, old manager ~w, procsup ~w;"
                       " new manger ~w, procsup ~w",
                       [OldMgr, OldProcSup, Mgr1, ProcSup1]),
            State1 =
                save_t_info(ID,
                            TInfo#t_info{manager = Mgr1, proc_sup = ProcSup1},
                           State),
            {noreply, State1};
        undefined ->
            TInfo = #t_info{manager = Mgr1, proc_sup = ProcSup1},
            gen_server:reply(Client, ok),
            lager:info([{tournament, ID}],
                      "tournament started: manger ~w, procsup ~w",
                       [Mgr1, ProcSup1]),
            State1 =
                save_t_info(ID, TInfo, State),
            {noreply, State1}
    end;
handle_cast({tournament_failed, Client, ID, Reason}, State) ->
    lager:error([{tournament, ID}],
                "tournament procs failed to start: ~w", [Reason]),
    gen_server:reply(Client, {error, Reason}),
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
terminate(_Reason, _State) ->
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

start_t_procs_async(Client, ID, Params) ->
    nsg_tourney_starter:start(Client, ID, Params).


save_t_info(ID, #t_info{proc_sup = ProcSup} = TInfo,
            #state{tournaments = T} = State) ->
    T0 =
        case get_t_info(ID, State) of
            undefined ->
                T;
            #t_info{proc_sup = ProcSup} ->
                dict:erase(ProcSup, T)
        end,
    T1 = dict:store(ID, TInfo, T0),
    T2 =
        case ProcSup of
            undefined ->
                T1;
            PS when is_pid(PS) ->
                dict:store(ProcSup, ID, T1)
        end,
    State#state{tournaments = T2}.


get_t_info(ID, #state{tournaments = T}) ->
    case dict:find(ID, T) of
        {ok, TInfo} -> TInfo;
        error -> undefined
    end.
