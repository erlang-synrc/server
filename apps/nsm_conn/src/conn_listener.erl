%%%-------------------------------------------------------------------
%%% @author Paul Peregud <pawel@saturn.lan>
%%% @copyright (C) 2011, Paul Peregud
%%% @doc
%%%
%%% @end
%%% Created :  5 Jan 2011 by Paul Peregud <pawel@saturn.lan>
%%%-------------------------------------------------------------------
-module(conn_listener).

-behaviour(gen_server).

%% API
-export([start_link/1]).

-export([port/0]).

-include_lib("alog/include/alog.hrl").
-include_lib("nsg_srv/include/conf.hrl").

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([accept_loop/2]).

-define(SERVER, ?MODULE).
-define(TCP_OPTS, [binary, {packet, raw}, {nodelay, true}, {reuseaddr, true}, {active, false}]).

-record(state, {
          port,           % Listening port
          listener,       % Listening socket
          acceptor        % Synchronous acceptor process
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
start_link(Port) ->
    ?INFO("Starting on ~p port", [Port]),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

port() ->
    gen_server:call(?SERVER, get_port).

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

init([Port]) ->
    process_flag(trap_exit, true),
    {ok,Addr}=inet_parse:address(conn_opt:get_listen_ip()),
    Opts = ?TCP_OPTS ++ [{ip,Addr}],
    ?INFO("Tpc Options: ~p",[Opts]),
    case gen_tcp:listen(Port, Opts ) of
        {ok, LSocket} ->
            gen_server:cast(?MODULE, {create_acceptor}),
            {ok, #state{port = Port, listener = LSocket}};
        {error, Reason} ->
            {stop, Reason}
    end.

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
handle_call(get_port, _From, #state{} = State) ->
    {reply, State#state.port, State};

handle_call(_Request, _From, #state{} = State) ->
    ?INFO("unrecognized call: ~p", [_Request]),
    Reply = ok,
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
handle_cast({create_acceptor}, #state{listener = LSocket} = State) ->
    Owner = self(),
    Acceptor = proc_lib:spawn_link(fun() -> accept_loop(LSocket, Owner) end),
    {noreply, State#state{acceptor = Acceptor}};

handle_cast(_Msg, State) ->
    ?INFO("unrecognized cast: ~p", [_Msg]),
    {noreply, State}.

accept_loop(LSocket, Owner) ->
    %% ?INFO("accept_loop", []),
    case gen_tcp:accept(LSocket, 500) of
        {ok, Socket} ->
            ok = gen_tcp:controlling_process(Socket, Owner),
            Owner ! {ok, Socket};
        {error, timeout} ->
            ok;
        _Else ->
            Owner ! _Else
    end,
    ?MODULE:accept_loop(LSocket, Owner).

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
handle_info({ok, Socket}, #state{} = State) ->
%    ?INFO("CONNECTION ACCEPTED", []),
    {ok, Pid} = nsm_conn_app:start_client(),
    ok = gen_tcp:controlling_process(Socket, Pid),
    gen_server:call(Pid, {set_socket, Socket}),
    {noreply, State};

%%FIX: handle tcp errors which stops socket from listening
handle_info({error, Error}, #state{} = State) ->
    ?INFO("ERROR ~p", [Error]),
    {noreply, State};

handle_info(_Info, State) ->
    ?INFO("unrecognized info: ~p", [_Info]),
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

