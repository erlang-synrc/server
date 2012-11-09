%%%-------------------------------------------------------------------
%%% @author Paul Peregud <pawel@saturn.lan>
%%% @copyright (C) 2011, Paul Peregud
%%% @doc
%%%
%%% @end
%%% Created : 22 Feb 2011 by Paul Peregud <pawel@saturn.lan>
%%%-------------------------------------------------------------------
-module(tc_sock).

-include("conf.hrl").
-include("kamf.hrl").
-include("classes.hrl").
-include_lib("nsx_config/include/log.hrl").

-behaviour(gen_server).

-export([start_link/0]).
-export([connect/1, connect/3, close/1]).
-export([have_socket/2]).
-export([call_rpc/2, call_rpc_raw/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
          packet = <<>>,
          running_requests = dict:new(),

          parent,
          socket,
          id = 0
         }).

%%%===================================================================
%%% API
%%%===================================================================
connect(Pid) ->
    gen_server:call(Pid, {connect}).

connect(Pid, Host, Port) ->
    gen_server:call(Pid, {connect, {Host, Port}}).

have_socket(Pid, Socket) ->
    gen_server:call(Pid, {have_socket, Socket}).

call_rpc(Pid, Record) ->
    gen_server:call(Pid, {call_rpc, Record}, 60000).

call_rpc_raw(Pid, Type, Args) ->
    gen_server:call(Pid, {call_rpc_raw, Type, Args}, 60000).

close(Pid) ->
    gen_server:call(Pid, {close}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link(?MODULE, [self()], []).

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
init([Pid]) ->
    {ok, #state{parent = Pid}}.

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
handle_call({connect}, _From, State) ->
    {ok, Socket} = gen_tcp:connect('127.0.1.1', ?LISTEN_PORT, [{active, once}, binary, {packet, 0}]),
    ok = gen_tcp:send(Socket, <<?KAMF_MAGIC:48>>),
    {reply, ok, State#state{socket = Socket}};

handle_call({connect, {Host, Port}}, _From, State) ->
    ?INFO("TC_SOCK: ~p",[{Host,Port}]),
    {ok, Socket} = gen_tcp:connect(Host, Port, [{active, once}, binary, {packet, 0}]),
    ok = gen_tcp:send(Socket, <<?KAMF_MAGIC:48>>),
    {reply, ok, State#state{socket = Socket}};


handle_call({call_rpc, Record}, From, State = #state{id = Id, socket = Socket}) ->
    BIN = construct_kamf_rpc_request(Record, State#state.id),
    ok = gen_tcp:send(Socket, BIN),

    RR = dict:store(Id, From, State#state.running_requests),
    {reply, {ok, Id}, State#state{id = Id + 1, running_requests = RR}};

handle_call({call_rpc_raw, Method, Args}, From, State = #state{id = Id, socket = Socket}) ->
    Rec = ?AMF:create_request(Id, Method, Args),
    Object = ?AMF:record_to_object(Rec),
    AmfObject = ?AMF:encode(Object),
    BIN = ?AMF:construct(AmfObject),
    ok = gen_tcp:send(Socket, BIN),

    RR = dict:store(Id, From, State#state.running_requests),
    {reply, {ok, Id}, State#state{id = Id + 1, running_requests = RR}};

handle_call({have_socket, Socket}, _From, State) ->
    ok = inet:setopts(Socket, [{active, once}]),
    {reply, ok, State#state{socket = Socket}};

handle_call({close}, _From, State = #state{socket = Socket}) ->
    ok = gen_tcp:close(Socket),
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

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
handle_info({tcp, Socket, Body}, #state{packet = Saved} = State) ->

    NS = case ?AMF:reconstruct(Body, Saved) of
             {done, [], NSaved} ->
                 ok = inet:setopts(Socket, [{active, once}]),
                 State#state{packet = NSaved};
             {done, Packets, NSaved} ->
                 [],
                 decode_and_send(Packets, State),
                 ok = inet:setopts(Socket, [{active, once}]),
                 State#state{packet = NSaved}
         end,
    {noreply, NS};

handle_info({tcp_error, _Socket, Reason}, _State) ->
    ?INFO("Connection error: ~p.", [{tcp_error, Reason}]),
    {stop, {tcp_error, Reason}, _State};

handle_info({tcp_closed, _Socket}, _State) ->
    ?INFO("Server Disconnected.", []),
    {stop, normal, _State};

handle_info(_Info, #state{socket = _Socket} = State) ->
    ?INFO("unrecognized internal msg: ~p", [_Info]),
    {stop, {unknown_info, _Info}, State}.

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

construct_kamf_rpc_request(Record, Id) ->
    Method = api_utils:name(Record),
    Members = api_utils:members(Record),
    Rec = ?AMF:create_request(Id, Method, Members),
    Object = ?AMF:record_to_object(Rec),
    % ?DP("client send request: ~p", [Object]),
    AmfObject = ?AMF:encode(Object),
    ?AMF:construct(AmfObject).

decode_and_send([], _State) ->
    ok;
decode_and_send([T | H], #state{parent = Pid} = State ) ->
    {#object{} = Obj, _Rest} = ?AMF:decode(T),
%    ?INFO("Processing Packet (TCSOCK): ~p",[Obj]),
    Rec = ?AMF:object_to_record(Obj),
    case Rec of
        #'KamfRequest'{id = Id, method = Method, args = Args} = Msg ->
%	    ?INFO("           Method: ~p, Args: ~p", [Method, Args]),
	    Msg = api_utils:to_known_record(Method, Args),
%	    ?INFO("           Request: ~p", [Msg]),
	    game_session:process_request(Pid,"TC SOCK",Msg);
%            Pid ! Msg;
        #'KamfResponse'{} = Msg ->
%	    ?INFO("           Response: ~p", [Msg]),
           Pid ! Msg;
        #'KamfMessage'{event_type = Event, args = Args} ->
            Msg = api_utils:to_known_record(Event, Args),
%	    ?INFO("           Message: ~p", [Msg]),
%            game_session:process_request(Pid,"TC SOCK",Msg);
           Pid ! Msg;
        #'KamfFatalError'{} = Msg->
            Pid ! Msg;
        Other ->
            ?INFO("Packet is neither KamfRequest nor KamfResponse nor KamfMessage nor KamfFatalError. Packet: ~p", [Other]),
            erlang:error(not_kamf_packet)
    end,
    decode_and_send(H, State).
