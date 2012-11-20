-module(conn_listener).
-author('Paul Peregud <paulperegud@gmail.com>').
-behaviour(gen_server).
-compile(export_all).
-include_lib("nsx_config/include/log.hrl").
-include_lib("nsm_db/include/config.hrl").
-define(SERVER, ?MODULE).
-define(TCP_OPTS, [binary, {packet, raw}, {nodelay, true}, {reuseaddr, true}, {active, false}]).
-record(state, {port, listener, acceptor}).

start_link(Port) -> ?INFO("GAME PORT: ~p",[Port]), gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).
port() -> gen_server:call(?SERVER, get_port).

accept_loop(LSocket, Owner) ->
    case gen_tcp:accept(LSocket, 500) of
        {ok, Socket} -> ok = gen_tcp:controlling_process(Socket, Owner), Owner ! {ok, Socket};
        {error, timeout} -> ok;
        _Else -> Owner ! _Else
    end,
    ?MODULE:accept_loop(LSocket, Owner).

init([Port]) ->
    process_flag(trap_exit, true),
    {ok,Addr}=inet_parse:address(conn_opt:get_listen_ip()),
    Opts = ?TCP_OPTS ++ [{ip,Addr}],
    ?INFO("GAME SERVER TCP Options: ~p",[Opts]),
    case gen_tcp:listen(Port, Opts ) of
        {ok, LSocket} -> gen_server:cast(?MODULE, {create_acceptor}), {ok, #state{port = Port, listener = LSocket}};
        {error, Reason} -> {stop, Reason}
    end.

handle_info({ok, Socket}, #state{} = State) ->
    {ok, Pid} = nsm_conn_app:start_client(), ok = gen_tcp:controlling_process(Socket, Pid),
    gen_server:call(Pid, {set_socket, Socket}),
   {noreply, State};

handle_info({error, Error}, #state{} = State) -> {noreply, State};
handle_info(_Info, State) -> {noreply, State}.
handle_call(get_port, _From, #state{} = State) -> {reply, State#state.port, State};
handle_call(_Request, _From, #state{} = State) -> ?INFO("unrecognized call: ~p", [_Request]), {reply, ok, State}.
handle_cast({create_acceptor}, #state{listener = LSocket} = State) ->
    Owner = self(),
    Acceptor = proc_lib:spawn_link(fun() -> accept_loop(LSocket, Owner) end),
    {noreply, State#state{acceptor = Acceptor}};
handle_cast(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

