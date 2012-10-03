-module(tc).

-include("conf.hrl").
-include("kamf.hrl").
-include("classes.hrl").
-include("games.hrl").
-include_lib("amf/include/amf.hrl").
-include_lib("nsx_config/include/log.hrl").

-export([connect/0, connect/1, connect/2, close/1]).
-export([call_rpc/2]).
-export([call_rpc_raw/3]).
-export([response/2, async_test_client_loop/1, async_test_client_loop/0]).
% -export([get_event/1, get_event/2]).
-export([flush_events/1]).

%% stress testing
-export([log/1]).


%% tests
call_rpc(Sock, Record) ->
    t_start(),
    {ok, Id} = tc_sock:call_rpc(Sock, Record),
    get_result(Id, Record, null).

call_rpc_raw(Socket, Method, Args) ->
    {ok, Id} = tc_sock:call_rpc_raw(Socket, Method, Args),
    get_result(Id, Method, Args).

get_result(Id, Method, Args) ->
    FRes = receive
               #'KamfResponse'{id = Id, success = true, result = Result} ->
                   Result;
               #'KamfResponse'{id = Id, success = false, result = Result} ->
                   {error, Result};
               #'KamfFatalError'{id = Id, reason = Result} = Res ->
                   ?INFO("'KamfFatalError'! : ~p", [Res]),
                   {error, Result}
          after 30000 ->
                  ?INFO("server has timeouted on KamfRequest!!!!~nDetails: ~p", [{Method, Args}]),
                  log(network_rpc_timeout),
                  erlang:error({server_timeout, tc_call_rpc}),
                  {timeout, request_timeouted, Id}
          end,
    t_clock(),
    FRes.


connect() ->
    {ok, Pid} = tc_sock:start_link(),
    tc_sock:connect(Pid),
    Pid.

connect(Host, Port) ->
    ?INFO("Host: ~p, Port: ~p",[Host,Port]),
    {ok, Pid} = tc_sock:start_link(),
    tc_sock:connect(Pid, Host, Port),
    Pid.

connect(Socket) ->
    {ok, Pid} = tc_sock:start_link(),
    ok = gen_tcp:controlling_process(Socket, Pid),
    tc_sock:have_socket(Pid, Socket),
    Pid.

close(Sock) ->
    tc_sock:close(Sock).

flush_events(Timeout) ->
    receive
        Msg ->
            ?INFO("flushing events: ~p", [Msg]),
            flush_events(Timeout)
    after
        Timeout ->
            ?INFO("flushing successful", []),
            ok
    end.

async_test_client_loop() ->
    {ok, Socket} = gen_tcp:connect(localhost, ?LISTEN_PORT, [{active, once}, binary, {packet, 0}]),
    ok = gen_tcp:send(Socket, <<?KAMF_MAGIC:48>>),
    ok = inet:setopts(Socket, [{active, true}]),
    async_test_client_loop(Socket).

async_test_client_loop(Socket) ->
    Self = self(),
    receive
        {tcp, _, Data} ->
            {done, Packets, _} = ?AMF:reconstruct(Data),
            lists:map(fun(Bin) ->
                              {Obj0, _} = ?AMF:decode(Bin),
%                              ?INFO("Processing Packet (TC): ~p",[Obj0]),
                              case Obj = ?AMF:object_to_record(Obj0) of
				  #'KamfRequest'{id = Id, method = Method, args = Args} = _Packet ->
%  				       ?INFO("           Method: ~p, Args: ~p", [Method, Args]),
				       Msg = api_utils:to_known_record(Method, Args)
%				       ?INFO("           Request: ~p", [Msg])
			      end,
			      response(Obj, Self)
                      end, Packets),
            async_test_client_loop(Socket);
        {reply, Data} ->
            ok = gen_tcp:send(Socket, ?AMF:construct(?AMF:encode(Data))),
            async_test_client_loop(Socket);
        exit ->
            ok
    end.

response(Data, Pid) ->
    ?INFO("in response: ~p", [{Data, Pid}]),
    case Data of
        #'KamfRequest'{id = Id, method = <<"slowping">>} ->
            spawn_link(fun() ->
                               ?INFO("SLEEEPER", []),
                               timer:sleep(100),
                               SlowRespose = #object{name = <<"KamfResponse">>,
                                                     members = [{id, Id},
                                                                {success, true},
                                                                {result, <<"slowpong">>}]},
                               Pid ! {reply, SlowRespose}
                       end);

        #'KamfRequest'{id = Id, method = <<"fastping">>} ->
            ?INFO("QUICK!", []),
            FastRequest= #object{name = <<"KamfResponse">>,
                                 members = [{id, Id},
                                            {success, true},
                                            {result, <<"fastpong">>}]},
            Pid ! {reply, FastRequest};
        Other ->
            ?INFO("test client: unrecognized msg: ~p", [Other]),
            erlang:error(fail)
    end.


%% =================== internals =====================
t_start() ->
    put(t_start, now()).

t_clock() ->
    {_A0, B0, C0} = get(t_start),
    Start = 1000000 * B0 + C0,
    {_A, B, C} = now(),
    Finish = 1000000 * B + C,
    Res = Finish - Start,
    put(t_clock, Res),
    log({network_rpc, Res}),
    Res.

log(Msg) ->
    case get(use_log_server) of
        true ->
            log_server:log(Msg);
        _ ->
            ok
    end.

