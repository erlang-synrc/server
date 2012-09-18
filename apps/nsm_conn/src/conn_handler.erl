%%----------------------------------------------------------------------
%% @author Paul Peregud <paulperegud@gmail.com>
%% @copyright Paynet Internet ve Bilisim Hizmetleri A.S. All Rights Reserved.
%% @doc
%% Provides API via KAMF protocol and manages TCP connection.
%% Used by Flex client.
%% @end
%%-------------------------------------------------------------------
-module(conn_handler).
-behaviour(gen_server).

-include_lib("nsg_srv/include/conf.hrl").
-include_lib("nsg_srv/include/classes.hrl").
-include_lib("nsg_srv/include/requests.hrl").
-include_lib("nsg_srv/include/kamf.hrl").
-include_lib("nsg_srv/include/games.hrl").
-include_lib("amf/include/amf.hrl").
-include_lib("alog/include/alog.hrl").

-export([policy_file_text/0]).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
          initiated = false,
          buffer = <<>>,
          socket,
          request_id = 1,
          message_id = 1,
          running_requests = dict:new(),
          session       %% corresponding session pid
         }).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

handle_cast(_Msg, State) ->
    ?INFO("Unrecognized cast: ~p~n", [_Msg]),
    {stop, {unknown_cast, _Msg}, State}.

%%FIX: IMO it will be much nicer to move actual parsing out of
%%handle_info({tcp...}). handle_info should only gather data into a
%%buffer, decoding this data should be done in separate function

handle_info({tcp, Socket, <<"<policy-file-request/>", 0, Rest/binary>>}, #state{} = State) ->
    ok = inet:setopts(Socket, [{active, once}]),
    ?INFO("(KAMF) policy-file-request received", []),
    Bin = policy_file_text(),
    ok = gen_tcp:send(Socket, Bin),
    handle_info({tcp, Socket, Rest}, State);

handle_info({tcp, Socket, <<?KAMF_MAGIC:48, Rest/binary>>}, #state{} = State) ->
    ok = inet:setopts(Socket, [{active, once}]),
    ?INFO("protocol (KAMF) initialized, receiving data...", []),
    %FIX: it may be better to start sessions under simple_one_to_one
    %supervisor for better OTP-compliance
    {ok, SPid} = game_session:start_link(self()),
    ?INFO("FLEXCLIENT on socket ~p started with game_session pid ~p", [Socket, SPid]),
    handle_info({tcp, Socket, Rest}, State#state{initiated = true, session = SPid});

handle_info({tcp, _Socket, Data},
            #state{initiated = false, buffer = <<>>} = State) ->
    {noreply, State#state{buffer = Data}};

handle_info({tcp, Socket, Data},
            #state{initiated = false, buffer = Saved} = State) ->
    Size = size(Saved),
    case Size of
        _ when Size > 256 ->
            {stop, normal, State};
        _ -> handle_info({tcp, Socket, <<Saved:Size/binary, Data/binary>>},
                         State#state{buffer = <<>>})
    end;

%% TODO: long sessions grow because of running_requests dict
handle_info({tcp, Socket, Body}, #state{initiated = true, buffer = Saved} = State) ->
    NS = case ?AMF:reconstruct(Body, Saved) of
             {done, [], NSaved} ->
                 ok = inet:setopts(Socket, [{active, once}]),
                 State#state{buffer = NSaved};
             {done, Buffers, NSaved} ->
                 process_packets(Buffers, State),
                 ok = inet:setopts(Socket, [{active, once}]),
                 State#state{buffer = NSaved}
         end,
    {noreply, NS};

handle_info({tcp_error, _Socket, Reason}, _State) ->
    ?INFO("Connection error: ~p.", [{tcp_error, Reason}]),
    {stop, {tcp_error, Reason}, _State};

handle_info({tcp_closed, _Socket}, _State) ->
    ?INFO("Client Disconnected.", []),
    {stop, normal, _State};

handle_info(_Info, #state{socket = _Socket} = State) ->
    ?INFO("unrecognized internal msg: ~p", [_Info]),
    {stop, {unknown_info, _Info}, State}.

handle_call({send_request, RequestSpec}, From, #state{socket = Socket, request_id = Id0, running_requests = RR} = State) ->
    Id = Id0 + 1,
    Rec = ?AMF:create_request(Id, RequestSpec),
    ?INFO("AMF REQUEST: ~p", [Rec]),
    case send_record(Rec, Socket) of
        ok ->
            NState = State#state{request_id = Id, running_requests = dict:store(Id, From, RR)},
            {noreply, NState};
        {error, closed} ->
            ?INFO("socket fail. closed", []),
            gen_server:reply(From, tcp_closed),
            {stop, normal, State}
    end;

handle_call({send_message, MsgSpec}, From, #state{socket = Socket, message_id = Id0} = State) ->
    Id = Id0+1,
    Rec = ?AMF:create_message(Id, MsgSpec),
    case send_record(Rec, Socket) of
        ok ->
            NState = State#state{message_id = Id},
            ?INFO("AMF: ~p", [Rec]),
            {reply, ok, NState};
        {error, closed} ->
            ?INFO("socket fail. closed", []),
            gen_server:reply(From, tcp_closed),
            {stop, normal, State}
    end;

handle_call({reply, Id, {api_error, Reason}}, From, #state{socket = Socket} = State) ->
    ?INFO("notifying client about error. Request id: ~p, reason: ~p", [Id, Reason]),
    Rec = ?AMF:create_fatalerror(Id, <<"request">>, Reason),
    case send_record(Rec, Socket) of
        ok ->
            {reply, ok, State};
        {error, closed} ->
            ?INFO("socket fail. closed", []),
            gen_server:reply(From, tcp_closed),
            {stop, normal, State}
    end;

handle_call({reply, Id, {api_success, Answer}}, From, #state{socket = Socket} = State) ->
    Rec = ?AMF:create_response(Id, Answer),
    case send_record(Rec, Socket) of
        ok ->
            {reply, ok, State};
        {error, closed} ->
            ?INFO("socket fail. closed", []),
            gen_server:reply(From, tcp_closed),
            {stop, normal, State}
    end;

handle_call({set_socket, Socket}, _From, #state{} = State) ->
    ok = inet:setopts(Socket, [{active, once}]),
    {reply, ok, State#state{socket = Socket}};

handle_call(Request, From, State) ->
    ?INFO("Unrecognized call: ~p from ~p", [Request, From]),
    {stop, {unknown_call, Request, From}, unrecognized_call, State}.

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

send_record(Record, Socket) ->
    Obj = ?AMF:record_to_object(Record),
    Bin = ?AMF:encode(Obj),
    gen_tcp:send(Socket, ?AMF:construct(Bin)).


process_packets([], _State) ->
    ok;
process_packets([T | H], #state{running_requests = RR, session = SPid} = State ) ->
    Self = self(),
    {#object{} = Obj, _Rest} = ?AMF:decode(T),
    case ?AMF:object_to_record(Obj) of
        #'KamfRequest'{id = Id, method = Method, args = Args} = _Packet ->
            _Pid = proc_lib:spawn_link(
                     fun() ->
                             Res = try
                                       Msg = api_utils:to_known_record(Method, Args),
                                       Answer = game_session:process_request(SPid, "FLEX CLIENT", Msg),
%                                       ?INFO("           Method: ~p, Args: ~p", [Method, Args]),
%                                       ?INFO(" Id ~p Answer ~p", [Id, Answer]),
                                       {reply, Id, {api_success, Answer}}
                                   catch
                                       _Err:Reason ->
%                                           ?INFO("process_packets ~p failed with msg: ~p", [T, {_Err,Reason}]),
                                           {reply, Id, {api_error, Reason}}
                                   end,
                             gen_server:call(Self, Res)
                     end);
        #'KamfResponse'{id = Id, result = Result} ->
            From = dict:fetch(Id, RR),
            gen_server:reply(From, Result)
    end,
    process_packets(H, State).

%% allow-everything policy file. Need to be changed to be secure.
policy_file_text() ->
    Bin = [<<"<?xml version=\"1.0\"?>\n",
            "<!DOCTYPE cross-domain-policy"
            " SYSTEM \"http://www.macromedia.com/xml/dtds/cross-domain-policy.dtd\">\n",
            "<cross-domain-policy>\n",
            "<allow-access-from domain=\"*\" to-ports=\"">>,
           integer_to_list(?LISTEN_PORT),
           <<"\"/>\n",
            "</cross-domain-policy>\n",
            0>>],
    iolist_to_binary(Bin).

