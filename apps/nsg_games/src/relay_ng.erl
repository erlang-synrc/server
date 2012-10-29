%%% -------------------------------------------------------------------
%%% Author  : Sergei Polkovnikov <serge.polkovnikov@gmail.com>
%%% Description :
%%%
%%% Created : Oct 16, 2012
%%% -------------------------------------------------------------------
-module(relay_ng).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("nsg_srv/include/basic_types.hrl").
-include_lib("nsx_config/include/log.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([start/1, table_message/2, table_request/2]).
-export([stop/1, subscribe/4, publish/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {subscribers,
                players,
                observers_allowed  :: boolean(),
                table              :: {atom(), pid()}
               }).

-record(subscriber,
        {pid          :: pid(),
         user_id,
         player_id    :: integer()
        }).

-record(player,
        {id           :: integer(),
         user_id,
         status       :: online | offline
        }).

%% ====================================================================
%% External functions
%% ====================================================================


start(Params) ->
    gen_server:start(?MODULE, [Params], []).

stop(Relay) ->
    table_message(Relay, stop).

subscribe(Relay, SessionPid, User, RegNum) ->
    client_request(Relay, {subscribe, SessionPid, User, RegNum}).

publish(Relay, Message) ->
    client_message(Relay, {publish, Message}).

table_message(Relay, Message) ->
    gen_server:cast(Relay, {table_message, Message}).

table_request(Relay, Request) ->
    table_request(Relay, Request, 5000).

table_request(Relay, Request, Timeout) ->
    gen_server:call(Relay, {table_request, Request}, Timeout).

client_message(Relay, Message) ->
    gen_server:cast(Relay, {client_message, Message}).

client_request(Relay, Request) ->
    client_request(Relay, Request, 5000).

client_request(Relay, Request, Timeout) ->
    gen_server:call(Relay, {client_request, Request}, Timeout).

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([Params]) ->
    PlayersInfo = proplists:get_value(players, Params),
    ObserversAllowed = proplists:get_value(observers_allowed, Params),
    Table = proplists:get_value(table, Params),
    Players = init_players(PlayersInfo),
    {ok, #state{subscribers = subscribers_init(),
                players = Players,
                observers_allowed = ObserversAllowed,
                table = Table}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call({client_request, Request}, From, State) ->
    handle_client_request(Request, From, State);

handle_call({table_request, Request}, From, State) ->
    handle_table_request(Request, From, State);

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast({client_message, Msg}, State) ->
    handle_client_message(Msg, State);

handle_cast({table_message, Msg}, State) ->
    handle_table_message(Msg, State);

handle_cast(_Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

handle_info({'DOWN', _, process, Pid, _},
            #state{subscribers = Subscribers, players = Players,
                   table = {TableMod, TablePid}} = State) ->
    case find_subscriber(Pid, Subscribers) of
        {ok, #subscriber{player_id = undefined}} ->
            NewSubscribers = del_subscriber(Pid, Subscribers),
            {noreply, State#state{subscribers = NewSubscribers}};
        {ok, #subscriber{player_id = PlayerId}} ->
            NewSubscribers = del_subscriber(Pid, Subscribers),
            case find_subscribers_by_player_id(PlayerId, NewSubscribers) of
                [] ->
                    NewPlayers = update_player_status(PlayerId, offline, Players),
                    TableMod:relay_message(TablePid, {player_disconnected, PlayerId}),
                    {noreply, State#state{subscribers = NewSubscribers, players = NewPlayers}};
                [_|_] ->
                    {noreply, State#state{subscribers = NewSubscribers}}
            end;
        error ->
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

handle_client_request({subscribe, Pid, #'PlayerInfo'{id = UserId}, observer}, _From,
                      #state{observers_allowed = ObserversAllowed,
                             subscribers = Subscribers} = State) ->
    if ObserversAllowed ->
           NewSubscribers = store_subscriber(Pid, UserId, undefined, Subscribers),
           erlang:monitor(process, Pid),
           {reply, ok, State#state{subscribers = NewSubscribers}};
       true ->
           {reply, {error, observers_not_allowed}, State}
    end;


handle_client_request({subscribe, Pid, #'PlayerInfo'{id = UserId}, PlayerId}, _From,
                      #state{players = Players, subscribers = Subscribers,
                             table = {TableMod, TablePid}} = State) ->
    ?INFO("RELAY_NG Subscription request from user ~p, PlayerId: ~p", [UserId, PlayerId]),
    case find_player(PlayerId, Players) of
        {ok, #player{user_id = UserId}} ->
            NewPlayers = update_player_status(PlayerId, online, Players),
            NewSubscribers = store_subscriber(Pid, UserId, PlayerId, Subscribers),
            erlang:monitor(process, Pid),
            TableMod:relay_message(TablePid, {player_connected, PlayerId}),
            {reply, ok, State#state{players = NewPlayers, subscribers = NewSubscribers}};
        {ok, #player{}=P} ->
           ?INFO("RELAY_NG Subscription for user ~p rejected. Another owner of the PlayerId <~p>: ~p", [UserId, PlayerId, P]),
           {reply, {error, not_player_id_owner}, State};
        error ->
            {reply, {error, unknown_player_id}, State}
    end;


handle_client_request(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%%===================================================================

handle_table_request({register_player, UserId, PlayerId}, _From,
                     #state{players = Players} = State) ->
    NewPlayers = store_player(PlayerId, UserId, offline, Players),
    {reply, ok, State#state{players = NewPlayers}};


handle_table_request({unregister_player, PlayerId}, _From,
                     #state{players = Players,
                            subscribers = Subscribers} = State) ->
    NewPlayers = del_player(PlayerId, Players),
    UpdSubscribers = find_subscribers_by_player_id(PlayerId, Subscribers),
    F = fun(S, Acc) ->
                store_subscriber_rec(S#subscriber{player_id = undefined}, Acc)
        end,
    NewSubscribers = lists:foldl(F, Subscribers, UpdSubscribers),
    {reply, ok, State#state{players = NewPlayers,
                            subscribers = NewSubscribers}};


handle_table_request(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%===================================================================


handle_client_message(_Msg, State) ->
    {noreply, State}.

%%===================================================================

handle_table_message({publish, Msg}, #state{subscribers = Subscribers} = State) ->
    ?INFO("RELAY_NG The table publish message: ~p", [Msg]),
    Receipients = subscribers_to_list(Subscribers),
    [gen_server:cast(Pid, Msg) || #subscriber{pid = Pid} <- Receipients], %% XXX
    {noreply, State};

handle_table_message({to_client, PlayerId, Msg}, #state{subscribers = Subscribers} = State) ->
    Receipients = find_subscribers_by_player_id(PlayerId, Subscribers),
    [Pid ! Msg || #subscriber{pid = Pid} <- Receipients], %% XXX
    {noreply, State};

handle_table_message(stop, State) ->
    {stop, normal, State};

handle_table_message(_Msg, State) ->
    {noreply, State}.


%%===================================================================



init_players(PlayersInfo) ->
    init_players(PlayersInfo, players_init()).

init_players([], Players) ->
    Players;
init_players([{PlayerId, UserId} | PlayersInfo], Players) ->
    NewPlayers = store_player(PlayerId, UserId, undefined, Players),
    init_players(PlayersInfo, NewPlayers).

%%===================================================================

players_init() -> midict:new().

store_player(PlayerId, UserId, Status, Players) ->
    store_player_rec(#player{id = PlayerId, user_id = UserId, status = Status}, Players).

store_player_rec(#player{id = PlayerId, user_id = _UserId, status = _Status} = Rec, Players) ->
    midict:store(PlayerId, Rec, [], Players).

del_player(PlayerId, Players) ->
    midict:erase(PlayerId, Players).

fetch_player(PlayerId, Players) ->
    midict:fetch(PlayerId, Players).

find_player(PlayerId, Players) ->
    midict:find(PlayerId, Players).

update_player_status(PlayerId, Status, Players) ->
    Rec = fetch_player(PlayerId, Players),
    store_player_rec(Rec#player{status = Status}, Players).

%%===================================================================

subscribers_init() -> midict:new().

store_subscriber(Pid, UserId, PlayerId, Subscribers) ->
    store_subscriber_rec(#subscriber{pid = Pid, user_id = UserId, player_id = PlayerId}, Subscribers).

store_subscriber_rec(#subscriber{pid = Pid, user_id = _UserId, player_id = PlayerId} = Rec, Subscribers) ->
    midict:store(Pid, Rec, [{player_id, PlayerId}], Subscribers).

del_subscriber(Pid, Subscribers) ->
    midict:erase(Pid, Subscribers).

find_subscriber(Pid, Subscribers) ->
    midict:find(Pid, Subscribers).

find_subscribers_by_player_id(PlayerId, Subscribers) ->
    midict:geti(PlayerId, player_id, Subscribers).

subscribers_to_list(Subscribers) ->
    midict:all_values(Subscribers).
