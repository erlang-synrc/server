%%----------------------------------------------------------------------
%% @author Vladimir Baranov <baranoff.vladimir@gmail.com>
%% @copyright Paynet Internet ve Bilisim Hizmetleri A.S. All Rights Reserved.
%% @doc
%%    Tournaments lobby holder. Hold list of active users and other stuff.
%% @end
%%--------------------------------------------------------------------
-module(nsm_srv_tournament_lobby).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("nsm_mq/include/nsm_mq.hrl").
-include_lib("nsm_srv/include/user.hrl").
-include_lib("nsm_srv/include/tournaments.hrl").
-include_lib("alog/include/alog.hrl").
-include("config.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([start_link/1]).

-export([heartbeat/1,
         heartbeat_finish/1,
	 check_tournament_time/1,
         messages_callback/2]).

-export([active_users/1,
         chat_history/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		 terminate/2, code_change/3]).

-record(state, {active_users,
				heartbeat_users, %% heartbeats acc
				tournament_id,
				server,
				chat_history,    %% place to store chart messages
                                start_date,
				tournament
				}).

-define(HEARTBEAT_INTERVAL, 10000).

-define(HEARTBEAT_TIMEOUT, 1000).

-define(CHAT_HISTORY_SIZE, 100).


%% ====================================================================
%% External functions
%% ====================================================================
start_link(TID) ->
	%% FIXME: here we transform integer or other type to atom, this can
    %% bring us to memory leaks
	gen_server:start_link({local, tid_to_atom(TID)}, ?MODULE, [TID], []).

heartbeat(Server) ->
	gen_server:cast(Server, heartbeat).

check_tournament_time(Server) ->
    ?INFO("check tournament time~n"),
	gen_server:call(Server, check_tournament_time).

start_tournament(Server) ->
	gen_server:cast(Server, start_tournament).

%% called after heartbeat timeout. After this call, received heartbeats became
%% active user list
heartbeat_finish(Server) ->
	gen_server:cast(Server, heartbeat_finish).

%% callback called from message from queue delivered
messages_callback(Envelope, Server) ->
	gen_server:cast(Server, {messages_callback, Envelope}).

active_users(TID) ->
	Server = tid_to_atom(TID),
	gen_server:call(Server, active_users).

chat_history(TID) ->
	Server = tid_to_atom(TID),
	gen_server:call(Server, chat_history).


%% ====================================================================
%% Server functions
%% ====================================================================

%% TID - tournament id
init([TID]) ->
    {H,M,S} = time(),
    Tour = tournaments:get(TID),
    {TH,TM,TS} = Tour#tournament.start_time,
    Server = self(),

    %% start heartbeat timer
    timer:apply_interval(?HEARTBEAT_INTERVAL, ?MODULE, heartbeat, [Server]),
    timer:apply_after(timer:hms(H,M+1,S) - timer:hms(H,M,S), ?MODULE, check_tournament_time, [Server]),

    %% subscribe to heartbeat replies
    nsx_util_notification:subscribe_tournament_lobby(TID, {?MODULE, messages_callback}, Server),

    %% make heartbeat request
    ?MODULE:heartbeat(Server),
    ?INFO("~w: started", [Server]),

    {ok, #state{tournament_id = TID,
				active_users = dict:new(),
				heartbeat_users = empty,
				server = Server,
				chat_history = queue:new(),
				start_date = Tour#tournament.start_date,
                                tournament = Tour
				}}.

handle_call(active_users, _From, #state{active_users = AU} = State) ->
	?DBG("active users request: ~p", [self()]),
	List = dict:to_list(AU),
	?DBG("active users request. Active users count: ~p", [length(List)]),
    {reply, [User || {_, User} <- List], State};

handle_call(check_tournament_time, _From, State) ->
    StartDate = State#state.start_date,
    Status = case date() of
        StartDate ->
            ?INFO("Tournament activated"),
            start_tournament(self()),
            active;
        _ ->
            ?INFO("Tournament idle"),
            timer:apply_interval({24,0,0},?MODULE,check_tournament_time,self()),
            idle
    end,
    {reply, Status, State};

handle_call(chat_history, _From, #state{chat_history = ChatHistory} = State) ->
	List = queue:to_list(ChatHistory),
	?DBG("chat history request. History records count: ~p", [length(List)]),
    {reply, List, State}.

create_tables(List) ->
    Tables = dict:new(),
    ok.

dump_tables(ArraysList, Check) -> lists:map(fun(A) -> ?INFO("~p",[A]) end, ArraysList).

start_tournament(TID, ListUsers) ->
    ?INFO("Start Tournament ~p", [TID]),
    {C,Tables} = rpc:call(?GAMESERVER_NODE,shuffle,generate_tournament,[4,4]),

    dump_tables(Tables,""),

%    Tour = State#state.tournament,
%    Teams = [ begin
%        Team = tournaments:create_team(User#user.username),
%        nsm_db:put(Team),
%        Team
%    end || {_,User} <- List],

%    nsm_db:put(Tour#tournament{teams = Teams}),

    nsx_util_notification:notify_tournament_start_game(TID,ListUsers,{"DATA"}),
    ok.

handle_cast(start_tournament, State) ->
    Tour = State#state.tournament,
    List = dict:to_list(State#state.active_users),
    ListUsers = [ "kunthar","maxim","alice","kate"], %User#user.username || {_,User} <- List],
    start_tournament(Tour#tournament.id,ListUsers),
    {noreply, State};

handle_cast(heartbeat, State) ->
	TID = State#state.tournament_id,
	nsx_util_notification:notify_tournament_heartbeat_request(TID),

	{ok, _} = timer:apply_after(?HEARTBEAT_TIMEOUT, ?MODULE, heartbeat_finish,
					  [State#state.server]),
	?DBG("heartbeat sent"),
    {noreply, State#state{heartbeat_users = dict:new()}};

handle_cast(heartbeat_finish, #state{heartbeat_users = HU} = State) ->
	%% move heratbeat users to active users. If we will get heartbeat reply
	%% after this move - just add active user to active users list
	?DBG("heartbeat results waiting finished. Users: ~p", [dict:size(HU)]),
	{noreply, State#state{active_users = HU, heartbeat_users = empty}};

handle_cast({messages_callback, Envelope},
			#state{active_users = AU, heartbeat_users = HU,
				   chat_history = ChatHistory} = State) ->
    ?DBG("message callback: Envelope=~p", [Envelope]),

    ParsedKey = nsm_mq_lib:key_to_list(Envelope#envelope.routing_key),

    case ParsedKey of
        ["tournament", _TId, "heartbeat", "reply"] ->
            User = Envelope#envelope.payload,
            ?DBG("heartbeat reply: User: ~p", [User]),
            {AU1, HU1} = add_to_userlist(User, AU, HU),
            {noreply, State#state{heartbeat_users = HU1, active_users = AU1}};

        %% user signals that he is ready
        ["tournament", _TId, "user", UserId, "ready"] ->
            ?INFO("user activated: User: ~p", [UserId]),
            User = Envelope#envelope.payload,
            {AU1, HU1} = add_to_userlist(User, AU, HU),
            {noreply, State#state{active_users = AU1, heartbeat_users = HU1}};

        %% chat event. Payload has format of {User, Action, Message}
        ["tournament", _TId, "chat", _Action] ->
            NewChatHistory = save_chat(Envelope#envelope.payload,
                                       ChatHistory),
            {noreply, State#state{chat_history = NewChatHistory}}
    end.

handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
%% Add user to userlis store. If Heartbeat is processing currently - add
%% to both active and heartbeat lists.

add_to_userlist(User, ActiveUsers, HeartbeatUsers) ->
    Key  = User#user.username,
    HeartbeatUsers1 = case HeartbeatUsers of
                          empty ->
                              empty;
                          %% we collecting heartbeat replies
                          _ ->
                              dict:store(Key, User, HeartbeatUsers)
                      end,
    {dict:store(Key, User, ActiveUsers), HeartbeatUsers1}.

tid_to_atom(String) when is_list(String) ->
	try
		list_to_existing_atom(String)
	catch
		_:_ ->
			list_to_atom(String)
	end;
tid_to_atom(Int) when is_integer(Int) ->
	tid_to_atom(integer_to_list(Int));
tid_to_atom(Other) ->
	[L] = io_lib:format("~p", Other),
	tid_to_atom(L).


save_chat({_UserId, _Action, _Message} = Msg, ChatHistory) ->
	Q0 = queue:in(Msg, ChatHistory),
	case queue:len(Q0) of
		Len when Len >= ?CHAT_HISTORY_SIZE ->
			queue:drop(Q0);
		_ ->
			Q0
	end.
