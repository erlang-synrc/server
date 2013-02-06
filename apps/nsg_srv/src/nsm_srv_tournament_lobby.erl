-module(nsm_srv_tournament_lobby).
-behaviour(gen_server).
-include_lib("nsm_mq/include/nsm_mq.hrl").
-include_lib("nsm_db/include/user.hrl").
-include_lib("nsm_db/include/tournaments.hrl").
-include_lib("nsx_config/include/log.hrl").
-include_lib("nsx_config/include/config.hrl").
-compile(export_all).

-record(state, {active_users,
                heartbeat_users, %% heartbeats acc
                tournament_id,
                server,
                chat_history,    %% place to store chart messages
                joined_users,
                start_date,
                last_check,
                start_time,
                tournament}).

-define(HEARTBEAT_INTERVAL, 10000).
-define(HEARTBEAT_TIMEOUT, 1000).
-define(CHAT_HISTORY_SIZE, 100).
-define(TOURNAMENT_START_QUANT, 100).

start_link(TID) -> gen_server:start_link({local, tid_to_atom(TID)}, ?MODULE, [TID], []).
start_link(TID,chat) -> gen_server:start_link({local, tid_to_atom(TID)}, ?MODULE, {[TID],chat}, []).
heartbeat(Server) -> gen_server:cast(Server, heartbeat).
check_tournament_time(Server) -> gen_server:call(Server, check_tournament_time).
start_tournament(Server) -> gen_server:cast(Server, start_tournament).
heartbeat_finish(Server) -> gen_server:cast(Server, heartbeat_finish).
messages_callback(Envelope, Server) -> 	gen_server:cast(Server, {messages_callback, Envelope}).
active_users(TID) -> Server = tid_to_atom(TID), gen_server:call(Server, active_users).
chat_history(TID) -> Server = tid_to_atom(TID), gen_server:call(Server, chat_history).
joined_users(TID) -> Server = tid_to_atom(TID), gen_server:call(Server, joined_users).

init([TID]) ->
    Tour = nsm_tournaments:get(TID),
    ?INFO("Lobby Starting: ~p",[Tour]),
    Server = self(),
    timer:apply_after(?TOURNAMENT_START_QUANT, ?MODULE, check_tournament_time, [Server]),
    nsx_msg:subscribe_tournament_lobby(TID, {?MODULE, messages_callback}, Server),
    ?MODULE:heartbeat(Server),
    ?INFO("~w: started", [Server]),
    {ok, #state{tournament_id = TID, active_users = dict:new(), heartbeat_users = empty, last_check = now(),
                server = Server, chat_history = queue:new(), start_date = Tour#tournament.start_date, 
                joined_users = nsm_tournaments:joined_users(Tour#tournament.id),
                start_time = Tour#tournament.start_time, tournament = Tour}};

init({[TID],chat}) ->
    Server = self(),
    ?INFO("Lobby Starting: ~p",[TID]),
    nsx_msg:subscribe_tournament_lobby(TID, {?MODULE, messages_callback}, Server),
    ?INFO("~w: started", [Server]),
    {ok, #state{tournament_id = TID, active_users = dict:new(), heartbeat_users = empty, last_check = now(),
                server = Server, chat_history = queue:new()}}.

handle_call(active_users, _From, #state{active_users = AU} = State) ->
    List = dict:to_list(AU),
    ?INFO("active users request. Active users count: ~p", [length(List)]),
    {reply, [User#user.username || {_, User} <- List], State};

handle_call(joined_users, _From, State) ->
    {JU,T} = case timer:now_diff(now(),State#state.last_check) div 1000000 > 30 of
        true ->  Full = nsm_tournaments:joined_users(list_to_integer(State#state.tournament_id)),
                 {Full,now()};
        false -> {State#state.joined_users,State#state.last_check}
    end,
    ?INFO("Joined users request."),
    {reply, term_to_binary(JU), State#state{joined_users = JU, last_check = T}};

handle_call(check_tournament_time, _From, State) ->
    StartDate = State#state.start_date,
    {SH, SM, _} = State#state.start_time,
    Status = case {date(), time()} of
        {StartDate, {SH, SM, _}} ->
            start_tournament(self()),
            active;
        _ ->
            timer:apply_after(?TOURNAMENT_START_QUANT, ?MODULE, check_tournament_time, [self()]),
            idle
    end,
    {reply, Status, State};

handle_call(chat_history, _From, #state{chat_history = ChatHistory} = State) ->
    List = queue:to_list(ChatHistory),
    ?INFO("chat history request. History records count: ~p", [length(List)]),
    {reply, List, State}.

handle_cast(start_tournament, State) ->  
    Tour = State#state.tournament,
    TId = Tour#tournament.id,
    case game_manager:get_tournament(TId) of
        [] ->
            NumberOfUsers = Tour#tournament.players_count,
            TIDinDB = Tour#tournament.id,
            Quota = Tour#tournament.quota,
            Tours = Tour#tournament.tours,
            Speed = Tour#tournament.speed,
            Gifts = Tour#tournament.awards,
            case length(nsm_tournaments:joined_users(TId)) < NumberOfUsers of
                true ->
                    ?INFO("Tournament ~p canceled", [TId]),
                    nsm_db:put(Tour#tournament{status=canceled});
                false ->
                    TourId = game_manager:start_tournament(TIDinDB,1,NumberOfUsers,Quota,Tours,Speed,Gifts),
                    ?INFO("Tournament ~p started as ~p", [TId, TIDinDB]),
                    nsx_msg:notify(["tournament", integer_to_list(TIDinDB), "start"], {TourId})
            end;
        _ -> ok
    end,
    {noreply, State};

handle_cast(heartbeat, State) ->
    TID = State#state.tournament_id,
%    nsx_msg:notify_tournament_heartbeat_request(TID),

%    {ok, _} = timer:apply_after(?HEARTBEAT_TIMEOUT, ?MODULE, heartbeat_finish,  [State#state.server]),
    {noreply, State#state{heartbeat_users = dict:new()}};

handle_cast(heartbeat_finish, #state{heartbeat_users = HU} = State) -> 
    {noreply, State#state{active_users = HU, heartbeat_users = empty}};

handle_cast({messages_callback, Envelope}, #state{active_users = AU, heartbeat_users = HU, chat_history = ChatHistory} = State) ->
    ParsedKey = nsm_mq_lib:key_to_list(Envelope#envelope.routing_key),
    case ParsedKey of
        ["tournament", _TId, "heartbeat", "reply"] ->
            User = Envelope#envelope.payload,
            {AU1, HU1} = add_to_userlist(User, AU, HU),
            {noreply, State#state{heartbeat_users = HU1, active_users = AU1}};
        ["tournament", _TId, "user", _UserId, "ready"] ->
            User = Envelope#envelope.payload,
            {AU1, HU1} = add_to_userlist(User, AU, HU),
            {noreply, State#state{active_users = AU1, heartbeat_users = HU1}};
        ["tournament", _TId, "chat", _Action] ->
            NewChatHistory = save_chat(Envelope#envelope.payload, ChatHistory),
            {noreply, State#state{chat_history = NewChatHistory}}
    end.

handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

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

tid_to_atom(String) when is_list(String) -> try list_to_existing_atom(String) catch _:_ -> list_to_atom(String) end;
tid_to_atom(Int) when is_integer(Int) -> tid_to_atom(integer_to_list(Int));
tid_to_atom(Atom) when is_atom(Atom) -> Atom;
tid_to_atom(Other) -> [L] = io_lib:format("~p", Other), tid_to_atom(L).

save_chat({_UserId, _Action, _Message} = Msg, ChatHistory) ->
    Q0 = queue:in(Msg, ChatHistory),
    case queue:len(Q0) of
         Len when Len >= ?CHAT_HISTORY_SIZE -> queue:drop(Q0);
         _ -> Q0
    end.
