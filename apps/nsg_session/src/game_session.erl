-module(game_session).
-behaviour(gen_server).

-include_lib("nsg_srv/include/social_actions.hrl").
-include_lib("nsg_srv/include/logging.hrl").
-include_lib("nsg_srv/include/games.hrl").
-include_lib("nsg_srv/include/classes.hrl").
-include_lib("nsg_srv/include/requests.hrl").
-include_lib("nsg_srv/include/settings.hrl").
-include_lib("nsm_db/include/config.hrl").
-include_lib("nsx_config/include/log.hrl").

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([process_request/3, get_player_info/1, get_player_id/1, logout/1]).
-export([bot_session_attach/2, bot_join_game/2]).

-define(SERVER, ?MODULE).

-record(state, {
          user = undefined,
          rpc,     %% rpc process
          rpc_mon, %% rpc monitor referense
          games = [],
          chats = [],
          match_request,
          rels_notif_channel = undefined,
          rels_players =[]
         }).

-record(participation, {
          game_id       :: 'GameId'(), %% generated by id_generator
          reg_num       :: integer(),
          rel_module    :: atom(),
          rel_pid       :: pid(),      %% relay, which handles communication gameman maps game_id onto pid
          tab_module    :: atom(),
          tab_pid       :: pid(),
          ref           :: any(),      %% monitor reference to relay
          role = viewer :: atom()      %% [viewer, player, ghost]
         }).

start_link(RPC) when is_pid(RPC) ->
    gen_server:start_link(?MODULE, [RPC], []).

get_player_info(Pid) ->
    gen_server:call(Pid, #get_player_info{player_id = 0}).

get_player_id(Pid) ->
    gen_server:call(Pid, get_player_id).

logout(Pid) ->
    Pid ! terminate.

bot_session_attach(Pid, UserInfo) ->
    gen_server:cast(Pid, {bot_session_attach, UserInfo}).

bot_join_game(Pid, GameId) ->
    gen_server:cast(Pid, {bot_join_game, GameId}).

process_request(Pid, Source, Msg) ->
    ?INFO("API from ~p payload ~p pid ~p",[Source,Msg,Pid]),
    gen_server:call(Pid, Msg).

init([RPC]) ->
    MonRef = erlang:monitor(process, RPC),
    {ok, #state{rpc = RPC, rpc_mon = MonRef}}.

handle_call(#session_attach{token = Token}, _From, State = #state{user = undefined}) ->
    ?INFO("checking session token: ~p", [Token]),
    case auth_server:get_user_info(Token) of
        false ->
            self() ! terminate,
            ?INFO("failed session attach: ~p", [Token]),
            {reply, {error, invalid_token}, State};
        UserInfo ->
            ?INFO("successfull session attach. Your user info: ~p", [UserInfo]),
            {reply, UserInfo, State#state{user = UserInfo}}
    end;

handle_call(#session_attach_debug{token = Token, id = Id}, _From, State = #state{user = undefined}) ->
    ?INFO("checking debug session token: ~p", [{Token,Id}]),
    case {?IS_TEST, auth_server:get_user_info(Token, Id)} of
        {_Test, false} ->
            ?INFO("... ~p", [{_Test,false}]),
            self() ! terminate,
            {reply, {error, invalid_token}, State};
        {false, true} ->
            ?INFO("... ~p", [{false,true}]),
            self() ! terminate,
            {reply, {error, invalid_token}, State};
        {true, UserInfo} ->
            ?INFO("... ~p", [{true,UserInfo}]),
            {reply, UserInfo, State#state{user = UserInfo}};
        {false, UserInfo} ->
            ?INFO("... ~p", [{true,UserInfo}]),
            {reply, UserInfo, State#state{user = UserInfo}}
    end;

handle_call(_, _From, #state{user = undefined} = State) ->
    ?INFO("unknown session call", []),
    {reply, {error, do_session_attach_first}, State};

handle_call(#get_game_info{}, _From, State) ->
    ?INFO("session get game info", []),
    {reply, {error, not_implemented}, State};

handle_call(#logout{}, _From, State) ->
    ?INFO("client requests #logout{}", []),
    self() ! terminate,
    {reply, ok, State};

handle_call(#get_player_stats{player_id = PlayerId, game_type = Game}, _From, State) when is_binary(Game) ->
    ?INFO("get player stats", []),
    GameModule = api_utils:gametype_to_gamemodule(api_utils:gametype_to_atom(Game)),
    Res = GameModule:get_player_stats(PlayerId),
    {reply, Res, State};

handle_call(#get_player_info{player_id = 0}, _From, State = #state{user = User}) ->
     {reply, User, State};
handle_call(#get_player_info{player_id = _PlayerId}, _From, State = #state{}) ->
     {reply, {error, not_implemented}, State};
handle_call(get_player_id, _From, State = #state{user = User}) ->
    {reply, User#'PlayerInfo'.id, State};

handle_call(#chat{chat_id = GameId, message = Msg0}, _From,
            State = #state{user = User, games = Games, chats = Chats}) ->
    ?INFO("chat", []),
    Msg = #chat_msg{chat = GameId, content = Msg0,
                    author_id = User#'PlayerInfo'.id,
                    author_nick = User#'PlayerInfo'.login
                   },
    AllRelays = lists:append(Games, Chats),
    Participation = get_relay(GameId, AllRelays),
    Res = case Participation of
              false ->
                  {error, chat_not_registered};
              #participation{rel_pid = Srv, rel_module = RMod} ->
                  RMod:publish(Srv, Msg)
          end,
    {reply, Res, State};

handle_call(#social_action_msg{type=Type, initiator=P1, recipient=P2} = _Msg,
            _From, #state{user=User} = State) when User =/= undefined ->
    UserIdBin = User#'PlayerInfo'.id,
    ?INFO("social action msg from ~p to ~p (casted by ~p)", [P1, P2, UserIdBin]),
    UserId = binary_to_list(UserIdBin),
    case Type of
        ?SOCIAL_ACTION_SUBSCRIBE ->
            Subject = binary_to_list(P2),
            nsm_users:subscribe_user(UserId, Subject),
            {reply, ok, State};
        ?SOCIAL_ACTION_UNSUBSCRIBE ->
            Subject = binary_to_list(P2),
            nsm_users:remove_subscribe(UserId, Subject),
            {reply, ok, State};
        ?SOCIAL_ACTION_BLOCK ->
            Subject = binary_to_list(P2),
            nsx_msg:notify(["subscription", "user", UserId, "block_user"], {Subject}),
            {reply, ok, State};
        ?SOCIAL_ACTION_UNBLOCK ->
            Subject = binary_to_list(P2),
            nsx_msg:notify(["subscription", "user", UserId, "unblock_user"], {Subject}),
            {reply, ok, State};
        ?SOCIAL_ACTION_LOVE ->
            {reply, ok, State};
        ?SOCIAL_ACTION_HAMMER ->
            {reply, ok, State};
        UnknownAction ->
            ?ERROR("Unknown social action msg from ~p to ~p: ~w",
                   [P1,P2, UnknownAction]),
            {reply, {error, unknown_action}, State}
    end;

handle_call(#social_action{} = Msg, _From, State = #state{}) ->
    ?INFO("social action", []),
    #state{user = User, games = Games, chats = Chats} = State,
    GameId = Msg#social_action.game,
    Res = #social_action_msg{type = Msg#social_action.type,
                             game = GameId,
                             recipient = Msg#social_action.recipient,
                             initiator = User#'PlayerInfo'.id},
    AllRelays = lists:append(Games, Chats),
    Participation = get_relay(GameId, AllRelays),
    Ans = case Participation of
              false ->
                  {error, chat_not_registered};
              #participation{rel_pid = Srv, rel_module=RMod} ->
                  RMod:publish(Srv, Res)
          end,
    {reply, Ans, State};


handle_call(#subscribe_player_rels{players = Players}, _From,
            #state{user = User,
                   rels_notif_channel = RelsChannel,
                   rels_players = RelsPlayers,
                   rpc = RPC} = State) ->
    ?INFO("subscribe player relations notifications: ~p", [Players]),
    UserId = User#'PlayerInfo'.id,
    UserIdStr = binary_to_list(UserId),
    %% Create subscription if we need
    NewRelsChannel =
        if RelsChannel == undefined ->
               {ok, Channel} = nsx_msg:subscribe_for_user_actions(UserIdStr, self()),
               Channel;
           true ->
               RelsChannel
        end,
    %% Add players relations to which we need to common list
    F = fun(PlayerId, Acc) ->
                case lists:member(PlayerId, Acc) of
                    true -> Acc;
                    false -> [PlayerId | Acc]
                end
        end,
    NewRelsPlayers = lists:foldl(F, RelsPlayers, Players),

    %% Notify the client about current state of subscription relations.
    %% (Blocking relations state should be "false" at the start)
    F2 =
        fun(PlayerId) ->
                PlayerIdStr = binary_to_list(PlayerId),
                Type = case nsm_users:is_user_subscr(UserIdStr, PlayerIdStr) of
                           true -> ?SOCIAL_ACTION_SUBSCRIBE;
                           false -> ?SOCIAL_ACTION_UNSUBSCRIBE
                       end,
                Msg = #social_action_msg{initiator = UserId,
                                         recipient = PlayerId,
                                         type = Type
                                         },
                ok = send_message_to_player(RPC, Msg)
        end,
    lists:foreach(F2, Players),

    NewState = State#state{rels_players = NewRelsPlayers,
                           rels_notif_channel = NewRelsChannel},
    {reply, ok, NewState};


handle_call(#unsubscribe_player_rels{players = Players}, _From,
            #state{rels_notif_channel = RelsChannel,
                   rels_players = RelsPlayers
                  } = State) ->
    ?INFO("unsubscribe player relations notifications", []),

    %% Remove players from common list
    NewRelsPlayers = RelsPlayers -- Players,

    %% Remove subscription if we don't need it now
    NewRelsChannel =
        if NewRelsPlayers == [] -> nsm_mq_channel:close(RelsChannel),
               undefined;
           true ->
               RelsChannel
        end,

    NewState = State#state{rels_players = NewRelsPlayers,
                           rels_notif_channel = NewRelsChannel},
    {reply, ok, NewState};


%% handle_call(#match_me{game_type = Game}, _From, State) when is_binary(Game) ->
%% %    ?INFO("match me", []),
%%     GameModule = api_utils:gametype_to_gamemodule(api_utils:gametype_to_atom(Game)),
%%     RequestRef = match_maker:match_me(GameModule), %% FIXME : not exists
%%     {reply, RequestRef, State#state{match_request = RequestRef}};

handle_call(#join_game{game = GameId}, _From, #state{user = User, rpc = RPC} = State) ->
    UserId = User#'PlayerInfo'.id,
    ?INFO("join game ~p user ~p from ~p", [GameId, UserId,_From]),
    case get_relay(GameId, State#state.games) of
        #participation{} ->
            {reply, {error, already_joined}, State};
        false ->
            ?INFO("Requesting main relay info...",[]),
            case game_manager:get_relay_mod_pid(GameId) of
                {FLMod, FLPid} ->
                    ?INFO("Found the game: ~p. Trying to register...",[{FLMod, FLPid}]),
                    case FLMod:reg(FLPid, User) of
                        {ok, {RegNum, {RMod, RPid}, {TMod, TPid}}} ->
                            ?INFO("join to game relay: ~p",[{RMod, RPid}]),
                            ok = RMod:subscribe(RPid, self(), User, RegNum),
                            Ref = erlang:monitor(process, RPid),
                            Part = #participation{ref = Ref, game_id = GameId, reg_num = RegNum,
                                                  rel_module = RMod, rel_pid = RPid,
                                                  tab_module = TMod, tab_pid = TPid, role = player},
                            Res = #'TableInfo'{}, %relay:get_table_info(SecondLevelRelay),
                            {reply, Res, State#state{games = [Part | State#state.games]}};
                        {error, finished} ->
                            ?INFO("The game is finished: ~p.",[GameId]),
                            maybe_send_message(RPC, #disconnect{reason = <<"The game is finished">>}, State),
                            {reply, {error, finished}, State};
                        {error, out} ->
                            ?INFO("Out of the game: ~p.",[GameId]),
                            maybe_send_message(RPC, #disconnect{reason = <<"You was disconnected from the game">>}, State),
                            {reply, {error, out}, State};
                        {error, not_allowed} ->
                            ?ERROR("Not allowed to connect: ~p.",[GameId]),
                            maybe_send_message(RPC, #disconnect{reason = <<"You are not allowed to connect to this game">>}, State),
                            {reply, {error, not_allowed}, State}
                    end;
                undefined ->
                    ?ERROR("Game not found: ~p.",[GameId]),
                    maybe_send_message(RPC, #disconnect{reason = <<"The game you are trying to connect doesn't exist">>}, State),
                    {reply, {error, not_exists}, State}
            end
    end;


%% handle_call(#rematch{game = GameId}, _From, State) -> %% XXX WTF?
%%     ?INFO("rematch", []),
%%     UId = (State#state.user)#'PlayerInfo'.id,
%%     Participation = get_relay(GameId, State#state.games),
%%     ?INFO("ID: ~p, request rematch: ~p, my games: ~p", [UId, GameId, State#state.games]),
%%     case Participation of
%%         false ->
%%             ?INFO("A", []),
%%             {reply, {error, game_not_found}, State};
%%         #participation{pid = Pid, module = RMod} ->
%%             Res = RMod:do_rematch(Pid, self()),
%%             ?INFO("B. Res: ~p", [Res]),
%%             {reply, Res, State}
%%     end;

handle_call(#game_action{game = GameId} = Msg, _From, State) ->
%    ?INFO("game action ~p", [{GameId,Msg,_From}]),
    Participation = get_relay(GameId, State#state.games),
    case Participation of
        false ->
            {reply, {error, game_not_found}, State};
        #participation{reg_num = RegNum, tab_pid = TPid, tab_module = TMod} ->
            UId = (State#state.user)#'PlayerInfo'.id,
            ?INFO("PLAYER ~p MOVES ~p in GAME ~p",[UId,Msg,GameId]),
            {reply, TMod:submit(TPid, RegNum, Msg), State}
    end;

handle_call(#pause_game{game = GameId, action = Action}, _From, State) ->
%    ?INFO("pause game", []),
    UId = (State#state.user)#'PlayerInfo'.id,
    Participation = get_relay(GameId, State#state.games),
%    ?INFO("ID: ~p, pause game: ~p, my games: ~p", [UId, GameId, State#state.games]),
    case Participation of
        false ->
            ?INFO("A", []),
            {reply, {error, game_not_found}, State};
        #participation{reg_num = RegNum, tab_pid = TPid, tab_module = TMod} ->
            Signal = case Action of
                         <<"pause">> -> pause_game;
                         <<"resume">> -> resume_game
                     end,
            Res = TMod:signal(TPid, RegNum, {Signal, self()}),
            ?INFO("B. Res: ~p", [Res]),
            {reply, Res, State}
    end;

handle_call(_Request, _From, State) ->
    ?INFO("unrecognized call: ~p", [_Request]),
    Reply = ok,
    {stop, Reply, State}.

%% The notification from the current table to rejoin to the game
%% because the user for example was moved to another table.
handle_cast({rejoin, GameId} = Message, State = #state{user = User, games = Games, rpc = RPC}) ->
    ?INFO("Recived a notification from the table: ~p", [Message]),
    ?INFO("Requesting main relay info...",[]),
    case game_manager:get_relay_mod_pid(GameId) of
        {FLMod, FLPid} ->
            ?INFO("Found the game: ~p. Trying to register...",[{FLMod, FLPid}]),
            case FLMod:reg(FLPid, User) of
                {ok, {RegNum, {RMod, RPid}, {TMod, TPid}}} ->
                    ?INFO("join to game relay: ~p",[{RMod, RPid}]),
                    ok = RMod:subscribe(RPid, self(), User, RegNum),
                    Ref = erlang:monitor(process, RPid),
                    Part = #participation{ref = Ref, game_id = GameId, reg_num = RegNum,
                                          rel_module = RMod, rel_pid = RPid,
                                          tab_module = TMod, tab_pid = TPid, role = player},
                    NewGames = lists:keyreplace(GameId, #participation.game_id, Games, Part),
                    {noreply, State#state{games = NewGames}};
                {error, finished} ->
                    ?INFO("The game is finished: ~p.",[GameId]),
                    maybe_send_message(RPC, #disconnect{reason = <<"The game is finished">>}, State),
                    {stop, normal, State};
                {error, out} ->
                    ?INFO("Out of the game: ~p.",[GameId]),
                    maybe_send_message(RPC, #disconnect{reason = <<"You are kicked from the game">>}, State),
                    {stop, normal, State};
                {error, not_allowed} ->
                    ?ERROR("Not allowed to connect: ~p.",[GameId]),
                    maybe_send_message(RPC, #disconnect{reason = <<"You are not allowed to connect to this game">>}, State),
                    {stop, {error, not_allowed_to_join}, State}
            end;
        undefined ->
            ?ERROR("Game not found: ~p.",[GameId]),
            maybe_send_message(RPC, #disconnect{reason = <<"The game you are trying to connect doesn't exist">>}, State),
            {stop, {error, game_not_found}, State}
    end;

handle_cast({disconnect, table_closed} = Message, State = #state{rpc = RPC}) ->
    ?INFO("Recived a notification from the table: ~p", [Message]),
    maybe_send_message(RPC, #disconnect{reason = <<"The table you are sitting on has been just closed">>}, State),
    {stop, normal, State};


handle_cast({bot_session_attach, UserInfo}, State = #state{user = undefined}) ->
%    ?INFO("bot session attach", []),
    {noreply, State#state{user = UserInfo}};

%% handle_cast({bot_join_game, GameId}, State) ->
%% %    ?INFO("bot join game", []),
%%     Relay = game_manager:get_relay(GameId),
%%     Part = #participation{game_id = GameId, pid = Relay, role = player},
%%     {noreply, State#state{games = [Part | State#state.games]}};

handle_cast(#game_rematched{} = Msg, State = #state{rpc = RPC}) ->
    ?INFO("game rematched", []),
    maybe_send_message(RPC, Msg, State);

handle_cast(#game_crashed{game = GameId} = Msg, State) ->
    ?INFO("game crashed", []),
    Relay = get_relay(GameId, State#state.games),
    case Relay of
        false ->
            {noreply, State};
        #participation{} ->
            RPC = State#state.rpc,
            maybe_send_message(RPC, Msg, State)
    end;

handle_cast(#game_event{} = Msg, State) ->
    PlayerInfo = State#state.user,
    Game = Msg#game_event.game,
    Event = Msg#game_event.event,
    Args = Msg#game_event.args,
%    ?INFO("cast game event ~p for user ~p in game ~p data ~p", [Event,PlayerInfo#'PlayerInfo'.id,Game,Args]),
    RPC = State#state.rpc,
    maybe_send_message(RPC, Msg, State);

handle_cast(#game_paused{} = Msg, State) ->
    ?INFO("game paused", []),
    RPC = State#state.rpc,
    maybe_send_message(RPC, Msg, State);

handle_cast(#chat_msg{} = Msg, State) ->
    ?INFO("chat msg", []),
    RPC = State#state.rpc,
    maybe_send_message(RPC, Msg, State);

handle_cast(#social_action_msg{} = Msg, State) ->
    ?INFO("social action msg", []),
    RPC = State#state.rpc,
    maybe_send_message(RPC, Msg, State);

 handle_cast(#disconnect{} = Msg, State) ->
     ?INFO("disconnect", []),
     RPC = State#state.rpc,
     maybe_send_message(RPC, Msg, State);

handle_cast(Msg, State) ->
    ?INFO("session: unrecognized cast: ~p", [Msg]),
    {stop, {error, {unknown_cast, Msg}}, State}.

handle_info(#game_event{} = Msg, State) ->
    PlayerInfo = State#state.user,
    Game = Msg#game_event.game,
    Event = Msg#game_event.event,
    Args = Msg#game_event.args,
%    ?INFO("info game event ~p for user ~p in game ~p data ~p", [Event,PlayerInfo#'PlayerInfo'.id,Game,Args]),
    RPC = State#state.rpc,
    maybe_send_message(RPC, Msg, State);
%    handle_cast(Msg, State);

%% handle_info(#match_found{ref = Ref} = Msg, State = #state{rpc = RPC, games = Games0, match_request = Ref}) ->
%%     ?INFO("match found", []),
%%     true = is_pid(RPC),
%%     GID = Msg#match_found.game_id,
%%     IsReplacing = Msg#match_found.is_replacing,
%%     Relay = game_manager:subscribe(self(), GID),
%%     Event = #game_matched{ref = Ref, game = GID, is_replacing = IsReplacing},
%%     Part = #participation{game_id = GID, pid = Relay, role = player},
%%     maybe_send_message(RPC, Event, State#state{games = [Part | Games0]});

handle_info(dummy_player_change, #state{rpc = RPC} = State) ->
    Event = #dummy_player_change{player = crypto:rand_uniform(0, 4)},
    maybe_send_message(RPC, Event, State);


handle_info({delivery, ["user_action", Action, Who, Whom], _} = Notification,
            #state{rels_players = RelsPlayers,
                   user = User,
                   rpc = RPC
                  } = State) ->
    ?INFO("~w:handle_info/2 Delivery: ~p", [?MODULE, Notification]),
    UserId = User#'PlayerInfo'.id,
    case list_to_binary(Who) of
        UserId ->
            PlayerId = list_to_binary(Whom),
            case lists:member(PlayerId, RelsPlayers) of
                true ->
                    Type = case Action of
                               "subscribe" -> ?SOCIAL_ACTION_SUBSCRIBE;
                               "unsubscribe" -> ?SOCIAL_ACTION_UNSUBSCRIBE;
                               "block" -> ?SOCIAL_ACTION_BLOCK;
                               "unblock" -> ?SOCIAL_ACTION_UNBLOCK
                           end,
                    Msg = #social_action_msg{initiator = UserId,
                                             recipient = PlayerId,
                                             type = Type
                                            },

                    % TODO: put real db change notification from users:343 module here
                    %       nsx_msg:notify_db_subscription_change
                    %       should be additionalyy subscribed in bg feed worker binded to USER_EXCHANGE

                    ok = send_message_to_player(RPC, Msg);
                false ->
                    do_nothing
            end;
        _ ->
            do_nothing
    end,
    {noreply, State};


handle_info({'DOWN', MonitorRef, _Type, _Object, _Info} = Msg, State = #state{rpc_mon = MonitorRef}) ->
    ?INFO("connection closed, shutting down session:~p", [Msg]),
    {stop, normal, State};

handle_info({'DOWN', OtherRef, _Type, _Object, _Info} = _Msg, State) ->
    case lists:keyfind(OtherRef, #participation.ref, State#state.games ++ State#state.chats) of
        #participation{} = P ->
            Games1 = lists:delete(P, State#state.games),
            Chats1 = lists:delete(P, State#state.chats),
            {noreply, State#state{games = Games1, chats = Chats1}};
        _ ->
            {noreply, State}
    end;

handle_info(terminate, State) ->
    ?INFO("received terminate, stopping", []),
    {stop, normal, State};

handle_info(ack, State) ->
    {noreply, State};

handle_info(Info, State) ->
    {stop, {error, {unknown_info, Info}}, State}.

terminate(Reason, _State) ->
    ?INFO("terminating session. reason: ~p", [Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

get_relay(GameId, GameList) ->
    lists:keyfind(GameId, #participation.game_id, GameList).


maybe_send_message(RPC, Msg, State) ->
%    try conn_kamf_worker:send_message(RPC, Msg) of
    try send_message_to_player(RPC, Msg) of
        ok -> 
    	    {noreply, State};
        tcp_closed -> 
    	    {stop, normal, State}
    catch
        exit:{normal, {gen_server,call, [RPC, {send_message, _}]}} ->
            {stop, normal, State};
        exit:{noproc, {gen_server,call, [RPC, {send_message, _}]}} ->
            {stop, normal, State}
    end.

send_message_to_player(Pid, Message) ->
    ?INFO("MESSAGE to ~p ~p",[Pid,Message]),
    gen_server:call(Pid,{send_message,Message}).
