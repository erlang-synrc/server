-module(game_session).
-behaviour(gen_server).

-include_lib("nsg_srv/include/social_actions.hrl").
-include_lib("nsg_srv/include/logging.hrl").
-include_lib("nsg_srv/include/games.hrl").
-include_lib("nsg_srv/include/classes.hrl").
-include_lib("nsg_srv/include/requests.hrl").
-include_lib("nsg_srv/include/settings.hrl").
-include_lib("nsg_srv/include/setup.hrl").
-include_lib("alog/include/alog.hrl").

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
          pid           :: pid(),      %% relay, which handles communication gameman maps game_id onto pid
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
    case rpc:call(?GAMESERVER_NODE,auth_server,get_user_info,[Token]) of
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
    case {?IS_TEST, rpc:call(?GAMESERVER_NODE,auth_server,get_user_info,[Token, Id])} of
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
              #participation{pid = Srv} ->
                  relay:publish(Srv, Msg)
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
            rpc:call(?APPSERVER_NODE, nsm_users, subscribe_user, [UserId, Subject]),
            {reply, ok, State};
        ?SOCIAL_ACTION_UNSUBSCRIBE ->
            Subject = binary_to_list(P2),
            rpc:call(?APPSERVER_NODE, nsm_users, remove_subscribe, [UserId, Subject]),
            {reply, ok, State};
        ?SOCIAL_ACTION_BLOCK ->
            Subject = binary_to_list(P2),
            rpc:call(?APPSERVER_NODE, nsm_users, block_user, [UserId, Subject]),
            {reply, ok, State};
        ?SOCIAL_ACTION_UNBLOCK ->
            Subject = binary_to_list(P2),
            rpc:call(?APPSERVER_NODE, nsm_users, unblock_user, [UserId, Subject]),
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
              #participation{pid = Srv} ->
                  relay:publish(Srv, Res)
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
               {ok, Channel} =
                   rpc:call(?APPSERVER_NODE,
                            nsx_util_notification, subscribe_for_user_actions,
                            [UserIdStr, self()]),
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
                Type = case rpc:call(?APPSERVER_NODE,
                                     nsm_users, is_user_subscribed,
                                     [UserIdStr, PlayerIdStr]) of
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
        if NewRelsPlayers == [] ->
               rpc:call(?APPSERVER_NODE,
                        nsm_mq_channel, close, [RelsChannel]),
               undefined;
           true ->
               RelsChannel
        end,

    NewState = State#state{rels_players = NewRelsPlayers,
                           rels_notif_channel = NewRelsChannel},
    {reply, ok, NewState};


handle_call(#match_me{game_type = Game}, _From, State) when is_binary(Game) ->
%    ?INFO("match me", []),
    GameModule = api_utils:gametype_to_gamemodule(api_utils:gametype_to_atom(Game)),
    RequestRef = match_maker:match_me(GameModule),
    {reply, RequestRef, State#state{match_request = RequestRef}};

handle_call(#join_game{game = GameId}, _From, #state{user = User} = State) ->
    UserId = User#'PlayerInfo'.id,
    ?INFO("join game user ~p from ~p", [UserId,_From]),
    case get_relay(GameId, State#state.games) of
        #participation{} ->
            {reply, {error, already_joined}, State};
        false ->
            case game_manager:get_relay(GameId) of
                FirstLevelRelay when is_pid(FirstLevelRelay) ->
                    case get_second_level_relay(FirstLevelRelay, User) of
                        {ok, SecondLevelRelay} ->
                            ?INFO("join to game relay: ~p",[SecondLevelRelay]),
                            relay:subscribe(SecondLevelRelay, self(), User),
                            Ref = erlang:monitor(process, SecondLevelRelay),
                            Part = #participation{ref = Ref, game_id = GameId, pid = SecondLevelRelay, role = player},
                            Res = #'TableInfo'{}, %relay:get_table_info(SecondLevelRelay),
                            {reply, Res, State#state{games = [Part | State#state.games]}};
                        {error, not_allowed} ->
                            {reply, {error, this_game_is_private}, State};
                        {error, timeout} ->
                            {reply, {error, game_not_found}, State}
                    end;
                undefined ->
                    {reply, {error, game_not_found}, State}
            end
    end;

handle_call(#rematch{game = GameId}, _From, State) ->
    ?INFO("rematch", []),
    UId = (State#state.user)#'PlayerInfo'.id,
    Participation = get_relay(GameId, State#state.games),
    ?INFO("ID: ~p, request rematch: ~p, my games: ~p", [UId, GameId, State#state.games]),
    case Participation of
        false ->
            ?INFO("A", []),
            {reply, {error, game_not_found}, State};
        #participation{} ->
            Res = relay:do_rematch(Participation#participation.pid, self()),
            ?INFO("B. Res: ~p", [Res]),
            {reply, Res, State}
    end;

handle_call(#game_action{game = GameId} = Msg, _From, State) ->
%    ?INFO("game action ~p", [{GameId,Msg,_From}]),
    Participation = get_relay(GameId, State#state.games),
    case Participation of
        false ->
            {reply, {error, game_not_found}, State};
        #participation{} ->
            UId = (State#state.user)#'PlayerInfo'.id,
    	    ?INFO("PLAYER ~p MOVES ~p in GAME ~p",[UId,Msg,GameId]),
            {reply, relay:submit(Participation#participation.pid, Msg), State}
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
        #participation{} ->
            Signal = case Action of
                         <<"pause">> -> pause_game;
                         <<"resume">> -> resume_game
                     end,
            Res = relay:signal(Participation#participation.pid, {Signal, self()}),
            ?INFO("B. Res: ~p", [Res]),
            {reply, Res, State}
    end;

handle_call(_Request, _From, State) ->
    ?INFO("unrecognized call: ~p", [_Request]),
    Reply = ok,
    {stop, Reply, State}.

handle_cast({bot_session_attach, UserInfo}, State = #state{user = undefined}) ->
%    ?INFO("bot session attach", []),
    {noreply, State#state{user = UserInfo}};

handle_cast({bot_join_game, GameId}, State) ->
%    ?INFO("bot join game", []),
    Relay = game_manager:get_relay(GameId),
    Part = #participation{game_id = GameId, pid = Relay, role = player},
    {noreply, State#state{games = [Part | State#state.games]}};

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

handle_info(#match_found{ref = Ref} = Msg, State = #state{rpc = RPC, games = Games0, match_request = Ref}) ->
    ?INFO("match found", []),
    true = is_pid(RPC),
    GID = Msg#match_found.game_id,
    IsReplacing = Msg#match_found.is_replacing,
    Relay = game_manager:subscribe(self(), GID),
    Event = #game_matched{ref = Ref, game = GID, is_replacing = IsReplacing},
    Part = #participation{game_id = GID, pid = Relay, role = player},
    maybe_send_message(RPC, Event, State#state{games = [Part | Games0]});

handle_info(dummy_player_change, #state{rpc = RPC} = State) ->
    Event = #dummy_player_change{player = crypto:rand_uniform(0, 4)},
    maybe_send_message(RPC, Event, State);


handle_info({delivery, ["user_action", Action, Who, Whom], _} = Notification,
            #state{rels_players = RelsPlayers,
                   user = User,
                   rpc = RPC
                  } = State) ->
    ?INFO("~w:handle_info/2 Delivery: ~9999p", [?MODULE, Notification]),
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
                    %       nsx_util_notification:notify_db_subscription_change
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

%% @spec get_second_level_relay(FirstLevelRelay, UserId) -> {ok, Pid} |
%%                                                          {error, Reason}
%% @doc
%% Types:
%%     FirstLevelRelay = pid()
%%     UserId = binary()
%%     Reason = not_allowed |
%%              timeout
%% @end

get_second_level_relay(FirstLevelRelay, UserId) ->
    Ref = make_ref(),
    From = {self(), Ref},
    FirstLevelRelay ! {get_second_level_relay, From, UserId},
    receive
        {FirstLevelRelay, {Ref, {ok, Pid}}} ->
            {ok, Pid};
        {FirstLevelRelay, {Ref, {error, not_allowed}}} ->
            {error, not_allowed}
    after 5000 ->
            {error, timeout}
    end.


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
