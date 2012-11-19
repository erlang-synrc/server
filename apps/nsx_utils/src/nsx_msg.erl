-module(nsx_msg).
-author('Vladimir Baranov <baranoff.vladimir@gmail.com>').
-compile(export_all).
-include_lib("nsm_mq/include/nsm_mq.hrl").
-include_lib("nsm_db/include/user.hrl").
-include_lib("nsm_db/include/membership_packages.hrl").
-include_lib("nsx_config/include/log.hrl").
-include("notification.hrl").
-include("setup.hrl").

notify(EventPath, Data) -> RoutingKey = routing_key(EventPath), nsm_mq:publish(?NOTIFICATIONS_EX, RoutingKey, Data).

subscribe_user(UserId, ReplyTo) -> do_user_subscribe(UserId, ReplyTo).
subscribe_group(GroupId, ReplyTo) -> do_group_subscribe(GroupId, ReplyTo).
subscribe_for_tournament(Id, UserId, ReplyTo) -> do_subscribe_user_for_tournament(Id, UserId, ReplyTo).
subscribe_tournament_lobby(Id, Callback, InitialState) -> do_subscribe_tournament_lobby(Id, Callback, InitialState).
subscribe_for_user_actions(UserId, ReplyTo) -> do_subscribe_user_actions(UserId, ReplyTo).

% TODO: remove all this syntax sugar

notify_tournament_start_game(TournamentId, Users, Data) -> [ notify([tournament, TournamentId, User, start_game], Data) || User <- Users ].
notify_tournament_heartbeat_reply(TournamentId, UserRecord) -> notify([tournament, TournamentId, heartbeat, reply], UserRecord).
notify_tournament_heartbeat_request(TournamentId) -> notify([tournament, TournamentId, heartbeat, request], tournament_heartbeat).
notify_tournament_user_ready(TournamentId, UserRecord) -> UserId = UserRecord#user.username, notify([tournament, TournamentId, user, UserId, ready], UserRecord).
notify_tournament_chat(TournamentId, Action, UserId, Message) -> notify([tournament, TournamentId, chat, Action], {UserId, Action, Message}).
notify_email(Subject, Content, To) -> notify([email, send], {Subject, Content, To}).
notify_email(Subject, TextContent, HTMLContent, To) -> notify([email, send], {Subject, TextContent, HTMLContent, To}).
notify_transaction(User, Transaction) -> notify([transaction, user, User, add_transaction], Transaction).
notify_user_subscribe(Who, Whom) -> notify([user_action, subscribe, Who, Whom], {}).
notify_user_unsubscribe(Who, Whom) -> notify([user_action, unsubscribe, Who, Whom], {}).
notify_user_block(Who, Whom) -> notify([user_action, block, Who, Whom], {}).
notify_user_unblock(Who, Whom) -> notify([user_action, unblock, Who, Whom], {}).
notify_purchase(PurchaseRecord) ->
    PurchaseId = PurchaseRecord#membership_purchase.id,
    State = PurchaseRecord#membership_purchase.state,
    Package = PurchaseRecord#membership_purchase.membership_package,
    PaymentType = Package#membership_package.payment_type,
    User =  PurchaseRecord#membership_purchase.user_id,
    ?INFO("Purchase notification: ~p", [PurchaseId]),
    notify([purchase, User, PurchaseId, PaymentType, State], PurchaseRecord).

do_user_subscribe(UserId, ReplyTo) ->
    Callback = fun comet_callback/2,
    {ok, Channel} = nsm_mq:open([{consumer, ReplyTo}]),
    {ok, Queue} = nsm_mq_channel:create_queue(Channel, <<"">>, [auto_delete]),
    ok = nsm_mq_channel:bind_queue(Channel, Queue, ?USER_EXCHANGE(UserId), <<"">>),
    {ok, _} = nsm_mq_channel:consume(Channel, Queue, [{callback, Callback}, {state, ReplyTo}]),
    {ok, Channel}.

do_group_subscribe(GroupId, ReplyTo) ->
    Callback = fun comet_callback/2,
    {ok, Channel} = nsm_mq:open([{consumer, ReplyTo}]),
    {ok, Queue} = nsm_mq_channel:create_queue(Channel, <<"">>, [auto_delete]),
    [ok = nsm_mq_channel:bind_queue(Channel, Queue, ?NOTIFICATIONS_EX, routing_key(Key)) ||
            Key <-[[feed, group, GroupId, '*', '*', '*'], [feed, delete, GroupId]]],
    {ok, _} = nsm_mq_channel:consume(Channel, Queue, [{callback, Callback}, {state, ReplyTo}]),
    {ok, Channel}.

do_subscribe_user_for_tournament(Id, UserId, ReplyTo) ->
    Callback = fun comet_callback/2,
    TournamentKeys = build_tournament_keys(Id, UserId),
    {ok, Channel} = nsm_mq:open([{consumer, ReplyTo}]),
    {ok, Queue} = nsm_mq_channel:create_queue(Channel, <<"">>, [auto_delete]),
    [ok = nsm_mq_channel:bind_queue(Channel, Queue,?NOTIFICATIONS_EX, Key) || Key <- TournamentKeys ],
    {ok, _} = nsm_mq_channel:consume(Channel, Queue, [{callback, Callback}, {state, ReplyTo}]),
    {ok, Channel}.

do_subscribe_tournament_lobby(TournamentId, Callback, InitialState) ->
    {ok, Channel} = nsm_mq:open([]),
    RoutingKeys = build_tournament_lobby_keys(TournamentId),
    ?INFO("subscribe lobby: ~p. Keys=~p",[{TournamentId, Callback, InitialState}, RoutingKeys]),
    {ok, Queue} = nsm_mq_channel:create_queue(Channel, <<"">>, [auto_delete]),
    [ ok = nsm_mq_channel:bind_queue(Channel, Queue, ?NOTIFICATIONS_EX, Key) || Key <- RoutingKeys],
    {ok, _} = nsm_mq_channel:consume(Channel, Queue, [{callback, Callback}, {state, InitialState}]),
    {ok, Channel}.

do_subscribe_user_actions(UserId, ReplyTo) ->
    Callback = fun comet_callback/2,
    {ok, Channel} = nsm_mq:open([{consumer, ReplyTo}]),
    {ok, Queue} = nsm_mq_channel:create_queue(Channel, <<"">>, [auto_delete]),
    RK = [user_action, '*', UserId, '*'],
    ok = nsm_mq_channel:bind_queue(Channel, Queue, ?NOTIFICATIONS_EX, routing_key(RK)),
    {ok, _} = nsm_mq_channel:consume(Channel, Queue, [{callback, Callback}, {state, ReplyTo}, {consumer, ReplyTo}]),
    {ok, Channel}.

build_tournament_keys(Id, UserId) ->
    Keys = [[tournament, Id, '*'],         %% for global tournament events
            [tournament, Id, UserId, '*'], %% user specific (create_game, etc.)
            [tournament, Id, chat, '*'],   %% chat (message, kick, join)
            [tournament, Id, '*', ready],  %% user change state to ready
            [tournament, Id, heartbeat, request]],
    [routing_key(Things) || Things <- Keys].

build_tournament_lobby_keys(Id) ->
    Keys = [[tournament, Id, chat, '*'],          %% for chart events
            [tournament, Id, heartbeat, reply],   %% listen heartbeat replies
            [tournament, Id, user, '*', ready]],
    [routing_key(Things) || Things <- Keys].

comet_callback(#envelope{} = E, ReplyTo) ->
    RoutingKey = nsm_mq_lib:key_to_list(E#envelope.routing_key),
    Payload = E#envelope.payload,
    ReplyTo!{delivery, RoutingKey, Payload},
    ok.

routing_key(List) -> nsm_mq_lib:list_to_key(List).

