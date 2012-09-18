%%----------------------------------------------------------------------
%% @author Vladimir Baranov <baranoff.vladimir@gmail.com>
%% @copyright Paynet Internet ve Bilisim Hizmetleri A.S. All Rights Reserved.
%% @doc
%%   Deal with notifications
%% @end
%%--------------------------------------------------------------------
-module(nsx_util_notification).

%%
%% Include files
%%

-include_lib("nsm_mq/include/nsm_mq.hrl").
-include_lib("nsm_srv/include/user.hrl").
-include_lib("nsm_srv/include/membership_packages.hrl").
-include_lib("alog/include/alog.hrl").
-include("notification.hrl").
-include("setup.hrl").

%%
%% Exported Functions
%%
-export([notify/2,

         notify_tournament_start_game/3,

         notify_tournament_heartbeat_reply/2,
         notify_tournament_heartbeat_request/1,
         notify_tournament_user_ready/2,

         notify_tournament_chat/4,

         notify_email/3, notify_email/4,

         notify_purchase/1,

         notify_user_subscribe/2,
         notify_user_unsubscribe/2,
         notify_user_block/2,
         notify_user_unblock/2,

         qa_create_group/5,
         qa_add_to_group/3,
         qa_remove_from_group/2,

         subscribe_for_tournament/3,

         subscribe_tournament_lobby/3,
         subscribe_for_user_actions/2,
         subscribe_user/2,
         subscribe_group/2]).


%%
%% API Functions
%%


subscribe_user(UserId, ReplyTo) ->
    do_user_subscribe(UserId, ReplyTo).

subscribe_group(GroupId, ReplyTo) ->
    do_group_subscribe(GroupId, ReplyTo).

subscribe_for_tournament(Id, UserId, ReplyTo) ->
    do_subscribe_user_for_tournament(Id, UserId, ReplyTo).

%% make all needed subscriptions for lobby server backend
subscribe_tournament_lobby(Id, Callback, InitialState) ->
    do_subscribe_tournament_lobby(Id, Callback, InitialState).

subscribe_for_user_actions(UserId, ReplyTo) ->
    ?INFO("~w:subscribe_for_user_actions/2 UserId: ~99p ReplyTo: ~w", [?MODULE, UserId, ReplyTo]),
    do_subscribe_user_actions(UserId, ReplyTo).


%% Hi level notifications API

notify_tournament_start_game(TournamentId, Users, Data) ->
    [ notify([tournament, TournamentId, User, start_game], Data)
        || User <- Users ].

%% reply to hearbeat request
notify_tournament_heartbeat_reply(TournamentId, UserRecord) ->
    %% add additional paht key heartbeat to prevent all user of receiving
    %% replies of another users
    notify([tournament, TournamentId, heartbeat, reply], UserRecord).

notify_tournament_heartbeat_request(TournamentId) ->
    notify([tournament, TournamentId, heartbeat, request], tournament_heartbeat).

notify_tournament_user_ready(TournamentId, UserRecord) ->
    UserId = UserRecord#user.username,
    notify([tournament, TournamentId, user, UserId, ready], UserRecord).

%% Action = join | kick | message
notify_tournament_chat(TournamentId, Action, UserId, Message) ->
    notify([tournament, TournamentId, chat, Action], {UserId, Action, Message}).


%% send email with notice worker. Those messages will be processed in
%% nsm_bg_worker_email.erl

notify_email(Subject, Content, To) ->
    notify([email, send], {Subject, Content, To}).
%% multipart

notify_email(Subject, TextContent, HTMLContent, To) ->
    notify([email, send], {Subject, TextContent, HTMLContent, To}).

%% notify about purchase state change.

notify_purchase(PurchaseRecord) ->
    PurchaseId = PurchaseRecord#membership_purchase.id,
    State = PurchaseRecord#membership_purchase.state,
    Package = PurchaseRecord#membership_purchase.membership_package,
    PaymentType = Package#membership_package.payment_type,
    User =  PurchaseRecord#membership_purchase.user_id,
    ?INFO("Purchase notification: ~p", [PurchaseId]),
    notify([purchase, User, PurchaseId, PaymentType, State], PurchaseRecord).


%% @spec notify_user_subscribe(Who, Whom) -> any()
notify_user_subscribe(Who, Whom) ->
    notify([user_action, subscribe, Who, Whom], {}).

%% @spec notify_user_unsubscribe(Who, Whom) -> any()
notify_user_unsubscribe(Who, Whom) ->
    notify([user_action, unsubscribe, Who, Whom], {}).

%% @spec notify_user_block(Who, Whom) -> any()
notify_user_block(Who, Whom) ->
    notify([user_action, block, Who, Whom], {}).

%% @spec notify_user_unblock(Who, Whom) -> any()
notify_user_unblock(Who, Whom) ->
    notify([user_action, unblock, Who, Whom], {}).


%%%% groups
%% @spec qa_create_group(UId, GId, Name, Desc, Publicity) -> any()
qa_create_group(UId, GId, Name, Desc, Publicity) ->
    ?INFO("   ***   qa_create_group"),
    notify_user_exchange(UId, ["queue_action", "create_group"], {UId, GId, Name, Desc, Publicity}).

%% @spec qa_add_to_group(UId, GId, Type) -> any()
qa_add_to_group(UId, GId, Type) -> 
    ?INFO("   ***   qa_add_to_group"),
    notify_user_exchange(UId, ["queue_action", "add_to_group"], {UId, GId, Type}).

%% @spec qa_remove_from_group(UId, GId) -> any()
qa_remove_from_group(UId, GId) ->
    ?INFO("   ***   qa_remove_from_group"), 
    notify_user_exchange(UId, ["queue_action", "remove_from_group"], {UId, GId}).


%% Low level notification API

notify(EventPath, Data) ->
    RoutingKey = routing_key(EventPath),
    nsm_mq:publish(?NOTIFICATIONS_EX, RoutingKey, Data).

notify_user_exchange(UserId, EventPath, Data) ->
    RoutingKey = routing_key(EventPath),
    nsm_mq:publish(?USER_EXCHANGE(UserId), RoutingKey, Data).

%%
%% Local Functions
%%

%% coment subscription
do_user_subscribe(UserId, ReplyTo) ->
    Callback = fun comet_callback/2,

    %% channel linked to client (consumer option)
    {ok, Channel} = nsm_mq:open([{consumer, ReplyTo}]),
    {ok, Queue} = nsm_mq_channel:create_queue(Channel, <<"">>, [auto_delete]),
    ok = nsm_mq_channel:bind_queue(Channel, Queue, ?USER_EXCHANGE(UserId), <<"">>),

    {ok, _} = nsm_mq_channel:consume(Channel, Queue,
                                     [{callback, Callback}, {state, ReplyTo}]),

    {ok, Channel}.

do_group_subscribe(GroupId, ReplyTo) ->
    Callback = fun comet_callback/2,

    %% channel linked to client (consumer option)
    {ok, Channel} = nsm_mq:open([{consumer, ReplyTo}]),
    {ok, Queue} = nsm_mq_channel:create_queue(Channel, <<"">>, [auto_delete]),

    [ok = nsm_mq_channel:bind_queue(Channel, Queue,
                                    ?NOTIFICATIONS_EX, routing_key(Key)) ||
            Key <-[[feed, group, GroupId, '*', '*', '*'],
                   [feed, delete, GroupId]]
    ],

    {ok, _} = nsm_mq_channel:consume(Channel, Queue,
                                     [{callback, Callback}, {state, ReplyTo}]),

    {ok, Channel}.

do_subscribe_user_for_tournament(Id, UserId, ReplyTo) ->

    Callback = fun comet_callback/2,

    %% build routing keys for tournament
    TournamentKeys = build_tournament_keys(Id, UserId),

    %% channel linked to client (consumer option)
    {ok, Channel} = nsm_mq:open([{consumer, ReplyTo}]),

    %% create tmp queue and bind it to notifications exchange, start consuming
    {ok, Queue} = nsm_mq_channel:create_queue(Channel, <<"">>, [auto_delete]),

    [
     ok = nsm_mq_channel:bind_queue(Channel, Queue,
                                    ?NOTIFICATIONS_EX, Key)
            || Key <- TournamentKeys ],

    {ok, _} = nsm_mq_channel:consume(Channel, Queue, [{callback, Callback},
                                                      {state, ReplyTo}]),

    {ok, Channel}.

do_subscribe_tournament_lobby(TournamentId, Callback, InitialState) ->
    {ok, Channel} = nsm_mq:open([]),
    RoutingKeys = build_tournament_lobby_keys(TournamentId),

    ?INFO("subscribe lobby: ~p. Keys=~p",[{TournamentId, Callback, InitialState},
                                          RoutingKeys]),

    {ok, Queue} = nsm_mq_channel:create_queue(Channel, <<"">>, [auto_delete]),

    [ ok = nsm_mq_channel:bind_queue(Channel, Queue, ?NOTIFICATIONS_EX, Key)
             || Key <- RoutingKeys],

    {ok, _} = nsm_mq_channel:consume(Channel, Queue, [{callback, Callback},
                                                      {state, InitialState}]),

    {ok, Channel}.


do_subscribe_user_actions(UserId, ReplyTo) ->
    Callback = fun comet_callback/2,

    %% channel linked to client (consumer option)
    {ok, Channel} = nsm_mq:open([{consumer, ReplyTo}]),
    {ok, Queue} = nsm_mq_channel:create_queue(Channel, <<"">>, [auto_delete]),

    RK = [user_action, '*', UserId, '*'],
    ok = nsm_mq_channel:bind_queue(Channel, Queue,
                                   ?NOTIFICATIONS_EX, routing_key(RK)),

    {ok, _} = nsm_mq_channel:consume(Channel, Queue,
                                     [{callback, Callback},
                                      {state, ReplyTo},
                                      {consumer, ReplyTo}]),

    {ok, Channel}.


build_tournament_keys(Id, UserId) ->

    Keys = [
            [tournament, Id, '*'],         %% for global tournament events
            [tournament, Id, UserId, '*'], %% user specific (create_game, etc.)
            [tournament, Id, chat, '*'],   %% chat (message, kick, join)
            [tournament, Id, '*', ready],  %% user change state to ready
            [tournament, Id, heartbeat, request] %% listen only requests
           ],

    [routing_key(Things) || Things <- Keys].

build_tournament_lobby_keys(Id) ->
    Keys = [
            [tournament, Id, chat, '*'],          %% for chart events
            [tournament, Id, heartbeat, reply],   %% listen heartbeat replies
            [tournament, Id, user, '*', ready]    %% user became ready
           ],

    [routing_key(Things) || Things <- Keys].

comet_callback(#envelope{} = E, ReplyTo) ->
    RoutingKey = nsm_mq_lib:key_to_list(E#envelope.routing_key),
    Payload = E#envelope.payload,
    ReplyTo!{delivery, RoutingKey, Payload},
    ok.


routing_key(List) ->
    nsm_mq_lib:list_to_key(List).

