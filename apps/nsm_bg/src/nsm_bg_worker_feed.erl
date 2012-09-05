%%----------------------------------------------------------------------
%% @author Vladimir Baranov <baranoff.vladimir@gmail.com>
%% @copyright Paynet Internet ve Bilisim Hizmetleri A.S. All Rights Reserved.
%% @doc
%%   Feed worker
%% @end
%%--------------------------------------------------------------------
-module(nsm_bg_worker_feed).

-behaviour(nsm_bg_gen_worker).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("alog/include/alog.hrl").
-include_lib("nsm_srv/include/feed.hrl").
-include_lib("nsm_srv/include/user.hrl").
-include("nsm_bg.hrl").
-include_lib("nsm_mq/include/nsm_mq.hrl").

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([]).

%% gen_worker callbacks
-export([init/1, handle_notice/3, get_opts/1, handle_info/2]).

-record(state, {owner = "",   %% id of the owner (user or group)
                type,         %% user | group
                feed,         %% link to the feed
                direct        %% link to the dorect feed
               }).

%% ====================================================================
%% External functions
%% ====================================================================

%% ====================================================================
%% Server functions
%% ====================================================================

init(Params) ->
    Owner = ?gv(owner, Params),
    ?INFO("Start params: ~p", [Params]),
    case feed:get_feed_by_user_or_group(Owner) of
        {ok, Type, FeedId, DirectId} ->
            ?INFO("Owner: ~p, Type: ~p, FeedId: ~p, DirectId: ~p",
                  [Owner, Type, FeedId, DirectId]),
            {ok, #state{owner = Owner,
                        type = Type,
                        feed = FeedId,
                        direct = DirectId}};
        Error ->
            {stop, Error}
    end.

handle_notice(["feed", "delete", Owner] = Route, Message,
              #state{owner = Owner} = State) ->
    ?INFO("feed(~p): notification received: User=~p, Route=~p, Message=~p",
          [self(), Owner, Route, Message]),
    {stop, normal, State};

%% message added to group, add it to feed
handle_notice(["feed", "group", GroupId, "entry", EntryId, "add"] = Route,
              [From|_] = Message,
              #state{owner = Owner, feed = Feed} = State) ->
    ?INFO("feed(~p): group message: Owner=~p, Route=~p, Message=~p",
          [self(), Owner, Route, Message]),
    [From, _Destinations, Desc, Medias] = Message,
    feed:add_group_entry(Feed, From, [{GroupId, group}], EntryId,
                         Desc, Medias, {group, direct}),
    {noreply, State};

handle_notice(["feed", "user", FeedOwner, "entry", EntryId, "add"] = Route,
              [From|_] = Message,
              #state{owner = WorkerOwner, feed = Feed, direct = Direct} = State) ->
    ?INFO("feed(~p): message: Owner=~p, Route=~p, Message=~p",
          [self(), WorkerOwner, Route, Message]),
    [From, Destinations, Desc, Medias] = Message,

    if
        %% user added message to own feed
        FeedOwner == From andalso FeedOwner == WorkerOwner->
            FilteredDst = [D || {_, group} = D <- Destinations],
            feed:add_entry(Feed, From, FilteredDst, EntryId, Desc, Medias,
                           {user, normal});

        %% friend added message to public feed
        FeedOwner == From ->
            feed:add_entry(Feed, From, [], EntryId, Desc, Medias,
                           {user, normal});

        %% direct message to worker owner
        FeedOwner == WorkerOwner ->
            feed:add_direct_message(Direct, From, [{FeedOwner, user}],
                                    EntryId, Desc, Medias);

        %% user sent direct message to friend, add copy to his direct feed
        From == WorkerOwner ->
            feed:add_direct_message(Direct, WorkerOwner, Destinations,
                                    EntryId, Desc, Medias);
        true ->
            ?INFO("not matched case in entry->add")
    end,
    {noreply, State};

% add/delete system message
handle_notice(["feed", "user", FeedOwner, "entry", EntryId, "add_system"] = Route,
              [From|_] = Message,
              #state{owner = WorkerOwner, feed = Feed, direct = Direct} = State) ->
    ?INFO("feed(~p): system message: Owner=~p, Route=~p, Message=~p",
          [self(), WorkerOwner, Route, Message]),
    [From, Destinations, Desc, Medias] = Message,

    feed:add_entry(Feed, From, [], EntryId, Desc, Medias, {user, system}),
    {noreply, State};

handle_notice(["feed", "group", _Group, "entry", EntryId, "delete_system"] = Route,
              Message,
              #state{owner = Owner, feed = Feed} = State) ->
    ?INFO("feed(~p): remove entry: Owner=~p, Route=~p, Message=~p",
          [self(), Owner, Route, Message]),
    %% all group subscribers shold delete entry from their feeds
    feed:remove_entry(Feed, EntryId),
    {noreply, State};


%% share entry
handle_notice(["feed", _Type, WhoShares, "entry", EntryId, "share"],
              #entry{entry_id = EntryId, raw_description = Desc, media = Medias,
                     to = Destinations, from = From} = E,
              #state{feed = Feed, type = user} = State) ->
    %% FIXME: sharing is like posting to the wall
    ?INFO("share: ~p, WhyShares: ~p", [E, WhoShares]),
    feed:add_entry(Feed, From, Destinations, EntryId, Desc, Medias,
                   {user, normal}),
    {noreply, State};

%% delete entry from feed
handle_notice(["feed", "group", _Group, "entry", EntryId, "delete"] = Route,
              Message,
              #state{owner = Owner, feed = Feed} = State) ->
    ?INFO("feed(~p): remove entry: Owner=~p, Route=~p, Message=~p",
          [self(), Owner, Route, Message]),
    %% all group subscribers shold delete entry from their feeds
    feed:remove_entry(Feed, EntryId),
    {noreply, State};

handle_notice(["feed", _Type, EntryOwner, "entry", EntryId, "delete"] = Route,
              Message,
              #state{owner = Owner} = State) ->
    case {EntryOwner, Message} of
        %% owner of the antry has deleted entry, we will delete it too
        {_, [EntryOwner|_]} ->
            ?INFO("feed(~p): remove entry: Owner=~p, Route=~p, Message=~p",
                  [self(), Owner, Route, Message]),
            remove_entry(State, EntryId);
        %% we are owner of the entry - delete it
        {Owner, _} ->
            ?INFO("feed(~p): remove entry: Owner=~p, Route=~p, Message=~p",
                  [self(), Owner, Route, Message]),
            remove_entry(State, EntryId);
        %% one of the friends has deleted some entry from his feed. Ignore
        _ ->
            ok
    end,
    {noreply, State};

handle_notice(["feed", _Type, _EntryOwner, "entry", EntryId, "edit"] = Route,
              Message,
              #state{owner = Owner} = State) ->
    [NewDescription|_] = Message,
    ?INFO("feed(~p): edit: Owner=~p, Route=~p, Message=~p",
          [self(), Owner, Route, Message]),

    %% edit entry in all feeds
    edit_entry(State, EntryId, NewDescription),

    {noreply, State};

handle_notice(["feed", _Type, _EntryOwner, "comment", CommentId, "add"] = Route,
              Message,
              #state{owner = Owner} = State) ->
    [From, EntryId, ParentComment, Content, Medias] = Message,

    ?INFO("feed(~p): add comment: Owner=~p, Route=~p, Message=~p",
          [self(), Owner, Route, Message]),
    add_comment(State, From, EntryId, ParentComment,
                CommentId, Content, Medias),
    {noreply, State};

handle_notice(["queue_action", "subscribe_user"] = Route,
              Message,
              #state{owner = Owner,
                     type =Type
                    } = State) ->
    {Whom} = Message,

    ?INFO("queue_action(~p): subscribe user: Owner=~p, Route=~p, Message=~p",
          [self(), {Type, Owner}, Route, Message]),
    subscribe_user(Owner, Whom),
    {noreply, State};

handle_notice(["queue_action", "unsubscribe_user"] = Route,
              Message,
              #state{owner = Owner,
                     type =Type
                    } = State) ->
    {Whom} = Message,

    ?INFO("queue_action(~p): unsubscribe user: Owner=~p, Route=~p, Message=~p",
          [self(), {Type, Owner}, Route, Message]),
    unsubscribe_user(Owner, Whom),
    {noreply, State};

handle_notice(Route, Message, #state{owner = User} = State) ->
    ?DBG("feed(~p): unexpected notification received: User=~p, "
              "Route=~p, Message=~p", [self(), User, Route, Message]),
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.

get_opts(#state{type = user, owner = Owner}) ->
    Name = ?FEED_WORKER_NAME(user, Owner),
    %% feeds workers queues has form: feed.worker.Owner
    QueueName = nsm_mq_lib:list_to_key(Name),
    [{routes, [""]},
     {queue, QueueName},
     %% all binds have to be done for this exchange
     {exchange, ?USER_EXCHANGE(Owner)},
     {gproc_name, Name},
     {consume_options, [exclusive]},
     {queue_options, queue_options()}];

get_opts(#state{type = group, owner = Owner}) ->
    Name = ?FEED_WORKER_NAME(group, Owner),
    %% feeds workers queues has form: feed.worker.Owner
    QueueName = nsm_mq_lib:list_to_key(Name),
    %% group worker listen only for direct messages
    %% and special message to stop worker
    [{routes, [[feed, delete, Owner],
               [feed, group, Owner, '*', '*', '*']]},
     {gproc_name, Name},
     {consume_options, [exclusive]},
     {queue, QueueName},
     {queue_options, queue_options()}].

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

queue_options() ->
    [durable,
     {ttl, 10000},
     {auto_delete, false},
     {dead_letter_exchange, ?DEAD_LETTER_EXCHANGE}].

%% remove entry from all owners feeds
remove_entry(#state{feed = Feed, direct = Direct}, EntryId) ->
    [feed:remove_entry(FId, EntryId) || FId <- [Feed, Direct]].

edit_entry(#state{feed=Feed, direct = Direct}, EntryId, NewDescription) ->
    [feed:edit_entry(FId, EntryId, NewDescription) || FId <- [Feed, Direct]].


add_comment(#state{feed = Feed, direct = Direct}, From, EntryId, ParentComment,
            CommentId, Content, Medias) ->
    [feed:entry_add_comment(FId, From, EntryId, ParentComment,
                            CommentId, Content, Medias)
       || FId <- [Feed, Direct]].


%%===================================================================

subscribe_user(Who, Whom) ->
    case users_backend:is_user_blocked(Who, Whom) of
        false ->
            zealot_db:subscribe_user(Who, Whom),
            nsx_util_notification:notify_user_subscribe(Who, Whom),
            subscribe_user_mq(user, Who, Whom);
        true ->
            do_nothing
    end.

unsubscribe_user(Who, Whom) ->
    case users_backend:is_user_subscribed(Who, Whom) of
        true ->
            zealot_db:remove_subscription(Who, Whom),
            nsx_util_notification:notify_user_unsubscribe(Who, Whom),
            remove_subscription_mq(user, Who, Whom);
        false ->
            do_nothing
    end.

subscribe_user_mq(Type, MeId, ToId) ->
    process_subscription_mq(Type, add, MeId, ToId).

remove_subscription_mq(Type, MeId, ToId) ->
    process_subscription_mq(Type, delete, MeId, ToId).

process_subscription_mq(Type, Action, MeId, ToId) ->
    %% FIXME: perform this actions with anonymous common channel
    {ok, Channel} = nsm_mq:open([]),
    %% bind MeId exchange to messages from FrId
    Routes = case Type of
                 user ->
                     rk_user_feed(ToId);
                 group ->
                     rk_group_feed(ToId)
             end,
    case Action of
        add ->
            bind_user_exchange(Channel, MeId, Routes);
        delete ->
            unbind_user_exchange(Channel, MeId, Routes)
    end,
    nsm_mq_channel:close(Channel),
    ok.

bind_user_exchange(Channel, User, RoutingKey) ->
    %% add routing key tagging to quick find errors
    {bind, RoutingKey, ok} =
        {bind, RoutingKey,
         nsm_mq_channel:bind_exchange(Channel, ?USER_EXCHANGE(User),
                                      ?NOTIFICATIONS_EX, RoutingKey)}.

unbind_user_exchange(Channel, User, RoutingKey) ->
    %% add routing key tagging to quick find errors
    {unbind, RoutingKey, ok} =
        {unbind, RoutingKey,
         nsm_mq_channel:unbind_exchange(Channel, ?USER_EXCHANGE(User),
                                        ?NOTIFICATIONS_EX, RoutingKey)}.

rk(List) ->
    nsm_mq_lib:list_to_key(List).

rk_user_feed(User) ->
    rk([feed, user, User, '*', '*', '*']).

rk_group_feed(Group) ->
    rk([feed, group, Group, '*', '*', '*']).

