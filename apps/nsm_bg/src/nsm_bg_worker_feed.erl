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
-include_lib("nsm_db/include/feed.hrl").
-include_lib("nsm_db/include/user.hrl").
-include("nsm_bg.hrl").
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
    GivenFeedId = ?gv(feed_id, Params),
    ?INFO("Init worker with start params: ~p", [Params]),
    case GivenFeedId of
        undefined ->
            case feed:get_feed_by_user_or_group(Owner) of
                {ok, Type, FeedId, DirectId} ->
                    ?INFO("Owner: ~p, Type: ~p, FeedId: ~p, DirectId: ~p",
                          [Owner, Type, FeedId, DirectId]),
                    {ok, #state{owner = Owner,
                                type = Type,
                                feed = FeedId,
                                direct = DirectId}};
                Error ->
                    ?INFO("Worker init error ~p", [Error]),
                    {stop, Error}
            end;
        OkFeedId ->
            ?INFO("Inited from given owner: ~p, FeedId: ~p", [Owner, OkFeedId]),
            {ok, #state{owner = Owner,
                        type = group,
                        feed = OkFeedId,
                        direct = undefined}}
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
handle_notice(["feed", "user", _FeedOwner, "entry", EntryId, "add_system"] = Route,
              [From|_] = Message,
              #state{owner = WorkerOwner, feed = Feed, direct = _Direct} = State) ->
    ?INFO("feed(~p): system message: Owner=~p, Route=~p, Message=~p",
          [self(), WorkerOwner, Route, Message]),
    [From, _Destinations, Desc, Medias] = Message,

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


% score statistics
handle_notice(["feed", "user", UId, "scores", _Null, "add"] = _Route,
              Message,
              #state{owner = Owner} = State) ->
    case UId == Owner of 
        true ->
            scoring:add_score(UId, hd(Message), perm);
        false ->
            ok
    end,
    {noreply, State};

% nsm calls
handle_notice(["db", "group", Owner, "put"] = Route, Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO("queue_action(~p): group put: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),
    nsm_db:put(Message),
    {noreply, State};

handle_notice(["db", "user", Owner, "put"] = Route, Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO("queue_action(~p): user put: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),
    nsm_db:put(Message),
    {noreply, State};

% groups
handle_notice(["wrong", "user", UId, "create_group"] = Route, Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO("queue_action(~p): create_group: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),
    {GId, Name, Desc, Publicity} = Message,
    FId = nsm_db:feed_create(),
    CTime = erlang:now(),
    %%FIX: match results of such calls for success case
    ok = nsm_db:put(#group{username = GId,
                              name = Name,
                              description = Desc,
                              publicity = Publicity,
                              creator = UId,
                              created = CTime,
                              owner = UId,
                              feed = FId}),
    nsx_util_notification:notify([group, init], {GId, FId}),
    nsm_users:init_mq_for_group(GId),
    nsx_util_notification:notify(["subscription", "user", UId, "add_to_group"], {GId, admin}),
    {noreply, State};

handle_notice(["wrong", "user", UId, "update_group"] = Route, Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO("queue_action(~p): update_group: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),
    {GroupId, Username, Name, Description, Owner, Publicity} = Message,
    case catch nsm_groups:check_rights(GroupId, UId, admin) of
        true ->
            %% Sanitize input to be sure we don't overwrite any other group
            SaneUsername = case Username of
                undefined -> undefined;
                GroupId -> undefined; % No need to rename if it same
                _ ->
                    LCUName = string:to_lower(Username),
                    case LCUName of
                        GroupId -> undefined;
                        _ ->
                            case nsm_db:get(group, LCUName) of
                                {ok, #group{}} -> throw({error, already_exists});
                                {error, _} ->
                                    case re:run(LCUName, "^[a-z0-9][-a-z0-9_]*$", [dollar_endonly]) of
                                        nomatch -> throw({error, invalid_username});
                                        match -> LCUName;
                                        {match, _} -> LCUName
                                    end
                            end
                    end
            end,
            SaneName = Name,
            SaneDescription = Description,
            SaneOwner = Owner,
            SanePublicity = case Publicity of
                "public" -> public;
                "moderated" -> moderated;
                "private" -> private;
                _ -> undefined
            end,
            {ok, #group{}=Group} = nsm_db:get(group, GroupId),
            NewGroup = Group#group{
                           username = coalesce(SaneUsername,Group#group.username),
                           name = coalesce(SaneName,Group#group.name),
                           description = coalesce(SaneDescription,Group#group.description),
                           owner = coalesce(SaneOwner,Group#group.owner),
                           publicity = coalesce(SanePublicity,Group#group.publicity)},
            ok = nsm_db:put(NewGroup),
            % If username changed, need to update users membership from old group to new one, and remove old group
            case Username of
                undefined -> ok;
                _ ->
                    ok = nsm_db:delete(group, Group#group.username),
                    ok = nsm_db:move_group_members(GroupId, Username, coalesce(Name,Group#group.name))
                    % TODO: change in members' message copies
            end,
            % Update group name in cache
            Name =/= undefined andalso nsm_db:change_group_name(coalesce(Username,GroupId), Name),
            case Owner of
                undefined -> ok;
                _ ->
                    ok = nsm_db:add_to_group(UId, GroupId, member),
                    ok = nsm_db:add_to_group(Owner, GroupId, admin)
            end,
            ok;
        _ ->
            ?ERROR("Group update error: user ~p doesn't have permission to!", [UId])
    end,
    {noreply, State};

handle_notice(["subscription", "user", UId, "add_to_group"] = Route, Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO("queue_action(~p): add_to_group: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),
    {GId, UType} = Message,
    ?INFO("add ~p to group ~p", [UId, GId]),
    nsm_users:subscribe_user_mq(group, UId, GId),
    nsm_db:add_to_group(UId, GId, UType),
    {noreply, State};

handle_notice(["subscription", "user", UId, "remove_from_group"] = Route, Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO("queue_action(~p): remove_from_group: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),    
    {GId} = Message,
    ?INFO("remove ~p from group ~p", [UId, GId]),
    nsm_users:remove_subscription_mq(group, UId, GId),
    nsm_db:remove_from_group(UId, GId),
    {noreply, State};

handle_notice(["db", "group", GId, "remove_group"] = Route, Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO("queue_action(~p): remove_group: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),
    nsx_util_notification:notify([feed, delete, GId], empty),
    Group = nsm_groups:get_group(GId),
    lists:foreach(
        fun(#entry{feed_id=F,entry_id=E})-> feed:remove_entry(F, E) end,
        nsm_db:select(feed, fun(#entry{feed_id=Fid}) when Fid=:=Group#group.feed->true;(_)->false end)),
    {ok, UsersEntry} = nsm_db:get(group_member_rev, GId),
    UsersEntryList = UsersEntry#group_member_rev.who,
    UserList = [GMR#group_member_rev.who || GMR <- UsersEntryList],
    ProcessUser = fun(User) ->
        {ok, GM} = nsm_db:get(group_member, User),
        UserGroupList = GM#group_member.group,
        NewList = lists:filter(fun(#group_member{group=GMGId}) when GMGId=:=GId->false;(_)->true end, UserGroupList),
        case NewList == [] of
            true -> % user is not in a single group
                nsm_db:delete(group_member, GM#group_member.who);
            false -> % user is still in some other groups
                nsm_db:put(#group_member{who = GM#group_member.who, group = NewList})
        end
    end,
    lists:map(ProcessUser, UserList),
    nsm_db:delete(group_member_rev, GId),
    nsm_db:delete(feed, Group#group.feed),
    nsm_db:delete(group, GId),
    % unbind exchange
    {ok, Channel} = nsm_mq:open([]),
    Routes = nsm_users:rk_group_feed(GId),
    nsm_users:unbind_group_exchange(Channel, GId, Routes),
    nsm_mq_channel:close(Channel),

    {noreply, State};


handle_notice(["subscription", "user", UId, "invite_to_group"] = Route, Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO("queue_action(~p): invite_to_group: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),
    {GId, Invited} = Message,
    case nsm_groups:check_rights(GId, Invited, invsent) of
        req -> % User requested in past; just join him
            nsx_util_notification:notify(["subscription", "user", Invited, "add_to_group"], {GId, member});
        true -> % Invite already sent in past
            ?ERROR("Invite to user ~p already sent!", [Invited]);
        false ->
            case nsm_users:get_user(Invited) of
                {ok, #user{feed=Feed}} ->
                    feed:add_direct_message(Feed, UId, "Let's join us in group!"),
                    nsx_util_notification:notify(["subscription", "user", Invited, "add_to_group"], {GId, invsent});
                _ ->
                    ?ERROR("Invitation error: user ~p not found!", [Invited])
            end
    end,
    {noreply, State};

handle_notice(["subscription", "user", UId, "reject_invite_to_group"] = Route, Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO("queue_action(~p): invite_to_group: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),
    {GId, Invited, Reason} = Message,
    case nsm_groups:check_rights(GId, Invited, invsent) of
        req -> % User requested, reject
            case nsm_users:get_user(Invited) of
                {ok, #user{feed=Feed}} ->
                    feed:add_direct_message(Feed, UId, "Invite rejected: " ++ Reason),
                    nsx_util_notification:notify(["subscription", "user", Invited, "add_to_group"], {GId, rejected});
                _ ->
                    ?ERROR("Invitation error: user ~p not found!", [Invited])
            end;
        true -> % Invite was sent to user -- just remove invite
            nsx_util_notification:notify(["subscription", "user", Invited, "add_to_group"], {GId, rejected});
        false ->
            ?ERROR("Invitation error: user ~p has no request!", [Invited])
    end,
    {noreply, State};

handle_notice(["subscription", "user", UId, "leave_group"] = Route, Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO(" queue_action(~p): leave_group: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),
    {GId} = Message,
    case nsm_groups:check_rights(GId, UId, admin) of
        true -> % user is admin, check is it owner
            {ok, #group{}=Group} = nsm_db:get(group, GId),
            case Group#group.owner of
                UId -> % User is owner, transfer ownership to someone else
                    Members = nsm_db:get_group_members(GId),
                    Sorted = slice_members(Members, UId, admin) ++ slice_members(Members, UId, moder) ++ slice_members(Members, UId, member),
                    case Sorted of
                        [ #group_member_rev{who=Who} | _ ] ->
                            NewOwner = Who,
                            ok = nsm_db:add_to_group(NewOwner, GId, admin),
                            ok = nsm_db:put(Group#group{owner = NewOwner}),
                            nsx_util_notification:notify(["subscription", "user", UId, "remove_from_group"], {GId});
                        [] ->
                            % Nobody left in group, remove group at all
                            nsx_util_notification:notify([db, group, GId, remove_group], [])
                    end;
                _ -> % Plain user removes -- just remove it
                    nsx_util_notification:notify(["subscription", "user", UId, "remove_from_group"], {GId})
            end;
        _ -> % user is just someone, remove it
            nsx_util_notification:notify(["subscription", "user", UId, "remove_from_group"], {GId})
    end,
    {noreply, State};

handle_notice(["login", "user", UId, "update_after_login"] = Route, Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO("queue_action(~p): remove_from_group: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),    
    Update =
        case nsm_users:user_status(UId) of
            {error, status_info_not_found} ->
                #user_status{username = UId,
                             last_login = erlang:now()};
            {ok, UserStatus} ->
                UserStatus#user_status{last_login = erlang:now()}
        end,
    nsm_db:put(Update),

    case nsm_db:get(user_counter, UId) of
        {error, not_found} ->
            UC = #user_counter{username = UId},
            nsm_db:put(UC);
        _ ->
            ok
    end,
    {noreply, State};

% unexpected
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
    [{routes, [""]},
     {gproc_name, Name},
     {consume_options, [exclusive]},
     {queue, QueueName},
     {exchange, ?GROUP_EXCHANGE(Owner)},
     {queue_options, queue_options()}].

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

coalesce(undefined, B) -> B;
coalesce(A, _) -> A.

slice_members(Members,SkipUser,ReqType) ->
    lists:filter(fun(#group_member_rev{type=Type,who=Who}) -> Type==ReqType andalso Who /= SkipUser end, Members).

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

