-module(nsm_writer).
-behaviour(nsm_consumer).
-include_lib("nsx_config/include/log.hrl").
-include_lib("nsm_db/include/feed.hrl").
-include_lib("nsm_db/include/user.hrl").
-include_lib("nsm_db/include/table.hrl").
-include_lib("nsm_db/include/mhits.hrl").
-include_lib("nsm_db/include/tournaments.hrl").
-include_lib("nsm_gifts/include/common.hrl").
-include("nsm_bg.hrl").
-export([init/1, handle_notice/3, get_opts/1, handle_info/2, handle_call/3, 
        cached_feed/3, cached_direct/3, feed_refresh/3, direct_refresh/3, start_link/2]).
-record(state, {owner = "feed_owner", type :: user | group | system, feed, direct, cached_feed,cached_direct }).

start_link(Mod,Args) -> gen_server:start_link(Mod, Args, []).

init(Params) -> 
    Owner   = proplists:get_value(name,   Params),
    Type    = proplists:get_value(type,   Params),
    Feed    = proplists:get_value(feed,   Params),
    Direct  = proplists:get_value(direct, Params, undefined),
    gproc:reg({p,l,Owner}, {Type,Feed,Direct}),
    ?INFO("Init worker: ~p", [Params]),
    {ok, #state{owner = Owner, type = Type, feed = Feed, direct = Direct}}.

cached_feed(Uid,Fid,Page) -> Pid = nsm_bg:pid(Uid), gen_server:call(Pid,{cached_feed,Fid,Page}).
cached_direct(Uid,Fid,Page) -> Pid = nsm_bg:pid(Uid), gen_server:call(Pid,{cached_direct,Fid,Page}).
feed_refresh(Pid,Fid,Page) -> gen_server:call(Pid,{feed_refresh,Fid,Page}).
direct_refresh(Pid,Fid,Page) -> gen_server:call(Pid,{direct_refresh,Fid,Page}).

cached_friends(Uid,Fid,Page) -> Pid = nsm_bg:pid(Uid), gen_server:call(Pid,{cached_friends,Fid,Page}).
cached_groups(Uid,Fid,Page) -> Pid = nsm_bg:pid(Uid), gen_server:call(Pid,{cached_groups,Fid,Page}).
friends_refresh(Pid,Fid,Page) -> gen_server:call(Pid,{friens_refresh,Fid,Page}).
groups_refresh(Pid,Fid,Page) -> gen_server:call(Pid,{groups_refresh,Fid,Page}).

handle_call({cached_feed,FId,Page},From,State) ->
    Reply = case State#state.cached_feed of
                 undefined -> nsm_db:entries_in_feed(FId,Page);
                 A -> A end,
    {reply,Reply,State#state{cached_feed=Reply}};

handle_call({cached_direct,FId,Page},From,State) ->
    Reply = case State#state.cached_direct of
                 undefined -> nsm_db:entries_in_feed(FId,Page);
                 A -> A end,
    {reply,Reply,State#state{cached_direct=Reply}};

handle_call({feed_refresh,FId,Page},From,State) ->
    Reply = nsm_db:entries_in_feed(FId,Page),
    {reply,ok,State#state{cached_feed=Reply}};

handle_call({direct_refresh,FId,Page},From,State) ->
    Reply = nsm_db:entries_in_feed(FId,Page),
    {reply,ok,State#state{cached_direct=Reply}}.

handle_notice(["feed", "delete", Owner] = Route, Message,
              #state{owner = Owner} = State) ->
    ?INFO("feed(~p): notification received: User=~p, Route=~p, Message=~p",
          [self(), Owner, Route, Message]),
    {stop, normal, State};

handle_notice(["feed", "group", GroupId, "entry", EntryId, "add"] = Route,
              [From|_] = Message,
              #state{owner = Owner, feed = Feed} = State) ->
    ?INFO("feed(~p): group message: Owner=~p, Route=~p, Message=~p",
          [self(), Owner, Route, Message]),
    [From, _Destinations, Desc, Medias] = Message,
    feed:add_group_entry(Feed, From, [{GroupId, group}], EntryId,
                         Desc, Medias, {group, direct}),
    % statistics
    case Owner == GroupId of
        false -> ok;
        true ->
            {ok, Group} = nsm_db:get(group, GroupId),
            GE = Group#group.entries_count,
            nsm_db:put(Group#group{entries_count = GE+1}),
            {ok, Subs} = nsm_db:get(group_subs, {From, GroupId}),
            SE = Subs#group_subs.user_posts_count,
            nsm_db:put(Subs#group_subs{user_posts_count = SE+1})
    end,    

    self() ! {feed_refresh,Feed,20},
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
                           {user, normal}), self() ! {feed_refresh,Feed,20};

        %% friend added message to public feed
        FeedOwner == From ->
            feed:add_entry(Feed, From, [], EntryId, Desc, Medias,
                           {user, normal}), self() ! {feed_refresh,Feed,20};

        %% direct message to worker owner
        FeedOwner == WorkerOwner ->
            feed:add_direct_message(Direct, From, [{FeedOwner, user}],
                                    EntryId, Desc, Medias), self ! {direct_refresh,Direct,20};

        %% user sent direct message to friend, add copy to his direct feed
        From == WorkerOwner ->
            feed:add_direct_message(Direct, WorkerOwner, Destinations,
                                    EntryId, Desc, Medias), self ! {direct_refresh,Direct,20};
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

handle_notice(["feed", "group", GroupId, "entry", EntryId, "add_system"] = Route,
              [From|_] = Message,
              #state{owner = Owner, feed = Feed} = State) ->
    ?INFO("feed(~p): group system message: Owner=~p, Route=~p, Message=~p",
          [self(), Owner, Route, Message]),
    [From, _Destinations, Desc, Medias] = Message,
    feed:add_group_entry(Feed, From, [{GroupId, group}], EntryId,
                         Desc, Medias, {group, system}),
    {noreply, State};

handle_notice(["feed", "user", UId, "post_note"] = Route, Message, 
        #state{owner = Owner, feed = Feed} = State) ->
    ?INFO("feed(~p): post_note: Owner=~p, Route=~p, Message=~p", [self(), Owner, Route, Message]),
    Note = Message,
    Id = utils:uuid_ex(),
    feed:add_entry(Feed, UId, [], Id, Note, [], {user, system_note}),
    {noreply, State};


handle_notice(["system", "game_begins_note"] = Route, Message, State) ->    % this doesn't work! have to figure why
    ?INFO("feed(~p): game_begins_note: Route=~p, Message=~p", [self(), Route, Message]),
    {URL, UId, TName, GName, GRounds, GSpeed, GMode} = Message,
    SGRounds = case GRounds of   % CD10 fix
        undefined -> "no";
        I -> integer_to_list(I)
    end,
    Desc = lists:flatten( URL ++ "|" ++ UId ++ "|" ++ TName ++ "|" ++ GName ++ "|" ++ SGRounds ++ "|" ++ GSpeed ++ "|" ++ GMode),
    %UserList = nsm_groups:list_group_members("kakaranet"),
    %UserList = [To || #subs{who=To} <- nsm_users:list_subscr_me(UId)] ++ [UId],
    UserList = [UId],
    ID = utils:uuid_ex(),
    Destinations = [{User, user} || User <- UserList],
    Route = [feed, user, UId, entry, ID, add_system],
    nsx_msg:notify(Route, [UId, Destinations, Desc, []]),
    {noreply, State};


handle_notice(["system", "tournament_tour_note"] = Route, Message, State) ->
    ?INFO("feed(~p): tournament_tour_note: Route=~p, Message=~p", [self(), Route, Message]),
    {TId, TourNum, TotalTours, _TourType, TourRes} = Message,
    {ok, Tour} = nsm_db:get(tournament, TId),
    Desc = case Tour#tournament.description of
        "" -> "";
        D -> " (" ++ D ++ ")"
    end,
    Winners = [W || W = {_, _, _, Status} <- TourRes, Status == active],
    SNum = integer_to_list(length(Winners)),
    [begin
        case TotalTours - TourNum of
            0 -> ok;
            1 ->
                NoteString = "tourtour_with_winners" ++ "|name=" ++ Tour#tournament.name ++ "|desc=" ++ Desc ++ "|total=" ++ SNum ++
                    "|player=" ++ UId ++ "|pos=" ++ integer_to_list(CommonPos) ++ "|points=" ++ integer_to_list(Points) ++ "|tourstatus=" ++ atom_to_list(Status) ++
                    "|player_count=" ++ SNum ++
                lists:flatten([
                    begin
                        {RUId, RPos, RPoints, _Status} = lists:nth(N, Winners),
                        SN = integer_to_list(N),
                        "|n"++SN++"=" ++ integer_to_list(N) ++ "|winner"++SN++"="++RUId++"|pos"++SN++"="++integer_to_list(RPos)++"|points"++SN++"="++integer_to_list(RPoints)
                    end
                    || N <- lists:seq(1, length(Winners))]),
                nsx_msg:notify(["feed", "user", UId, "post_note"], NoteString);
            _ ->
                NoteString = "tourtour" ++ "|name=" ++ Tour#tournament.name ++ "|desc=" ++ Desc ++ "|total=" ++ SNum ++
                    "|player=" ++ UId ++ "|pos=" ++ integer_to_list(CommonPos) ++ "|points=" ++ integer_to_list(Points) ++ "|tourstatus=" ++ atom_to_list(Status),
                nsx_msg:notify(["feed", "user", UId, "post_note"], NoteString)
        end
    end
    || {UId, CommonPos, Points, Status} <- TourRes],
    {noreply, State};    

handle_notice(["system", "tournament_ends_note"] = Route, Message, State) ->
    ?INFO("feed(~p): tournament_ends_note: Route=~p, Message=~p", [self(), Route, Message]),
    {TId, Results} = Message,
    {ok, Tour} = nsm_db:get(tournament, TId),
    ok = nsm_db:put(Tour#tournament{winners = Results, status = finished}),
    Users = [UId || #play_record{who=UId} <- nsm_tournaments:joined_users(TId)],
    Desc = case Tour#tournament.description of
        "" -> "";
        D -> " (" ++ D ++ ")"
    end,
    NoteString = "tour" ++ integer_to_list(length(Results)) ++ "|name=" ++ Tour#tournament.name ++ "|desc=" ++ Desc ++ lists:flatten([
        begin
            {ok, {Gift, _}} = nsm_gifts_db:get_gift(G),
            SP = integer_to_list(Pos),
            SKakush = integer_to_list(Gift#gift.kakush_point),
            SName = Gift#gift.gift_name,
            "|winner" ++ SP ++ "=" ++ UId ++ "|kakush" ++ SP ++ "=" ++ SKakush ++ "|prize" ++ SP ++ "=" ++ SName
        end
    || {UId, Pos, G} <- Results]),
    [nsx_msg:notify(["feed", "user", UId, "post_note"], NoteString) || UId <- Users],
    {noreply, State};

handle_notice(["system", "count_user"] = Route, Message, State) ->
    case nsm_mhits:stat_word_ip_date(Message#mhits.word,Message#mhits.ip,Message#mhits.date) of
                [] -> nsm_db:put(Message#mhits{count = 1});
                DB -> DB1 = lists:nth(1,DB), nsm_db:put(Message#mhits{count = Message#mhits.count + DB1#mhits.count}) end,
    ?INFO("feed(~p): count_user: Route=~p, Message=~p", [self(), Route, Message]),
    {noreply, State};

handle_notice(["system", "game_ends_note"] = Route, Message, State) ->
    ?INFO("feed(~p): game_ends_note: Route=~p, Message=~p", [self(), Route, Message]),
    {{GameName, GameType}, Results} = Message,
    NResults = lists:zip(lists:seq(1, length(Results)), Results),
    [
        case Robot of
            true -> ok;
            _ ->
                Prefix = case Pos of
                    0 -> % disabled for now
                        "game_won" ++ integer_to_list(length(Results)) ++ "|winner=" ++ UId ++ "|kakush=" ++ integer_to_list(KP) ++ "|points=" ++ integer_to_list(GP);
                    _ ->
                        "game_ended" ++ integer_to_list(length(Results))
                end,
                NoteString = Prefix ++ "|tablename=" ++ GameName ++ "|gametype=" ++ atom_to_list(GameType) ++ lists:flatten([
                    begin
                        SP = integer_to_list(P),
                        SKP = integer_to_list(RKP),
                        SGP = integer_to_list(RGP),
                        "|winner" ++ SP ++ "=" ++ RUId ++ "|kakush" ++ SP ++ "=" ++ SKP ++ "|points" ++ SP ++ "=" ++ SGP
                    end
                || {P, {RUId, _, _, RKP, RGP}} <- NResults]),
                nsx_msg:notify(["feed", "user", UId, "post_note"], NoteString)
        end
    || {UId, Robot, Pos, KP, GP} <- Results],
    %{GameId, [{UserId, Pos, KakushPoints, GamePoints}]}
    {noreply, State};


handle_notice(["feed", _, WhoShares, "entry", NewEntryId, "share"],
              #entry{entry_id = _EntryId, raw_description = Desc, media = Medias,
                     to = Destinations, from = From} = E,
              #state{feed = Feed, type = user} = State) ->
    %% FIXME: sharing is like posting to the wall
    ?INFO("share: ~p, WhoShares: ~p", [E, WhoShares]),
%    NewEntryId = utils:uuid_ex(),
    feed:add_shared_entry(Feed, From, Destinations, NewEntryId, Desc, Medias, {user, normal}, WhoShares),
    {noreply, State};

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

handle_notice(["feed", "user", UId, "scores", _Null, "add"] = Route,
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO("queue_action(~p): score statistics put: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),
    case UId == Owner of 
        true ->
            scoring:add_score(UId, hd(Message), perm);
        false ->
            ok
    end,
    {noreply, State};

handle_notice(["feed", "user", UId, "count_entry_in_statistics"] = Route, 
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO("queue_action(~p): count_entry_in_statistics: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),
    case nsm_db:get(user_etries_count, UId) of
        {ok, UEC} -> 
            nsm_db:put(UEC#user_etries_count{
                entries = UEC#user_etries_count.entries+1
            }),
            nsm_users:attempt_active_user_top(UId, UEC#user_etries_count.entries+1);
        {error, notfound} ->
            nsm_db:put(#user_etries_count{
                user_id = UId,
                entries = 1
            }),
            nsm_users:attempt_active_user_top(UId, 1)
    end,
    {noreply, State};

handle_notice(["feed", "user", UId, "count_comment_in_statistics"] = Route, 
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO("queue_action(~p): count_comment_in_statistics: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),
    case nsm_db:get(user_etries_count, UId) of
        {ok, UEC} -> 
            nsm_db:put(UEC#user_etries_count{
                comments = UEC#user_etries_count.comments+1
            });
        {error, notfound} ->
            nsm_db:put(#user_etries_count{
                user_id = UId,
                comments = 1
            })
    end,
    {noreply, State};

handle_notice(["db", "group", Owner, "put"] = Route, 
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO("queue_action(~p): group put: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),
    nsm_db:put(Message),
    {noreply, State};

handle_notice(["db", "user", Owner, "put"] = Route, 
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO("queue_action(~p): user put: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),
    nsm_db:put(Message),
    {noreply, State};

handle_notice(["system", "put"] = Route, 
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO("queue_action(~p): system put: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),
    nsm_db:put(Message),
    {noreply, State};

handle_notice(["system", "delete"] = Route, 
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO("queue_action(~p): system delete: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),
    {Where, What} = Message,
    nsm_db:delete(Where, What),
    {noreply, State};


handle_notice(["system", "create_group"] = Route, 
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO("queue_action(~p): create_group: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),
    {UId, GId, Name, Desc, Publicity} = Message,
    FId = nsm_db:feed_create(),
    CTime = erlang:now(),
    ok = nsm_db:put(#group{username = GId,
                              name = Name,
                              description = Desc,
                              publicity = Publicity,
                              creator = UId,
                              created = CTime,
                              owner = UId,
                              feed = FId}),
    nsx_msg:notify([group, init], {GId, FId}),
    nsm_users:init_mq_for_group(GId),
    add_to_group(UId, GId, member),
    {noreply, State};

handle_notice(["db", "group", GroupId, "update_group"] = Route, 
    Message, #state{owner=ThisGroupOwner, type=Type} = State) ->
    ?INFO("queue_action(~p): update_group: Owner=~p, Route=~p, Message=~p", [self(), {Type, ThisGroupOwner}, Route, Message]),    
    {_UId, _GroupUsername, Name, Description, Owner, Publicity} = Message,
    SanePublicity = case Publicity of
        "public" -> public;
        "moderated" -> moderated;
        "private" -> private;
        _ -> undefined
    end,
    SaneOwner = case nsm_db:get(user, Owner) of
        {ok, _} -> Owner;
        _ -> undefined
    end,
    {ok, #group{}=Group} = nsm_db:get(group, GroupId),
    NewGroup = Group#group{
                   name = coalesce(Name,Group#group.name),
                   description = coalesce(Description,Group#group.description),
                   publicity = coalesce(SanePublicity,Group#group.publicity),
                   owner = coalesce(SaneOwner,Group#group.owner)},
    nsm_db:put(NewGroup),
    {noreply, State};

handle_notice(["db", "group", GId, "remove_group"] = Route, 
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO("queue_action(~p): remove_group: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),
    {_, Group} = nsm_groups:get_group(GId),
    case Group of 
        notfound -> ok;
        _ ->
            nsx_msg:notify([feed, delete, GId], empty),
            nsm_db:delete_by_index(group_subs, <<"group_subs_group_id_bin">>, GId),         
            nsm_db:delete(feed, Group#group.feed),
            nsm_db:delete(group, GId),
            % unbind exchange
            {ok, Channel} = nsm_mq:open([]),
            Routes = nsm_users:rk_group_feed(GId),
            nsm_users:unbind_group_exchange(Channel, GId, Routes),
            nsm_mq_channel:close(Channel)
    end,
    {noreply, State};

handle_notice(["subscription", "user", UId, "add_to_group"] = Route,
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO("queue_action(~p): add_to_group: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),
    {GId, Who, UType} = Message,
    add_to_group(Who, GId, UType),
    ?INFO("add ~p to group ~p with Type ~p by ~p", [Who, GId,UType,UId]),
    nsm_users:subscribe_user_mq(group, Who, GId),
    {noreply, State};

handle_notice(["subscription", "user", UId, "remove_from_group"] = Route,
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO("queue_action(~p): remove_from_group: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),    
    {GId} = Message,
    ?INFO("remove ~p from group ~p", [UId, GId]),
    nsm_users:remove_subscription_mq(group, UId, GId),
    remove_from_group(UId, GId),
    {noreply, State};

handle_notice(["subscription", "user", UId, "leave_group"] = Route,
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO(" queue_action(~p): leave_group: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),
    {GId} = Message,
    {R, Group} = nsm_db:get(group, GId),
    case R of 
        error -> ?ERROR(" Error reading group ~p for leave_group", [GId]);
        ok ->
            case Group#group.owner of
                UId -> % User is owner, transfer ownership to someone else
                    Members = nsm_groups:list_group_members(GId),
                    case Members of
                        [ FirstOne | _ ] ->
                            ok = nsm_db:put(Group#group{owner = FirstOne}),
                            nsx_msg:notify(["subscription", "user", UId, "remove_from_group"], {GId});
                        [] ->
                            % Nobody left in group, remove group at all
                            nsx_msg:notify([db, group, GId, remove_group], [])
                    end;
                _ -> % Plain user removes -- just remove it
                    nsx_msg:notify(["subscription", "user", UId, "remove_from_group"], {GId})
            end;
        _ -> % user is just someone, remove it
            nsx_msg:notify(["subscription", "user", UId, "remove_from_group"], {GId})
    end,
    {noreply, State};

handle_notice(["subscription", "user", UId, "subscribe_user"] = Route,
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO(" queue_action(~p): subscribe_user: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),
    {Whom} = Message,
    nsm_users:subscr_user(UId, Whom),
    {noreply, State};

handle_notice(["subscription", "user", UId, "remove_subscribe"] = Route,
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO(" queue_action(~p): remove_subscribe: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),
    {Whom} = Message,
    nsm_users:unsubscr_user(UId, Whom),
    {noreply, State};

handle_notice(["subscription", "user", UId, "set_user_game_status"] = Route,
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO(" queue_action(~p): set_user_game_status: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),
    {Status} = Message,
    nsm_users:set_user_game_status(UId, Status),
    {noreply, State};

handle_notice(["subscription", "user", _UId, "update_user"] = Route,
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO(" queue_action(~p): update_user: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),
    {NewUser} = Message,
    nsm_users:update_user(NewUser),
    {noreply, State};

handle_notice(["subscription", "user", Who, "block_user"] = Route,
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO(" queue_action(~p): block_user: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),
    {Whom} = Message,
    nsm_users:unsubscr_user(Who, Whom),
    case nsm_db:get(user_ignores, Who) of
        {error, notfound} ->
            nsm_db:put(#user_ignores{who = Who, whom = [Whom]});
        {ok, #user_ignores{whom = List}} ->
            case lists:member(Whom, List) of
                false ->
                    NewList = [Whom | List],
                    nsm_db:put(#user_ignores{who = Who, whom = NewList});
                true ->
                    do_nothing
            end
    end,
    case nsm_db:get(user_ignores_rev, Whom) of
        {error,notfound} ->
            nsx_msg:notify(["db", "user", Whom, "put"], #user_ignores_rev{whom=Whom, who=[Who]});
        {ok,#user_ignores_rev{who=RevList}} ->
            case lists:member(Who, RevList) of
                false ->
                    NewRevList = [Who | RevList],
                    nsx_msg:notify(["db", "user", Whom, "put"], #user_ignores_rev{whom=Whom, who=NewRevList});
                true ->
                    do_nothing
            end
    end,
    nsx_msg:notify_user_block(Who, Whom),
    {noreply, State};

handle_notice(["subscription", "user", Who, "unblock_user"] = Route,
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO(" queue_action(~p): unblock_user: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),
    {Whom} = Message,
    List = nsm_db:list_blocks(Who),
    case lists:member(Whom, List) of
        true ->
            NewList = [ UID || UID <- List, UID =/= Whom ],
            nsm_db:put(#user_ignores{who = Who, whom = NewList});
        false ->
            do_nothing
    end,
    List2 = nsm_db:list_blocked_me(Whom),
    case lists:member(Who, List2) of
        true ->
            NewRevList = [ UID || UID <- List2, UID =/= Who ],
            nsx_msg:notify(["db", "user", Whom, "put"], #user_ignores_rev{whom = Whom, who = NewRevList});
        false ->
            do_nothing
    end,
    nsx_msg:notify_user_unblock(Who, Whom),
    {noreply, State};


handle_notice(["gifts", "user", UId, "buy_gift"] = Route,
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO(" queue_action(~p): buy_gift: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),
    {GId} = Message,
    nsm_users:buy_gift(UId, GId),
    {noreply, State};

handle_notice(["gifts", "user", UId, "give_gift"] = Route,
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO(" queue_action(~p): give_gift: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),
    {GId} = Message,
    nsm_users:give_gift(UId, GId),
    {noreply, State};

handle_notice(["gifts", "user", UId, "mark_gift_as_deliving"] = Route,
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO(" queue_action(~p): mark_gift_as_deliving: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),
    {GId, GTimestamp} = Message,
    nsm_users:mark_gift_as_deliving(UId, GId, GTimestamp),
    {noreply, State};


handle_notice(["login", "user", UId, "update_after_login"] = Route,
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO("queue_action(~p): update_after_login: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),    
    Update =
        case nsm_users:user_status(UId) of
            {error, status_info_not_found} ->
                #user_status{username = UId,
                             last_login = erlang:now()};
            {ok, UserStatus} ->
                UserStatus#user_status{last_login = erlang:now()}
        end,
    nsm_db:put(Update),
    {noreply, State};

handle_notice(["invite", "user", UId, "add_invite_to_issuer"] = Route,
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO("queue_action(~p): add_invite_to_issuer: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),
    {O} = Message,
    nsm_db:add_invite_to_issuer(UId, O),
    {noreply, State};

handle_notice(["system", "use_invite"] = Route,
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO("queue_action(~p): use_invite: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),
    {Code, UId} = Message,
    invite:use_code(Code, UId),
    {noreply, State};

handle_notice(["tournaments", "user", UId, "create"] = Route,
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO("queue_action(~p): create: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),
    {TourName, TourDesc, {Y,M,D}, Time, MaxPlayers, Quota, Award, TourType, GameType} = Message,
    case nsm_tournaments:create(UId, TourName, TourDesc, {Y,M,D}, Time, MaxPlayers, Quota, Award, TourType, GameType) of
        {error,X} -> 
            ?ERROR("Error creating tournament: ~p", X);
        TId -> 
            nsm_srv_tournament_lobby_sup:start_lobby(TId)
    end,
    {noreply, State};

handle_notice(["tournaments", "user", UId, "create_and_join"] = Route,
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO("queue_action(~p): create_and_join: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),
    {TourName, TourDesc, {Y,M,D}, Time, MaxPlayers, Quota, Award, TourType, GameType} = Message,
    case nsm_tournaments:create(UId, TourName, TourDesc, {Y,M,D}, Time, MaxPlayers, Quota, Award, TourType, GameType) of
        {error,X} -> 
            ?ERROR("Error creating tournament: ~p", X);
        TId -> 
            nsm_srv_tournament_lobby_sup:start_lobby(TId),
            tournaments:join(UId, TId)
    end,
    {noreply, State};

handle_notice(["likes", _, _, "add_like"] = Route,  % _, _ is here beacause of the same message used for comet update
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO("queue_action(~p): add_like: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),
    {UId, E} = Message,
    {EId, FId} = E#entry.id,
    feed:add_like(FId, EId, UId),
    {noreply, State};

handle_notice(["personal_score", "user", UId, "add"] = Route,
    Message, #state{owner = Owner, type = Type} = State) ->
    ?INFO("queue_action(~p): personal_score add: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),
    {Games, Wins, Loses, Disconnects, Points, AverageTime} = Message,
    scoring:add_personal_score(UId, Games, Wins, Loses, Disconnects, Points, AverageTime),
    {noreply, State};

handle_notice(["system", "create_contract"] = Route,
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO("queue_action(~p): create_contract: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),    
    Res = apply(nsm_affiliates, create_contract, tuple_to_list(Message)),
    case Res of
        ok -> ok;
        NotOk -> ?ERROR("Create contract error: ~p", [NotOk])
    end,
    {noreply, State};

handle_notice(["system", "create_contract_type"] = Route,
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO("queue_action(~p): create_contract_type: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),
    {Name, Duration, Limit, Commission} = Message,
    nsm_affiliates:create_contract_type(Name, Duration, Limit, Commission),
    {noreply, State};

handle_notice(["system", "disable_contract_type"] = Route,
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO("queue_action(~p): disable_contract_type: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),    
    {Id} = Message,
    nsm_affiliates:disable_contract_type(Id),
    {noreply, State};

handle_notice(["affiliates", "user", UId, "create_affiliate"] = Route,
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO("queue_action(~p): create_affiliate: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),   
    nsm_affiliates:create_affiliate(UId),
    {noreply, State};

handle_notice(["affiliates", "user", UId, "delete_affiliate"] = Route,
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO("queue_action(~p): delete_affiliate: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),
    nsm_affiliates:delete_affiliate(UId),
    {noreply, State};

handle_notice(["affiliates", "user", UId, "enable_to_look_details"] = Route,
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO("queue_action(~p): enable_to_look_details: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),  
    nsm_affiliates:enable_to_look_details(UId),  
    {noreply, State};

handle_notice(["affiliates", "user", UId, "disable_to_look_details"] = Route,
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO("queue_action(~p): disable_to_look_details: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),
    nsm_affiliates:disable_to_look_details(UId),
    {noreply, State};

handle_notice(["system", "add_package"] = Route,
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO("queue_action(~p): add_package: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),
    {MP} = Message,
    case nsm_membership_packages:add_package(MP) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            ?ERROR("Unable to add membership package: ~p, Reason ~p", [MP, Reason])
    end,
    {noreply, State};

handle_notice(["purchase", "user", _, "set_purchase_state"] = Route,
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO("queue_action(~p): set_purchase_state: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),  
    {MPId, NewState, Info} = Message,
    nsm_membership_packages:set_purchase_state(MPId, NewState, Info),
    {noreply, State};

handle_notice(["purchase", "user", _, "add_purchase"] = Route,
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO("queue_action(~p): add_purchase: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),    
    {MP} = Message,
    nsm_membership_packages:add_purchase(MP),
    {noreply, State};

handle_notice(["transaction", "user", User, "add_transaction"] = Route,
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO("queue_action(~p): add_transaction: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),    
    MP = Message,
    nsm_db:add_transaction_to_user(User,MP),
    {noreply, State};

handle_notice(["purchase", "user", _, "set_purchase_external_id"] = Route,
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO("queue_action(~p): set_purchase_external_id: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),
    {PurchaseId, TxnId} = Message,
    nsm_membership_packages:set_purchase_external_id(PurchaseId, TxnId),
    {noreply, State};

handle_notice(["purchase", "user", _, "set_purchase_info"] = Route,
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO("queue_action(~p): set_purchase_info: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),
    {OrderId, Info} = Message,
    nsm_membership_packages:set_purchase_info(OrderId, Info),
    {noreply, State};

handle_notice(["system", "tournament_join"] = Route,
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO("queue_action(~p): tournament_join: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),
    {UId, TId} = Message,
    nsm_tournaments:join(UId, TId),
    {noreply, State};

handle_notice(["system", "tournament_remove"] = Route,
    Message, #state{owner = Owner, type =Type} = State) ->
    ?INFO("queue_action(~p): tournament_remove: Owner=~p, Route=~p, Message=~p", [self(), {Type, Owner}, Route, Message]),
    {UId, TId} = Message,
    nsm_tournaments:remove(UId, TId),
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
    [{routes, [""]},
     {gproc_name, Name},
     {consume_options, [exclusive]},
     {queue, QueueName},
     {exchange, ?GROUP_EXCHANGE(Owner)},
     {queue_options, queue_options()}];

get_opts(#state{type = system, owner = Owner}) ->
    Name = ?FEED_WORKER_NAME(group, Owner),
    QueueName = nsm_mq_lib:list_to_key(Name),
    [{routes, [[system, put],
                [system, delete],
                [system, create_group],
                % affiliates
                [system, create_contract],
                [system, create_contract_type],
                [system, disable_contract_type],
                % membership pachages
                [system, add_package],
                % invites
                [system, use_invite],
                [system, count_user],
                % notifications
                [system, game_begins_note], % out of order
                [system, tournament_tour_note],
                [system, tournament_ends_note],
                [system, game_ends_note],
                %tournaments
                [system, tournament_join],
                [system, tournament_remove]
                ]},
     {gproc_name, Name},
     {consume_options, [exclusive]},
     {queue, QueueName},
     {queue_options, queue_options()}].

coalesce(undefined, B) -> B;
coalesce(A, _) -> A.

queue_options() ->
    [durable,
     {ttl, 10000},
     {auto_delete, false},
     {dead_letter_exchange, ?DEAD_LETTER_EXCHANGE}].

remove_entry(#state{feed = Feed, direct = Direct}, EntryId) ->
    [feed:remove_entry(FId, EntryId) || FId <- [Feed, Direct]].

edit_entry(#state{feed=Feed, direct = Direct}, EntryId, NewDescription) ->
    [feed:edit_entry(FId, EntryId, NewDescription) || FId <- [Feed, Direct]].


add_comment(#state{feed = Feed, direct = Direct}, From, EntryId, ParentComment,
            CommentId, Content, Medias) ->
    [feed:entry_add_comment(FId, From, EntryId, ParentComment,
                            CommentId, Content, Medias)
       || FId <- [Feed, Direct]].


add_to_group(UId, GId, Type) ->
    ?INFO("U:~p G:~p T:~p",[UId, GId, Type]),
    case nsm_db:get(group_subs, {UId, GId}) of
        {error, notfound} ->
            {R, Group} = nsm_db:get(group, GId),
            case R of 
                error -> ?INFO("Add to group failed reading group");
                _ ->
                    GU = Group#group.users_count,
                    nsm_db:put(Group#group{users_count = GU+1})
            end;
        _ ->
            ok
    end,
    OK = nsm_db:put({group_subs,UId,GId,Type,0}),
    ?INFO("RES:~p",[OK]).

remove_from_group(UId, GId) ->
    nsm_db:delete(group_subs, {UId, GId}),
    {R, Group} = nsm_db:get(group, GId),
    case R of
        error -> ?INFO("Remove ~p from group failed reading group ~p", [UId, GId]);
        _ ->
            GU = Group#group.users_count,
            nsm_db:put(Group#group{users_count = GU-1})
    end.


