-module(nsm_db_update).

-include("user.hrl").
-include("feed.hrl").
-include("nsm_bg.hrl").
-include("affiliates.hrl").

-compile(export_all).

transform_user() ->
    All = nsm_db:all(user),
    [begin
         io:format("transform ~p ... ", [element(2, U)]),
         UT = transform_user(U),
         io:format("ok. Write to db ... ", []),
         R = nsm_db:put(UT),
         io:format("~p~n", [R])
     end || U <- All, size(U) == 20].

transform_group()->
    [begin
         io:format("transform ~p ... ", [G#group.username]),
         R = nsm_db:put(G#group{feed = nsm_db:feed_create()}),
         io:format("~p~n", [R])
     end || G <- nsm_db:all(group)].

add_subscription_exchanges() ->
    [begin
         Groups = nsm_groups:list_group_per_user(User),
         GroupNames = [GN || #group_member{group_name = GN} <- Groups],
         nsm_users:init_mq(User#user.username, GroupNames)
     end || User <- nsm_db:all(user)].

delete_worker_queues() ->
    Users = nsm_db:all(user),
    Groups = nsm_db:all(group),

    {ok, Ch} = nsm_mq:open([]),
    [begin
         QueueName = nsm_mq_lib:list_to_key(
                       ?FEED_WORKER_NAME(user, U#user.username)),
         nsm_mq_channel:delete_queue(Ch, QueueName)
     end  || U <- Users],
    [begin
         QueueName = nsm_mq_lib:list_to_key(
                       ?FEED_WORKER_NAME(group, G#group.username)),
         nsm_mq_channel:delete_queue(Ch, QueueName)
     end  || G <- Groups].


fix_subscription_exchanges() ->

    [begin
         Groups = nsm_groups:list_group_per_user(User),
         GroupNamesRK = [group_rk(GN) || #group_member{group_name = GN} <- Groups],
         GroupIdsRK = [group_rk(GN) || #group_member{group = GN} <- Groups],
         try
             io:format("~p: try to unbind/bind: ~p", [User, GroupIdsRK]),
             {ok, Ch} = nsm_mq:open([]),
             [users:bind_user_exchange(Ch, User, RK) || RK <- GroupIdsRK],
             [users:unbind_user_exchange(Ch, User, RK) || RK <- GroupNamesRK],
             nsm_mq_channel:close(Ch)
         catch
             _:E->
                 io:format("~p: error: ~p", [User, E])
         end
     end || #user{username = User} <- nsm_db:all(user)].


delete_feeds() ->
    [nsm_db:delete(feed, F#feed.id) || F <- nsm_db:all(feed)],
    [nsm_db:delete(commnent, element(2, C)) || C <- nsm_db:all(comment)],
    % by entry id
    [nsm_db:delete(entry, element(3, E)) || E <- nsm_db:all(entry)].

transform_user(U) ->
    Starred = nsm_db:feed_create(),
    Pinned = nsm_db:feed_create(),
    Comments = nsm_db:feed_create(),
    Discussions = nsm_db:feed_create(),
    U1 = insert_after(U, #user.direct, [Starred, Pinned, Comments, Discussions]),
    %% reset old feeds
    U1#user{feed = nsm_db:feed_create(), direct = nsm_db:feed_create()}.

insert_after(Tuple, Pos, Values) ->
    L = tuple_to_list(Tuple),
    {H, T} = lists:split(Pos, L),
    list_to_tuple(H ++ Values ++ T).

group_rk(Group) ->
     nsm_mq_lib:list_to_key([feed, group, Group, '*', '*', '*']).


%% Clean old format records. 10 Oct 2012
clean_affiliates_buckets() ->
    Rels = nsm_db:all(affiliates_rels),
    [nsm_db:delete(affiliates_rels, term_to_binary(UserId)) || #affiliates_rels{user = UserId} <- Rels],

    Contracts = nsm_db:all(affiliates_contracts),
    [nsm_db:delete(affiliates_contracts, term_to_binary(Id)) || #affiliates_contracts{id = Id} <- Contracts],

    ContractTypes = nsm_db:all(affiliates_contract_types),
    [nsm_db:delete(affiliates_contract_types, term_to_binary(Id)) || #affiliates_contract_types{id = Id} <- ContractTypes],

    Purchases = nsm_db:all(affiliates_purchases),
    [nsm_db:delete(affiliates_purchases, term_to_binary({ContractId, UserId})) ||
       #affiliates_purchases{contract_id = ContractId, user_id = UserId} <- Purchases],

    Perms = nsm_db:all(affiliates_look_perms),
    [nsm_db:delete(affiliates_look_perms, term_to_binary(UserId)) || #affiliates_look_perms{user_id = UserId} <- Perms],
    ok.

%% Clean multiple likes. 16 Oct 2012
is_user_in_like_list(_, []) ->
    false;
is_user_in_like_list(User, LikeList) ->
    case (hd(LikeList))#one_like.user_id of
        User -> true;
        _ -> is_user_in_like_list(User, tl(LikeList))
    end.

clean_like_list(Dirty) ->
    clean_like_list(Dirty, []).
clean_like_list([], Clean) ->
    Clean;
clean_like_list(Dirty, Clean) ->
    case is_user_in_like_list((hd(Dirty))#one_like.user_id, Clean) of
        true ->
            clean_like_list(tl(Dirty), Clean);
        false ->
            clean_like_list(tl(Dirty), Clean ++ [hd(Dirty)])
    end.

relink_like_list([]) ->
    [];
relink_like_list([H|[]]) ->
    [H#one_like{next = undefined}];
relink_like_list([H|T]) ->
    [H#one_like{next = (hd(T))#one_like.id}] ++ relink_like_list(T).
    

clean_multiple_likes() ->
    EntryLikes = nsm_db:all(entry_likes),
    [
        begin
            OneLikeList = feed:get_entries_likes(EntryLike#entry_likes.entry_id),
            New = relink_like_list( clean_like_list(OneLikeList) ),
            [nsm_db:put(NOL) || NOL <- New],
            nsm_db:put(EntryLike#entry_likes{one_like_head = (hd(New))#one_like.id} )
        end
    || EntryLike <- EntryLikes].

% fill top with users depending on entry count 18 Oct 2012
populate_active_users_top() ->
    [nsm_users:attempt_active_user_top(UId, feed:get_entries_count(UId)) || #user{username=UId} <- nsm_db:all(user)].

% converting group subscriptions to leveldb 19 Oct 2012
count_entries(UId, GId) ->
    AllEntries = nsm_db:all(entry),
    {_, Group} = nsm_groups:get_group(GId),
    case Group of
        notfound -> ok;
        _ ->
            GFId = Group#group.feed,
            length([1 || E <- AllEntries, E#entry.feed_id == GFId, E#entry.from == UId])
    end.

group_member_to_group_subs() ->
    [[nsm_db:put(#group_subs{user_id=Who, group_id=Group, user_type=Type, user_posts_count=count_entries(Who, Group)}) 
        || #group_member{who=Who, group=Group, type=Type} <- Subs] 
            || #group_member{group=Subs} <- nsm_db:all(group_member)].

% enriching group for useful statistics 22 Oct 2012
count_group_users(GId) -> 
    length(nsm_groups:list_group_members(GId)).

count_group_entries(GId) ->
    AllEntries = nsm_db:all(entry),
    {ok, Group} = nsm_groups:get_group(GId),
    GFId = Group#group.feed,
    length([1 || E <- AllEntries, E#entry.feed_id == GFId]).

add_two_0_to_group_if_needed() ->
    Groups = nsm_db:all(group),
    [case size(Group) of
        9 -> nsm_db:put(list_to_tuple(tuple_to_list(Group) ++ [0,0]));
        _ -> ok
     end   
        || Group <- Groups].

enrich_groups_with_statistics() ->
    add_two_0_to_group_if_needed(),
    Groups = nsm_db:all(group),
    [nsm_db:put(Group#group{users_count=count_group_users(Group#group.username), entries_count=count_group_entries(Group#group.username)})
        || Group <- Groups].

% in order to properly update groups with one call you should call this:
update_groups_to_leveldb() ->
    add_two_0_to_group_if_needed(),
    group_member_to_group_subs(),
    enrich_groups_with_statistics().

convert_twitter()->
  Users = [  #user{
    username = UserName,
    password = Password,
    facebook_id = FacebookId,
    twitter_id = undefined,
    email = Email,
    avatar = Avatar,
    name = Name,
    surname = Surname,
    age = Age,
    sex = Sex,
    location = Location,
    education = Education,
    register_date = RegisterDate,
    status = Status,
    verification_code = VerificationCode,
    type = Type,
    feed = Feed,
    direct = Direct,
    starred = Starred,
    pinned = Pinned,
    comments = Comments,
    discussions = Discussions,
    team = Team,
    aclver = Aclver  }
  || {user,
      UserName, Password, FacebookId, Email, Avatar, Name, Surname, Age,
      Sex, Location, Education, RegisterDate, Status, VerificationCode,
      Type, Feed, Direct, Starred, Pinned, Comments, Discussions, Team, Aclver} <- nsm_db:all(user)],
  [nsm_db:put(U) || U<-Users].
