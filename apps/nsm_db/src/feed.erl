-module(feed).

-export([create/0,
         add_entry/4,
         add_entry/5,
         add_entry/7,
         add_shared_entry/8,
         add_like/3,
         broadcast/2,
         multi_broadcast/2,
         get_entries_in_feed/1,
         get_entries_in_feed/2,
         get_entries_in_feed/3,
         get_entries_in_feed/4,        
         get_feed/1,
         get_entries_likes/1,
         get_entries_likes_count/1,
         get_user_likes/1,
         get_user_likes/2,
         get_user_likes_count/1,
         get_entries_count/1,
         get_comments_count/1,
         remove_entry/2,
         edit_entry/3,
         add_direct_message/4,
         add_direct_message/6,
         entry_add_comment/7,
         get_direct_messages/2,
         get_direct_messages/3,
         is_subscribed_user/2,
         user_friends_count/1,
         user_subscription_count/1,
         get_comments_entries/4,
         get_my_discussions/4,
         get_feed_by_user_or_group/1,
         add_group_entry/5,
         add_group_entry/7,
         remove_entry_comments/2,
         
         test_likes/0
         ]).


-include("feed.hrl").
-include_lib("nsm_db/include/table.hrl").
-include("user.hrl").
-include("config.hrl").

-include_lib("nsx_config/include/log.hrl").

-define(ROOT, "site/static").

create() ->
    FId = nsm_db:next_id("feed", 1),
    ok = nsm_db:put(#feed{id = FId} ),
    FId.

add_direct_message(FId, User, EntryId, Desc) ->
    add_direct_message(FId, User, undefined, EntryId, Desc, []).
add_direct_message(FId, User, To, EntryId, Desc, Medias) ->
    nsm_db:feed_add_direct_message(FId, User, To, EntryId, Desc, Medias).

add_group_entry(FId, User, EntryId, Desc, Medias) ->
    nsm_db:feed_add_entry(FId, User, EntryId, Desc, Medias).
add_group_entry(FId, User, To, EntryId, Desc, Medias, Type) ->
    nsm_db:feed_add_entry(FId, User, To, EntryId, Desc, Medias, Type).

add_entry(FId, User, EntryId, Desc) ->
    add_entry(FId, User, EntryId, Desc, []).
add_entry(FId, User, EntryId, Desc, Medias) ->
    nsm_db:feed_add_entry(FId, User, EntryId, Desc, Medias).
add_entry(FId, User, To, EntryId, Desc, Medias, Type) ->
    nsm_db:feed_add_entry(FId, User, To, EntryId, Desc, Medias, Type, "").
add_shared_entry(FId, User, To, EntryId, Desc, Medias, Type, SharedBy) ->
    nsm_db:feed_add_entry(FId, User, To, EntryId, Desc, Medias, Type, SharedBy).

add_like(Fid, Eid, Uid) ->
    Write_one_like = fun(Next) ->
        Self_id = nsm_db:next_id("one_like", 1),   
        nsm_db:put(#one_like{    % add one like
            id = Self_id,
            user_id = Uid,
            entry_id = Eid,
            feed_id = Fid,
            created_time = now(),
            next = Next
        }),
        Self_id
    end,
    % add entry - like
    case nsm_db:get(entry_likes, Eid) of
        {ok, ELikes} -> 
            nsm_db:put(ELikes#entry_likes{
                one_like_head = Write_one_like(ELikes#entry_likes.one_like_head), 
                total_count = ELikes#entry_likes.total_count + 1
            });
        {error, notfound} ->
            nsm_db:put(#entry_likes{
                entry_id = Eid,                
                one_like_head = Write_one_like(undefined),
                total_count = 1
            })
    end,
    % add user - like
    case nsm_db:get(user_likes, Uid) of
        {ok, ULikes} -> 
            nsm_db:put(ULikes#user_likes{
                one_like_head = Write_one_like(ULikes#user_likes.one_like_head),
                total_count = ULikes#user_likes.total_count + 1
            });
        {error, notfound} ->
            nsm_db:put(#user_likes{
                user_id = Uid,                
                one_like_head = Write_one_like(undefined),
                total_count = 1
            })
    end.

% statistics

get_entries_count(Uid) ->
    case nsm_db:get(user_etries_count, Uid) of
        {ok, UEC} -> 
            UEC#user_etries_count.entries;
        {error, notfound} ->
            0
    end.

get_comments_count(Uid) ->
    case nsm_db:get(user_etries_count, Uid) of
        {ok, UEC} -> 
            UEC#user_etries_count.comments;
        {error, notfound} ->
            0
    end.

broadcast({group, Group}, Entry) ->
    Users = nsm_groups:list_group_members(Group),
    multi_broadcast(Users, Entry);

broadcast(new_table, #game_table{owner=UId} = Table) ->
    GIds = nsm_groups:list_groups_per_user(UId),

    UsersList0 = rpc:call(?WEBSERVER_NODE,site_utils,get_usort_user,[GIds, []]),
    Fun = fun(User) -> table_manager:filter_table(User, Table) end,
    UsersList = lists:filter(Fun, UsersList0) -- [UId],

    Message = create_message(Table),
    multi_broadcast(UsersList, Message);

broadcast(User, Entry) ->
    Users = [Friend || #subs{who = Friend} <- nsm_users:list_subscr_me(User)],
    multi_broadcast(Users, Entry).

multi_broadcast(Users, #entry{entry_id=EId} = Entry) ->
    [ begin
          {ok, U} = nsm_users:get_user(UserId),
          FId = U#user.feed,
          NewEntry = Entry#entry{id={EId, FId},
                                 feed_id = FId},
        nsm_db:put(NewEntry)
      end
      || UserId <- Users ],
    ok.


get_feed(FId) ->
    nsm_db:get(feed, FId).

get_entries_in_feed(FId) ->
    nsm_db:entries_in_feed(FId).
get_entries_in_feed(FId, Count) ->
    nsm_db:entries_in_feed(FId, Count).
get_entries_in_feed(FId, StartFrom, Count) ->
    nsm_db:entries_in_feed(FId, StartFrom, Count).

get_direct_messages(FId, Count) ->
    nsm_db:entries_in_feed(FId, undefined, Count).

get_direct_messages(FId, StartFrom, Count) ->
    nsm_db:entries_in_feed(FId, StartFrom, Count).

get_entries_in_feed(FId, StartFrom, Count, FromUserId)->
	Entries = nsm_db:entries_in_feed(FId, StartFrom, Count),
	[E || #entry{from = From} = E <- Entries, From == FromUserId].

create_message(Table) ->
    EId = nsm_db:next_id("entry", 1),
    #entry{id = {EId, system_info},
        entry_id = EId,
        from = system,
        type = {system, new_table},
        created_time = now(),
        description = Table}.


remove_entry(FeedId, EId) ->
    {ok, #feed{top = TopId} = Feed} = get_feed(FeedId),

    case nsm_db:get(entry, {EId, FeedId}) of
        {ok, #entry{prev = Prev, next = Next}}->
            ?INFO("P: ~p, N: ~p", [Prev, Next]),
            case nsm_db:get(entry, Next) of
                {ok, NE} ->
                    nsm_db:put(NE#entry{prev = Prev});
                _ ->
                    ok
            end,
            case nsm_db:get(entry, Prev) of
                {ok, PE} ->
                    nsm_db:put(PE#entry{next = Next});
                _ ->
                    ok
            end,

            case TopId of
                {EId, FeedId} ->
                    nsm_db:put(Feed#feed{top = Prev});
                _ ->
                    ok
            end;

        {error, notfound} ->
            ?INFO("Not found"),
            ok
    end,

    nsm_db:delete(entry, {EId, FeedId}).


% edit
edit_entry(FeedId, EId, NewDescription) ->
    case nsm_db:entry_by_id({EId, FeedId}) of
        {ok, OldEntry} ->
            NewEntryRaw =  OldEntry#entry{description = NewDescription,
                                          raw_description = NewDescription},
            NewEntry = feedformat:format(NewEntryRaw),
            nsm_db:put(NewEntry);
        {error, notfound}->
            {error, notfound}
    end.


remove_entry_comments(FId, EId) ->
    AllComments = nsm_db:comments_by_entry(FId, EId),
    [begin
          nsm_db:delete(comment, ID),
          remove_media(M)
     end || #comment{id = ID, media = M} <- AllComments].

entry_add_comment(FId, User, EntryId, ParentComment, CommentId, Content, Medias) ->
     case nsm_db:entry_by_id({EntryId, FId}) of
         {ok, _E} ->
             nsm_db:add_comment(FId, User, EntryId, ParentComment, CommentId, Content, Medias);
         _ ->
             ok
     end.

remove_media([]) -> ok;
remove_media([#media{url=undefined, thumbnail_url=undefined}|T]) ->
    remove_media(T);
remove_media([#media{url=undefined, thumbnail_url=TUrl}|T])      ->
    file:delete(?ROOT ++ TUrl),
    remove_media(T);
remove_media([#media{url=Url, thumbnail_url=undefined}|T])       ->
    file:delete(?ROOT ++ Url),
    remove_media(T);
remove_media([#media{url=Url, thumbnail_url=TUrl}|T])            ->
    file:delete(?ROOT ++ Url),file:delete(?ROOT ++ TUrl),
    remove_media(T).


% likes
get_one_like_list(undefined) ->
    [];
get_one_like_list(Id) ->
    {ok, OneLike} = nsm_db:get(one_like, Id),
    [OneLike] ++ get_one_like_list(OneLike#one_like.next).

get_entries_likes(Entry_id) ->
    case nsm_db:get(entry_likes, Entry_id) of
        {ok, Likes} -> get_one_like_list(Likes#entry_likes.one_like_head);
        {error, notfound} -> []
    end.

get_entries_likes_count(Entry_id) ->
    case nsm_db:get(entry_likes, Entry_id) of
        {ok, Likes} ->
            Likes#entry_likes.total_count;
        {error, notfound} -> 0
    end.

get_user_likes_count(UserId) ->
    case nsm_db:get(user_likes, UserId) of
        {ok, Likes} -> Likes#user_likes.total_count;
        {error, notfound} -> 0
    end.

get_user_likes(UserId) ->
    case nsm_db:get(user_likes, UserId) of
        {ok, Likes} -> get_one_like_list(Likes#user_likes.one_like_head);
        {error, notfound} -> []
    end.

get_one_like_list(undefined, _) ->
    [];
get_one_like_list(_, 0) ->
    [];
get_one_like_list(Id, N) ->
    {ok, OneLike} = nsm_db:get(one_like, Id),
    [OneLike] ++ get_one_like_list(OneLike#one_like.next, N-1).

get_user_likes(UserId, {Page, PageAmount}) ->
    case nsm_db:get(user_likes, UserId) of
        {ok, Likes} -> lists:nthtail((Page-1)*PageAmount, get_one_like_list(Likes#user_likes.one_like_head, PageAmount*Page));
        {error, notfound} -> []
    end.

% we have same in nsm_user? Why?
is_subscribed_user(UserUidWho, UserUidWhom) ->
    nsm_users:is_user_subscr(UserUidWho, UserUidWhom).

user_subscription_count(UserUid) ->
    length(nsm_users:list_subscr(UserUid)).

user_friends_count(UserUid) ->
    length(nsm_users:list_subscr_me(UserUid)).

get_comments_entries(UserUid, _, _Page, _PageAmount) ->
    Pids = [Eid || #comment{entry_id=Eid} <- nsm_db:select(comment,
        fun(#comment{author_id=Who}) when Who=:=UserUid ->true;(_)->false end)],
    %?PRINT({"GCE pids length: ", length(Pids)}),
    lists:flatten([nsm_db:select(entry,[{where, fun(#entry{entry_id=ID})-> ID=:=Pid end},
        {order, {1, descending}},{limit, {1,1}}]) || Pid <- Pids]).

get_my_discussions(_FId, Page, PageAmount, UserUid) ->
    _Offset= case (Page-1)*PageAmount of
        0 -> 1
        ;M-> M
    end,
    Pids = [Eid || #comment{entry_id=Eid} <- nsm_db:select(comment,
        fun(#comment{author_id=Who}) when Who=:=UserUid ->true;(_)->false end)],
    lists:flatten([nsm_db:select(entry,[{where, fun(#entry{entry_id=ID})-> ID=:=Pid end},
        {order, {1, descending}},{limit, {1,1}}]) || Pid <- Pids]).

get_feed_by_user_or_group(UserOrGroup) ->
    case nsm_users:get_user(UserOrGroup) of
        {ok, User} -> {ok, user, User#user.feed, User#user.direct};
        _          -> case nsm_db:get(group, UserOrGroup) of
                        {ok, Group} -> {ok, group, Group#group.feed, undefined};
                        _           -> {ok, group, undefined, undefined}    % group may be late to a base
%                        _           -> {error, not_found}
                      end
    end.

test_likes() ->
    add_like(1, "17a10803-f064-4718-ae46-4a6d3c88415c-0796e2bb", "derp"),
    add_like(1, "17a10803-f064-4718-ae46-4a6d3c88415c-0796e2bb", "derpina"),
    add_like(1, "17a10803-f064-4718-ae46-4a6d3c88415c-0796e2bb", "derpington"),
    add_like(1, "17a10803-f064-4718-ae46-4a6d3c88415c-0796e2bc", "derp"),
    add_like(1, "17a10803-f064-4718-ae46-4a6d3c88415c-0796e2bc", "lederpeaux"),
    add_like(2, "17a10803-f064-4718-ae46-4a6d3c88415c-0796e2bd", "derp"),
    add_like(2, "17a10803-f064-4718-ae46-4a6d3c88415c-0796e2be", "derp"),
    [
        get_entries_likes("17a10803-f064-4718-ae46-4a6d3c88415c-0796e2bb"),
        get_entries_likes_count("17a10803-f064-4718-ae46-4a6d3c88415c-0796e2bb") == 3,
        get_user_likes("derp"),
        get_user_likes("derp", {1, 2}),
        get_user_likes_count("derp") == 4
    ].
