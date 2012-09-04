%%% @author JLarky <jlarky@punklan.net>
%%% @copyright (C) 2011, JLarky
%%% @copyright Paynet Internet ve Bilisim Hizmetleri A.S. All Rights Reserved.
%%% @doc
%%% Module containing utilites transforming database. Used to migrate
%%% data from one version to other
%%% @end
%%% Created : 11 Jul 2011 by JLarky <jlarky@punklan.net>

-module(zealot_db_update).

-include("user.hrl").
-include("feed.hrl").
-include_lib("nsm_bg/include/nsm_bg.hrl").

-export([transform_user/0,
         transform_group/0,
         delete_feeds/0,
         add_subscription_exchanges/0,
         fix_subscription_exchanges/0,
         delete_worker_queues/0]).


transform_user() ->
    All = zealot_db:all(user),
    [begin
         io:format("transform ~p ... ", [element(2, U)]),
         UT = transform_user(U),
         io:format("ok. Write to db ... ", []),
         R = zealot_db:put(UT),
         io:format("~p~n", [R])
     end || U <- All, size(U) == 20].

transform_group()->
    [begin
         io:format("transform ~p ... ", [G#group.username]),
         R = zealot_db:put(G#group{feed = feed:create()}),
         io:format("~p~n", [R])
     end || G <- zealot_db:all(group)].

add_subscription_exchanges() ->
    [begin
         Groups = groups:list_group_per_user(User),
         GroupNames = [GN || #group_member{group_name = GN} <- Groups],
         users:init_mq(User#user.username, GroupNames)
     end || User <- zealot_db:all(user)].

delete_worker_queues() ->
    Users = zealot_db:all(user),
    Groups = zealot_db:all(group),

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
         Groups = groups:list_group_per_user(User),
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
     end || #user{username = User} <- zealot_db:all(user)].


delete_feeds() ->
    [zealot_db:delete(feed, F#feed.id) || F <- zealot_db:all(feed)],
    [zealot_db:delete(commnent, element(2, C)) || C <- zealot_db:all(comment)],
    % by entry id
    [zealot_db:delete(entry, element(3, E)) || E <- zealot_db:all(entry)].

transform_user(U) ->
    Starred = feed:create(),
    Pinned = feed:create(),
    Comments = feed:create(),
    Discussions = feed:create(),
    U1 = insert_after(U, #user.direct, [Starred, Pinned, Comments, Discussions]),
    %% reset old feeds
    U1#user{feed = feed:create(), direct = feed:create()}.

insert_after(Tuple, Pos, Values) ->
    L = tuple_to_list(Tuple),
    {H, T} = lists:split(Pos, L),
    list_to_tuple(H ++ Values ++ T).

group_rk(Group) ->
     nsm_mq_lib:list_to_key([feed, group, Group, '*', '*', '*']).





