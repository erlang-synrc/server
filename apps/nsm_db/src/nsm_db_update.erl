-module(nsm_db_update).

-include("user.hrl").
-include("feed.hrl").
-include("nsm_bg.hrl").
-include("affiliates.hrl").

-export([transform_user/0,
         transform_group/0,
         delete_feeds/0,
         add_subscription_exchanges/0,
         fix_subscription_exchanges/0,
         delete_worker_queues/0,
         clean_affiliates_buckets/0]).


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

