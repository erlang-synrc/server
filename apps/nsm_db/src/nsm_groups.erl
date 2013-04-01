-module(nsm_groups).
-compile(export_all).
-include("user.hrl").
-include("feed.hrl").
-include_lib("nsx_config/include/log.hrl").

retrieve_groups(User) ->
    ?INFO("retrieve_groups: ~p",[User]),
    case nsm_groups:list_groups_per_user(User) of
         [] -> [];
         Gs -> UC_GId = lists:sublist(lists:reverse(
                              lists:sort([{nsm_groups:group_members_count(GId), GId} || GId <- Gs])), 
                                    20),
               Result = [begin case nsm_groups:get_group(GId) of
                                   {ok, Group} -> {Group#group.name,GId,UC};
                                   _ -> undefined end end || {UC, GId} <- UC_GId],
               [X||X<-Result,X/=undefined] end.

create_group_directly_to_db(UId, GId, Name, Desc, Publicity) ->
    FId = nsm_db:feed_create(),
    CTime = erlang:now(),
    nsm_db:put(#group{username = GId,
                      name = Name,
                      description = Desc,
                      publicity = Publicity,
                      creator = UId,
                      created = CTime,
                      owner = UId,
                     feed = FId}),
    nsm_users:init_mq_for_group(GId),
    nsm_groups:add_to_group_directly_to_db(UId, GId, member),
    GId.

add_to_group(Who, GId, Type, Owner) -> nsx_msg:notify(["subscription", "user", Owner, "add_to_group"], {GId, Who, Type}).

add_to_group_directly_to_db(UId, GId, Type) ->
    nsm_db:put(#group_subs{user_id=UId, group_id=GId, user_type=Type}),
    {ok, Group} = nsm_db:get(group, GId),
    GU = Group#group.users_count,
    nsm_db:put(Group#group{users_count = GU+1}).

delete_group(GId) ->
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
    end.

list_groups_per_user(UId) -> [GId || #group_subs{group_id=GId} <- nsm_db:all_by_index(group_subs, <<"group_subs_user_id_bin">>, UId) ].
list_group_members(GId) -> [UId || #group_subs{user_id=UId, user_type=UT} <- nsm_db:all_by_index(group_subs, <<"group_subs_group_id_bin">>, GId), UT == member ].
list_group_members_by_type(GId, Type) -> [UId || #group_subs{user_id=UId, user_type=UT} <- nsm_db:all_by_index(group_subs, <<"group_subs_group_id_bin">>, GId), UT == Type ].
list_group_members_with_types(GId) -> [{UId, UType} || #group_subs{user_id=UId, user_type=UType} <- nsm_db:all_by_index(group_subs, <<"group_subs_group_id_bin">>, list_to_binary(GId)) ].

get_group(GId) -> nsm_db:get(group, GId).

user_is_owner(UId, GId) ->
    {R, Group} = nsm_db:get(group, GId),
    case R of
        ok -> case Group#group.owner of
                UId -> true;
                _ -> false
            end;
        _ -> false
    end.

user_in_group(UId, GId) ->
    case nsm_db:get(group_subs, {UId, GId}) of
        {error, notfound} -> false;
        _ -> true
    end.

group_user_type(UId, GId) ->
    case nsm_db:get(group_subs, {UId, GId}) of
        {error, notfound} -> not_in_group;
        {ok, #group_subs{user_type=Type}} -> Type
    end.

get_all_groups() -> nsm_db:all(group).
get_popular_groups() -> ["kakaranet", "yeniler"].   % :-)

%% join if group public, or send join request to group
join_group(GId, User) ->
    {ok, Group} = get_group(GId),
    case Group of
        #group{username = GId, publicity = public} ->
            % Join to this group
            add_to_group(User, GId, member, Group#group.owner),
            {ok, joined};
        #group{username = GId, publicity = _, feed=_Feed} ->
            case group_user_type(User, GId) of
                member -> {ok, joined};
                req -> {error, already_sent};
                reqrejected -> {error, request_rejected};
                not_in_group -> add_to_group(User, GId, req, Group#group.owner), {ok, requested};
                _ -> {error, unknown_type}
            end;
        _ -> {error, notfound}
    end.

approve_request(UId, GId, Owner) -> add_to_group(UId, GId, member, Owner).
reject_request(UId, GId, Owner) -> add_to_group(UId, GId, reqrejected, Owner).
change_group_user_type(UId, GId, Type) -> nsx_msg:notify(["subscription", "user", UId, "add_to_group"], {GId, UId, Type}).

group_exists(GId) ->
    {R, _} = get_group(GId),
    case R of
        ok -> true;
        _ -> false
    end.

group_publicity(GId) ->
    {_, Group} = get_group(GId),
    case Group of
        notfound ->
            no_such_group;
        _ ->
            Group#group.publicity
    end.

group_members_count(GId) ->
    {_, Group} = get_group(GId),
    case Group of
        notfound ->
            no_such_group;
        _ ->
            Group#group.users_count
    end.

user_has_access(UId, GId) ->
    UType = group_user_type(UId, GId),
    {_, Group} = get_group(GId),
    case Group of
        notfound ->
            false;
        _ ->
            GPublicity = Group#group.publicity,
            case {GPublicity, UType} of
                {public, _} -> true;
                {private, member} -> true;
                _ -> false
            end
    end.
