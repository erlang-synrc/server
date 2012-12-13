-module(nsm_groups).

-include("user.hrl").
-include("feed.hrl").
-include_lib("nsx_config/include/log.hrl").  % need this to run the test

-export([
            create_group_directly_to_db/5,
            add_to_group_directly_to_db/3,
            delete_group_directly_from_db/1,

            list_groups_per_user/1,
            list_group_members/1, % I need to store group user count somewhere!
            list_group_members_with_types/1,

            get_all_groups/0,
            get_popular_groups/0,
            get_group/1,
            group_exists/1,
            group_publicity/1,
            group_members_count/1,

            join_group/2,
            user_in_group/2,
            group_user_type/2,
            change_group_user_type/3,
            user_has_access/2
        ]).

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
    nsm_groups:add_to_group_directly_to_db(UId, GId, admin),
    GId.

add_to_group(UId, GId, Type) -> % for internal use only
    nsx_msg:notify(["subscription", "user", UId, "add_to_group"], {GId, Type}).

add_to_group_directly_to_db(UId, GId, Type) ->
    nsm_db:put(#group_subs{user_id=UId, group_id=GId, user_type=Type}),
    {ok, Group} = nsm_db:get(group, GId),
    GU = Group#group.users_count,
    nsm_db:put(Group#group{users_count = GU+1}).

delete_group_directly_from_db(GId) ->
    [nsm_db:delete(group_subs, {UId, GId}) || UId <- list_group_members(GId)],
    nsm_db:delete(group, GId).

list_groups_per_user(UId) ->
    [GId || #group_subs{group_id=GId} <- nsm_db:all_by_index(group_subs, <<"group_subs_user_id_bin">>, list_to_binary(UId)) ].

list_group_members(GId) ->
    [UId || #group_subs{user_id=UId} <- nsm_db:all_by_index(group_subs, <<"group_subs_group_id_bin">>, list_to_binary(GId)) ].

list_group_members_with_types(GId) ->
    [{UId, UType} || #group_subs{user_id=UId, user_type=UType} <- nsm_db:all_by_index(group_subs, <<"group_subs_group_id_bin">>, list_to_binary(GId)) ].

get_group(GId) ->
    nsm_db:get(group, GId).

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

get_all_groups() ->
    nsm_db:all(group).

get_popular_groups() -> ["kakaranet", "yeniler"].   % :-)


%% join if group public, or send join request to group
join_group(GId, User) ->
    {ok, Group} = get_group(GId),
    case Group of
        #group{username = GId, publicity = public} ->
            % Join to this group
            add_to_group(User, GId, member),
            {ok, joined};
        #group{username = GId, publicity = _, feed=Feed} ->
            case group_user_type(User, GId) of
                invsent -> add_to_group(User, GId, member), {ok, joined};
                admin ->   add_to_group(User, GId, member), {ok, joined};
                member ->  add_to_group(User, GId, member), {ok, joined};
                req ->     {error, already_sent};
                invreq ->  {error, already_sent};
                not_in_group ->  add_to_group(User, GId, invreq), {ok, requested}
            end;
        _ -> {error, notfound}
    end.

change_group_user_type(UId, GId, Type) ->
    nsx_msg:notify(["subscription", "user", UId, "add_to_group"], {GId, Type}).

group_exists(GId) ->
    {_, Group} = get_group(GId),
    case Group of
        notfound -> false;
        _ -> true
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
                {moderated, _} -> true;
                {private, member} -> true;
                _ -> false
            end
    end.


