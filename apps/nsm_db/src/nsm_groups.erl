-module(nsm_groups).

-include("user.hrl").
-include("feed.hrl").
-include_lib("alog/include/alog.hrl").  % need this to run the test

-export([create_group/4,
         create_group/5,
         safe_create_group/5,
         remove_group/1,
         add_to_group/3,
         remove_from_group/2,
         list_group_per_user/2, list_group_per_user/1,  % the old one /2 was for mnesia (uses id), new is for riak.
         list_group_membership/1,
         list_user_in_group/1, list_user_in_group/2, list_user_in_group/3,
         list_user_with_name_in_group/1, list_user_with_name_in_group/2, list_user_with_name_in_group/3,
         list_group_per_user_with_count/2, list_group_per_user_with_count/1, % same here
         list_group_per_user_with_count/4, list_group_per_user_with_count/3, % and here
         get_group/1,
         get_all_groups/0,
         get_group_by_feed_id/1,
         user_inside/2,
         user_access/2,
         find_group/3,
         get_members_count/1,
         get_popular_groups/2,
         get_popular_groups/0,
         get_popular_groups/1,
         get_active_members/1,
         get_active_members/0,
         % web api
         update_group/7,
         join_group/2,
         invite_user/3,
         reject_invite/4,
         change_user_rights/4,
         leave_group/2,
         test/0
         ]).

create_group(UId, GId, Name, Desc) ->
    create_group(UId, GId, Name, Desc, public).

create_group(UId, GId, Name, Desc, Publicity) ->
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
    add_to_group(UId, GId, admin),
    nsx_util_notification:notify([group, init], GId),
    GId.

safe_create_group(UId, GId, Name, Desc, Publicity) ->
    case nsm_db:get(group, Name) of
        {ok, _}            -> {error, already_exists};
        {error, not_found} -> create_group(UId, GId, Name, Desc, Publicity)
    end.

add_to_group(UId, GId, Type) ->
    nsm_users:subscribe_user_mq(group, UId, GId),
    nsm_db:add_to_group(UId, GId, Type).

remove_from_group(UId, GId) ->
    nsm_users:remove_subscription_mq(group, UId, GId),
    nsm_db:remove_from_group(UId, GId).


% remove group, feed, records and comments, group_member, group_member_rev
remove_group(GId) ->
    %% send notification to worker, to stop
    nsx_util_notification:notify([feed, delete, GId], empty),

    Group = get_group(GId),

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

    nsm_db:delete(feed, Group#group.feed),   % NB! group.feed is a number

    nsm_db:delete(group, GId).


list_group_per_user(User) ->
    list_group_per_user(User, undefined).

list_group_per_user(#user{username = UId}, ReqUser) -> % the second argument is currently unused #group_member.id
    list_group_per_user(UId, ReqUser);
list_group_per_user(UId, ReqUser) when is_list(UId) ->
    [ G ||
        G <- nsm_db:list_membership(UId),
        GId <- [ G#group_member.group ],
        _Gr <- case get_group(GId) of
            #group{publicity = public}=GG -> [ GG ];
            #group{publicity = moderated}=GG -> [ GG ];
            #group{publicity = private}=GG ->
                case ReqUser of
                    undefined -> [];
                    _ -> case user_inside(GId, ReqUser) of
                            true -> [ GG ];
                            false -> []
                        end
                end;
            _ -> []
        end
    ].

%
% quick and dirty.
%
list_group_per_user_with_count(User) ->
    list_group_per_user_with_count(User, undefined).

list_group_per_user_with_count(#user{username = UId}, ReqUser) ->
    list_group_per_user_with_count(UId, ReqUser);
list_group_per_user_with_count(UId, ReqUser) when is_list(UId) ->
    [ {G, length(list_user_in_group(G#group_member.group))}
    || G <- list_group_per_user(UId, ReqUser)
    ].

list_group_per_user_with_count(User, Page, PageAmount) ->
    list_group_per_user_with_count(User, undefined, Page, PageAmount).

list_group_per_user_with_count(#user{username = UId}, ReqUser, Page, PageAmount) ->
    list_group_per_user_with_count(UId, ReqUser, Page, PageAmount);
list_group_per_user_with_count(UId, ReqUser, Page, PageAmount) when is_list(UId) ->
	Offset= case (Page-1)*PageAmount of
				0 -> 1
				;M-> M
			end,
    All = list_group_per_user(UId, ReqUser),
    Sub = lists:sublist(All, Offset, PageAmount),
    [ {G, length(list_user_in_group(G#group_member.group))}
    || G <- Sub
    ].

list_group_membership(GName) ->
    nsm_db:get_group_members(GName).

list_user_in_group(GName) ->
    [ Who || #group_member_rev{ who = Who } <- nsm_db:get_group_members(GName) ].
list_user_with_name_in_group(GName) ->
    [ {Who,WhoName} || #group_member_rev{ who = Who, who_name=WhoName } <- nsm_db:get_group_members(GName) ].

list_user_in_group(GName,PageNumber,PageAmount) ->
	All = list_user_in_group(GName),
	Offset = case (PageNumber-1)*PageAmount of
				 I when is_integer(I), I>0 -> I
				 ;_                        -> 1
			 end,
	lists:sublist(All, Offset, PageAmount).
list_user_with_name_in_group(GName,PageNumber,PageAmount) ->
	All = list_user_with_name_in_group(GName),
	Offset = case (PageNumber-1)*PageAmount of
				 I when is_integer(I), I>0 -> I
				 ;_                        -> 1
			 end,
	lists:sublist(All, Offset, PageAmount).

list_user_with_name_in_group(GName,"") ->
    list_user_with_name_in_group(GName);
list_user_with_name_in_group(GName,SearchQuery) ->
    All = list_user_with_name_in_group(GName),
    SQ = string:to_lower(SearchQuery),
    F = fun({Who,WhoName}) ->
            case string:str(string:to_lower(Who), SQ) of
                0 -> case string:str(string:to_lower(lists:flatten(WhoName)), SQ) of
                        0 -> false;
                        _ -> true
                    end;
                _ -> true
            end
    end,
    lists:filter(F, All).

list_user_in_group(GName,"") ->
    list_user_in_group(GName);
list_user_in_group(GName,SearchQuery) ->
    Res = list_user_with_name_in_group(GName,SearchQuery),
    [ Who || {Who,_Whom} <- Res ].

%%FIX: should return {ok, Group} or error or {error, Details}
get_group(GId) ->
    case nsm_db:get(group, GId) of
        {ok, Group} ->
            Group;
        {error, not_found} ->
            #group{}
    end.

user_inside(GId, UId) ->
    check_rights(GId, UId, member).

user_access(GId, UId) ->
    case nsm_db:membership(UId, GId) of
        {error, not_found} -> nothing; % Absent user have no rights
        {ok, #group_member{type=Type}} -> Type
    end.

get_all_groups() ->
    nsm_db:all(group).

find_group(Pattern, Page, PageAmount) ->
    Offset= case (Page-1)*PageAmount of
        0 -> 1
        ;M-> M
    end,
    F = fun(#group{username=Username, name=Name, description=Desc}) ->
        P = string:to_lower(Pattern),
        case string:str(Username, P) of
            0 ->
                case string:str(string:to_lower(Name), P) of
                    0 -> case string:str(string:to_lower(Desc), P) of
                            0 -> false;
                            _ -> true
                         end;
                    _ -> true
                end;
            _ -> true
        end
    end,
    nsm_db:select(group, [{where, F},
            {order, {1, descending}},{limit, {Offset,PageAmount}}]).

get_members_count(GName) ->
    nsm_db:get_group_members_count(GName).

%
% ToDo
% for efficient this function we need separate table/bucket "group_info" #{groupInfo, group_ID, members_count .....}
% i know only this way to make this function fast
% for now - slow prototype
%
get_popular_groups(_User, NumberOfGroups) ->
    GroupMembers = nsm_db:all(group_member_rev),
    GroupNumberPairs = [{GId, length(Users)} || #group_member_rev{group=GId, who=Users} <- GroupMembers],
    lists:sublist(lists:sort(fun({_, A}, {_, B}) -> A > B end, GroupNumberPairs), NumberOfGroups).

get_group_by_feed_id(FeedID) ->
    nsm_db:select(group, fun(#group{feed=F}) when FeedID=:=F->true;(_)->false end).

get_popular_groups()      ->  get_popular_groups(10).
get_popular_groups(Count) ->  get_popular_groups("Lalala", Count).

%
% ToDo
% also we need separate table/bucket "users_activity_info" #users_activity_info{totalPosts,period,user_id .... }
%
% now slow prototype
%
get_active_members()      -> get_active_members(10).
get_active_members(Count) ->
    CurrentTime = now(),
    Users = [U || #entry{from=U} <- nsm_db:select(entry,
        fun(#entry{created_time=CT, type = ET}) when ET=:={user, normal};ET=:={user, group}->
            case timer:now_diff(CurrentTime, CT) of
                M when M < 2592000000000 -> true  % 30 days in microseconds
                ;_                       -> false
            end
           ;(_)->false end)],
    Dict = lists:foldl(fun(U, A)->dict:update(U, fun(V) -> V+1 end, 1, A) end, dict:new(), Users),
    lists:sublist(lists:sort(fun({_,N1},{_,N2}) when N1 > N2-> true;(_,_)->false end, dict:to_list(Dict)), Count).

%%% ====================================
%%% web api
%%% ====================================

%% check that User have rights in Group at least MinRight
check_rights(Group, User, MinRight) ->
    check_rights(user_access(Group, User), MinRight).

check_rights(admin, _) -> true;
check_rights(moder, admin) -> false;
check_rights(moder, _) -> true;
check_rights(member, admin) -> false;
check_rights(member, moder) -> false;
check_rights(member, member) -> true;
check_rights(invsent, invsent) -> true;
check_rights(invreq, invsent) -> req;
check_rights(_, _) -> false.

coalesce(undefined, B) -> B;
coalesce(A, _) -> A.
%% change group settings, if user have permissions to
update_group(GroupId, User, Username, Name, Description, Owner, Publicity) ->
    case catch check_rights(GroupId, User, admin) of
        true ->
            %% Saintize input to be sure we don't overwrite any other group
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
                    ok = nsm_db:add_to_group(User, GroupId, member),
                    ok = nsm_db:add_to_group(Owner, GroupId, admin)
            end,
            ok;
        _ ->
            {error, permission}
    end.

%% join if group public, or send join request to group
join_group(GId, User) ->
    case get_group(GId) of
        #group{username = GId, publicity = public} ->
            % Join to this group
            add_to_group(User, GId, member),
            {ok, joined};
        #group{username = GId, publicity = _, feed=Feed} ->
            case check_rights(GId, User, invsent) of
                true ->
                    % Invite was sent to user -- join
                    add_to_group(User, GId, member),
                    {ok, joined};
                req ->
                    % Request was already sent in past
                    {error, already_sent};
                false ->
                    feed:add_direct_message(Feed, User, "Please let me join your group!"),
                    add_to_group(User, GId, invreq),
                    {ok, requested}
            end;
        _ -> {error, not_found}
    end.

%% send to user Invited invite to join into Group
invite_user(GId, User, Invited) ->
    case check_rights(GId, Invited, invsent) of
        req -> % User requested in past; just join him
            add_to_group(Invited, GId, member);
        true -> % Invite already sent in past
            {error, already_sent};
        false ->
            case nsm_users:get_user(Invited) of
                {ok, #user{feed=Feed}} ->
                    feed:add_direct_message(Feed, User, "Let's join us in group!"),
                    add_to_group(Invited, GId, invsent);
                _ ->
                    {error, user_not_found}
            end
    end.

%% User rejects invite to Group for user Invited by Reason.
reject_invite(GId, User, Invited, Reason) ->
    case check_rights(GId, Invited, invsent) of
        req -> % User requested, reject
            case nsm_users:get_user(Invited) of
                {ok, #user{feed=Feed}} ->
                    feed:add_direct_message(Feed, User, "Invite rejected: " ++ Reason),
                    add_to_group(Invited, GId, rejected);
                _ ->
                    {error, user_not_found}
            end;
        true -> % Invite was sent to user -- just remove invite
            add_to_group(Invited, GId, rejected);
        false ->
            {error, no_request}
    end.

%% Change rights of ChangeUser in Group to NewRights
% Admin can change anybody to any level
% Anyone can lower their level to user. (e.g.: moderator=>member)
change_user_rights(GId, User, ChangeUser, NewRights) ->
    R = case NewRights of
        "admin" -> admin;
        "moder" -> moder;
        "member" -> member;
        _ -> throw({error, bad_right})
    end,
    case check_rights(GId, User, admin) of
        true -> % User is admin, and can change user rights as he wish
            add_to_group(ChangeUser, GId, R);
        _ ->
            case {User, NewRights} of
                {ChangeUser, member} ->
                    case check_rights(GId, User, member) of
                        true ->
                            add_to_group(ChangeUser, GId, R);
                        false ->
                            {error, no_access}
                    end;
                _ ->
                    {error, no_access}
            end
    end.

%% Leave group.
% If user was only group admin -- let anyone other to be admin
% If user was last user -- remove group also
slice_members(Members,SkipUser,ReqType) ->
    lists:filter(fun(#group_member_rev{type=Type,who=Who}) -> Type==ReqType andalso Who /= SkipUser end, Members).

leave_group(GId, User) ->
    case check_rights(GId, User, admin) of
        true -> % user is admin, check is it owner
            {ok, #group{}=Group} = nsm_db:get(group, GId),
            case Group#group.owner of
                User -> % User is owner, transfer ownership to someone else
                    Members = nsm_db:get_group_members(GId),
                    Sorted = slice_members(Members, User, admin) ++ slice_members(Members, User, moder) ++ slice_members(Members, User, member),
                    case Sorted of
                        [ #group_member_rev{who=Who} | _ ] ->
                            NewOwner = Who,
                            ok = nsm_db:add_to_group(NewOwner, GId, admin),
                            ok = nsm_db:put(Group#group{owner = NewOwner}),
                            remove_from_group(User, GId);
                        [] ->
                            % Nobody left in group, remove group at all
                            remove_group(GId)
                    end;
                _ -> % Plain user removes -- just remove it
                    remove_from_group(User, GId)
            end;
        false -> % user is just someone, remove it
            remove_from_group(User, GId)
    end.



test() ->
    MrTesto = "demo1",
    MrGroupcreator = "demo2",
    ?INFO("~n~n  No testgroup: ~p", [nsm_db:all(group)]),
    ?INFO("~n~n  MrTesto membership ~p", [nsm_db:list_membership(MrTesto)]),

    Testgroup = create_group(MrGroupcreator, "Testgroup", "Testgroup full name", "Well, for testing"),

    ?INFO("~n~n  Testgroup: ~p", [nsm_db:all(group)]),

    add_to_group(MrTesto, Testgroup, member),
    ?INFO("~n~n  MrTesto added to Testgroup: ~p", [nsm_db:list_membership(MrTesto)]),

    ?INFO("~n~n  MrTesto is now in (~p) groups", [list_group_per_user(MrTesto)]),

    remove_from_group(MrTesto, Testgroup),
    ?INFO("~n~n  MrTesto removed from Testgroup: ~p", [nsm_db:list_membership(MrTesto)]),

    add_to_group(MrTesto, Testgroup, member),
    ?INFO("~n~n  Now lets invite him back: ~p", [nsm_db:list_membership(MrTesto)]),

    remove_group(Testgroup),
    ?INFO("~n~n  And remove group completely: ~p", [nsm_db:list_membership(MrTesto)]).


