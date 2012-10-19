-module(nsm_groups).

-include("user.hrl").
-include("feed.hrl").
-include_lib("nsx_config/include/log.hrl").  % need this to run the test

-export([
         create_group_directly_to_db/5,
         add_to_group_directly_to_db/3,
%         list_group_per_user/2, list_group_per_user/1,  % the old one /2 was for mnesia (uses id), new is for riak.
         list_groups_per_user/1,
         list_group_members/1,
%         list_group_membership/1,
%         list_user_in_group/1, list_user_in_group/2, list_user_in_group/3,
%         list_user_with_name_in_group/1, list_user_with_name_in_group/2, list_user_with_name_in_group/3,
%         list_group_per_user_with_count/2, list_group_per_user_with_count/1, % same here
%         list_group_per_user_with_count/4, list_group_per_user_with_count/3, % and here
         get_group/1,
         get_all_groups/0,
%         get_group_by_feed_id/1,
%         user_inside/2,
         user_in_group/2,
         group_user_type/2,
%         user_access/2,
%         find_group/3,
%         get_members_count/1,
%         get_popular_groups/2,
         get_popular_groups/0,
%         get_popular_groups/1,
         % web api
         join_group/2,
         change_group_user_type/3
%         change_user_rights/4,
%         check_rights/3
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
    nsx_util_notification:notify(["subscription", "user", UId, "add_to_group"], {GId, Type}).

add_to_group_directly_to_db(UId, GId, Type) ->
    nsm_db:put(#group_subs{user_id=UId, group_id=GId, user_type=Type}).
%    nsm_users:subscribe_user_mq(group, UId, GId),
%    MeShow = case nsm_db:get(user, UId) of
%        {ok, #user{name=MeName,surname=MeSur}} ->
%            io_lib:format("~s ~s", [MeName,MeSur]);
%        _ ->
%            UId
%    end,
%    FrShow = case nsm_db:get(group, GId) of
%        {ok, #group{name=FrName}} -> FrName;
%        _ -> GId
%    end,
%    List = [#group_member{who=UId, group=GId, group_name=FrShow, type=Type} |
%        case nsm_db:get(group_member, UId) of
%        {error,notfound} ->
%            nsm_db:delete(group_member, UId),
%            [];
%        {ok,#group_member{group=Subscriptions}} ->
%            [ Sub || Sub <- Subscriptions, Sub#group_member.group=/=GId ]
%        end],
%    nsm_db:put(#group_member{who=UId, group=List, type=list}),
%    RevList = [#group_member_rev{ group= GId, who=UId, who_name=MeShow, type= Type} |
%        case nsm_db:get(group_member_rev, GId) of
%        {error,notfound} ->
%            nsm_db:delete(group_member_rev, GId),
%            [];
%        {ok,#group_member_rev{who=RevSubscriptions}} ->
%            [ Sub || Sub <- RevSubscriptions, Sub#group_member_rev.who=/=UId ]
%        end],
%    nsm_db:put(#group_member_rev{who=RevList, group=GId, type=list}).


list_groups_per_user(UId) ->
    [GId || #group_subs{group_id=GId} <- nsm_db:all_by_index(group_subs, <<"group_subs_user_id_bin">>, list_to_binary(UId)) ].
%    list_group_per_user(User, undefined).

%list_group_per_user(#user{username = UId}, ReqUser) -> % the second argument is currently unused #group_member.id
%    list_group_per_user(UId, ReqUser);
%list_group_per_user(UId, ReqUser) when is_list(UId) ->
%    [ G ||
%        G <- nsm_db:list_membership(UId),
%        GId <- [ G#group_member.group ],
%        _Gr <- case get_group(GId) of
%            #group{publicity = public}=GG -> [ GG ];
%            #group{publicity = moderated}=GG -> [ GG ];
%            #group{publicity = private}=GG ->
%                case ReqUser of
%                    undefined -> [];
%                    _ -> case user_inside(GId, ReqUser) of
%                            true -> [ GG ];
%                            false -> []
%                        end
%                end;
%            _ -> []
%        end
%    ].

%
% quick and dirty.
%
%list_group_per_user_with_count(User) ->
%    list_group_per_user_with_count(User, undefined).

%list_group_per_user_with_count(#user{username = UId}, ReqUser) ->
%    list_group_per_user_with_count(UId, ReqUser);
%list_group_per_user_with_count(UId, ReqUser) when is_list(UId) ->
%    [ {G, length(list_user_in_group(G#group_member.group))}
%    || G <- list_group_per_user(UId, ReqUser)
%    ].

%list_group_per_user_with_count(User, Page, PageAmount) ->
%    list_group_per_user_with_count(User, undefined, Page, PageAmount).

%list_group_per_user_with_count(#user{username = UId}, ReqUser, Page, PageAmount) ->
%    list_group_per_user_with_count(UId, ReqUser, Page, PageAmount);
%list_group_per_user_with_count(UId, ReqUser, Page, PageAmount) when is_list(UId) ->
%    Offset= case (Page-1)*PageAmount of
%				0 -> 1
%				;M-> M
%			end,
%    All = list_group_per_user(UId, ReqUser),
%    Sub = lists:sublist(All, Offset, PageAmount),
%    [ {G, length(list_user_in_group(G#group_member.group))}
%    || G <- Sub
%    ].

%list_group_membership(GName) ->
%    nsm_db:get_group_members(GName).

list_group_members(GId) ->
    [UId || #group_subs{user_id=UId} <- nsm_db:all_by_index(group_subs, <<"group_subs_group_id_bin">>, list_to_binary(GId)) ].

%list_user_in_group(GName) ->
%    [ Who || #group_member_rev{ who = Who } <- nsm_db:get_group_members(GName) ].
%list_user_with_name_in_group(GName) ->
%    [ {Who,WhoName} || #group_member_rev{ who = Who, who_name=WhoName } <- nsm_db:get_group_members(GName) ].

%list_user_in_group(GName,PageNumber,PageAmount) ->
%	All = list_user_in_group(GName),
%	Offset = case (PageNumber-1)*PageAmount of
%				 I when is_integer(I), I>0 -> I
%				 ;_                        -> 1
%			 end,
%	lists:sublist(All, Offset, PageAmount).
%list_user_with_name_in_group(GName,PageNumber,PageAmount) ->
%	All = list_user_with_name_in_group(GName),
%	Offset = case (PageNumber-1)*PageAmount of
%				 I when is_integer(I), I>0 -> I
%				 ;_                        -> 1
%			 end,
%	lists:sublist(All, Offset, PageAmount).

%list_user_with_name_in_group(GName,"") ->
%    list_user_with_name_in_group(GName);
%list_user_with_name_in_group(GName,SearchQuery) ->
%    All = list_user_with_name_in_group(GName),
%    SQ = string:to_lower(SearchQuery),
%    F = fun({Who,WhoName}) ->
%            case string:str(string:to_lower(Who), SQ) of
%                0 -> case string:str(string:to_lower(lists:flatten(WhoName)), SQ) of
%                        0 -> false;
%                        _ -> true
%                    end;
%                _ -> true
%            end
%    end,
%    lists:filter(F, All).

%list_user_in_group(GName,"") ->
%    list_user_in_group(GName);
%list_user_in_group(GName,SearchQuery) ->
%    Res = list_user_with_name_in_group(GName,SearchQuery),
%    [ Who || {Who,_Whom} <- Res ].

%%FIX: should return {ok, Group} or error or {error, Details}
get_group(GId) ->
    nsm_db:get(group, GId).
%    case nsm_db:get(group, GId) of
%        {ok, Group} ->
%            Group;
%       {error, not_found} ->
%            #group{}
%    end.

%user_inside(GId, UId) ->
%    check_rights(GId, UId, member).

user_in_group(UId, GId) ->
    case nsm_db:get(group_subs, {UId, GId}) of
        {error, not_found} -> false;
        _ -> true
    end.

group_user_type(UId, GId) ->
    case nsm_db:get(group_subs, {UId, GId}) of
        {error, not_found} -> not_in_group;
        #group_subs{user_type=Type} -> Type
    end.

%user_access(GId, UId) ->
%    case nsm_db:membership(UId, GId) of
%        {error, not_found} -> nothing; % Absent user have no rights
%        {ok, #group_member{type=Type}} -> Type
%    end.

get_all_groups() ->
    nsm_db:all(group).

%find_group(Pattern, Page, PageAmount) ->
%    Offset= case (Page-1)*PageAmount of
%        0 -> 1
%        ;M-> M
%    end,
%    F = fun(#group{username=Username, name=Name, description=Desc}) ->
%        P = string:to_lower(Pattern),
%        case string:str(Username, P) of
%            0 ->
%                case string:str(string:to_lower(Name), P) of
%                    0 -> case string:str(string:to_lower(Desc), P) of
%                            0 -> false;
%                            _ -> true
%                         end;
%                    _ -> true
%                end;
%            _ -> true
%        end
%    end,
%    nsm_db:select(group, [{where, F},
%            {order, {1, descending}},{limit, {Offset,PageAmount}}]).

%get_members_count(GName) ->
%    nsm_db:get_group_members_count(GName).

%
% ToDo
% for efficient this function we need separate table/bucket "group_info" #{groupInfo, group_ID, members_count .....}
% i know only this way to make this function fast
% for now - slow prototype
%
%get_popular_groups(_User, NumberOfGroups) ->
%    GroupMembers = nsm_db:all(group_member_rev),
%    GroupNumberPairs = [{GId, length(Users)} || #group_member_rev{group=GId, who=Users} <- GroupMembers],
%    lists:sublist(lists:sort(fun({_, A}, {_, B}) -> A > B end, GroupNumberPairs), NumberOfGroups).

%get_group_by_feed_id(FeedID) ->
%    nsm_db:select(group, fun(#group{feed=F}) when FeedID=:=F->true;(_)->false end).

%get_popular_groups()      ->  get_popular_groups(10).
%get_popular_groups(Count) ->  get_popular_groups("Lalala", Count).

get_popular_groups() -> ["kakaranet", "yeniler"].   % :-)

%%% ====================================
%%% web api
%%% ====================================

%% check that User have rights in Group at least MinRight
%check_rights(Group, User, MinRight) ->
%    check_rights(user_access(Group, User), MinRight).

%check_rights(admin, _) -> true;
%check_rights(moder, admin) -> false;
%check_rights(moder, _) -> true;
%check_rights(member, admin) -> false;
%check_rights(member, moder) -> false;
%check_rights(member, member) -> true;
%check_rights(invsent, invsent) -> true;
%check_rights(invreq, invsent) -> req;
%check_rights(_, _) -> false.


%% join if group public, or send join request to group
join_group(GId, User) ->
    case get_group(GId) of
        #group{username = GId, publicity = public} ->
            % Join to this group
            add_to_group(User, GId, member),
            {ok, joined};
        #group{username = GId, publicity = _, feed=Feed} ->
            case group_user_type(User, GId) of
                invsent ->
                    % Invite was sent to user -- join
                    add_to_group(User, GId, member),
                    {ok, joined};
                req ->
                    % Request was already sent in past
                    {error, already_sent};
                not_in_group ->
                    feed:add_direct_message(Feed, User, "Please let me join your group!"),
                    add_to_group(User, GId, invreq),
                    {ok, requested}
            end;
        _ -> {error, not_found}
    end.


%% Change rights of ChangeUser in Group to NewRights
% Admin can change anybody to any level
% Anyone can lower their level to user. (e.g.: moderator=>member)
%change_user_rights(GId, User, ChangeUser, NewRights) ->
%    R = case NewRights of
%        "admin" -> admin;
%        "moder" -> moder;
%        "member" -> member;
%        _ -> throw({error, bad_right})
%    end,
%    case check_rights(GId, User, admin) of
%        true -> % User is admin, and can change user rights as he wish
%            add_to_group(ChangeUser, GId, R);
%        _ ->
%            case {User, NewRights} of
%                {ChangeUser, member} ->
%                    case check_rights(GId, User, member) of
%                        true ->
%                            add_to_group(ChangeUser, GId, R);
%                        false ->
%                            {error, no_access}
%                    end;
%                _ ->
%                    {error, no_access}
%            end
%    end.

change_group_user_type(UId, GId, Type) ->
    nsx_util_notification:notify(["subscription", "user", UId, "add_to_group"], {GId, Type}).


