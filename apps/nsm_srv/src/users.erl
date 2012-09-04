-module(users).

-include("user.hrl").
-include("config.hrl").
-include("accounts.hrl").
-include_lib("nsm_mq/include/nsm_mq.hrl").
-include_lib("alog/include/alog.hrl").

-export([subscribe_user/2,
         remove_subscribe/2,
         list_subscription_users/1,
         list_subscription/1,
         list_subscription_me/1,
         list_subscription/2,
         list_subscription/3,
         get_subscription_count/1,
         search_user/1,
         get_user/1,
         update_after_login/1,
         user_status/1,
         user_status/2,
         user_status/3,
         get_user_point/1,
         set_user_point/2,
         get_all_users/0,
         check_register_data/1,
         register/1,
         delete_user/1,
         get_user_by_feed_id/1,
         if_exist/1,
         get_user_game_status/1,
         set_user_game_status/2,
         block_user/2,
         unblock_user/2,
         get_blocked_users/1,
         get_blocked_users_feed_id/1,
         is_user_subscribed/2,
         is_user_blocked/2,
         update_user/1,
         login_posthook/1,
         init_mq/2, subscribe_user_mq/3, remove_subscription_mq/3,
         %% for use from zealot_db_update
         bind_user_exchange/3, unbind_user_exchange/3
         ]).


if_exist(UId) ->
    case zealot_db:get(user, UId) of
        {ok, _User} ->
            true;
        {error, _} ->
            false
    end.

-spec check_register_data(record(user)) -> ok | {error, any()}.
%% @doc Checking that user have only numbers, letters, underscore in
%% username, non empty password, age 18 or more, valid e-mail.
check_register_data(RegisterData) ->
    case (catch check_register_data_(RegisterData)) of
	ok -> ok;
	{error, Error} -> {error, Error};
	Error -> {error, Error}
    end.

check_register_data_(#user{username = UserName,
			   password = Password,
			   email = Email} = RegisterData) ->
    case length(UserName) >= 3 of
	true -> ok;
	_ -> throw({error, username_too_short})
    end,
    case re:run(UserName, "^([A-Za-z0-9_]*)$",[]) of %%"
	{match, _} -> ok;
	nomatch -> throw({error, wrong_username})
    end,
    case length(Password) >= 6 of
	true -> ok;
	_ -> throw({error, password_to_short})
    end,
    case rpc:call(?WEBSERVER_NODE,validator_is_email,validate,["", Email]) of
	true -> ok;
	_ -> throw({error, wrong_email})
    end,
    case table_manager:get_user_age(RegisterData) of
	Age when is_number(Age) andalso Age >= 18 ->
	    ok;
	_ -> throw({error, user_too_young})
    end.

-spec register(record(user)) -> {ok, register} | {error, any()}.
register(#user{username=U, email=Email, facebook_id=FBId} = RegisterData0) ->
    FindUser =
	case get_user(U) of
	    {error, _NotFound} ->
		case get_user({email, Email}) of
		    {error, _NotFound} ->
			case FBId of
			    undefined -> ok; %% no one took this username or e-mail
			    _ ->
				case get_user({facebook, FBId}) of
				    {error, _NotFound} -> ok;
				    {ok, _} -> {error, facebook_taken}
				end
			end;
		    {ok, _} -> {error, email_taken}
		end;
	    {ok, _} ->
		{error, username_taken}
	end,

    % have to check groups for this name now
    % also groups:get_group/1 doesn't work as it should, so have to do check like this
    FindUser2 = case FindUser of
    	ok ->
            AllGroups = [G#group.username || G <- groups:get_all_groups()],
            case lists:member(U, AllGroups) of
                true ->
                    {error, username_taken};
                false ->
                    ok
            end;
        SomethingElse ->
            SomethingElse
    end,
            

    case FindUser2 of
	ok ->
	    case check_register_data(RegisterData0) of
		ok ->
            PlainPassword  = RegisterData0#user.password,
            HashedPassword = utils:sha(PlainPassword),

            RegisterData = RegisterData0#user{feed     = feed:create(),
                                              direct   = feed:create(),
                                              pinned   = feed:create(),
                                              starred  = feed:create(),
                                              password = HashedPassword},
		    ok = zealot_db:put(RegisterData),
			nsm_srv_accounts:create_account(U),
			%% assign quota
			nsm_srv_accounts:transaction(U, ?CURRENCY_QUOTA,
                                         app_opt:get_default_quota(),
                                         #ti_default_assignment{}),
            %% init message queues infrastructure
            init_mq(U, ["kakaranet", "yeniler"]),

			groups:add_to_group(U, "kakaranet", member),
			groups:add_to_group(U, "yeniler", member),

            login_posthook(U),

			{ok, register};
		{error, Error} ->
		    {error, Error}
	    end;
	{error, username_taken} ->
	    {error, user_exist};
	{error, facebook_taken} ->
	    {error, facebook_taken};
	{error, email_taken} ->
	    {error, email_taken}
    end.

% @doc
% Removes user with all tigth connected entries relying on user_id
delete_user(UserName) ->
   % get User record
   case get_user(UserName) of
	{ok, User} ->
	   %% remove from all groups
	   G = groups:list_group_per_user(User),
	   G2R = [ {UId, GId} || #group_member{who = UId, group = GId} <- G],
	   [ groups:remove_from_group(UId, GId) || {UId, GId} <- G2R],
	   %% remove from subcribtions
	   S = list_subscription(User),
	   F2U = [ {MeId, FrId} || #subscription{who = MeId, whom = FrId} <- S ],
	   [ remove_subscribe(MeId, FrId) || {MeId, FrId} <- F2U ],
	   [ remove_subscribe(FrId, MeId) || {MeId, FrId} <- F2U ],
	   %% remove save_game_table
	   zealot_db:delete(save_game_table, UserName),
	   zealot_db:delete(user_counter, UserName),
	   zealot_db:delete(user_status, UserName),
	   %% TODO: delete feed? or not?
	   %% delete user
	   zealot_db:delete(User),
	   {ok, User};
	E -> E
   end.


get_user({username, UserName}) ->
    zealot_db:user_by_username(UserName);
get_user({facebook, FBId}) ->
    zealot_db:user_by_facebook_id(FBId);
get_user({email, Email}) ->
    case rpc:call(?WEBSERVER_NODE,validator_is_email,validate,["", Email]) of
	true -> zealot_db:user_by_email(Email);
	_ -> {error, bad_email}
    end;
get_user(UId) ->
    zealot_db:get(user, UId).

get_all_users() ->
    zealot_db:all(user).

subscribe_user(MeId, FrId) ->
    case is_user_blocked(MeId, FrId) of
        false ->
            zealot_db:subscribe_user(MeId, FrId),
            nsx_util_notification:notify_user_subscribe(MeId, FrId),
            subscribe_user_mq(user, MeId, FrId);
        true ->
            do_nothing
    end.

remove_subscribe(MeId, FrId) ->
    case is_user_subscribed(MeId, FrId) of
        true ->
            zealot_db:remove_subscription(MeId, FrId),
            nsx_util_notification:notify_user_unsubscribe(MeId, FrId),
            remove_subscription_mq(user, MeId, FrId);
        false ->
            do_nothing
    end.

list_subscription(#user{username = UId}) ->
    list_subscription(UId);
list_subscription(UId) when is_list(UId) -> zealot_db:list_subscriptions(UId).
list_subscription_me(UId) -> zealot_db:list_subscription_me(UId).

list_subscription(_UId, _Limit) -> ok.

list_subscription(#user{username = UId}, PageNumber, PageAmount) -> list_subscription(UId, PageNumber, PageAmount);
list_subscription(UId, PageNumber, PageAmount) when is_list(UId) ->
	All = zealot_db:list_subscriptions(UId),
	Offset = case (PageNumber-1)*PageAmount of
				 I when is_integer(I), I>0 -> I+1
				 ;_                        -> 1
			 end,

	lists:sublist(All, Offset, PageAmount).

%PHASE1 to fix paging bug. Actually this shoud come together with list_subscription, but for now it is ok to get this thing separated
get_subscription_count(UId) ->
	All = zealot_db:list_subscriptions(UId),
    length(All).


list_subscription_users(#user{username = UId}) -> list_subscription_users(UId);
list_subscription_users(Uid) ->
    [ {Who,WhoName} || #subscription{whom = Who, whom_name = WhoName} <- list_subscription(Uid) ].

update_after_login(User) ->
    Update =
        case users:user_status(User) of
            {error, status_info_not_found} ->
                #user_status{username = User,
                             last_login = erlang:now()};
            {ok, UserStatus} ->
                UserStatus#user_status{last_login = erlang:now()}
        end,
    zealot_db:put(Update),

    case zealot_db:get(user_counter, User) of
        {error, not_found} ->
            UC = #user_counter{username = User},
            zealot_db:put(UC);
        _ ->
            ok
    end.

user_status(User) ->
    case zealot_db:get(user_status, User) of
        {ok, Status} ->
            {ok, Status};
        {error, notfound} ->
            {error, status_info_not_found}
    end.


user_status(User, Key) ->
    case user_status(User) of
        {ok, Status0} ->
            Fields = record_info(fields, user_status),
            [user_status | List] = tuple_to_list(Status0),
            Status = lists:zip(Fields, List),
            {_, V} = lists:keyfind(Key, 1, Status),
            {ok, V};
        _ ->
            {error, status_info_not_found}
    end.

user_status(User, Key, Value) ->
    case user_status(User) of
        {ok, Status0} ->
            Fields = record_info(fields, user_status),
            [user_status | List] = tuple_to_list(Status0),
            Status = lists:zip(Fields, List),
            NewStatus0 = lists:keyreplace(Key, 1, Status, {Key, Value}),
            NewStatus = [user_status | element(2, lists:unzip(NewStatus0))],
            zealot_db:put(list_to_tuple(NewStatus));
        _ ->
            {error, status_info_not_found}
    end.

get_user_point(User) ->
    {ok, #user_counter{point = Point}} = zealot_db:get(user_counter, User),
    Point.

set_user_point(User, Point) ->
    R = #user_counter{username = User, point = Point},
    zealot_db:put(R).

get_user_by_feed_id(Fid) ->
    zealot_db:select(user, fun(#user{feed=F}) when F=:=Fid-> true;(_)->false end).

search_user("") ->
	zealot_db:all(user);
search_user(Str) ->
    zealot_db:select(user,
        fun(#user{email=E}) when E=:=Str-> true;
        (#user{username=N}) when N=:=Str-> true;
        (#user{name=N})     when N=:=Str-> true;
        (#user{surname=S})  when S=:=Str-> true;
        (#user{surname=S, name=N})      ->
            case S ++ " " ++ N of
                Sum when Sum=:=Str -> true;
                _                  -> false
            end;
        (_)                             ->false
        end).

get_user_game_status(User) ->
    case zealot_db:get(user_game_status, User) of
        {ok, #user_game_status{status=Status}} -> Status
        ;_                                     -> "offline"
    end.

set_user_game_status(User, Status) -> zealot_db:put(#user_game_status{user=User, status=Status}).


block_user(Who, Whom) ->
    ?INFO("~w:block_user/2 Who=~9999p Whom=~9999p", [?MODULE, Who, Whom]),
    remove_subscription_mq_if_subscribed(Who, Whom),
    zealot_db:block_user(Who, Whom),
    nsx_util_notification:notify_user_block(Who, Whom).

unblock_user(Who, Whom) ->
    ?INFO("~w:unblock_user/2 Who=~9999p Whom=~9999p", [?MODULE, Who, Whom]),
    zealot_db:unblock_user(Who, Whom),
    nsx_util_notification:notify_user_unblock(Who, Whom).

remove_subscription_mq_if_subscribed(Who, Whom) ->
    case is_user_subscribed(Who, Whom) of
        true ->
            remove_subscribe(Who, Whom);
        false ->
            do_nothing
    end.


get_blocked_users(UserId) ->
    zealot_db:list_blocks(UserId).

get_blocked_users_feed_id(UserId) ->
    UsersId = zealot_db:list_blocks(UserId),
    Users = zealot_db:select(user, fun(#user{username=U})-> lists:member(U, UsersId) end),
    {UsersId, [Fid || #user{feed=Fid} <- Users]}.

is_user_subscribed(Who, Whom) ->
    zealot_db:is_user_subscribed(Who,Whom).

is_user_blocked(Who, Whom) ->
    zealot_db:is_user_blocked(Who,Whom).

update_user(#user{username=UId,name=Name,surname=Surname} = NewUser) ->
    OldUser = case zealot_db:get(user,UId) of
        {error,notfound} -> NewUser;
        {ok,#user{}=User} -> User
    end,
    zealot_db:put(NewUser),
    case Name==OldUser#user.name andalso Surname==OldUser#user.surname of
        true -> ok;
        false -> zealot_db:update_user_name(UId,Name,Surname)
    end.

%% This function will be called from zealot_auth, after successfull login.
login_posthook(User) ->
    %% send notification about user initialization.
    nsx_util_notification:notify([user, init], User).

subscribe_user_mq(Type, MeId, ToId) ->
    process_subscription_mq(Type, add, MeId, ToId).

remove_subscription_mq(Type, MeId, ToId) ->
    process_subscription_mq(Type, delete, MeId, ToId).

process_subscription_mq(Type, Action, MeId, ToId) ->
    %% FIXME: perform this actions with anonymous common channel
    {ok, Channel} = nsm_mq:open([]),
    %% bind MeId exchange to messages from FrId
    Routes = case Type of
                 user ->
                     rk_user_feed(ToId);
                 group ->
                     rk_group_feed(ToId)
             end,
    case Action of
        add ->
            bind_user_exchange(Channel, MeId, Routes);
        delete ->
            unbind_user_exchange(Channel, MeId, Routes)
    end,
    nsm_mq_channel:close(Channel),
    ok.

%% create needed part for Rabbit
init_mq(User, Groups) ->
    %% create user's exchange to have all subsribtions in one place,
    %% Then if user will open pages or run comet processes on different servers
    %% we just will create queue to consume from this exchange without any
    %% additional db requests.
    ?INFO("~w init mq. Groups: ~p", [User, Groups]),

    UserExchange = ?USER_EXCHANGE(User),
    %% we need fanout exchange to give all information to all users queues
    ExchangeOptions = [{type, <<"fanout">>},
                       durable,
                       {auto_delete, false}],
    {ok, Channel} = nsm_mq:open([]),
    ok = nsm_mq_channel:create_exchange(Channel, UserExchange,
                                        ExchangeOptions),
    %% build routing keys for user's relations
    Relations = build_user_relations(User, Groups),

    %% RK = Routing Key. Bind exchange to all user related keys.
    [bind_user_exchange(Channel, User, RK)
       || RK <- [rk([feed, delete, User])|Relations]],

    nsm_mq_channel:close(Channel),
    ok.

build_user_relations(User, Groups) ->
    %% Feed Keys. Subscribe for self events, system and groups events
    %% feed.FeedOwnerType.FeedOwnerId.ElementType.ElementId.Action
    %% feed.system.ElementType.Action
    [rk_user_feed(User),
     %% system message format: feed.system.ElementType.Action
     rk( [feed, system, '*', '*']) |
     [rk_group_feed(G) || G <- Groups]].

bind_user_exchange(Channel, User, RoutingKey) ->
    %% add routing key tagging to quick find errors
    {bind, RoutingKey, ok} =
        {bind, RoutingKey,
         nsm_mq_channel:bind_exchange(Channel, ?USER_EXCHANGE(User),
                                      ?NOTIFICATIONS_EX, RoutingKey)}.

unbind_user_exchange(Channel, User, RoutingKey) ->
    %% add routing key tagging to quick find errors
    {unbind, RoutingKey, ok} =
        {unbind, RoutingKey,
         nsm_mq_channel:unbind_exchange(Channel, ?USER_EXCHANGE(User),
                                        ?NOTIFICATIONS_EX, RoutingKey)}.

rk(List) ->
    nsm_mq_lib:list_to_key(List).

rk_user_feed(User) ->
    rk([feed, user, User, '*', '*', '*']).

rk_group_feed(Group) ->
    rk([feed, group, Group, '*', '*', '*']).
