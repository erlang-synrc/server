-module(nsm_users).
-include("user.hrl").
-include("config.hrl").
-include("accounts.hrl").
-include_lib("nsm_mq/include/nsm_mq.hrl").
-include_lib("nsm_db/include/common.hrl").
-include_lib("nsx_config/include/log.hrl").
-compile(export_all).

if_exist(UId) ->
    case nsm_db:get(user, UId) of
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

get_user_age(#user{age = UAge}) ->
    case UAge of
        {undefined, undefined, undefined} ->
            undefined;
        undefined ->
            undefined;
        {By, Bm, Bd} ->
            {{Cy, Cm, Cd}, _} = calendar:local_time(),
            Age = Cy-By+
                if Cm < Bm -> -1; % if current day before user's birthday
                   Cd < Bd -> -1; % than he/she is yonger by 1 year
                   true -> 0 % -> current day is birthday or since that date
                end,
            Age
    end.

check_register_data_(#user{username = UserName,
			   password = Password,
			   email = Email} = RegisterData) ->
    case length(UserName) >= 3 of
	true -> ok;
	_ -> throw({error, username_too_short})
    end,
    case re:run(UserName, "^([A-Za-z0-9_\\.]*)$",[]) of %%"
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
    case get_user_age(RegisterData) of
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
    FindUser2 = case FindUser of
    	ok ->
            case nsm_groups:get_group(U) of
                {error, notfound} ->
                    ok; % it means username is free
                _ ->
                    {error, username_taken}
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

            RegisterData = RegisterData0#user{feed     = nsm_db:feed_create(),
                                              direct   = nsm_db:feed_create(),
                                              pinned   = nsm_db:feed_create(),
                                              starred  = nsm_db:feed_create(),
                                              password = HashedPassword},
		    %ok = nsm_db:put(RegisterData),
            nsx_msg:notify(["system", "put"], RegisterData),
			nsm_accounts:create_account(U),
			%% assign quota
            {ok, DefaultQuota} = nsm_db:get(config, "accounts/default_quota",  300),
			nsm_accounts:transaction(U, ?CURRENCY_QUOTA,
                                         DefaultQuota,
                                         #ti_default_assignment{}),
            %% init message queues infrastructure
            init_mq(U, []),

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
       GIds = nsm_groups:list_groups_per_user(UserName),
       [nsx_msg:notify(["subscription", "user", UserName, "remove_from_group"], {GId}) || GId <- GIds],
	   %% remove from subcribtions
	   S = list_subscr(User),
	   F2U = [ {MeId, FrId} || #subscription{who = MeId, whom = FrId} <- S ],
	   [ unsubscr_user(MeId, FrId) || {MeId, FrId} <- F2U ],
	   [ unsubscr_user(FrId, MeId) || {MeId, FrId} <- F2U ],
	   %% remove save_game_table
	   nsm_db:delete(save_game_table, UserName),
	   nsm_db:delete(user_counter, UserName),
	   nsm_db:delete(user_status, UserName),
	   %% TODO: delete feed? or not?
	   %% delete user
	   nsm_db:delete(user, UserName),
	   {ok, User};
	E -> E
   end.

get_user({username, UserName}) ->
    nsm_db:user_by_username(UserName);
get_user({facebook, FBId}) ->
    nsm_db:user_by_facebook_id(FBId);
get_user({email, Email}) ->
    case rpc:call(?WEBSERVER_NODE,validator_is_email,validate,["", Email]) of
	true -> nsm_db:user_by_email(Email);
	_ -> {error, bad_email}
    end;
get_user(UId) ->
    nsm_db:get(user, UId).

get_all_users() ->
    nsm_db:all(user).



subscr_user(Who, Whom) ->
    case is_user_blocked(Who, Whom) of
        false ->
            Record = #subs{who = Who, whom = Whom},
            ok = nsm_db:put(Record),
            subscribe_user_mq(user, Who, Whom);
        true -> do_nothing
    end.

unsubscr_user(Who, Whom) ->
    case is_user_subscr(Who, Whom) of
        true ->
            ok = nsm_db:delete(subs, {Who, Whom}),
            remove_subscription_mq(user, Who, Whom);
        false ->
            do_nothing
    end.

list_subscr(#user{username = UId}) ->
    list_subscr(UId);
list_subscr(UId) when is_list(UId) ->
    lists:sort( nsm_db:all_by_index(subs, <<"subs_who_bin">>, list_to_binary(UId)) ).
list_subscr(UId, PageNumber, PageAmount) when is_list(UId) -> 
    Offset = case (PageNumber-1)*PageAmount of
        I when is_integer(I), I>0 -> I+1;
        _ -> 1
	 end,
	lists:sublist(list_subscr(UId), Offset, PageAmount).
list_subscr_for_metalist(UId) ->
    [{UserId, user_realname(UserId)} || {subs, _, UserId} <- list_subscr(UId)].

list_subscr_me(#user{username = UId}) ->
    list_subscr_me(UId);
list_subscr_me(UId) when is_list(UId) ->
    lists:sort( nsm_db:all_by_index(subs, <<"subs_whom_bin">>, list_to_binary(UId)) ).

is_user_subscr(Who, Whom) ->
    lists:member({subs, Who, Whom}, list_subscr(Who)).




update_after_login(User) -> %RPC to cleanup
    Update =
        case user_status(User) of
            {error, status_info_not_found} ->
                #user_status{username = User,
                             last_login = erlang:now()};
            {ok, UserStatus} ->
                UserStatus#user_status{last_login = erlang:now()}
        end,
    nsm_db:put(Update),

    case nsm_db:get(user_counter, User) of
        {error, not_found} ->
            UC = #user_counter{username = User},
            nsm_db:put(UC);
        _ ->
            ok
    end.

user_status(User) ->
    case nsm_db:get(user_status, User) of
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
            nsm_db:put(list_to_tuple(NewStatus));
        _ ->
            {error, status_info_not_found}
    end.

get_user_by_feed_id(Fid) ->
    nsm_db:select(user, fun(#user{feed=F}) when F=:=Fid-> true;(_)->false end).

search_user("") ->
	nsm_db:all(user);
search_user(Str) ->
    nsm_db:select(user,
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
    case nsm_db:get(user_game_status, User) of
        {ok, #user_game_status{status=Status}} -> Status
        ;_                                     -> "offline"
    end.

set_user_game_status(User, Status) -> nsm_db:put(#user_game_status{user=User, status=Status}).

% TODO: game_session:525 move real DB operation from here behind rabbit
%       two level: first message received by session pid
%                   then it goes to change db bg worker

block_user(Who, Whom) ->
    ?INFO("~w:block_user/2 Who=~p Whom=~p", [?MODULE, Who, Whom]),
    unsubscr_user(Who, Whom),
    nsm_db:block_user(Who, Whom),
    nsx_msg:notify_user_block(Who, Whom).

unblock_user(Who, Whom) ->
    ?INFO("~w:unblock_user/2 Who=~p Whom=~p", [?MODULE, Who, Whom]),
    nsm_db:unblock_user(Who, Whom),
    nsx_msg:notify_user_unblock(Who, Whom).

get_blocked_users(UserId) ->
    nsm_db:list_blocks(UserId).

get_blocked_users_feed_id(UserId) ->
    UsersId = nsm_db:list_blocks(UserId),
    Users = nsm_db:select(user, fun(#user{username=U})-> lists:member(U, UsersId) end),
    {UsersId, [Fid || #user{feed=Fid} <- Users]}.


is_user_blocked(Who, Whom) ->
    nsm_db:is_user_blocked(Who,Whom).

update_user(#user{username=UId,name=Name,surname=Surname} = NewUser) ->
    OldUser = case nsm_db:get(user,UId) of
        {error,notfound} -> NewUser;
        {ok,#user{}=User} -> User
    end,
    nsm_db:put(NewUser),
    case Name==OldUser#user.name andalso Surname==OldUser#user.surname of
        true -> ok;
        false -> nsm_db:update_user_name(UId,Name,Surname)
    end.


% gifts
can_buy_gift(UId, GiftId) ->
    {ok, Kakush} = nsm_accounts:balance(UId, ?CURRENCY_KAKUSH),
    {ok, {ThisGift, _}} = nsm_gifts_db:get_gift(GiftId),
    KakushPrice = ThisGift#gift.kakush_point,
    Kakush >= KakushPrice.

buy_gift(UId, GiftId) ->
    {ok, {ThisGift, _}} = nsm_gifts_db:get_gift(GiftId),
    KakushPrice = ThisGift#gift.kakush_point,
    GiftName = ThisGift#gift.gift_name,
    nsm_accounts:transaction(UId, ?CURRENCY_KAKUSH, -KakushPrice, "Buying gift: "++GiftName++" for "++integer_to_list(KakushPrice)++" kakush."),
    nsm_db:put(#user_bought_gifts{username=UId, timestamp=now(), gift_id=GiftId}).

list_gifts_of(UId) ->
    nsm_db:all_by_index(user_bought_gifts, <<"user_bought_gifts_username_bin">>, list_to_binary(UId)).


% active_user_top
calculate_activity(E, Timestamp) ->
    GSnow = calendar:datetime_to_gregorian_seconds( calendar:now_to_datetime(erlang:now()) ),
    GS = calendar:datetime_to_gregorian_seconds( calendar:now_to_datetime(Timestamp) ),
    E/(1.0 + (GSnow-GS)/2592000).

attempt_active_user_top(UId, UEC) ->
    {_, Outsider} = nsm_db:get(active_users_top, ?ACTIVE_USERS_TOP_N),
    case Outsider of
        notfound ->    % Top is not yet filled
            N = length(nsm_db:all(active_users_top)) + 1,
            nsm_db:put(#active_users_top{
                no = N,
                user_id = UId,
                entries_count = UEC,
                last_one_timestamp = erlang:now()
            });
        #active_users_top{entries_count=EC, last_one_timestamp=LOT} ->
            case calculate_activity(EC, LOT) < UEC of
                true -> % Attemting user should be anywhere in top
                    FullTop = lists:filter(fun(#active_users_top{user_id=OUId}) -> UId =/= OUId end, nsm_db:all(active_users_top)) ++ 
                        [#active_users_top{user_id=UId, entries_count=UEC, last_one_timestamp = erlang:now()}],
                    SortedTop = lists:sort(
                        fun(#active_users_top{entries_count=E1, last_one_timestamp=T1},
                            #active_users_top{entries_count=E2, last_one_timestamp=T2}) ->
                            calculate_activity(E1, T1) >= calculate_activity(E2, T2)
                        end, FullTop),
                    ResetTop = [(lists:nth(I, SortedTop))#active_users_top{no=I} || I <- lists:seq(1, ?ACTIVE_USERS_TOP_N)],
                    [nsm_db:put(TopEntry) || TopEntry <- ResetTop];
                false -> % Attempting user don't get to the top
                    ok
            end
    end.

get_active_user_top() ->
    SortedTop = lists:sort(nsm_db:all(active_users_top)),
    [{UId, N} || #active_users_top{no = N, user_id = UId} <- SortedTop].

user_realname(UId) ->
    {ok, User} = get_user(UId),
    Name = if
        is_binary(User#user.name) -> binary_to_list(User#user.name);
        is_atom(User#user.name) -> atom_to_list(User#user.name);
        true -> User#user.name
    end,
    Surname = if
        is_binary(User#user.surname) -> binary_to_list(User#user.surname);
        is_atom(User#user.surname) -> atom_to_list(User#user.surname);
        true -> User#user.surname
    end,
    if
        Name=="undefined", Surname=="undefined" -> UId;
        Name=="undefined" -> Surname;
        Surname=="undefined" -> Name;
        true -> Name ++ [" "] ++ Surname
    end.


%% This function will be called from nsm_auth, after successfull login.
login_posthook(User) ->
    %% send notification about user initialization.
    nsx_msg:notify([user, init], User).

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
    ?INFO("~p init mq. nsm_users: ~p", [User, Groups]),

    UserExchange = ?USER_EXCHANGE(User),
    %% we need fanout exchange to give all information to all users queues
    ExchangeOptions = [{type, <<"fanout">>},
                       durable,
                       {auto_delete, false}],
    {ok, Channel} = nsm_mq:open([]),
    ?INFO("Cration Exchange: ~p,",[{Channel,UserExchange,ExchangeOptions}]),
    ok = nsm_mq_channel:create_exchange(Channel, UserExchange,
                                        ExchangeOptions),
                                          ?INFO("Created OK"),
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
     %% API
     rk( [db, user, User, put] ),
     rk( [subscription, user, User, add_to_group]),
     rk( [subscription, user, User, remove_from_group]),
     rk( [subscription, user, User, invite_to_group]),
     rk( [subscription, user, User, reject_invite_to_group]),
     rk( [subscription, user, User, leave_group]),
     rk( [login, user, User, update_after_login]),
     rk( [likes, user, User, add_like]),

     rk( [feed, user, User, count_entry_in_statistics] ),
     rk( [feed, user, User, count_comment_in_statistics] ),

     rk( [subscription, user, User, subscribe_user]),
     rk( [subscription, user, User, remove_subscribe]),
     rk( [subscription, user, User, set_user_game_status]),
     rk( [subscription, user, User, update_user]),
     rk( [subscription, user, User, block_user]),
     rk( [subscription, user, User, unblock_user]),

     rk( [affiliates, user, User, create_affiliate]),
     rk( [affiliates, user, User, delete_affiliate]),
     rk( [affiliates, user, User, enable_to_look_details]),
     rk( [affiliates, user, User, disable_to_look_details]),

     rk( [purchase, user, User, set_purchase_external_id]),
     rk( [purchase, user, User, set_purchase_state]),
     rk( [purchase, user, User, set_purchase_info]),
     rk( [purchase, user, User, add_purchase]),

     rk( [transaction, user, User, add_transaction]),

     rk( [invite, user, User, add_invite_to_issuer]),

     rk( [tournaments, user, User, create]),
     rk( [tournaments, user, User, create_and_join]),

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
%% same stuff for groups
init_mq_for_group(Group) ->
    GroupExchange = ?GROUP_EXCHANGE(Group),
    ExchangeOptions = [{type, <<"fanout">>},
                       durable,
                       {auto_delete, false}],   
    {ok, Channel} = nsm_mq:open([]),
    ok = nsm_mq_channel:create_exchange(Channel, GroupExchange, ExchangeOptions),
    Relations = build_group_relations(Group),
    [bind_group_exchange(Channel, Group, RK) || RK <- Relations],
    nsm_mq_channel:close(Channel),
    ok.

build_group_relations(Group) ->
    [
        rk( [db, group, Group, put] ),
        rk( [db, group, Group, update_group] ),
        rk( [db, group, Group, remove_group] ),
        rk( [feed, delete, Group] ),
        rk( [feed, group, Group, '*', '*', '*'] )
    ].

bind_group_exchange(Channel, Group, RoutingKey) ->
    %% add routing key tagging to quick find errors
    {bind, RoutingKey, ok} =
        {bind, RoutingKey,
         nsm_mq_channel:bind_exchange(Channel, ?GROUP_EXCHANGE(Group),
                                      ?NOTIFICATIONS_EX, RoutingKey)}.

unbind_group_exchange(Channel, Group, RoutingKey) ->
    %% add routing key tagging to quick find errors
    {unbind, RoutingKey, ok} =
        {unbind, RoutingKey,
         nsm_mq_channel:unbind_exchange(Channel, ?GROUP_EXCHANGE(Group),
                                        ?NOTIFICATIONS_EX, RoutingKey)}.

rk(List) ->
    nsm_mq_lib:list_to_key(List).

rk_user_feed(User) ->
    rk([feed, user, User, '*', '*', '*']).

rk_group_feed(Group) ->
    rk([feed, group, Group, '*', '*', '*']).
