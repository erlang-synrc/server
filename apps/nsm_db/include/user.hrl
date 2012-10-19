-include("types.hrl").

-record(user,
        {username :: username_type() | '_', %% Dialyzer and record MatchSpec warnings http://j.mp/vZ8670
         password,
         facebook_id,
         email,
         avatar,

         name = undefined,
         surname = undefined,
         age,
         sex,
         location,
         education,
         register_date,

         status = 'not_verified' :: user_state() | '_',
         verification_code :: string() | '_',

         type,
         feed,
         direct,
         starred,
         pinned,
         comments,
         discussions,
         team,
         aclver}).

-record(user_status,
    {username :: username_type(),
        last_login,
        show_splash = true :: boolean()
    }).

-record(user_info,
        {username :: username_type(),
         name,
         surname,
         age,
         avatar_url,
         sex,
         skill = 0 :: integer(),
         score = 0 :: integer()}).

-record(user_counter,
    {username :: username_type(),
        point = 150}).

-record(user_type,
        {id,
         aclver}).

-record(subs,
        {who,
         whom}).

-record(subscription, %% Obsoleted by #subs{}
        {who,
         whom,
         whom_name}).

-record(subscription_rev, %% Obsoleted by #subs{}
        {whom,
         who,
         who_name}).


-record(group,
        {username,  % this is an id, has nothing to do with users or name
         name,
         description,
         publicity,
         creator,
         created,
         owner,
         feed}).

-record(group_member, % this contains a list of group for one user
        {who,
         group,
         group_name,
         id, % unused in riak, left for mnesia
         type = user}).

-record(group_member_rev, % this contains a list of users for a group
        {group,
         who,
         who_name,
         type = user}).

-record(group_subs,
       {user_id,
        group_id,
        user_type}).

-record(forget_password,
        {token :: string(),
         uid   :: string(),
         create :: {integer(), integer(), integer()}}).

-record(prohibited,
        {ip        :: {string(), atom()},
         activity  :: any(),
         time      :: {integer(),integer(),integer()},
         uid = undefined :: 'undefined' | string()}).

-record(avatar,
	{big :: string(),
	 small :: string(),
	 tiny :: string()}).

-record(user_game_status,
    {user,
	 status  %% strings: online|offline|busy|free_for_game|invisible
	}).

-record(user_ignores, {who, whom}).
-record(user_ignores_rev, {whom, who}).

-record(user_bought_gifts, {
        username,
        timestamp,
        gift_id
    }).

% users activity top
-define(ACTIVE_USERS_TOP_N, 12).

-record(active_users_top, {
        no,
        user_id,
        entries_count,
        last_one_timestamp    
    }).

%% Message queues stuff related to user
%% User exchange name
-define(USER_EXCHANGE(UserId),
        list_to_binary("user_exchange."++UserId++".fanout")).
%% Group exchange name
-define(GROUP_EXCHANGE(GroupId),
        list_to_binary("group_exchange."++GroupId++".fanout")).
