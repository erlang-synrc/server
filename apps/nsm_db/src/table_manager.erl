-module(table_manager).
-compile(export_all).
-include_lib("nsx_config/include/log.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include_lib("nsm_db/include/table.hrl").
-include("common.hrl").
-include("user.hrl").
-include("feed.hrl").
-include("accounts.hrl").

create_table(User, S) ->
    DoublePoints =  get_setting(double_points, S, 1),
    GameType = get_setting(game, S),
    GameMode = get_setting(game_mode, S),
    Rounds   = get_setting(rounds, S),
    {ok, PR, PREx} = pointing_rules:get_rules(GameType, GameMode, Rounds),
    %% quota will be charged from player when game will begin. If Double parameter
    %% is set for table, quota have to be changed too.
    DoubledRules = pointing_rules:double_points(PR, DoublePoints),
    Quota = DoubledRules#pointing_rule.quota,
    ?INFO("Rounds: ~p",[Rounds]),
    ?INFO("Pointing and Double Rules: ~p",[{PR,PREx,DoubledRules}]),
    ?INFO("Quota: ~p",[Quota]),

    QuotaCheck = nsm_accounts:check_quota(User#user.username, Quota),
    ?INFO("QuotaCheck: ~p",[QuotaCheck]),

    case QuotaCheck of
        {error, hard_limit} ->
            %% if user will start the game hard limit will be reached.
            %% Deny game creation
            {error, quota_hard_limit};

        OK when OK == ok; OK == {error, soft_limit} ->
            %% if user will reach only soft limit - give him a chance to play
            NodeAtom = nsx_opt:get_env(nsm_db,game_srv_node,'game@doxtop.cc'),
            TableId = rpc:call(NodeAtom,id_generator,get_id,[]), %nsx_opt:get_env(nsx_idgen,game_pool,1000000) + 500000 + nsm_db:next_id("table"),

            Table = #game_table{id = TableId,
                                name             = get_setting(table_name, S),
                                owner            = User#user.username,
%                                users            = [User#user.username],
                                game_type        = GameType,
                                game_mode        = GameMode,
                                creator          = User#user.username,
                                rounds           = Rounds,
                                sets             = get_setting(sets, S),
                                timestamp        = erlang:now(),
                                game_speed       = get_setting(speed, S),
                                friends_only     = get_setting(friends_only, S, false),
                                invited_users    = get_list_setting(user, S),
                                private          = get_setting(private, S, false),
                                age_limit        = get_setting(age, S),
                                groups_only      = get_list_setting(group, S),
                                gender_limit     = get_setting(sex, S),
                                location_limit   = get_list_setting(location, S),
                                deny_robots      = get_setting(deny_robots, S, false),
                                gosterge_finish  = get_setting(gosterge_finish, S, false),
                                double_points    = DoublePoints,
                                paid_only        = get_setting(paid_only, S),
                                slang            = get_setting(slang, S),
                                deny_observers   = get_setting(deny_observers, S),
                                pointing_rules   = PR,
                                pointing_rules_ex = PREx,
                                robots_replacement_allowed = not get_setting(robots_replacement_disallowed, S, false)
                               },

            {ok, Table}
    end.

game_table_to_settings(#game_table{name           = Name,
                                  game_type      = GameType,
                                  game_mode      = Gamemode,
                                  game_speed     = GameSpeed,
                                  sets           = Sets,
                                  rounds         = Rounds,
                                  owner          = Owner,
                                  users          = Users,
                                  users_options  = UsersOptions,
                                  friends_only   = FriendsOnly,
                                  invited_users  = ForUsers,
                                  private        = Private,
                                  age_limit      = AgeLimit,
                                  groups_only    = GroupOnly,
                                  gender_limit   = Sex,
                                  location_limit = Location,
                                  deny_robots    = DenyRobots,
                                  gosterge_finish= GostergeFinish,
                                  paid_only      = Paid,
                                  slang          = Slang,
                                  deny_observers = DenyObservers,
                                  double_points  = DoublePoints,
                                  pointing_rules = GeneralPointingRule,
                                  pointing_rules_ex = AdditionalPointingRules,
                                  robots_replacement_allowed = RobotsReplacementAllowed
                                                    }) ->
    AllowReplacement = case GameType of
                           game_okey -> true;
                           game_tavla -> true
                       end,
    Setting =
        [{table_name, Name},
         {game, GameType},
         {game_mode, Gamemode},
         {speed, GameSpeed},
         {sets, Sets},
         {rounds, Rounds},

         {owner, Owner},
         {users, Users},
         {users_options, UsersOptions},
         {slang,Slang},
         {deny_observers,DenyObservers},
         {friends_only, FriendsOnly},
         split_to_tuple(user, ForUsers),
         {private, Private},
         {age, AgeLimit},
         split_to_tuple(group, GroupOnly),
         {gender_limit, Sex},
         split_to_tuple(location, Location),
         {deny_robots, DenyRobots},
         {gosterge_finish, GostergeFinish},
         {double_points, DoublePoints},
         {pointing_rules, GeneralPointingRule},
         {pointing_rules_ex, AdditionalPointingRules},
         {paid_only, Paid},
         {allow_replacement, AllowReplacement},
         {robots_replacement_allowed, RobotsReplacementAllowed}],
    lists:flatten(Setting).


get_setting(Key, Setting) -> get_setting(Key, Setting, undefined).
get_setting(Key, Setting, Default) -> proplists:get_value(Key, Setting, Default).
get_list_setting(Key, Setting) -> [ V || {K, V} <- Setting, K == Key].
split_to_tuple(Prefix, Data) -> [ {Prefix, D} || D <- Data ].

