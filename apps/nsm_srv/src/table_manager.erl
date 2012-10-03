%%%-------------------------------------------------------------------
%%% @author Gleb Peregud <gleber@first.lan>
%%% @copyright (C) 2011, Gleb Peregud
%%% @doc
%%%
%%% @end
%%% Created : 17 Apr 2011 by Gleb Peregud <gleber@first.lan>
%%%-------------------------------------------------------------------
-module(table_manager).

-behaviour(gen_server).

%% THIS FILE ARE TO BE DELETED

-export([start_link/0,
         stop/0,
         create_table/2,
         join_table/2,
         join_table/3,
         leave_table/2,
         update_table/1,
         game_started/3,
         get_all_tables/0,
         get_table/1,
         game_requirements/1,
         delete_table/1,
         get_only_game/1,
         filter_tables/1,
         filter_per_user/1,
         create_example_table/0,
         game_table_to_settings/1,
         save_game_table_to_settings/1,
         save_table/2,
         delete_save_table/1,
         get_save_tables/1,
         get_save_table/1,
         get_save_table_setting/1,
         get_user_age/1,
         filter_table/2,
         is_active/1
        ]).

%pushsub
-export([publish/2, subscribe/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("nsx_config/include/log.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include("accounts.hrl").

-define(SERVER, ?MODULE).

-include("common.hrl").
-include("table.hrl").
-include("user.hrl").
-include("feed.hrl").
-include("accounts.hrl").

-record(state, {tables = ets:new(tables, [public, {keypos, 2}]),
    callbacks = []}).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:call(?SERVER, stop).

create_table(User, Params)  when is_record(User, user) ->
    gen_server:call(?SERVER, {create_table, User, Params}).
join_table(User, Table, Options) when is_record(User, user) ->
    gen_server:call(?SERVER, {join_table, User, Table, Options}).
join_table(User, Table) when is_record(User, user) ->
    gen_server:call(?SERVER, {join_table, User, Table, []}).
leave_table(User, Table) when is_record(User, user) ->
    gen_server:call(?SERVER, {leave_table, User, Table}).
get_table(TableId) ->
    gen_server:call(?SERVER, {get_table, TableId}).
update_table(NewData) ->
    gen_server:call(?SERVER, {update_table, NewData}).
game_started(Table, GameId, GamePid) ->
    gen_server:call(?SERVER, {game_started, Table, GameId, GamePid}).

is_active(TId) ->
    gen_server:call(?SERVER, {is_active, TId}).

-spec filter_tables(list()) -> {'ok', list()}.
filter_tables(Filters) ->
    gen_server:call(?SERVER, {filter_tables, Filters}).
filter_per_user(UId) when UId /= undefined ->
    gen_server:call(?SERVER, {filter_per_user, UId}).

-spec get_only_game(atom()) -> {'ok', list()}.
get_only_game(Type) ->
    gen_server:call(?SERVER, {get_only_game, Type}).


get_all_tables() ->
    gen_server:call(?SERVER, get_all_tables).

delete_table({gameId, GameId}) ->
    gen_server:call(?SERVER, {delete_table_per_gameid, GameId});
delete_table(TableId) ->
    gen_server:call(?SERVER, {delete_table, TableId}).


publish(Msg, TableId) when is_integer(TableId) ->
    topman:publish(Msg, lists:concat(["table_manager_",TableId])).

subscribe(Pid, TableId) when is_pid(Pid), is_integer(TableId) ->
    topman:subscribe(Pid, lists:concat(["table_manager_",TableId])).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{callbacks = [{feed, broadcast, []}]}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

qlc11(Id) ->
    qlc:e(qlc:q([Val || {{_,_,Val=#game_table{id = _Id}},_,_} <- gproc:table(props), Id == _Id])).

handle_call({create_table, User, S}, _From, State) ->
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
            {reply, {error, quota_hard_limit}, State};

        OK when OK == ok; OK == {error, soft_limit} ->
            %% if user will reach only soft limit - give him a chance to play
            #state{tables = Tables, callbacks = Callbacks} = State,
            TableId = nsm_db:next_id("table"),

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
                                paid_only        = get_setting(paid, S),
                                pointing_rules   = PR,
                                pointing_rules_ex = PREx
                               },

            case Table#game_table.game_type of
                undefined ->
                    {reply, {error, game_type_missing}, State};
                _ ->
                    Query = #game_table{name      = Table#game_table.name,
                                        game_type = Table#game_table.game_type,
                                        owner     = Table#game_table.owner,
                                        _='_'},
                    MatchSpec = [{Query, [], ['$_']}],
                    Select = ets:select(Tables, MatchSpec),
                    ?INFO("table:manager:select: ~p",[Select]),
                    case Select of
                        [_|_] ->
                            ?INFO("LIST"),
                            {reply, {error, table_name_conflict}, State};
                        [] ->
                            ?INFO("EMPTY"),
%                            ets:insert(Tables, Table),
                            publish({create_table, User}, TableId),
                            [ apply(Module, Function, [new_table, Table | Args])
                                || {Module, Function, Args} <- Callbacks ],
                            {reply, {ok, Table}, State}
%                            {reply, {ok, TableId}, State}
                    end
            end
    end;

handle_call({join_table, User, TableId, Options}, _From, State) ->
    #state{tables = Tables} = State,
    
%    case ets:lookup(Tables, TableId) of
    case qlc11(TableId) of
        [] ->
            {reply, {error, table_not_found}, State};
        [Table|T] ->
            #game_table{game_type = GameType, users = Users, users_options = UserOptions} = Table,
            MaxGameTypeUsers = proplists:get_value(max_users, game_requirements(GameType)),
            AlreadyJoined = lists:member(User#user.username, Users),
            DoublePoints = 1,
            PR = Table#game_table.pointing_rules,

            %% FIXME: not the best solution
            Rules = case nsx_opt:opt(feellucky, Options, false) of
                        %% special case for okey game IFeelLucky mode
                        true ->
                            [PRLucky] = Table#game_table.pointing_rules_ex,
                            PRLucky;
                        false ->
                            pointing_rules:double_points(PR, DoublePoints)
                    end,

            Quota = Rules#pointing_rule.quota,
            QuotaCheck = nsm_accounts:check_quota(User#user.username, Quota),
            IsHardLimitReached = {error, hard_limit} == QuotaCheck,

            if
                AlreadyJoined ->
                    {reply, {error, already_joined}, State};
                length(Users) >= MaxGameTypeUsers ->
                    {reply, {error, too_much_users}, State};
                IsHardLimitReached ->
                    {reply, {error, quota_hard_limit}, State};

                ?ELSE ->
                    Table2 = Table#game_table{users = [User#user.username | Users],
                                              users_options = [{User#user.username, Options}|UserOptions]},
%                    ets:insert(Tables, Table2),
                    ?INFO("GProc registration in JOIN"),
                    publish({join, User}, TableId),
                    {reply, ok, State}
            end
    end;

handle_call({delete_table, TableId}, _From, State) ->
    #state{tables = Tables} = State,
    ets:delete(Tables, TableId),
    publish(delete_table, TableId),
    {reply, ok, State};

handle_call({delete_table_per_gameid, GameId}, _From, State) ->
    #state{tables = Tables} = State,
    Query = #game_table{gameid = GameId,
                        _='_'},
    MatchSpec = [{Query, [], ['$_']}],
    Msg =
	case ets:select(Tables, MatchSpec) of
	    [Table] ->
		ets:delete_object(Tables, Table),
		delete;
	    [] ->
		not_found
	end,
    {reply, {ok, Msg}, State};


handle_call({leave_table, User, TableId}, _From, State) ->
    #state{tables = Tables} = State,
    case ets:lookup(Tables, TableId) of
        [] ->
            {reply, {error, table_not_found}, State};
        [Table] ->
            #game_table{users = Users} = Table,
            case lists:delete(User#user.username,Users) of
                Users ->
                    {reply, {error, not_joined}, State};
                NewUsers ->
                    case lists:filter(fun(E) -> E/=robot end, NewUsers) of
                        [] -> % delete table if it's empty or contain only robots
                            ets:delete_object(Tables, Table),
                            publish({leave_table, User}, TableId),
                            {reply, ok, State};
                        [NewOwner | _] ->
                            CurrentOwner = Table#game_table.owner,
                            Table2 =
                            case User#user.username of
                                CurrentOwner ->
                                    publish({change_owner, NewOwner}, TableId),
                                    Table#game_table{owner = NewOwner};
                                _ -> Table
                            end,

                            Table3 = Table2#game_table{users = NewUsers},
                            ets:insert(Tables, Table3),
                            publish({leave_table, User}, TableId),
                            {reply, ok, State}
                    end
            end
    end;

handle_call({game_started, Table, GameId, GamePid}, From, State) ->
    Monit = erlang:monitor(process, GamePid),
    handle_call({update_table, Table#game_table{gameid=GameId,
                                                game_process = GamePid,
                                                game_process_monitor = Monit}}, From, State);

handle_call({is_active, TId}, _From, State) ->
    #state{tables = Tables} = State,
    Exist = ets:member(Tables, TId),
    {reply, {ok, Exist}, State};


handle_call({update_table, NewData}, _From, State) ->
    TableId = NewData#game_table.id,
    #state{tables = Tables} = State,

    case ets:lookup(Tables, TableId) of
        [] ->
            {reply, {error, table_not_found}, State};
        [_Table] ->
            publish({update_table, NewData}, TableId),
            ets:insert(Tables, NewData),
            {reply, ok, State}
    end;



handle_call({get_table, TableId}, _From, State) ->
    ?INFO("table_manager:get_table: ~p",[TableId]),
    #state{tables = Tables} = State,
    ?INFO("tables: ~p",[State]),
    case ets:lookup(Tables, TableId) of
        [T] ->
            {reply, {ok, T}, State};
        [] ->
            {reply, {error, not_found}, State}
    end;

handle_call(get_all_tables, _From, State) ->
    #state{tables = Tables} = State,
    {reply, {ok, ets:tab2list(Tables)}, State};

handle_call({filter_per_user, User}, _From, State) ->
    #state{tables = Tables} = State,

    Fun = fun(Table) -> filter_table(User, Table) end,

    Lists = ets:tab2list(Tables),
    NewLists = lists:filter(Fun, Lists),
    {reply, {ok, NewLists}, State};

handle_call({filter_tables, Filters}, _From, State) when is_list(Filters)  ->
    #state{tables = Tables} = State,
    Speed = proplists:get_value(speed, Filters, '_'),
    Mode = proplists:get_value(mode, Filters, '_'),
    Friend = proplists:get_value(friend, Filters, '_'),



    Query = #game_table{game_speed = Speed,
                        game_mode = Mode,
                        friends_only = Friend,
                        _='_'},
    MatchSpec = [{Query, [], ['$_']}],
    {reply, {ok,ets:select(Tables, MatchSpec)}, State};

handle_call({get_only_game, Game}, _From, State) ->
    #state{tables = Tables} = State,
    Query = #game_table{game_type = Game,
                        _='_'},
    MatchSpec = [{Query, [], ['$_']}],
    {reply, {ok,ets:select(Tables, MatchSpec)}, State};


handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(Request, From, State) ->
    {stop, {unknown_call, Request, From}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({'DOWN', Ref, _, _, _}, #state{} = State) ->
    #state{tables = Tables} = State,
    Query = #game_table{game_process_monitor = Ref,
                        _='_'},
    MatchSpec = [{Query, [], ['$_']}],
    [#game_table{users = Users,
                 id = TableId} = Table] = ets:select(Tables, MatchSpec),
    [ publish({leave_table, User}, TableId) || User <- Users ],
    ets:delete_object(Tables, Table),
    {noreply, State};
handle_info(_Info, State) ->
    io:format("Unknown message: ~p~n", [_Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


%% fill_table_params(Table, Params) ->
%%     fill_table_params0(lists:reverse(Params), Table).

%% fill_table_params0([], Table) ->
%%     Table;
%% fill_table_params0([{game_type,GameType} | Rest], Table) ->
%%     Table2 = Table#game_table{game_type = GameType},
%%     fill_table_params0(Rest, Table2).

game_requirements(game_tavla) ->
    [{max_users, 2}];
game_requirements(game_okey) ->
    [{max_users, 4}];
game_requirements(okey) ->
    [{max_users, 4}].

-spec get_user_age(record(user)) -> integer() | undefined.
%% @doc returns user's age in years
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

-spec filter_table(iolist(), record(game_table)) -> true | false.
filter_table(User0, #game_table{owner = Owner} = GameTable) ->
    {ok, User} = nsm_users:get_user(User0),
    AllSub = nsm_users:list_subscr(User0),
    case Owner of
        User0 ->
            true;
        _ ->
            Age = check({age, GameTable, User}),
            Group = check({group, GameTable, User}),
            Country = check({country, GameTable, User}),
            Sex = check({sex, GameTable, User}),
            OnlyInvite = check({only_invite, GameTable, User}),
            Sub =
            case Group of
                true ->
                    true;
                _ ->
                    check({sub, GameTable, AllSub, User})
            end,
            Filter = [Age, Group, Country, Sex, Sub, OnlyInvite],
            io:fwrite("Filter: ~p~n", [Filter]),
            case lists:usort(Filter) of
                [true] ->
                    true;
                _ ->
                    false
            end
    end.


check({age, #game_table{age_limit = TAge}, User}) when is_record(User, user) ->
    case {get_user_age(User), TAge} of
        {_, undefined} ->
            true;
        {undefined, _} ->
            false;
        {UAge, [TAgeL, TAgeH]} ->
            TAgeL =< UAge andalso UAge =< TAgeH
    end;
check({group, #game_table{groups_only = GId}, #user{username = UId}}) ->
    case GId of
        [] ->
            true;
        _ ->
            lists:any(fun(G) ->
                              groups:user_inside(G, UId)
                      end, GId)
    end;

check({country, #game_table{location_limit = TLoc}, #user{location = ULoc}}) ->
    case TLoc of
        [] ->
            true;
        ULoc ->
            true;
        _ ->
            false
    end;
check({sex, #game_table{gender_limit = TSex}, #user{sex = USex}}) ->
    case TSex of
        undefined ->
            true;
        USex ->
            true;
        _ ->
            false
    end;
check({only_invite, #game_table{invited_users = TUsers}, #user{username = UserName}}) ->
    io:fwrite("TUsers: ~p~n User: ~p~n", [TUsers, UserName]),
    case TUsers of
        [] -> true;
        _ -> lists:member(UserName, TUsers)
    end;
check({sub, #game_table{owner = Owner}, AllSub, _User}) ->
    Names = lists:map(fun(#subs{whom=Whom}) ->
                              Whom
                      end, AllSub),
    lists:member(Owner, Names).

get_setting(Key, Setting) ->
    get_setting(Key, Setting, undefined).

get_setting(Key, Setting, Default) ->
    Req = [ V || {K, V} <- Setting, K == Key],
    case Req of
        [] ->
            Default;
        [OneElements] ->
            OneElements
    end.

get_list_setting(Key, Setting) ->
    [ V || {K, V} <- Setting, K == Key].

-spec game_table_to_settings(record(game_table)) -> proplist().
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
                                  double_points  = DoublePoints,
                                  pointing_rules = GeneralPointingRule,
                                  pointing_rules_ex = AdditionalPointingRules
                                                    }) ->

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
         {paid, Paid}],
    lists:flatten(Setting).

-spec save_game_table_to_settings(record(save_game_table)) -> proplist().
save_game_table_to_settings(#save_game_table{name     = Name,
                                       settings = Sett}) ->
    DefaultTable = game_table_to_settings(#game_table{}),
    %% we will use default values from record #game_table{} for
    %% options that not set so every field will be set
    S = Sett ++ DefaultTable,
    Setting =
        [{table_name, Name},
         {game, proplists:get_value(game, S)},
         {game_mode, proplists:get_value(game_mode, S)},
         {speed, proplists:get_value(speed, S)},
         {sets, proplists:get_value(sets, S)},
         {rounds, proplists:get_value(rounds, S)},
         {friends_only, proplists:get_value(friends_only, S)},
         {user, proplists:get_value(user, S)},
         {private, proplists:get_value(private, S)},
         {age, proplists:get_value(age, S)},
         {group, proplists:get_value(group, S)},
         {gender_limit, proplists:get_value(gender_limit, S)},
         {location, proplists:get_value(location, S)},
         {deny_robots, proplists:get_value(deny_robots, S)},
         {gosterge_finish, proplists:get_value(gosterge_finish, S)},
         {double_points, proplists:get_value(double_points, S)},
         {paid, proplists:get_value(paid, S)}],

    lists:flatten(Setting).


save_table(UId, Settings) ->
    Name = proplists:get_value(table_name, Settings),
    R = #save_game_table{uid = UId,
                         id = nsm_db:next_id("save_table"),
                         name = Name,
                         create_time = erlang:now(),
                         settings = Settings},
    nsx_util_notification:notify(["system", "put"], R).
    %nsm_db:put(R).

get_save_tables(UId) ->
    nsm_db:get_save_tables(UId).

get_save_table_setting(Id) ->
    {ok, Table} = get_save_table(Id),
    {ok, Table#save_game_table.settings}.

get_save_table(Id) ->
    nsm_db:save_game_table_by_id(Id).

delete_save_table(Id) ->
    {ok, Table} = get_save_table(Id),
    nsx_util_notification:notify(["system", "delete"], Table).
    %nsm_db:delete(Table).

create_example_table() ->
    Alice = #user{username = "alice", password="1",
                  name = "Alicja", surname = "Example", feed = feed:create(),
                  facebook_id = "1234567890",
                  age=25,
                  sex=f},

    ExampleTable = [ [{name,"fast, standard, rounds 1"},
                      {rounds,20},
                      {game_mode,standard},
                      {speed,fast},
                      {game,game_okey}],

                     [{name,"normal, color, chanak, deny_robots, 5rounds, gr: Erlang"},
                      {game_options,deny_robots},
                      {game_options,chanak},
                      {game_mode,color},
                      {speed,normal},
                      {rounds,20},
                      {group,"Erlang"},
                      {game,game_okey}],

                     [{name,"fast, standard, 1rounds, gr: erlang, geek; age: 5-60"},
                      {age,[5,60]},
                      {group,"Computer geek"},
                      {group,"Erlang"},
                      {rounds,20},
                      {game_mode,standard},
                      {speed,fast},
                      {game,game_okey}],

                     [{name,"normal, odd, chanak, deny_robots, 18 rounds, age: 66-100"},
                      {age,[66,100]},
                      {rounds,20},
                      {game_options,deny_robots},
                      {game_options,chanak},
                      {game_mode,even_odd},
                      {speed,normal},
                      {game,game_okey}],

                     [{name,"slow, countdown, 100 rounds, gr: i love kakaranet"},
                      {group,"I love Kakaranet"},
                      {rounds,20},
                      {game_mode,countdown},
                      {speed,slow},
                      {game,game_okey}]
                   ],

    [ create_table(Alice, Table) || Table <- ExampleTable ].

%% Internal

split_to_tuple(Prefix, Data) ->
    [ {Prefix, D} || D <- Data ].


%%%===================================================================
%%% Tests
%%%===================================================================

simple_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     {timeout, 100, fun basic_t/0}
    }.

setup() ->
    nsm_db:stop(),
    nsm_db:delete(),
    nsm_db:initialize(),
    nsm_db:start(),
    nsm_db:init_db(),
    ok = application:start(nsx_utils),
    ok = application:start(nsm_srv).


cleanup(_) -> % ? These functions are no longer exists
    nsm_db:stop(),
    nsm_db:delete(),
    application:stop(zealot).

basic_t() ->
    User1 = #user{username = "user1"},
    User2 = #user{username = "user2"},
    User3 = #user{username = "user3"},
    User4 = #user{username = "user4"},
    User5 = #user{username = "user5"},
    ?assertMatch({ok, []}, table_manager:get_all_tables()),
    ?assertMatch({error, game_type_missing}, table_manager:create_table(User1, #game_table{})),
    {ok, TableId} = table_manager:create_table(User1, #game_table{game_type = okey}),
    ?assertMatch({ok, [_]}, table_manager:get_all_tables()),
    ?assertMatch({ok, #game_table{game_type = okey}}, table_manager:get_table(TableId)),
    ?assertMatch({error, already_joined}, table_manager:join_table(User1, TableId)),
    ?assertMatch(ok, table_manager:join_table(User2, TableId)),
    ?assertMatch({error, already_joined}, table_manager:join_table(User2, TableId)),
    ?assertMatch(ok, table_manager:join_table(User3, TableId)),
    ?assertMatch(ok, table_manager:join_table(User4, TableId)),
    ?assertMatch({error, too_much_users}, table_manager:join_table(User5, TableId)),
    ?assertMatch(ok, table_manager:leave_table(User4, TableId)),
    ?assertMatch(ok, table_manager:join_table(User5, TableId)),
    ?assertMatch({ok, [_]}, table_manager:get_all_tables()),
    ?assertMatch(ok, table_manager:leave_table(User1, TableId)),
    ?assertMatch(ok, table_manager:leave_table(User2, TableId)),
    ?assertMatch(ok, table_manager:leave_table(User3, TableId)),
    ?assertMatch({error, not_joined}, table_manager:leave_table(User4, TableId)),
    ?assertMatch(ok, table_manager:leave_table(User5, TableId)),
    ?assertMatch({ok, []}, table_manager:get_all_tables()),
    ?assertMatch({error, not_found}, table_manager:get_table(TableId)),
    ok.
