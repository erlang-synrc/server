-module(nsm_riak).
-author('Maxim Sokhatsky <maxim@synrc.com>').

-include("config.hrl").
-include("user.hrl").
-include("feed.hrl").
-include("acl.hrl").
-include("invite.hrl").
-include("attachment.hrl").
-include("user_counter.hrl").
-include("table.hrl").
-include("tournaments.hrl").
-include("uri_translator.hrl").
-include("membership_packages.hrl").
-include("accounts.hrl").
-include("scoring.hrl").

-include_lib("nsx_config/include/config.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include_lib("alog/include/alog.hrl").

-export([start/0, stop/0, initialize/0, delete/0, init_db/0,
         put/1, count/1, get/2, select/2, multi_select/2, all/1, next_id/1, next_id/2, delete/1, delete/2,
         delete_browser_counter_older_than/1,browser_counter_by_game/1, unused_invites/0,
         riak_client/0, get_word/1, add_to_group/3, remove_from_group/2, list_membership_count/1,
         list_group_users/1, list_membership/1, move_group_members/3, get_group_members/1,
         get_group_members_count/1, change_group_name/2, riak_clean/1,
         riak_clean/0, acl_entries/1, acl_add_entry/3, update_user_name/3,
         user_by_verification_code/1, user_by_email/1, user_by_facebook_id/1, user_by_username/1,
         feed_add_entry/5, feed_add_entry/7, feed_add_direct_message/6,
         entry_by_id/1, comment_by_id/1, comments_by_entry/1, feed_direct_messages/5,
         feed_add_comment/7, entries_in_feed/3, purchases/1,
         subscribe_user/2, remove_subscription/2, list_subscriptions/1, list_subscription_me/1, is_user_subscribed/2,
         block_user/2, unblock_user/2, list_blocks/1, list_blocked_me/1, is_user_blocked/2,
         membership/2, get_save_tables/1, save_game_table_by_id/1, invite_code_by_issuer/1, invite_code_by_user/1,
         get_translation/1, tournament_waiting_queue/1,join_tournament/2,tournament_pop_waiting_player/1,
         play_record_add_entry/4,user_tournaments/1,riak_read_tournament_waiting_queue/3,
         get_purchases_by_user/3, get_purchases_by_user/4, put_into_invitation_tree/3, invitation_tree/2,
         invitation_tree_delete_child_link/2, add_purchase_to_user/2]).

delete() -> ok.
start() -> ok.
stop() -> stopped.

initialize() ->
    C = riak:client_connect(?RIAKSERVER),
    ets:new(config, [named_table,{keypos,#config.key}]),
    ets:insert(config, #config{ key = "riak_client", value = C}),
    ok.

init_db() ->
    ?INFO("~w:init_db/0: started", [?MODULE]),
    ok = nsm_affiliates:init_db(),
    ok = nsm_gifts_db:init_db(),
    ?INFO("~w:init_db/0: done", [?MODULE]),
    ok.

riak_clean() ->
    riak_clean(user),
    riak_clean(user_by_email),
    riak_clean(user_by_facebook_id),
    riak_clean(group),
    riak_clean(tournament),
    riak_clean(play_record),
    riak_clean(player_scoring),
    riak_clean(scoring_record),
    riak_clean(team),
    riak_clean(acl),
    riak_clean(acl_entry),
    riak_clean(feature),
    riak_clean(table),
    riak_clean(config),
    riak_clean(save_game_table),
    riak_clean(game_table),
    riak_clean(entry),
    riak_clean(likes),
    riak_clean(one_like),
    riak_clean(feed),
    riak_clean(comment),
    riak_clean(save_table),
    riak_clean(group_member),
    riak_clean(invite_code),
    riak_clean(group_member_rev),
    riak_clean(subscription),
    riak_clean(browser_counter),
    riak_clean(subscription_rev),
    riak_clean(ut_word),
    riak_clean(ut_translation),
    riak_clean(uploads),
    riak_clean(forget_password),
    riak_clean("unsuported"),
    riak_clean("__riak_client_test__"),
    riak_clean(id_seq),
    C=riak_client(),
    C:list_buckets().

riak_clean(Table) when is_list(Table)->
    C = riak_client(),
    {ok,Keys}=C:list_keys(erlang:list_to_binary(Table)),
    [ C:delete(erlang:list_to_binary(Table),Key) || Key <- Keys];
riak_clean(Table) ->
    C = riak_client(),
    [TableStr] = io_lib:format("~p",[Table]),
    {ok,Keys}=C:list_keys(erlang:list_to_binary(TableStr)),
    [ nsm_db:delete(Table,erlang:binary_to_list(Key)) || Key <- Keys].

% Convert Erlang records to Riak objects

make_object(T, account) -> [Key] = io_lib:format("~p", [T#account.id]), riak_object:new(<<"account">>, list_to_binary(Key), T);
make_object(T, feed) -> riak_object:new(<<"feed">>, list_to_binary(integer_to_list(T#feed.id)), T);
make_object(T, user) -> riak_object:new(<<"user">>, list_to_binary(T#user.username), T);
make_object(T, game_table) -> riak_object:new(<<"game_table">>, list_to_binary(T#game_table.id), T);
make_object(T, player_scoring) -> riak_object:new(<<"player_scoring">>, list_to_binary(T#player_scoring.id), T);
make_object(T, scoring_record) -> riak_object:new(<<"scoring_record">>, list_to_binary(integer_to_list(T#scoring_record.id)), T);
make_object(T, save_game_table) -> riak_object:new(<<"save_game_table">>, list_to_binary(integer_to_list(T#save_game_table.id)), T);
make_object(T, browser_counter) -> riak_object:new(<<"browser_counter">>, T#browser_counter.id, T);
make_object(T, feature) -> riak_object:new(<<"feature">>, list_to_binary(T#feature.name), T);
make_object(T, config) -> riak_object:new(<<"config">>, list_to_binary(T#config.key), T);
make_object(T = {user_by_email, _User, Email}, user_by_email) -> riak_object:new(<<"user_by_email">>, list_to_binary(Email), T);
make_object(T = {user_by_verification_code, _User, Code}, user_by_verification_code) -> riak_object:new(<<"user_by_verification_code">>, list_to_binary(Code), T);
make_object(T = {user_by_facebook_id, _User, FB}, user_by_facebook_id) -> riak_object:new(<<"user_by_facebook_id">>, list_to_binary(FB), T);
make_object(T, forget_password) -> riak_object:new(<<"forget_password">>, list_to_binary(T#forget_password.token), T);
make_object(T, entry) -> [Key] = io_lib:format("~p", [T#entry.id]), riak_object:new(<<"entry">>, list_to_binary(Key), T);
make_object(T, comment) -> [Key] = io_lib:format("~p", [T#comment.id]), riak_object:new(<<"comment">>, list_to_binary(Key), T);
make_object(T = {_,Key,_}, id_seq) -> riak_object:new(<<"id_seq">>, list_to_binary(Key), T);
make_object(T, group) -> riak_object:new(<<"group">>, list_to_binary(T#group.username), T);
make_object(T, tournament) -> riak_object:new(<<"tournament">>, list_to_binary(integer_to_list(T#tournament.id)), T);
make_object(T, team) -> riak_object:new(<<"team">>, list_to_binary(integer_to_list(T#team.id)), T);
make_object(T, play_record) -> riak_object:new(<<"play_record">>, list_to_binary(integer_to_list(T#play_record.id)), T);
make_object(T, acl) -> [Key] = io_lib:format("~p", [T#acl.id]), riak_object:new(<<"acl">>, list_to_binary(Key), T);
make_object(T, acl_entry) -> [Key] = io_lib:format("~p", [T#acl_entry.id]), riak_object:new(<<"acl_entry">>, list_to_binary(Key), T);
make_object(T, group_member) -> riak_object:new(<<"group_member">>, list_to_binary(T#group_member.who), T);
make_object(T, group_member_rev) -> riak_object:new(<<"group_member_rev">>, list_to_binary(T#group_member_rev.group), T);
make_object(T, subscription) -> riak_object:new(<<"subscription">>, list_to_binary(T#subscription.who), T);
make_object(T, subscription_rev) -> riak_object:new(<<"subscription_rev">>, list_to_binary(T#subscription_rev.whom), T);
make_object(T, user_ignores) -> riak_object:new(<<"user_ignores">>, list_to_binary(T#user_ignores.who), T);
make_object(T, user_ignores_rev) -> riak_object:new(<<"user_ignores_rev">>, list_to_binary(T#user_ignores_rev.whom), T);
make_object(T, ut_word) -> riak_object:new(<<"ut_word">>, list_to_binary(T#ut_word.english), T);
make_object(T, ut_translation) -> {Lang, English} = T#ut_translation.source, riak_object:new(<<"ut_translation">>, list_to_binary(Lang ++ "_" ++ English), T);
make_object(T, membership_package) -> riak_object:new(<<"membership_package">>, list_to_binary(T#membership_package.id), T);
make_object(T, membership_purchase) -> riak_object:new(<<"membership_purchase">>, list_to_binary(T#membership_purchase.id), T);
make_object(T, user_purchase) -> riak_object:new(<<"user_purchase">>, list_to_binary(T#user_purchase.user), T);
make_object(T, pointing_rule) -> [Key] = io_lib:format("~p", [T#pointing_rule.id]), riak_object:new(<<"pointing_rule">>, list_to_binary(Key), T);
make_object(T, transaction) -> riak_object:new(<<"transaction">>, list_to_binary(T#transaction.id), T);
make_object({_, T}, {transaction, _AccountId} = B) -> [Bucket] = io_lib:format("~p", [B]), riak_object:new(list_to_binary(Bucket), list_to_binary(T#transaction.id), T);
make_object(T = {feed_blocked_users, UserId, _BlockedUsers} = T, feed_blocked_users) -> riak_object:new(<<"feed_blocked_users">>, list_to_binary(UserId), T);
make_object(T, uploads) -> [Key] = io_lib:format("~p", [T#uploads.key]), riak_object:new(<<"uploads">>, list_to_binary(Key), T);
make_object(T, invite_code) -> riak_object:new(<<"invite_code">>, list_to_binary(T#invite_code.code), T);
make_object(T = {invite_code_by_issuer, User, _Code}, invite_code_by_issuer) -> riak_object:new(<<"invite_code_by_issuer">>, list_to_binary(User), T);
make_object(T = {invite_code_by_user, User, _Code}, invite_code_by_user) -> riak_object:new(<<"invite_code_by_user">>, list_to_binary(User), T);
make_object(T, entry_likes) -> riak_object:new(<<"entry_likes">>, list_to_binary(T#entry_likes.entry_id), T);
make_object(T, user_likes) -> riak_object:new(<<"user_likes">>, list_to_binary(T#user_likes.user_id), T);
make_object(T, one_like) -> riak_object:new(<<"one_like">>, list_to_binary(integer_to_list(T#one_like.id)), T);
make_object(T, user_etries_count) -> riak_object:new(<<"user_etries_count">>, list_to_binary(T#user_etries_count.user_id), T);
make_object(T, invitation_tree) -> [K] = io_lib:format("~p", [T#invitation_tree.user]), riak_object:new(<<"invitation_tree">>, list_to_binary(K), T);
make_object(T, Unsupported) -> riak_object:new(<<"unsuported">>, term_to_binary(Unsupported), T).

%% user purchases bucket
%make_object(T = {_, Id, _Next}, {membership_purchase_by_user, _UserId} = B) ->
%    [Bucket] = io_lib:format("~p", [B]),
%    riak_object:new(list_to_binary(Bucket), erlang:list_to_binary(Id), T);

%make_object(T = {_, UserId, _Top}, membership_purchase_by_user) ->
%    riak_object:new(<<"membership_purchase_by_user">>, erlang:list_to_binary(UserId), T);


%make_object(T, invitation_tree) ->
%    Key = case T#invitation_tree.user of
%        ?INVITATION_TREE_ROOT ->
%            [K] = io_lib:format("~p", [T#invitation_tree.user]),
%            K;
%        String ->
%            String
%    end,
%    riak_object:new(<<"invitation_tree">>, list_to_binary(Key), T);

riak_client() -> [{_,_,{_,C}}] = ets:lookup(config, "riak_client"), C.

% put

-spec put(tuple() | [tuple()]) -> ok.
put(Records) when is_list(Records) ->
    lists:foreach(fun riak_put/1, Records);
put(Record) ->
    put([Record]).

riak_put(Record) ->
    Class = element(1,Record),
    Object = make_object(Record, Class),
    Riak = riak_client(),
    Result = Riak:put(Object, [{allow_mult,false},{last_write_wins,true}]),
    post_write_hooks(Class, Record, Riak),
    Result.

post_write_hooks(Class,R,C) ->
    case Class of
        user -> case R#user.email of
                    undefined -> nothing;
                    _ -> C:put(make_object({user_by_email, R#user.username, R#user.email},
                                            user_by_email))
                end,
                case R#user.verification_code of
                    undefined -> nothind;
                    _ -> C:put(make_object({user_by_verification_code, R#user.username, R#user.verification_code},
                                            user_by_verification_code))
                end,
                case R#user.facebook_id of
                    undefined -> nothing;
                    _ -> C:put(make_object({user_by_facebook_id, R#user.username, R#user.facebook_id},
                                            user_by_facebook_id))
                end;

        invite_code ->
            #invite_code{created_user=User,
                         issuer = Issuer} = R,

            if Issuer =/= undefined,
               User =/= undefined ->
                   nsm_affiliates:invitation_hook(Issuer, User);
               true -> do_nothing
            end,

            case R#invite_code.created_user of
                undefined -> nothing;
                User -> C:put(make_object({invite_code_by_user, User, R#invite_code.code}, invite_code_by_user))
            end,

            case R#invite_code.issuer of
                undefined ->
                    nothing;
                Issuer ->
                    C:put(make_object({invite_code_by_issuer, Issuer, R#invite_code.code}, invite_code_by_issuer))
            end;

%        membership_purchase ->
%            User = R#membership_purchase.user_id,
%            add_purchase_to_user(User, R);
%           add_purchase_by_user(User, R#membership_purchase.id); % remove vlad's buggy approach

        transaction->
            %% FIXME: move this actions to db workers
            Acceptor = R#transaction.acceptor,
            Remitter = R#transaction.remitter,
            [nsm_db:put({{transaction, U}, R}) || U <- [Acceptor, Remitter]];

        _ -> continue
    end.

% get

-spec get(atom(), term()) -> {ok, tuple()} | {error, not_found | duplicated}.
get(RecordName, Key) when is_atom(Key) ->
    riak_get(format_recordname(RecordName), erlang:list_to_binary(erlang:atom_to_list(Key)));
get(RecordName, Key) when is_integer(Key) ->
    riak_get(format_recordname(RecordName), erlang:list_to_binary(erlang:integer_to_list(Key)));
get(RecordName, Key) when is_tuple(Key) ->
	[StrKey] = io_lib:format("~p",[Key]),
	riak_get(format_recordname(RecordName), erlang:list_to_binary(StrKey));
get(RecordName, Key) ->
    riak_get(format_recordname(RecordName), erlang:list_to_binary(Key)).

format_recordname(RecordName) ->
    [StringBucket] = io_lib:format("~p",[RecordName]),
    erlang:list_to_binary(StringBucket).

riak_get(Bucket,Key) -> %% TODO: add state monad here for conflict resolution when not last_win strategy used
    C = riak_client(),
    RiakAnswer = C:get(Bucket,Key,[{last_write_wins,true},{allow_mult,false}]),
    case RiakAnswer of
        {ok, O} -> {ok,riak_object:get_value(O)};
        X -> X
    end.

% translations

get_word(Word) ->
    get(ut_word,Word).

get_translation({Lang, Word}) ->
    get(ut_translation, Lang ++ "_" ++ Word).

% delete

-spec delete(tuple() | [tuple()]) -> ok.
delete(Keys) when is_list(Keys) ->
    lists:foreach(fun mnesia:delete_object/1, Keys); % TODO
delete(Keys) ->
    delete([Keys]).

-spec delete(atom(), term()) -> ok.
delete(Tab, Key) ->
    C = riak_client(),
    [StringBucket] = io_lib:format("~p",[Tab]),
    if is_integer(Key) ->
           C:delete(erlang:list_to_binary(StringBucket),erlang:list_to_binary(integer_to_list(Key)));
       is_list(Key) ->
           C:delete(erlang:list_to_binary(StringBucket),erlang:list_to_binary(Key));
       true ->
           [ListKey] = io_lib:format("~p", [Key]),
           C:delete(erlang:list_to_binary(StringBucket),erlang:list_to_binary(ListKey))
    end,
    ok.

% search

-spec multi_select(atom(), [term()]) -> [tuple()].
multi_select(_RecordName, _Keys) when is_list(_Keys) -> erlang:error(notimpl).
%    [mnesia:read({RecordName, Key}) || Key <- Keys].

select(RecordName, Pred) when is_function(Pred) ->
	%% FIXME: bruteforce select
	All = all(RecordName),
	lists:filter(Pred, All);

select(RecordName, Select) when is_list(Select) ->
	%% FIXME: dummy select!
	Where = proplists:get_value(where, Select, fun(_)->true end),
	{Position, _Order} = proplists:get_value(order, Select, {1, descending}),

	Limit = proplists:get_value(limit, Select, all),

	Selected = select(RecordName, Where),
	Sorted = lists:keysort(Position, Selected),

	case Limit of
		all ->
			Sorted;
		{Offset, Amoumt} ->
			lists:sublist(Sorted, Offset, Amoumt)
	end.


-spec count(atom()) -> non_neg_integer().
count(_RecordName) ->
    erlang:length(all(_RecordName)).
%   erlang:error(notimpl).
%   mnesia:table_info(RecordName, size). % TODO

-spec all(atom()) -> [tuple()].
all(RecordName) ->
    Riak = riak_client(),
    [RecordStr] = io_lib:format("~p",[RecordName]),
    RecordBin = erlang:list_to_binary(RecordStr),
    {ok,Keys} = Riak:list_keys(RecordBin),
    Results = [ get_record_from_table({RecordBin, Key, Riak}) || Key <- Keys ],
    [ Object || Object <- Results, Object =/= failure ].

get_record_from_table({RecordBin, Key, Riak}) ->
    case Riak:get(RecordBin, Key) of
        {ok,O} -> riak_object:get_value(O);
        X -> failure
    end.

% id generator

-spec next_id(list()) -> pos_integer().
next_id(RecordName) ->
    next_id(RecordName, 1).

-spec next_id(list(), integer()) -> pos_integer().
next_id(RecordName, Incr) ->
    case nsm_db:get(id_seq, RecordName) of
	{error,notfound} -> R = 0;
	{ok,{id_seq, _, Id}} -> R = Id + Incr
    end,
    Rec = #id_seq{thing = RecordName, id = R},
    nsm_db:put(Rec),
    R.

% browser counters

-spec delete_browser_counter_older_than(pos_integer()) -> ok.
delete_browser_counter_older_than(_MinTS) -> % TODO
    [].
%    MatchHead = #browser_counter{minute='$1', _ = '_'},
%    Guard = {'<', '$1', MinTS},
%    Result = '$_',
%    List = mnesia:select(browser_counter, [{MatchHead, [Guard], [Result]}]),
%          lists:foreach(fun(X) ->
%                    mnesia:delete_object(X)
%                end, List),
%    List.

-spec browser_counter_by_game(atom()) -> [#browser_counter{}].
browser_counter_by_game(Game) ->
    ?INFO("Game: ~p,",[Game]), % TODO
    [].
%     {atomic, Result} = mnesia:transaction(fun() -> mnesia:match_object(#browser_counter{game=Game,_= '_'}) end),
%	 Result.

unused_invites() -> % TODO
    List = nsm_db:all(invite_code),
    length([ #invite_code{created_user=undefined} || _Code <- List]).

user_by_verification_code(Code) ->
    R = case nsm_db:get(user_by_verification_code,Code) of
	{ok,{_,User,_}} -> nsm_db:get(user,User);
	Else -> Else
	end,
    R.

user_by_facebook_id(FBId) ->
    R = case nsm_db:get(user_by_facebook_id,FBId) of
	{ok,{_,User,_}} -> nsm_db:get(user,User);
	Else -> Else
	end,
    R.

user_by_email(FB) ->
    R = case nsm_db:get(user_by_email,FB) of
	{ok,{_,User,_}} -> nsm_db:get(user,User);
	Else -> Else
	end,
    R.

user_by_username(Name) ->
    case X = nsm_db:get(user,Name) of
	{ok,_Res} -> X;
	Else -> Else
    end.

invite_code_by_issuer(User) ->
    case nsm_db:get(invite_code_by_issuer, User) of
        {ok, {invite_code_by_user, _, Code}} ->
            case nsm_db:get(invite_code, Code) of
                {ok, #invite_code{} = C} ->
                    [C];
                _ ->
                    []
            end;
        _ ->
            []
    end.

invite_code_by_user(User) ->
    case nsm_db:get(invite_code_by_user, User) of
        {ok, {invite_code_by_user, _, Code}} ->
            case nsm_db:get(invite_code, Code) of
                {ok, #invite_code{} = C} ->
                    [C];
                _ ->
                    []
            end;
        _ ->
            []
    end.

% groups

add_to_group(MeId, FrId,Type) ->
    MeShow = case nsm_db:get(user, MeId) of
        {ok, #user{name=MeName,surname=MeSur}} ->
            io_lib:format("~s ~s", [MeName,MeSur]);
        _ ->
            MeId
    end,
    FrShow = case nsm_db:get(group, FrId) of
        {ok, #group{name=FrName}} -> FrName;
        _ -> FrId
    end,
    List = [#group_member{who=MeId, group=FrId, group_name=FrShow, type=Type} |
        case nsm_db:get(group_member, MeId) of
        {error,notfound} ->
            nsm_db:delete(group_member, MeId),
            [];
        {ok,#group_member{group=Subscriptions}} ->
            [ Sub || Sub <- Subscriptions, Sub#group_member.group=/=FrId ]
        end],
    nsm_db:put(#group_member{who=MeId, group=List, type=list}),
    RevList = [#group_member_rev{ group= FrId, who=MeId, who_name=MeShow, type= Type} |
        case nsm_db:get(group_member_rev, FrId) of
        {error,notfound} ->
            nsm_db:delete(group_member_rev, FrId),
            [];
        {ok,#group_member_rev{who=RevSubscriptions}} ->
            [ Sub || Sub <- RevSubscriptions, Sub#group_member_rev.who=/=MeId ]
        end],
    nsm_db:put(#group_member_rev{who=RevList, group=FrId, type=list}).

remove_from_group(MeId, FrId) ->
    List = list_membership(MeId),
    NewList = [ Rec || Rec<-List, not(Rec#group_member.who == MeId andalso Rec#group_member.group == FrId) ],
    nsm_db:put(#group_member{who = MeId, group=NewList, type=list}),
    RevList = list_group_users(FrId),
    NewRevList = [ Rec || Rec<-RevList, not(Rec#group_member_rev.who==MeId andalso Rec#group_member_rev.group==FrId) ],
    nsm_db:put(#group_member_rev{who = NewRevList, group = FrId, type=list}).

list_membership(#user{username = UId}) -> list_membership(UId);
list_membership(UId) when is_list(UId) ->
    case nsm_db:get(group_member, UId) of
	{ok,#group_member{group = C}} -> C;
	_ -> []
    end.

list_membership_count(#user{username = UId}) -> list_membership_count(UId);
list_membership_count(UId) when is_list(UId) ->
    [{G, length(list_group_users(element(3, G)))} || G <-list_membership(UId)].

list_group_users(UId) ->
    case nsm_db:get(group_member_rev, UId) of
	{ok,#group_member_rev{who = C}} -> C;
	_ -> []
    end.

membership(UserId, GroupId) ->
    GroupMembership = [ Group || Group <- list_membership(UserId), GroupId == Group#group_member.group ],
    case GroupMembership of
        [] -> {error, not_found};
        [ #group_member{}=Member ] -> {ok, Member}
    end.

get_group_members(GId) ->
    case nsm_db:get(group_member_rev, GId) of
        {error,notfound} ->
            {error,notfound};
        {ok,#group_member_rev{who=RevSubs}} ->
            RevSubs
    end.

get_group_members_count(GId) ->
    case nsm_db:get_group_members(GId) of
        {error, notfound} ->
            {error, notfound};
        Members when is_list(Members) ->
            length(Members)
    end.

% Todo: run in background
move_group_members(OldGId, NewGId, GName) ->
    case nsm_db:get(group_member_rev, OldGId) of
	{error,notfound} ->
        ok;
	{ok,#group_member_rev{who=RevSubscriptions}} ->
        RevList = [#group_member_rev{group=NewGId, who=Who, who_name=WhoName, type=Type} ||
                   #group_member_rev{group=_OldGId, who=Who, who_name=WhoName, type=Type} <- RevSubscriptions
                  ],
	    nsm_db:put(#group_member_rev{who=RevList, group=NewGId, type=list}),
        UpdateMember = fun(#group_member_rev{who=User,type=Type}, _) ->
            case nsm_db:get(group_member, User) of
            {error, notfound} ->
                nsm_db:put(#group_member{who = User,
                                            group = [#group_member{who=User, group=NewGId, group_name=GName, type=Type}],
                                            type=list});
            {ok,#group_member{group=Subs}} ->
                NewSubs = lists:map(fun(#group_member{group=Group}=M) when Group== OldGId ->
                                               M#group_member{group=NewGId};(M) -> M end, Subs),
                nsm_db:put(#group_member{who = User, group=NewSubs, type=list})
            end
        end,
        lists:foldl(UpdateMember, undefined, RevList)
    end.

% Todo: run in background
change_group_name(GId, GName) ->
    case nsm_db:get(group_member_rev, GId) of
    {error, notfound} -> ok;
    {ok, #group_member_rev{who=RevList}} ->
        UpdateGroupName = fun(#group_member_rev{who=User,type=Type}, _) ->
            case nsm_db:get(group_member, User) of
            {error, notfound} ->
                nsm_db:put(#group_member{who=User,
                                            group=[#group_member{who=User,type=Type,group=GId,group_name=GName}],
                                            type=list});
            {ok,#group_member{group=Subs}} ->
                NewSubs = lists:map(fun(#group_member{group=Group}=M) when Group==GId ->
                                           M#group_member{group_name=GName};(M) -> M end, Subs),
                nsm_db:put(#group_member{who = User, group=NewSubs, type=list})
            end
        end,
        lists:foldl(UpdateGroupName, undefined, RevList)
    end.

% Todo: run in background
update_user_name(UId,UName,Surname) ->
    Name = case {UName,Surname} of
        {undefined,undefined} -> UId;
        _ -> io_lib:format("~s ~s", [UName, Surname])
    end,
    case nsm_db:get(group_member, UId) of
    {error, notfound} -> ok;
    {ok, #group_member{group=List}} ->
        UpdateUserName = fun(#group_member{group=GId,type=Type}, _) ->
            case nsm_db:get(group_member_rev, GId) of
            {error, notfound} ->
                nsm_db:put(#group_member_rev{group=GId,
                                                who=[#group_member_rev{who=UId,who_name=Name,group=GId,type=Type}],
                                                type=list});
            {ok,#group_member_rev{who=Whos}} ->
                NewWhos = lists:map(fun(#group_member_rev{who=U}=M) when U==UId->
                                           M#group_member_rev{who_name=Name};(M)->M end, Whos),
                nsm_db:put(#group_member_rev{group=GId, who=NewWhos, type=list})
            end
        end,
        lists:foldl(UpdateUserName, undefined, List)
    end.

% game info

get_save_tables(Id) ->
    case nsm_db:get(save_game_table, Id) of
	{error,notfound} -> [];
	{ok,R} -> R
    end.

save_game_table_by_id(Id) -> nsm_db:get(save_game_table, Id).

% subscriptions

subscribe_user(MeId, FrId) ->
    MeShow = case nsm_db:get(user, MeId) of
        {ok, #user{name=MeName,surname=MeSur}} ->
            io_lib:format("~s ~s", [MeName,MeSur]);
        _Z ->
            io:format("Get ~s: ~p~n", [MeId, _Z]),
            undefined
    end,
    FrShow = case nsm_db:get(user, FrId) of
        {ok, #user{name=FrName,surname=FrSur}} ->
            io_lib:format("~s ~s", [FrName,FrSur]);
        _ ->
            undefined
    end,
    Rec = #subscription{who = MeId, whom = FrId, whom_name = FrShow},
    case nsm_db:get(subscription, MeId) of
        {error, notfound} ->
            nsm_db:put(#subscription{who = MeId, whom = [Rec]});
        {ok, #subscription{whom = List}} ->
            case lists:member(Rec, List) of
                false ->
                    NewList =
                        lists:keystore(FrId, #subscription.whom, List, Rec),
                    nsm_db:put(#subscription{who = MeId, whom = NewList});
                true ->
                    do_nothing
            end
    end,
    RevRec = #subscription_rev{whom=FrId, who=MeId, who_name=MeShow},
    case nsm_db:get(subscription_rev, FrId) of
        {error,notfound} ->
            nsm_db:put(#subscription_rev{whom=FrId, who=[RevRec]});
        {ok,#subscription_rev{who=RevList}} ->
            case lists:member(RevRec, RevList) of
                false ->
                    NewRevList =
                        lists:keystore(MeId, #subscription_rev.who, RevList, RevRec),
                    nsm_db:put(#subscription_rev{whom=FrId, who=NewRevList});
                true ->
                    do_nothing
            end
    end,
    ok.

remove_subscription(MeId, FrId) ->
    List = nsm_users:list_subscription(MeId),
    Subs = [ Sub || Sub <- List, not(Sub#subscription.who==MeId andalso Sub#subscription.whom==FrId) ],
    nsm_db:put(#subscription{who = MeId, whom = Subs}),
    List2 = nsm_users:list_subscription_me(FrId),
    Revs = [ Rev || Rev <- List2, not(Rev#subscription_rev.who==MeId andalso Rev#subscription_rev.whom==FrId) ],
    nsm_db:put(#subscription_rev{who = Revs, whom = FrId}).

list_subscriptions(#user{username = UId}) -> list_subscriptions(UId);
list_subscriptions(UId) when is_list(UId) ->
    case nsm_db:get(subscription, UId) of
	{ok,#subscription{whom = C}} -> C;
	_ -> []
    end.

list_subscription_me(UId) ->
    case nsm_db:get(subscription_rev, UId) of
	{ok,#subscription_rev{who = C}} -> C;
	_ -> []
    end.

is_user_subscribed(Who,Whom) ->
    case nsm_db:get(subscription, Who) of
    {ok,#subscription{whom = W}} ->
        lists:any(fun(#subscription{who=Who1, whom=Whom1}) -> Who1==Who andalso Whom1==Whom; (_)->false end, W);
    _ -> false
    end.


% blocking user

block_user(Who, Whom) ->
    case nsm_db:get(user_ignores, Who) of
        {error, notfound} ->
            nsm_db:put(#user_ignores{who = Who, whom = [Whom]});
        {ok, #user_ignores{whom = List}} ->
            case lists:member(Whom, List) of
                false ->
                    NewList = [Whom | List],
                    nsm_db:put(#user_ignores{who = Who, whom = NewList});
                true ->
                    do_nothing
            end
    end,
    case nsm_db:get(user_ignores_rev, Whom) of
        {error,notfound} ->
            nsm_db:put(#user_ignores_rev{whom=Whom, who=[Who]});
        {ok,#user_ignores_rev{who=RevList}} ->
            case lists:member(Who, RevList) of
                false ->
                    NewRevList = [Who | RevList],
                    nsm_db:put(#user_ignores_rev{whom=Whom, who=NewRevList});
                true ->
                    do_nothing
            end
    end,
    ok.

unblock_user(Who, Whom) ->
    List = list_blocks(Who),
    case lists:member(Whom, List) of
        true ->
            NewList = [ UID || UID <- List, UID =/= Whom ],
            nsm_db:put(#user_ignores{who = Who, whom = NewList});
        false ->
            do_nothing
    end,
    List2 = list_blocked_me(Whom),
    case lists:member(Who, List2) of
        true ->
            NewRevList = [ UID || UID <- List2, UID =/= Who ],
            nsm_db:put(#user_ignores_rev{whom = Whom, who = NewRevList});
        false ->
            do_nothing
    end.

list_blocks(#user{username = UId}) -> list_blocks(UId);
list_blocks(UId) when is_list(UId) ->
    case nsm_db:get(user_ignores, UId) of
        {ok,#user_ignores{whom = C}} -> C;
        _ -> []
    end.

list_blocked_me(UId) ->
    case nsm_db:get(user_ignores_rev, UId) of
        {ok,#user_ignores_rev{who = C}} -> C;
        _ -> []
    end.

is_user_blocked(Who, Whom) ->
    case nsm_db:get(user_ignores, Who) of
        {ok,#user_ignores{whom = List}} ->
            lists:member(Whom, List);
        _ -> false
    end.

% feeds

feed_add_direct_message(FId,User,To,EntryId,Desc,Medias) -> feed_add_entry(FId,User,To,EntryId,Desc,Medias,{user,direct}).
feed_add_entry(FId,From,EntryId,Desc,Medias) -> feed_add_entry(FId,From,undefined,EntryId,Desc,Medias,{user,normal}).
feed_add_entry(FId, User, To, EntryId,Desc,Medias,Type) ->
    %% prevent adding of duplicate records to feed
    case nsm_db:entry_by_id({EntryId, FId}) of
        {ok, _} -> ok;
        _ -> do_feed_add_entry(FId, User, To, EntryId, Desc, Medias, Type)
    end.

do_feed_add_entry(FId, User, To, EntryId, Desc, Medias, Type) ->
    {ok,Feed} = nsm_db:get(feed,erlang:integer_to_list(FId)),
    Id = {EntryId, FId},
    Next = undefined,
    Prev = case Feed#feed.top of
               undefined ->
                   undefined;
               X ->
                   case nsm_db:get(entry, X) of
                       {ok, TopEntry} ->
                           EditedEntry = TopEntry#entry{created_time = now(),
                                                        next = Id},
                           % update prev entry
                           nsm_db:put(EditedEntry),
                           TopEntry#entry.id;
                       {error,notfound} ->
                           undefined
                   end
           end,

    nsm_db:put(#feed{id = FId, top = {EntryId, FId}}), % update feed top with current

    Entry  = #entry{id = {EntryId, FId},
                    entry_id = EntryId,
                    feed_id = FId,
                    from = User,
                    to = To,
                    type = Type,
                    media = Medias,
                    created_time = now(),
                    description = Desc,
                    raw_description = Desc,
                    next = Next,
                    prev = Prev},

    ModEntry = case catch feedformat:format(Entry) of
                   {_, Reason} ->
                       ?ERROR("feedformat error: ~p", [Reason]),
                       Entry;
                   #entry{} = ME ->
                       ME
               end,

    nsm_db:put(ModEntry),
    {ok, ModEntry}.

feed_add_comment(FId, User, EntryId, ParentComment, CommentId, Content, Medias) ->
    FullId = {CommentId, {EntryId, FId}},

    Prev = case ParentComment of
        undefined ->
            {ok, Entry} = nsm_db:entry_by_id({EntryId, FId}),
            {PrevC, E} = case Entry#entry.comments of
                        undefined ->
                            {undefined, Entry#entry{comments_rear = FullId}};
                        Id ->
                            {ok, PrevTop} = nsm_db:get(comment, Id),
                            nsm_db:put(PrevTop#comment{next = FullId}),
                            {Id, Entry}
                   end,

            nsm_db:put(E#entry{comments=FullId}),
            PrevC;

        _ ->
            {ok, Parent} = nsm_db:get(comment, {{EntryId, FId}, ParentComment}),
            {PrevC, CC} = case Parent#comment.comments of
                        undefined ->
                            {undefined, Parent#comment{comments_rear = FullId}};
                        Id ->
                            {ok, PrevTop} = nsm_db:get(comment, Id),
                            nsm_db:put(PrevTop#comment{next = FullId}),
                            {Id, Parent}
                    end,
            nsm_db:put(CC#comment{comments = FullId}),
            PrevC
    end,
    Comment = #comment{id = FullId,
                       author_id = User,
                       comment_id = CommentId,
                       entry_id = EntryId,
                       raw_content = Content,
                       content = Content,
                       media = Medias,
                       create_time = now(),
                       prev = Prev,
                       next = undefined
                      },
    nsm_db:put(Comment),
    {ok, Comment}.

add_purchase_to_user(UserId,Purchase) ->
    {ok,Team} = case nsm_db:get(user_purchase, UserId) of
                     {ok,T} -> ?INFO("user_purchase found"), {ok,T};
                     _ -> ?INFO("user_purchase not found"),
                          Head = #user_purchase{ user = UserId, top = undefined},
                          {nsm_db:put(Head),Head}
                end,

    EntryId = Purchase#membership_purchase.id, %nsm_db:next_id("membership_purchase",1),
    Prev = undefined,
    case Team#user_purchase.top of
        undefined -> Next = undefined;
        X -> case nsm_db:get(membership_purchase, X) of
                 {ok, TopEntry} ->
                     Next = TopEntry#membership_purchase.id,
                     EditedEntry = #membership_purchase{
                           external_id = TopEntry#membership_purchase.external_id,
                           user_id = TopEntry#membership_purchase.user_id,
                           state = TopEntry#membership_purchase.state,
                           membership_package = TopEntry#membership_purchase.membership_package,
                           start_time = TopEntry#membership_purchase.start_time,
                           end_time = TopEntry#membership_purchase.end_time,
                           state_log = TopEntry#membership_purchase.state_log,
                           info = TopEntry#membership_purchase.info,
                           id = TopEntry#membership_purchase.id,
                           next = TopEntry#membership_purchase.next,
                           prev = EntryId},
                    nsm_db:put(EditedEntry); % update prev entry
                 {error,notfound} -> Next = undefined
             end
    end,

    nsm_db:put(#user_purchase{ user = UserId, top = EntryId}), % update teaam top with current

    Entry  = #membership_purchase{id = EntryId,
                                  user_id = UserId,
                                  external_id = Purchase#membership_purchase.external_id,
                                  state = Purchase#membership_purchase.state,
                                  membership_package = Purchase#membership_purchase.membership_package,
                                  start_time = Purchase#membership_purchase.start_time,
                                  end_time = Purchase#membership_purchase.end_time,
                                  state_log = Purchase#membership_purchase.state_log,
                                  info = Purchase#membership_purchase.info,
                                  next = Next,
                                  prev = Prev},

    case nsm_db:put(Entry) of ok -> {ok, EntryId};
                           Error -> ?INFO("Cant write purchase"), {failure,Error} end.


acl_add_entry(Resource, Accessor, Action) ->
    Acl = case nsm_db:get(acl, Resource) of
              {ok, A} ->
                  A;
              %% if acl record wasn't created already
              {error, notfound} ->
                  A = #acl{id = Resource, resource=Resource},
                  nsm_db:put(A),
                  A
          end,

    EntryId = {Accessor, Resource},

    case nsm_db:get(acl_entry, EntryId) of
        %% there is no entries for specified Acl and Accessor, we have to add it
        {error, notfound} ->
            Next = undefined,
            Prev = case Acl#acl.top of
                       undefined ->
                           undefined;

                       Top ->
                           case nsm_db:get(acl_entry, Top) of
                               {ok, TopEntry} ->
                                   EditedEntry = TopEntry#acl_entry{next = EntryId},
                                   nsm_db:put(EditedEntry), % update prev entry
                                   TopEntry#acl_entry.id;

                               {error, notfound} ->
                                   undefined
                           end
                   end,

            %% update acl with top of acl entries list
            nsm_db:put(Acl#acl{top = EntryId}),

            Entry  = #acl_entry{id = EntryId,
                                entry_id = EntryId,
                                accessor = Accessor,
                                action = Action,
                                next = Next,
                                prev = Prev},

            ok = nsm_db:put(Entry),
            Entry;

        %% if acl entry for Accessor and Acl is defined - just change action
        {ok, AclEntry} ->
            nsm_db:put(AclEntry#acl_entry{action = Action}),
            AclEntry
    end.


play_record_add_entry(TeamId, UserId, TournamentId, GameId) ->
    {ok,Team} = nsm_db:get(team, TeamId),
    EntryId = nsm_db:next_id("play_record",1),
    Prev = undefined,
    case Team#team.play_record of
        undefined ->
            Next = undefined;
	X ->
	    case nsm_db:get(play_record, erlang:integer_to_list(X)) of
	       {ok, TopEntry} ->
		    Next = TopEntry#play_record.id,
		    EditedEntry = #play_record{
		      id = TopEntry#play_record.id,
                      team = TopEntry#play_record.team,
                      tournament = TopEntry#play_record.tournament,
                      game_id = TopEntry#play_record.game_id,
                      score_points = TopEntry#play_record.score_points,
                      who = TopEntry#play_record.who,
                      next = TopEntry#play_record.next,
                      prev = EntryId},
                    nsm_db:put(EditedEntry); % update prev entry
            {error,notfound} -> Next = undefined
	    end
    end,

    nsm_db:put(#team{ id = TeamId, name = Team#team.name, play_record = EntryId}), % update teaam top with current

    Entry  = #play_record{id = EntryId,
                    team = TeamId,
                    who = UserId,
                    tournament = TournamentId,
                    game_id = GameId,
                    next = Next,
                    prev = Prev},

    case nsm_db:put(Entry) of
        ok ->
            {ok, Entry}
    end.

tournament_pop_waiting_player(TournamentID) ->
    {ok,Tournament} = nsm_db:get(tournament, erlang:integer_to_list(TournamentID)),
    Top = case Tournament#tournament.waiting_queue of
        undefined ->
            Prev = undefined;
        X ->
	    case nsm_db:get(play_record, erlang:integer_to_list(X)) of
		{ok,TopEntry} -> Prev = TopEntry#play_record.next, TopEntry;
		{error, notfound} -> Prev = undefined
	    end
    end,

    nsm_db:put(Tournament#tournament{id = TournamentID,
                                        waiting_queue = Prev
                                       }),
    Top.

join_tournament(UserId, TournamentId) ->
    {ok,Tournament} = nsm_db:get(tournament,erlang:integer_to_list(TournamentId)),
    {ok,User} = nsm_db:get(user, UserId),
    play_record_add_entry(User#user.team,User#user.username,TournamentId,undefined),
    EntryId = nsm_db:next_id("play_record",1),
    Next = undefined,
    case Tournament#tournament.last of
        undefined ->
            Prev = undefined;
        X ->
            case nsm_db:get(play_record, erlang:integer_to_list(X)) of
                {ok, TopEntry} ->
                    Prev = TopEntry#play_record.id,
                    EditedEntry = #play_record{
                        id = TopEntry#play_record.id,
                        team = TopEntry#play_record.team,
                        tournament = TopEntry#play_record.tournament,
                        game_id = TopEntry#play_record.game_id,
                        score_points = TopEntry#play_record.score_points,
                        who = TopEntry#play_record.who,
                        next = EntryId,
                        prev = TopEntry#play_record.prev},
                    nsm_db:put(EditedEntry); % update prev entry
                {error,notfound} -> Prev = undefined
            end
    end,


    nsm_db:put(Tournament#tournament{
                    waiting_queue = case Tournament#tournament.waiting_queue of
                                        undefined -> EntryId;
                                        _NotEmpty -> _NotEmpty
                                    end,
                    last = EntryId
                   }),

    Entry  = #play_record{id = EntryId,
                    team = undefined,
                    who = UserId,
                    game_id = undefined,
                    tournament = TournamentId,
                    next = Next,
                    prev = Prev},

    case nsm_db:put(Entry) of
        ok ->
            {ok, Entry}
    end.

user_tournaments(UID) -> user_tournaments_list(UID).

tournament_waiting_queue(TID) ->
    C = riak_client(),
    RA = nsm_db:get(tournament, TID),
    case RA of
        {ok,RO} -> riak_read_tournament_waiting_queue(C, RO#tournament.waiting_queue, []);
        {error, _} -> []
    end.

user_tournaments_list(TID) when is_list(TID) ->
    C = riak_client(),
    RA = nsm_db:get(user,TID),
    case RA of
        {ok,RO} ->
             case nsm_db:get(team,RO#user.team) of
                {ok,RO2} -> List = riak_read_tournament_waiting_queue(C, RO2#team.play_record, []),
                            case List of
                               [] -> [];
                               _X ->
                                 [ begin
                                     {ok,Tour} = nsm_db:get(tournament, El#play_record.tournament),
                                     {Tour, 0}
                                   end || El <- List]
                            end;
                {error, _} -> []
             end;
        {error, _} -> []
    end;
user_tournaments_list(TID) ->
    user_tournaments_list(TID#user.username).

riak_read_tournament_waiting_queue(_, undefined, Result) -> Result;
riak_read_tournament_waiting_queue(C, Next, Result) ->
    RA = nsm_db:get(play_record,Next),
    case RA of
        {ok,RO} ->
            riak_read_tournament_waiting_queue(C, RO#play_record.next, Result ++ [RO]);
        {error,notfound} -> Result
    end.

-spec entry_by_id(term()) -> {ok, #entry{}} | {error, not_found}.
entry_by_id(EntryId) -> nsm_db:get(entry, EntryId).

-spec comment_by_id({{EntryId::term(), FeedId::term()}, CommentId::term()}) -> {ok, #comment{}}.
comment_by_id(CommentId) -> nsm_db:get(CommentId).

-spec comments_by_entry(EId::{string(), term()}) -> [#comment{}].
comments_by_entry({EId, FId}) ->
    case nsm_db:entry_by_id({EId, FId}) of
        {ok, #entry{comments_rear = undefined}} ->
            [];
        {ok, #entry{comments_rear = First}} ->
            lists:flatten(read_comments_rev(First));
        _ ->
            []
    end.

purchases(UserId) -> purchases_in_basket(UserId, undefined, 1000).

get_purchases_by_user(UserId, Count, States) -> get_purchases_by_user(UserId, undefined, Count, States).
get_purchases_by_user(UserId, Start, Count, States) ->
    List = purchases_in_basket(UserId, Start, Count),
    case States == all of
        true -> List;
        false -> [P||P<-List, lists:member(P#membership_purchase.state, States)]
    end.

purchases_in_basket(UserId, undefined, PageAmount) ->
    case nsm_db:get(user_purchase, UserId) of
        {ok, O} -> purchases_in_basket(UserId, O#user_purchase.top, PageAmount);
        {error, notfound} -> []
    end;
purchases_in_basket(UserId, StartFrom, Limit) ->
    case nsm_db:get(membership_purchase,StartFrom) of
        {ok, #membership_purchase{next = N}=P} -> [ P | riak_traversal(membership_purchase, #membership_purchase.next, N, Limit)];
        X -> []
    end.

read_comments(undefined) -> [];
read_comments([#comment{comments = C} | Rest]) -> [read_comments(C) | read_comments(Rest)];
read_comments(C) -> riak_traversal(comment, #comment.prev, C, all).

read_comments_rev(undefined) -> [];
read_comments_rev([#comment{comments = C} | Rest]) -> [read_comments_rev(C) | read_comments_rev(Rest)];
read_comments_rev(C) -> riak_traversal(comment, #comment.next, C, all).

riak_traversal( _, _, undefined, _) -> [];
riak_traversal(_, _, _, 0) -> [];
riak_traversal(RecordType, PrevPos, Next, Count)->
    case nsm_riak:get(RecordType, Next) of
        {error,notfound} -> [];
        {ok, R} ->
            Prev = element(PrevPos, R),
            Count1 = case Count of C when is_integer(C) -> C - 1; _-> Count end,
            [R | riak_traversal(RecordType, PrevPos, Prev, Count1)]
    end.

riak_read_acl_entries(_, undefined, Result) -> Result;
riak_read_acl_entries(C, Next, Result) ->
    NextStr = io_lib:format("~p",[Next]),
    RA = C:get(<<"acl_entry">>,erlang:list_to_binary(NextStr)),
    case RA of
         {ok,RO} -> O = riak_object:get_value(RO), riak_read_acl_entries(C, O#acl_entry.prev, Result ++ [O]);
         {error,notfound} -> Result
    end.

entries_in_feed(FeedId, undefined, PageAmount) ->
    case nsm_db:get(feed, FeedId) of
        {ok, O} -> riak_traversal(entry, #entry.prev, O#feed.top, PageAmount);
        {error, notfound} -> []
    end;
entries_in_feed(FeedId, StartFrom, PageAmount) ->
    %% construct entry unic id
    case nsm_db:get(entry,{StartFrom, FeedId}) of
        {ok, #entry{prev = Prev}} -> riak_traversal(entry, #entry.prev, Prev, PageAmount);
        _ -> []
    end.

acl_entries(AclId) ->
    C = riak_client(),
    [AclStr] = io_lib:format("~p",[AclId]),
    RA = C:get(<<"acl">>, erlang:list_to_binary(AclStr)),
    case RA of
        {ok,RO} ->
            O = riak_object:get_value(RO),
            riak_read_acl_entries(C, O#acl.top, []);
        {error, notfound} -> []
    end.


feed_direct_messages(_FId, Page, PageAmount, CurrentUser, CurrentFId) ->
    Page, PageAmount, CurrentUser, CurrentFId,
    [].

%% @private
%check_purchase_by_user_double(User, PurchaseId) ->
%    case nsm_db:get(membership_purchase_by_user, User) of
%        {ok, {_, _, undefined}} ->
%            false;
%        {ok, {_, _, StartFrom}} ->
%            check_purchase_by_user_double(User, PurchaseId, StartFrom);
%        _ -> 
%            false
%    end.

%check_purchase_by_user_double(User, PurchaseId, This) ->
%    if
%        This == last ->
%            false;
%        PurchaseId == This ->
%            true;
%        true ->
%            {ok, {_, _, Next}} = nsm_db:get({membership_purchase_by_user, User}, This),
%            check_purchase_by_user_double(User, PurchaseId, Next)
%    end.

%add_purchase_by_user(UserId, PurchaseId) ->
%    case check_purchase_by_user_double(UserId, PurchaseId) of
%        true ->
%            ok; % error actually
%        false ->
%            Top = case nsm_db:get(membership_purchase_by_user, UserId) of
%                      {ok, {_, UserId, undefined}} ->
%                          last;
%                      {ok, {_, UserId, CurrentTop}} ->
%                          CurrentTop;
%                      _ ->
%                          last
%                  end,
%            if
%                Top /= PurchaseId ->
%                    %% current purcase became top
%                    nsm_db:put({membership_purchase_by_user, UserId, PurchaseId}),
%                    %% previos purchase became next of the top
%                    nsm_db:put({{membership_purchase_by_user, UserId}, PurchaseId, Top});
%                true ->
%                    ok
%           end
%    end.

%get_purchases_by_user(_UserId, Count, States) -> get_purchases_by_user(_UserId, undefined, Count, States).
%get_purchases_by_user(_UserId, _StartFrom, 0, _States) -> [];
%get_purchases_by_user(UserId, undefined, Count, States) ->
%    case nsm_db:get(membership_purchase_by_user, UserId) of
%        {ok, {_, UserId, undefined}} ->
%            [];
%        {ok, {_, UserId, Top}} ->
%            io:format("TOP: ~p~n", [Top]),
%            get_purchases_by_user(UserId, Top, Count, States);
%        _ ->
%            []
%    end;
%get_purchases_by_user(_UserId, last, _Count, _States) ->
%    [];
%get_purchases_by_user(UserId, PurchaseId, Count, States) ->
%    {ok, {_, _, NextId}} = nsm_db:get({membership_purchase_by_user, UserId},
%                                         PurchaseId),
%
%    case nsm_db:get(membership_purchase, PurchaseId) of
%        {ok, Purchase} when States == all ->
%            [Purchase | get_purchases_by_user(UserId, NextId, Count-1, States)];
%
%        {ok, Purchase} when is_list(States) ->
%            #membership_purchase{state = S} = Purchase,
%            case lists:member(S, States) of
%                true ->
%                    [Purchase | get_purchases_by_user(UserId, NextId, Count-1, States)];
%                false ->
%                    get_purchases_by_user(UserId, NextId, Count, States)
%            end;
%
%        {error, _} ->
%            ?WARNING("purchase ~p removed but still present in ~p list",
%                     [PurchaseId, UserId]),
%            get_purchases_by_user(UserId, NextId, Count, States)
%    end.


-spec put_into_invitation_tree(Parent::string()|{root}, User::string(),
                             InviteCode::string()) -> #invitation_tree{}.

put_into_invitation_tree(Parent, User, InviteCode) ->
    URecord =  #invitation_tree{user = User,
                                invite_code = InviteCode,
                                parent = Parent},

    case nsm_db:get(invitation_tree, Parent) of
        {ok, #invitation_tree{first_child = TopChild} = P} ->
            nsm_db:put(P#invitation_tree{first_child = User}),
            URecord1 = URecord#invitation_tree{next_sibling = TopChild},
            nsm_db:put(URecord1),
            URecord1;
        _ ->
            R = case Parent of
                    ?INVITATION_TREE_ROOT ->
                        #invitation_tree{user = ?INVITATION_TREE_ROOT};
                    _ ->
                        put_into_invitation_tree(?INVITATION_TREE_ROOT,
                                                 Parent, undefined)
                end,
            nsm_db:put(R#invitation_tree{first_child = User}),
            nsm_db:put(URecord),
            URecord
    end.


-spec invitation_tree(StartFrom::string()|{root}, Depth::integer()|all) ->
          [#invitation_tree{}].

invitation_tree(_, -1) ->
    [];
invitation_tree(none, _) ->
    [];
invitation_tree(UserId, Depth) ->
    case nsm_db:get(invitation_tree, UserId) of
        {ok, #invitation_tree{} = ITreeU} ->
            FirstChild = ITreeU#invitation_tree.first_child,
            SiblingId = ITreeU#invitation_tree.next_sibling,
            Depth1 = case Depth of
                         all ->
                             Depth;
                         _ ->
                             Depth - 1
                     end,
            [ITreeU#invitation_tree{children=invitation_tree(FirstChild, Depth1)}|
                                       invitation_tree(SiblingId, Depth)];
        {error, _} ->
            []
    end.

%% @doc delete link to child from children list
invitation_tree_delete_child_link(RootUser, User) ->
    case invitation_tree(RootUser, 1) of
        [#invitation_tree{first_child = User} = Root] ->
            Root1 = Root#invitation_tree{children = []},
            case nsm_db:get(invitation_tree, User) of
                {ok, #invitation_tree{next_sibling = none}} ->
                    nsm_db:put(Root1#invitation_tree{first_child = none});
                {ok, #invitation_tree{next_sibling = Next}} ->
                    nsm_db:put(Root1#invitation_tree{first_child = Next});
                _ ->
                    ok
            end;

        [#invitation_tree{children = Children}] ->
            case
                [E || #invitation_tree{next_sibling = NS, user = U} = E <- Children,
                      NS == User orelse U == User] of

                [Before, Exactly] ->
                    NextSibling =  Exactly#invitation_tree.next_sibling,
                    nsm_db:put(Before#invitation_tree{next_sibling = NextSibling,
                                                         children = []});
                _ ->
                    ok
            end;
        _ ->
            ok
    end.


