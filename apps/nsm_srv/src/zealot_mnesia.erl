%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <fernando.benavides@inakanetworks.com>
%%% @copyright (C) 2011, Gleb Peregud
%%% @doc Module that provides unified and abstract access to db for all other kakaweb modules
%%% @end
%%%-------------------------------------------------------------------
-module(zealot_mnesia).

-include("config.hrl").
-include("user.hrl").
-include("feed.hrl").
-include("acl.hrl").
-include("invite.hrl").
-include("attachment.hrl").
-include("user_counter.hrl").
-include("table.hrl").
-include("uri_translator.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([start/0, stop/0, initialize/0, delete/0]).
-export([init_db/0]).

-export([void/1]).

-export([put/1, count/1, get/2, select/2, multi_select/2, all/1, next_id/1, next_id/2, delete/1, delete/2]).
-export([delete_browser_counter_older_than/1,browser_counter_by_game/1, acl_add_entry/3,
         unused_invites/0,
         user_by_verification_code/1, user_by_email/1, user_by_facebook_id/1, user_by_username/1,
         membership/2, get_save_tables/1, save_game_table_by_id/1, get_word/1, list_membership_count/1,
         add_to_group/3, remove_from_group/2, list_membership/1, list_group_users/1,
         feed_add_entry/4, feed_add_direct_message/4, feed_entries/1, feed_entries/2, feed_entries/3, feed_entries/5,
         entry_by_id/1, comment_by_id/1, comments_by_entry/1, feed_direct_messages/5,
         subscribe_user/2, remove_subscription/2, list_subscriptions/1, list_subscription_me/1, is_user_subscribed/2,
         invite_code_by_issuer/1, invite_code_by_user/1, create_table/3, add_table_index/2,get_translation/1]).

% maintanance

-spec start() -> ok.
start() ->
    mnesia:start(),
    mnesia:change_table_copy_type(schema, node(), disc_copies).

-spec stop() -> 'stopped' | {'error',_}.
stop() -> mnesia:stop().

-spec initialize() -> ok.
initialize() -> mnesia:create_schema([node()]).

-spec delete() -> ok.
delete() -> mnesia:delete_schema([node()]).

-spec init_db() -> ok.
init_db() ->
    %% acl -----------------------------------------------------------------------------------------
    ok = create_table(acl, record_info(fields, acl), [{storage, permanent}]),
    %% comment -------------------------------------------------------------------------------------
    ok = create_table(comment, record_info(fields, comment), [{storage, permanent},
                                                              {type, ordered_set}]),
    ok = add_table_index(comment, entry_id),
    %% feed ----------------------------------------------------------------------------------------
    ok = create_table(feed, record_info(fields, feed), [{storage, permanent}]),

    ok = create_table(entry, record_info(fields, entry), [{storage, permanent},
                                                          {type, ordered_set}]),
    ok = add_table_index(entry, feed_id),
    ok = add_table_index(entry, entry_id),
    ok = add_table_index(entry, from),
    %% forget --------------------------------------------------------------------------------------
    ok = create_table(forget_password, record_info(fields, forget_password), [{storage, permanent}]),

    ok = create_table(prohibited, record_info(fields, prohibited), [{storage, permanent},
                                                                    {type, bag}]),
    %% group -----------------------------------------------------------------------------------
    ok = create_table(group_member, record_info(fields, group_member), [{storage, permanent},
                                                                        {type, bag}]),
    ok = create_table(group_member_rev, record_info(fields, group_member_rev), [{storage, permanent},
                                                                                {type, bag}]),
    ok = create_table(group, record_info(fields, group), [{storage, permanent}]),
    ok = add_table_index(group_member, id),
    %% invite ----------------------------------------------------------------------------------
    ok = create_table(invite_code,
                      record_info(fields, invite_code),
                      [{storage, permanent}]),
    ok = add_table_index(invite_code, issuer),
    ok = add_table_index(invite_code, created_user),
    %% user ----------------------------------------------------------------------------------
    ok = create_table(user, record_info(fields, user), [{storage, permanent}]),
    ok = create_table(user_status, record_info(fields, user_status), [{storage, permanent}]),
    ok = create_table(user_counter, record_info(fields, user_counter), [{storage, permanent}]),
    ok = create_table(subscription, record_info(fields, subscription), [{storage, permanent},
                                                                        {type, bag}]),
    ok = create_table(subscription_rev, record_info(fields, subscription_rev), [{storage, permanent},
                                                                                {type, bag}]),
    ok = add_table_index(user, verification_code),
    ok = add_table_index(user, facebook_id),
    ok = add_table_index(user, email),
    %% feed_attachment ----------------------------------------------------------------------------
    ok = create_table(uploads, record_info(fields, uploads),
                      [{storage, permanent}, {type, set}]),
    %% table_manager -------------------------------------------------------------------------------
    ok = create_table(save_game_table, record_info(fields, save_game_table), [{storage, permanent},
                                                                              {type, bag}]),
    ok = add_table_index(save_game_table, id),
    %% user_counter --------------------------------------------------------------------------------
    ok = create_table(browser_counter, record_info(fields, browser_counter), [{storage, permanent},
                                                                              {type, bag}]),
    %% seq_id --------------------------------------------------------------------------------------
    ok = create_table(id_seq, record_info(fields, id_seq), [{storage, permanent}]),

    ok = create_table(feature, record_info(fields, feature), [{storage, permanent}]),

%    add_seq_ids(),

    %% uri_translations ----------------------------------------------------------------------------
    ok = create_table(ut_word, record_info(fields, ut_word), [{storage, permanent}, {type, bag}]),
    ok = create_table(ut_translation, record_info(fields, ut_translation), [{storage, permanent},
                                                                            {type, ordered_set}]),
%    add_translations(),

    %% like_entry ----------------------------------------------------------------------------------
    ok = create_table(like_entry, record_info(fields, like_entry), [{storage, permanent}]),
    ok = add_table_index(like_entry, entry_id),
    ok = add_table_index(like_entry, feed_id),

%    add_sample_users(),
    nsm_srv_membership_packages:create_storage(),

    ok.

% put

-spec put(tuple() | [tuple()]) -> ok.
put(Records) when is_list(Records) ->
    void(fun() -> lists:foreach(fun mnesia:write/1, Records) end);
put(Record) ->
    put([Record]).

% delete

-spec delete(tuple() | [tuple()]) -> ok.
delete(Keys) when is_list(Keys) ->
    void(fun() -> lists:foreach(fun mnesia:delete_object/1, Keys) end);
delete(Keys) ->
    delete([Keys]).

-spec delete(atom(), term()) -> ok.
delete(Tab, Key) ->
    mnesia:transaction(fun()-> mnesia:delete({Tab, Key}) end),
    ok.

% search

-spec multi_select(atom(), [term()]) -> [tuple()].
multi_select(RecordName, Keys) when is_list(Keys) ->
    flatten(fun() -> [mnesia:read({RecordName, Key}) || Key <- Keys] end).
-spec select(atom(), term()) -> [tuple()].

select(From, PredicateFunction) when is_function(PredicateFunction) ->
    exec(qlc:q([Record || Record <- mnesia:table(From), apply(PredicateFunction, [Record])]));
select(From, [{where, Fn}, {order, {Idx, Order}}]) ->
    exec(qlc:q([R || R <- qlc:keysort(Idx, mnesia:table(From), [{order, Order}]), apply(Fn, [R])]));
select(From, [{where, Fn}, {order, {Idx, Order}}, {limit, {1, Length}}]) ->
    {atomic, Recs} = mnesia:transaction(fun()->
        QC = qlc:cursor(qlc:q(
            [R || R <- qlc:keysort(Idx, mnesia:table(From), [{order, Order}]), apply(Fn, [R])])),
        Ret = qlc:eval(qlc:next_answers(QC, Length)),
        qlc:delete_cursor(QC),
        Ret
    end),
    Recs;
select(From, [{where, Fn}, {order, {Idx, Order}}, {limit, {Offset, Length}}]) ->
    {atomic, Recs} = mnesia:transaction(fun()->
        QC = qlc:cursor(qlc:q(
            [R || R <- qlc:keysort(Idx, mnesia:table(From), [{order, Order}]), apply(Fn, [R])])),
        qlc:next_answers(QC, Offset - 1),
        Ret = qlc:eval(qlc:next_answers(QC, Length)),
        qlc:delete_cursor(QC),
        Ret
    end),
    Recs;
select(RecordName, Key) ->
    many(fun() -> mnesia:read({RecordName, Key}) end).

% get

-spec get(atom(), term()) -> {ok, tuple()} | {error, not_found | duplicated}.
get(RecordName, Key) ->
    just_one(fun() -> mnesia:read(RecordName, Key) end).

% translation

get_word(Word) ->
    case mnesia:transaction(fun() -> mnesia:match_object(#ut_word{english = Word, lang = '_', word = Word}) end) of
        {atomic, []} -> {error, not_found};
        {atomic, [R]} -> {ok, R};
        {atomic, [A|_]} -> A;
        _ -> {error, not_found}
    end.

get_translation({Lang, Word}) ->
    get(ut_translation, {Lang, Word}).

% aggregators

-spec count(atom()) -> non_neg_integer().
count(RecordName) ->
    mnesia:table_info(RecordName, size).

-spec all(atom()) -> [tuple()].
all(RecordName) ->
    flatten(fun() ->
                    Lists = mnesia:all_keys(RecordName),
                    [ mnesia:read({RecordName, G}) || G <- Lists ]
            end).

% id generator

-spec next_id(list()) -> pos_integer().
next_id(RecordName) ->
    next_id(RecordName, 1).

-spec next_id(list(), integer()) -> pos_integer().
next_id(RecordName, Incr) ->
    [RecordStr] = io_lib:format("~p",[RecordName]),
    mnesia:dirty_update_counter({id_seq, RecordStr}, Incr).

% browser counter

-spec delete_browser_counter_older_than(pos_integer()) -> ok.
delete_browser_counter_older_than(MinTS) ->
    MatchHead = #browser_counter{minute='$1', _ = '_'},
    Guard = {'<', '$1', MinTS},
    Result = '$_',
    Fun = fun() ->
          List = mnesia:select(browser_counter, [{MatchHead, [Guard], [Result]}]),
          lists:foreach(fun(X) ->
                    mnesia:delete_object(X)
                end, List)
      end,
    void(Fun).

-spec browser_counter_by_game(atom()) -> [#browser_counter{}].
browser_counter_by_game(Game) ->
     {atomic, Result} = mnesia:transaction(fun() -> mnesia:match_object(#browser_counter{game=Game,_= '_'}) end),
     Result.

% invites

unused_invites() ->
    {atomic, Result} =
        mnesia:transaction(
          fun() ->
                  length(mnesia:match_object(#invite_code{created_user=undefined, _='_'}))
          end),
    Result.

user_by_verification_code(Code) ->
    just_one(fun() -> mnesia:match_object(#user{verification_code = Code, _='_'}) end).

user_by_facebook_id(FBId) ->
    just_one(fun() -> mnesia:index_read(user, FBId, facebook_id) end).

user_by_email(Email) ->
    just_one(fun() -> mnesia:index_read(user, Email, email) end).

user_by_username(Name) ->
    just_one(fun() -> mnesia:match_object(#user{username = Name, _='_'}) end).

invite_code_by_issuer(User) ->
    just_one(fun() -> mnesia:match_object(#invite_code{issuer = User, _='_'}) end).

invite_code_by_user(User) ->
    just_one(fun() -> mnesia:match_object(#invite_code{created_user = User, _='_'}) end).

% groups

add_to_group(UId, GId, Type) ->
    zealot_db:put([#group_member{who = UId,
                                 group = GId,
                                 id = {UId, GId},
                                 type = Type},
                   #group_member_rev{group = GId,
                                     who = UId,
                                     type = Type}]).

remove_from_group(UId, GId) ->
    throw({error, needs_fix}),
    Type = zzzz,
    zealot_db:delete([#group_member{who = UId,
                                    group = GId,
                                    id = {UId, GId},
                                    type = Type},
                      #group_member_rev{group = GId,
                                        who = UId,
                                        type = Type}]).

list_membership(#user{username = UId}) ->
    list_membership(UId);
list_membership(UId) when is_list(UId) ->
    zealot_db:select(group_member, UId).

list_membership_count(#user{username = UId}) ->
    list_membership_count(UId);
list_membership_count(UId) when is_list(UId) ->
    [{G, length(list_group_users(element(3, G)))} || G <-zealot_db:select(group_member, UId)].

list_group_users(GId) ->
    zealot_db:select(group_member_rev, GId).

membership(UserId, GroupId) ->
    just_one(fun() -> mnesia:match_object(#group_member{id = {UserId, GroupId}, _='_'}) end).

% game info

get_save_tables(UId) ->
    zealot_db:select(save_game_table,UId).

save_game_table_by_id(Id) ->
    just_one(fun() -> mnesia:match_object(#save_game_table{id = Id, _='_'}) end).

% aux

-spec just_one(fun(() -> [tuple()])) -> {ok, tuple()} | {error, not_found | duplicated}.
just_one(Fun) ->
    case mnesia:transaction(Fun) of
        {atomic, []} -> {error, not_found};
        {atomic, [R]} -> {ok, R};
        {atomic, [_|_]} -> {error, duplicated};
        _ -> {error, not_found}
    end.

-spec flatten(fun(() -> [[tuple()]])) -> [tuple()].
flatten(Fun) ->
    case mnesia:transaction(Fun) of
        {atomic, R} -> lists:flatten(R);
        _ -> []
    end.

-spec many(fun(() -> [tuple()])) -> [tuple()].
many(Fun) ->
    case mnesia:transaction(Fun) of
        {atomic, R} -> R;
        _ -> []
    end.

-spec void(fun(() -> ok)) -> ok | {error, term()}.
void(Fun) ->
    case mnesia:transaction(Fun) of
        {atomic, ok} -> ok;
        {aborted, Error} -> {error, Error}
    end.

create_table(Record, RecordInfo,  Opts0) ->
    Attr = [{attributes, RecordInfo}],
    Opts = transform_opts(Opts0),
    AllOpts = lists:concat([Opts, Attr]),
    case mnesia:create_table(Record, lists:flatten(AllOpts)) of
        {atomic,ok}                          -> ok;
        {aborted, {already_exists, Record}}  -> ok;
        {aborted, Err}                       -> {error, Err}
    end.

add_table_index(Record, Field) ->
    case mnesia:add_table_index(Record, Field) of
        {atomic, ok}                        -> ok;
        {aborted,{already_exists,Record,_}} -> ok;
        {aborted, Err}                       -> {error, Err}
    end.

transform_opts(Opts) ->
    transform_opts(Opts, []).
transform_opts([], Acc) ->
    lists:reverse(Acc);
transform_opts([{storage, Value} | Rest], Acc0) ->
    NewOpts = storage_to_mnesia_type(Value),
    Acc = [NewOpts | Acc0],
    transform_opts(Rest, Acc);
transform_opts([Other | Rest], Acc0) ->
    Acc = [Other | Acc0],
    transform_opts(Rest, Acc).

storage_to_mnesia_type(permanent) ->
    {disc_copies, [node()]};
storage_to_mnesia_type(temporary) ->
    {ram_copies, [node()]};
storage_to_mnesia_type(ondisk) ->
    {disc_only_copies, [node()]}.

% qcl query helper

exec(Q) ->
    F = fun() -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.

% subscriptions

subscribe_user(MeId, FrId) ->
   zealot_db:put([#subscription{who = MeId, whom = FrId},
                  #subscription_rev{whom = FrId, who = MeId}]).

remove_subscription(MeId, FrId) ->
    zealot_db:delete([#subscription{who = MeId, whom = FrId},
                      #subscription_rev{whom = FrId, who = MeId}]).

list_subscriptions(#user{username = UId}) -> list_subscriptions(UId);
list_subscriptions(UId) when is_list(UId) -> select(subscription,UId).
list_subscription_me(UId) -> select(subscription_rev, UId).

is_user_subscribed(Who, Whom) ->
    case select(subscription,fun(#subscription{who=W1,whom=W2}) when W1=:=Who,W2=:=Whom->true;(_)->false end) of
        [] -> false
        ;_ -> true
    end.

% feeds

feed_add_direct_message(FId, User, Desc, Medias) ->
    EId = zealot_db:next_id(entry, 1),
    Entry  = #entry{id = {EId, FId},
                    entry_id = EId,
                    feed_id = FId,
                    from = User,
                    media = Medias,
                    created_time = now(),
                    description = Desc,
                    type = {user, direct}},

    ModEntry = feedformat:format(Entry),
    case zealot_db:put(ModEntry) of
        ok ->
            {ok, ModEntry}
        % ;Error -> {error, Error} %% put always return true
    end.

feed_add_entry(FId, User, Desc, Medias) ->
    EId = zealot_db:next_id(entry, 1),
    Entry  = #entry{id = {EId, FId},
                    entry_id = EId,
                    feed_id = FId,
                    from = User,
                    media = Medias,
                    created_time = now(),
                    description = Desc},

    ModEntry = feedformat:format(Entry),
    case zealot_db:put(ModEntry) of
        ok ->
            {ok, ModEntry}
        % ;Error -> {error, Error} %% put always return true
    end.

-spec entry_by_id(term()) -> {ok, #entry{}} | {error, not_found}.
entry_by_id(EntryId) ->
    just_one(fun() -> mnesia:match_object(#entry{entry_id = EntryId, _='_'}) end).

comment_by_id(CommentId) ->
    just_one(fun() -> mnesia:match_object(#comment{comment_id = CommentId, _='_'}) end).

-spec comments_by_entry(term()) -> [#comment{}].
comments_by_entry(EntryId) ->
    lists:reverse(many(fun() -> mnesia:match_object(#comment{entry_id = EntryId, _='_'}) end)).

-spec feed_entries(term()) -> [#entry{}].
feed_entries(FeedId) -> feed_entries(FeedId, '_').

-spec feed_entries(term(), term()) -> [#entry{}].
feed_entries(FeedId, UserId) ->
    lists:reverse(many(fun() ->
                               mnesia:match_object(#entry{feed_id = FeedId, from = UserId, _='_'})
                       end)).

-spec feed_entries(term(), term(), term()) -> [#entry{}].
feed_entries(FId, Page, PageAmount) -> entries_in_feed(FId, Page, PageAmount).

-spec feed_entries(term(), term(), term(), term(), term()) -> [#entry{}].
feed_entries(FId, Page, PageAmount, CurrentUser, CurrentFId) -> entries_in_feed(FId, Page, PageAmount, CurrentUser, CurrentFId).

entries_in_feed(FId, 1, PageAmount) ->
    F=fun(X) when element(4,X)=:=FId-> true;(_)->false end,
    zealot_db:select(entry,[{where, F},{order, {1, descending}},{limit, {1,PageAmount}}]);
entries_in_feed(FId, Page, PageAmount) when is_integer(Page), Page>1->
    Offset=(Page-1)*PageAmount,
    F=fun(X) when element(4,X)=:=FId-> true;(_)->false end,
    zealot_db:select(entry,[{where, F},{order, {1, descending}},{limit, {Offset,PageAmount}}]).
entries_in_feed(FId, 1, PageAmount, CurrentUser, CurrentFId) ->
    F=fun(X) when element(4,X)=:=FId andalso (element(9,X)=:={user,normal} orelse element(1,element(9,X))=:=system)-> true;
         (X) when element(5,X)=:=CurrentUser,element(9,X)=:={user,direct}-> true;
         (X) when element(4,X)=:=CurrentFId,element(9,X)=:={user,direct} -> true;
         (_)->false end,
    zealot_db:select(entry,[{where, F},{order, {1, descending}},{limit, {1,PageAmount}}]);
entries_in_feed(FId, Page, PageAmount, CurrentUser, CurrentFId) when is_integer(Page), Page>1->
    Offset=(Page-1)*PageAmount,
    F=fun(X) when element(4,X)=:=FId andalso (element(9,X)=:={user,normal} orelse element(1,element(9,X))=:=system)-> true;
         (X) when element(5,X)=:=CurrentUser,element(9,X)=:={user,direct}-> true;
         (X) when element(4,X)=:=CurrentFId,element(9,X)=:={user,direct} -> true;
         (_)->false end,
    zealot_db:select(entry,[{where, F},{order, {1, descending}},{limit, {Offset,PageAmount}}]).

feed_direct_messages(_FId, Page, PageAmount, CurrentUser, CurrentFId) when is_integer(Page), Page>0->
    Offset= case (Page-1)*PageAmount of
        0 -> 1
        ;M-> M
    end,
    F=fun(X) when element(5,X)=:=CurrentUser,element(9,X)=:={user,direct}-> true;
         (X) when element(4,X)=:=CurrentFId,element(9,X)=:={user,direct} -> true;
         (_)->false end,
    zealot_db:select(entry,[{where, F},{order, {1, descending}},{limit, {Offset,PageAmount}}]).

acl_add_entry(_A,_B,_C) -> ok.
