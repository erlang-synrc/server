% This is database handling application that hides database access
% and provides high-level rich API to stored data:
%
%    - users and groups
%    - search
%    - subscriptions
%    - feeds and comments
%    - invites
%    - translations
%    - tournaments, teams and play records
%    - table manager
%
% Currently nsm_db supports following store backends:
%
%    - Mnesia
%    - Riak


-module(nsm_db).
-author('Maxim Sokhatsky <maxim@synrc.com>').

-include("config.hrl").
-include("user.hrl").
-include("feed.hrl").
-include("acl.hrl").
-include("tournaments.hrl").
-include("invite.hrl").
-include("attachment.hrl").
-include("user_counter.hrl").
-include("table.hrl").
-include("uri_translator.hrl").
-include("accounts.hrl").
-include("common.hrl").
-include("affiliates.hrl").
-include("membership_packages.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include_lib("nsx_config/include/log.hrl").

-export([start/0, stop/0, initialize/0, delete/0, init_indexes/0,
         init_db/0, put/1, count/1, get/2, get/3, feed_create/0, create_team/1,
         all/1, all_by_index/3, next_id/1, next_id/2, delete/1, delete/2,
         delete_browser_counter_older_than/1,browser_counter_by_game/1,
         unused_invites/0, get_word/1, acl_add_entry/3, acl_entries/1,
         feed_add_entry/5, feed_add_entry/7, feed_add_direct_message/6,
         entries_in_feed/1, entries_in_feed/2, entries_in_feed/3, add_transaction_to_user/2,
         entry_by_id/1, comment_by_id/1, comments_by_entry/1, feed_direct_messages/3,
         add_comment/7, dir/0, purchases/1, transactions/1,
         user_by_verification_code/1, update_user_name/3,
         list_membership/1, list_group_users/1, list_membership_count/1,
         user_by_email/1, user_by_facebook_id/1, user_by_username/1, change_group_name/2,
         membership/2, move_group_members/3, get_group_members_count/1, get_group_members/1,
         get_save_tables/1, save_game_table_by_id/1, invite_code_by_issuer/1, add_invite_to_issuer/2,
         block_user/2, unblock_user/2, list_blocks/1, list_blocked_me/1, is_user_blocked/2,
         add_translations/0,
         select/2, multi_select/2, version/0,
         invite_code_by_user/1, get_translation/1, add_purchases/0,
         tournament_waiting_queue/1, join_tournament/2, tournament_pop_waiting_player/1,
         play_record_add_entry/4,user_tournaments/1, make_rich/1,
         subscriptions_to_subs/0, groups_184_update/0, subscriptions_update/0, fast_timeouts/0, make_admin/1]).

-export([get_purchases_by_user/3, get_purchases_by_user/4,
         put_into_invitation_tree/3, invitation_tree/2]).

-export([add_configs/0]).

-export([save_db/1, load_db/1]).

start() -> DBA = ?DBA, DBA:start().
dir() -> DBA = ?DBA, DBA:dir().
purchases(UserId) -> DBA = ?DBA, DBA:purchases(UserId).
transactions(UserId) -> DBA = ?DBA, DBA:transactions(UserId).
stop() -> DBA = ?DBA, DBA:stop().
initialize() -> DBA = ?DBA, DBA:initialize().
delete() -> DBA = ?DBA, DBA:delete().
init_indexes() -> DBA = ?DBA, DBA:init_indexes().

init_db() ->
    case nsm_db:get(user,"alice") of
       {error,_} ->
            DBA = ?DBA,
            DBA:init_db(),
            add_seq_ids(),
            nsm_accounts:create_account(?SYSTEM_ACCOUNT_ID),
            add_sample_users(),
            add_sample_packages(),
            add_translations(),
            pointing_rules:setup(),
            add_configs(),
            add_affiliates(),
            add_contracts(),
            add_purchases();
       {ok,_} -> ignore
    end.

add_affiliates() ->
%    ?INFO("~w:add_affiliates/0 Started", [?MODULE]),
    Cl=nsm_affiliates:start_client(),
    nsm_affiliates:create_affiliate(Cl, "kunthar"),
    nsm_affiliates:create_affiliate(Cl, "tour1"),
    nsm_affiliates:create_affiliate(Cl, "maxim"),
    nsm_affiliates:reg_follower(Cl, "demo1", "kunthar", 1),
    nsm_affiliates:reg_follower(Cl, "demo2", "kunthar", 1),
    nsm_affiliates:reg_follower(Cl, "kate", "kunthar", 2),
%    nsm_affiliates:reg_follower(Cl, "tour2", "tour1", 1),
%    nsm_affiliates:reg_follower(Cl, "tour3", "tour1", 1),
%    nsm_affiliates:reg_follower(Cl, "tour4", "tour1", 2),
%    nsm_affiliates:reg_follower(Cl, "tour5", "tour1", 2),
%    nsm_affiliates:reg_follower(Cl, "tour6", "tour1", 1),
%    nsm_affiliates:reg_follower(Cl, "tour7", "tour1", 3),
    nsm_affiliates:stop_client(Cl),
%    ?INFO("~w:add_affiliates/0 Finished", [?MODULE]),
    ok.

add_contracts() ->
%    ?INFO("~w:add_contracts/0 Started", [?MODULE]),
    {CurDate, _} = calendar:now_to_local_time(now()),
    CurDateDays = calendar:date_to_gregorian_days(CurDate),
    StartDate = calendar:gregorian_days_to_date(CurDateDays - 15),
    FinishDate = calendar:gregorian_days_to_date(CurDateDays + 15),
%    ok = nsm_affiliates:create_contract("tour1", "Test contract", % THIS BROKES CONSISTENCY ON BOOT
%                                         StartDate, FinishDate,
%                                         2, 10.2),
%    ?INFO("~w:add_contracts/0 Finished", [?MODULE]),
    ok.

add_purchases() ->
%    ?INFO("~w:add_purchases/0 Started", [?MODULE]),
    {ok, Pkg1} = nsm_membership_packages:get_package(1),
    {ok, Pkg2} = nsm_membership_packages:get_package(2),
    {ok, Pkg3} = nsm_membership_packages:get_package(3),
    {ok, Pkg4} = nsm_membership_packages:get_package(4),
    PList = [{"kunthar", Pkg1},{"maxim", Pkg2},{"maxim",Pkg4}, {"kate", Pkg3} ],
    [ok = add_purchase(U, P) || {U, P} <- PList],
%    ?INFO("~w:add_purchases/0 Finished", [?MODULE]),
    ok.

add_purchase(UserId, Package) ->
    {ok, MPId} = nsm_membership_packages:add_purchase(
                         #membership_purchase{user_id=UserId, membership_package=Package }),
%    ?INFO("Purchase Added: ~p",[MPId]),
    nsm_membership_packages:set_purchase_state(MPId, ?MP_STATE_DONE, undefined).



add_seq_ids() ->
    Init = fun(Key) ->
           case nsm_db:get(id_seq, Key) of
                {error, _} -> ok = nsm_db:put(#id_seq{thing = Key, id = 0});
                {ok, _} -> ignore
           end
    end,
    Init("player_scoring"),
    Init("scoring_record"),
    Init("tournament"),
    Init("user_transaction"),
    Init("transaction"),
    Init("team"),
    Init("play_record"),
    Init("membership_purchase"),
    Init("table"),
    Init("acl"),
    Init("acl_entry"),
    Init("feed"),
    Init("entry"),
    Init("like_entry"),
    Init("likes"),
    Init("one_like"),
    Init("comment"),
    Init("save_table").

add_translations() ->
    lists:foreach(fun({English, Lang, Word}) ->
                          ok = nsm_db:put(#ut_word{english = English, lang = "en",  word = English}),
                          ok = nsm_db:put(#ut_word{english = Word,    lang = Lang,  word = Word}),
                          ok = nsm_db:put(#ut_translation{source = {Lang, Word},    word = English}),
                          ok = nsm_db:put(#ut_translation{source = {"en", English}, word = English}),
                          ok = nsm_db:put(#ut_translation{source = {Lang, English}, word = Word}),
              ok
    end, ?URI_DICTIONARY).

add_sample_users() ->
    UserList =
                    [#user{username = "demo1", password="kakara20",
                           name = "Demo", surname = "Nstration", feed = feed_create(),
                           type = admin, direct = feed_create(),
                           sex=m,
                           status=ok,
                           team = create_team("tours"),
                           email="demo1@kakaranet.com"},
                     #user{username = "demo2", password="kakara20",
                           name = "Demo2 User", surname = "Two", feed = feed_create(),
                           type = admin, direct = feed_create(),
                           status=ok,
                           team = create_team("tours"),
                           sex=m},
                     #user{username = "maxim", password="kaka15ra",
                           name = "Maxim", surname = "Sokhatsky", feed = feed_create(),
                           type = admin, direct = feed_create(),
                           sex=m,
                           status=ok,
                           team = create_team("tours"),
                           email="maxim.sokhatsky@gmail.com"},
                     #user{username = "ahmettez", password="kaka15ra",
                           name = "Ahmet", surname = "Tez", feed = feed_create(),
                           type = admin, direct = feed_create(),
                           sex=m,
                           status=ok,
                           team = create_team("tours"),
                           email="tez.ahmettez@gmail.com"},
                     #user{username = "kunthar", password="kaka1224", name = "Kunthar", feed = feed_create(),
                           type = admin, direct = feed_create(),
                           sex=m,
                           status=ok,
                           team = create_team("tours"),
                           email="kunthar@gmail.com"},
                     #user{username = "sustel", password="kaka15ra", name = "Sustel",
                           feed = feed_create(), direct = feed_create(),
                           type = admin,
                           team = create_team("tours"),
                           status=ok,
                           email = "sinanustel@gmail.com",
                           sex=m},
                     #user{username = "kate", password="kaka15ra",
                           name = "Kate", surname = "Foxconn", feed = feed_create(),
                           status=ok, direct = feed_create(),
                           team = create_team("tours"),
                           sex=f},
                     #user{username = "alice", password="kaka15ra",
                           name = "Alicja", surname = "Example", feed = feed_create(),
                           team = create_team("tours"), direct = feed_create(),
                           facebook_id = "1234567890",
                           status=ok,
                           sex=f},
                     #user{username = "commonuser", password="123456",
                           name = "Usert", surname = "Userson", feed = feed_create(),
                           team = create_team("tours"), direct = feed_create(),
                           status=ok,
                           sex="male",
                           location="İstanbul",
                           age={1986,4,3},
                           education="highschool",
                           register_date={1345,14070,852889}
                     },
                     #user{username = "imagia", password="123456",
                           name = "Ima", surname = "Gionari", feed = feed_create(),
                           team = create_team("tours"), direct = feed_create(),
                           status=ok,
                           sex="female",
                           location="Ankara",
                           age={1988,4,3},
                           education="elementary school",
                           register_date={1345,14070,852889}
                     },
                     #user{username = "willbe", password="123456",
                           name = "Will", surname = "Beimagionary", feed = feed_create(),
                           team = create_team("tours"), direct = feed_create(),
                           status=ok,
                           sex="male",
                           location="Ankara",
                           age={1985,8,3},
                           education="Bc. S.",
                           register_date={1345,14070,852889}
                     },
                     #user{username = "serg", password="kaka1224",
                           name = "Serg", surname = "Polkovnikov", feed = feed_create(),
                           team = create_team("tours"), direct = feed_create(),
                           status=ok,
                           sex="male",
                           location="İstanbul",
                           age={1976,1,8},
                           education="Ph. D.",
                           register_date={1345,14070,852889}
                     },
                     #user{username = "doxtop", password="password",
                           feed = feed_create(),
                           name = "Andrii Zadorozhnii",
                           team = create_team("tours"), direct = feed_create(),
                           status=ok,
                           age={1981,9,29},
                           register_date={1345,14071,852889}
                     }],


    ?INFO("creating groups"),

    nsm_users:init_mq("ahmettez", ["kakaranet", "yeniler"]),

    GId1  = nsm_groups:create_group_directly_to_db("ahmettez", "kakaranet", "Kakaranet", "Kakaranet'e Hoşgeldiniz", public),
    GId2  = nsm_groups:create_group_directly_to_db("ahmettez", "yeniler", "Yeniler", "So, you must be new here.", public),

    T1 = nsm_tournaments:create("maxim","TAVLA", "Tavla Turnuvalar",  {2012,4,28},10,  10, undefined,pointing,game_tavla),
    T2 = nsm_tournaments:create("maxim","BATAK", "Batak Challenge",   {2012,4,28},100, 50, undefined,pointing,game_batak),
    T3 = nsm_tournaments:create("maxim","OKEY",  "OKEY Championship", {2012,4,28},1000,100,undefined,pointing,game_okey),

    ?INFO("adding users accounts"),
    [ begin
          nsm_accounts:create_account(Me#user.username),
          nsm_accounts:transaction(Me#user.username, ?CURRENCY_QUOTA, db_opt:get_default_quota(), #ti_default_assignment{}),
          nsm_db:put(Me#user{password = utils:sha(Me#user.password),
                                starred = feed_create(),
                                pinned = feed_create()})
      end || Me <- UserList],
    ?INFO("adding users to groups"),
    [ begin
          nsm_users:init_mq(Me#user.username, [GId1, GId2]),
          %subscribe_user_to_list(Me#user.username, UserList),
          case Me#user.username of
              "ahmettez" -> ok; % ahmettez already in groups, as admin
              _ ->
                  nsm_groups:add_to_group_directly_to_db(Me#user.username, GId1, member),
                  nsm_groups:add_to_group_directly_to_db(Me#user.username, GId2, member)
          end,
          nsm_tournaments:join(Me#user.username, T1),
          nsm_tournaments:join(Me#user.username, T3),
          nsm_tournaments:join(Me#user.username, T2)
      end || Me <- UserList ],
    %% define access for Maxim to feature admin
    nsm_acl:define_access({user, "maxim"},    {feature, admin}, allow),
    nsm_acl:define_access({user_type, admin}, {feature, admin}, allow),

    ?INFO("making all users each other friends"),

    [[case Me == Her of
        true -> ok;
        false -> nsm_users:subscr_user(Me#user.username, Her#user.username)
    end || Her <- UserList] || Me <- UserList],

    {ok, G} = nsm_gifts_vendor:get_gifts(1),
    nsm_gifts_tools:dumb_store(G),

    ok.

add_sample_packages() -> nsm_membership_packages:add_sample_data().
version() -> ?INFO("version: ~p", [?VERSION]).

% blocking

block_user(Who, Whom) -> DBA=?DBA, DBA:block_user(Who, Whom).
list_blocks(Who) -> DBA=?DBA, DBA:list_blocks(Who).
unblock_user(Who, Whom) -> DBA=?DBA, DBA:unblock_user(Who, Whom).
list_blocked_me(Me) -> DBA=?DBA, DBA:list_blocked_me(Me).
is_user_blocked(Who, Whom) -> DBA=?DBA, DBA:is_user_blocked(Who, Whom).

% configs

add_configs() ->
    %% smtp
    nsm_db:put(#config{key="smtp/user",     value="noreply@kakaranet.com"}),
    nsm_db:put(#config{key="smtp/password", value="kakam41l"}),
    nsm_db:put(#config{key="smtp/host",     value="posta.kakaranet.com"}),
    nsm_db:put(#config{key="smtp/port",     value=465}),
    nsm_db:put(#config{key="smtp/with_ssl", value=true}),

    %% accounts
    nsm_db:put(#config{key="accounts/default_quota", value=2000}),
    nsm_db:put(#config{key="accounts/quota_limit/soft",  value=-30}),
    nsm_db:put(#config{key="accounts/quota_limit/hard",  value=-100}),

    %%  purchases
    nsm_db:put(#config{key= "purchase/notifications/email",  value=["gokhan@kakaranet.com"]}).

% put

-spec put(tuple() | [tuple()]) -> ok.
put(Record) ->
    ?INFO("db:put ~p",[Record]),
    DBA=?DBA,
    DBA:put(Record).

% get

-spec get(atom(), term()) -> {ok, tuple()} | {error, not_found | duplicated}.
get(RecordName, Key) ->
    DBA=?DBA,
    case C = DBA:get(RecordName, Key) of
    {ok,_R} ->
        ?INFO("db:get ~p,", [{RecordName, Key}]),
        C;
    A -> A
    end.

get(RecordName, Key, Default) ->
    DBA=?DBA,
    case DBA:get(RecordName, Key) of
	{ok,{RecordName,Key,Value}} ->
	    ?INFO("db:get config value ~p,", [{RecordName, Key, Value}]),
	    {ok,Value};
	{error, _B} ->
	    ?INFO("db:get new config value ~p,", [{RecordName, Key, Default}]),
	    DBA:put({RecordName,Key,Default}),
	    {ok,Default}
    end.

get_word(Word) -> get(ut_word,Word).
get_translation({Lang,Word}) -> DBA=?DBA, DBA:get_translation({Lang,Word}).

% delete

delete(Keys) -> DBA=?DBA, DBA:delete(Keys).
delete(Tab, Key) -> ?INFO("db:delete ~p:~p",[Tab, Key]), DBA=?DBA,DBA:delete(Tab, Key).

% select

multi_select(RecordName, Keys) -> DBA=?DBA,DBA:multi_select(RecordName, Keys).
select(From, PredicateFunction) -> ?INFO("db:select ~p, ~p",[From,PredicateFunction]), DBA=?DBA, DBA:select(From, PredicateFunction).
count(RecordName) -> DBA=?DBA,DBA:count(RecordName).
all(RecordName) -> DBA=?DBA,DBA:all(RecordName).
all_by_index(RecordName, Index, IndexValue) -> DBA=?DBA,DBA:all_by_index(RecordName, Index, IndexValue).

% id generator

next_id(RecordName) -> DBA=?DBA,DBA:next_id(RecordName).
next_id(RecordName, Incr) -> DBA=?DBA,DBA:next_id(RecordName, Incr).

% browser counter

delete_browser_counter_older_than(MinTS) -> DBA=?DBA,DBA:delete_browser_counter_older_than(MinTS).
browser_counter_by_game(Game) -> DBA=?DBA,DBA:browser_counter_by_game(Game).

% invites

unused_invites() -> DBA=?DBA,DBA:unused_invites().
user_by_verification_code(Code) -> DBA=?DBA,DBA:user_by_verification_code(Code).
user_by_facebook_id(FBId) -> DBA=?DBA,DBA:user_by_facebook_id(FBId).
user_by_email(Email) -> DBA=?DBA,DBA:user_by_email(Email).
user_by_username(Name) -> DBA=?DBA,DBA:user_by_username(Name).
add_invite_to_issuer(User, O) -> DBA=?DBA,DBA:add_invite_to_issuer(User, O).
invite_code_by_issuer(User) -> DBA=?DBA,DBA:invite_code_by_issuer(User).
invite_code_by_user(User) -> DBA=?DBA,DBA:invite_code_by_user(User).

% game info

get_save_tables(Id) -> DBA=?DBA,DBA:get_save_tables(Id).
save_game_table_by_id(Id) -> DBA=?DBA,DBA:save_game_table_by_id(Id).

% groups

list_membership(U) -> DBA=?DBA,DBA:list_membership(U).
list_membership_count(U) -> DBA=?DBA,DBA:list_membership_count(U).
list_group_users(U) -> DBA=?DBA,DBA:list_group_users(U).
membership(UserId, GroupId) -> DBA=?DBA,DBA:membership(UserId, GroupId).
move_group_members(Old, New, Name) -> DBA=?DBA,DBA:move_group_members(Old,New,Name).
get_group_members(GId) -> DBA=?DBA,DBA:get_group_members(GId).
get_group_members_count(GId) -> DBA=?DBA,DBA:get_group_members_count(GId).
change_group_name(GId,GName) -> DBA=?DBA,DBA:change_group_name(GId,GName).
update_user_name(UId,Name,Surname) -> DBA=?DBA,DBA:update_user_name(UId,Name,Surname).

% feeds

feed_add_direct_message(FId, User, To, EntryId, Desc, Medias) -> DBA=?DBA,DBA:feed_add_direct_message(FId, User, To, EntryId, Desc, Medias).
feed_add_entry(FId, User, EntryId, Desc, Medias) -> DBA=?DBA,DBA:feed_add_entry(FId, User, EntryId, Desc, Medias).
feed_add_entry(FId, User, To, EntryId, Desc, Medias, Type) -> DBA=?DBA,DBA:feed_add_entry(FId, User, To, EntryId, Desc, Medias, Type).
acl_add_entry(AclId, Accessor, Action) -> DBA=?DBA,DBA:acl_add_entry(AclId, Accessor, Action).
acl_entries(AclId) -> DBA=?DBA,DBA:acl_entries(AclId).
entry_by_id(EntryId) -> DBA=?DBA,DBA:entry_by_id(EntryId).
comment_by_id(CommentId) -> DBA=?DBA,DBA:comment_by_id(CommentId).
comments_by_entry({_EId, _FId} = EntryId) -> DBA=?DBA,DBA:comments_by_entry(EntryId).
entries_in_feed(FeedId) -> DBA=?DBA,DBA:entries_in_feed(FeedId, undefined, all).
entries_in_feed(FeedId, Count) -> DBA=?DBA,DBA:entries_in_feed(FeedId, undefined, Count).
entries_in_feed(FeedId, StartFrom, Count) -> DBA=?DBA, DBA:entries_in_feed(FeedId, StartFrom, Count).
add_comment(FId, User, EntryId, ParentComment, CommentId, Content, Medias) -> DBA=?DBA, DBA:feed_add_comment(FId, User, EntryId, ParentComment, CommentId, Content, Medias).
feed_direct_messages(FId, StartFrom, Count) ->  DBA=?DBA, DBA:entries_in_feed(FId, StartFrom, Count).

% tournaments

tournament_waiting_queue(TID) -> DBA=?DBA, DBA:tournament_waiting_queue(TID).
join_tournament(UID,TID) -> DBA=?DBA, DBA:join_tournament(UID,TID).
tournament_pop_waiting_player(TID) -> DBA=?DBA, DBA:tournament_pop_waiting_player(TID).
play_record_add_entry(TeamId, UserId, Tournament, GameId) -> DBA=?DBA, DBA:play_record_add_entry(TeamId, UserId, Tournament, GameId).
user_tournaments(UID) -> DBA=?DBA, DBA:user_tournaments(UID).

groups_184_update() -> % predefined group creation
    nsm_groups:create_group("ahmettez", "kakaranet", "Kakaranet", "Kakaranet'e Hoşgeldiniz", public),
    nsm_groups:create_group("ahmettez", "yeniler", "Yeniler", "So, you must be new here.", public).

subscriptions_update() -> % public beta
    catch(nsm_db:delete(subscription,"kikiri")),

    Subscriptions = nsm_db:all(subscription),
    lists:foreach(fun (Subscription) ->

       case Subscription of

            {subscription,User,ToList} ->

       ?INFO("User, ToList: ~p ~p",[User,ToList]),
       lists:foreach(fun(Sub) ->
          case Sub of
             {subscription,User,To} ->
                  ?INFO("User, To: ~p ~p",[User,To]),
                  NewSubs = lists:foldl(fun({subscription,A,T},Acc) ->
                            ?INFO("User: ~p",[A]),
                            R = nsm_db:get(user,T),
                            case R of
                                  {ok,{user,_,_,_,_,_,Name,Surname,_,_,_,_,_,_,_,_,_,_,_,_}} ->
				            Acc ++ [{subscription,A,T,[Name,32,Surname]}];
                                   {error,_} -> Acc
                             end
                  end,[],ToList),
                  nsm_db:put({subscription,User,NewSubs,undefined}),
                  ok;
             _ -> io:format("Unknown: ~p",[Sub])
          end
       end, ToList);

           _ -> io:format("new: ~p",[Subscription])

       end

    end, Subscriptions).

subscriptions_to_subs() ->
    [[nsm_db:put(#subs{who=Who, whom=Whom}) || #subscription{who=Who, whom=Whom} <- Subs] 
    || #subscription{who=_, whom=Subs} <- nsm_db:all(subscription)].

%% mebership purchases

add_transaction_to_user(User, Tx) -> DBA=?DBA, DBA:add_transaction_to_user(User, Tx).
get_purchases_by_user(User, Count, States) -> DBA=?DBA, DBA:get_purchases_by_user(User, Count, States).
get_purchases_by_user(User, StartFromPurchase, Count, States) -> DBA=?DBA, DBA:get_purchases_by_user(User, StartFromPurchase, Count, States).

%% invitation tree

%% @doc Put invite to tree. Parent is user who has invited User.

-spec put_into_invitation_tree(Parent::string()|{root}, User::string(), InviteCode::string()) -> ok.

put_into_invitation_tree(Parent, User, InviteCode) -> DBA=?DBA, DBA:put_into_invitation_tree(Parent, User, InviteCode).


%% @doc build invitaion tree. Depth is shows how many levels of children will be
%%      returned. Children will de abbed to children field of the #invitaion_tree
%%      record.

-spec invitation_tree(StartFrom::string()|{root}, Depth::integer()|all) ->
          [#invitation_tree{}].

invitation_tree(StartFrom, Depth) -> DBA=?DBA, DBA:invitation_tree(StartFrom, Depth).

fast_timeouts() ->
    nsm_db:put({config,"games/okey/robot_delay_normal",100}),
    nsm_db:put({config,"games/okey/challenge_timeout_normal",5000}),
    nsm_db:put({config,"games/okey/turn_timeout_normal",200}).

make_admin(User) ->
    {ok,U} = nsm_db:get(User),
    nsm_db:put(U#user{type = admin}).

make_rich(User) -> nsm_accounts:transaction(User, ?CURRENCY_QUOTA, db_opt:get_default_quota() * 100, #ti_default_assignment{}).

feed_create() ->
    FId = nsm_db:next_id("feed", 1),
    ok = nsm_db:put(#feed{id = FId} ),
    FId.

create_team(Name) ->
    TID = nsm_db:next_id("team",1),
    ok = nsm_db:put(Team = #team{id=TID,name=Name}),
    TID.


list_to_term(String) ->
    {ok, T, _} = erl_scan:string(String++"."),
    case erl_parse:parse_term(T) of
        {ok, Term} ->
            Term;
        {error, Error} ->
            Error
    end.

list_buckets() ->
    [list_to_term(B) || B <- nsm_riak:dir()].

save_db(Path) ->
    Data = lists:append([all(B) || B <- list_buckets()]),
    salode:save(Path, Data).

load_db(Path) ->
    nsm_riak:clean(),
    add_seq_ids(),
    AllEntries = salode:load(Path),
    [{_,_,{_,Handler}}] = ets:lookup(config, "riak_client"),
    [case is_tuple(E) of
        false -> skip;
        true ->
            case element(1, E) of
                affiliates_rels -> %%%%%%%%%%%%%%%%%%%% affiliates
                    ?INFO("AR: ~p",[E]),
                    nsm_affiliates:write_affiliate_rel_record(Handler, E);
                affiliates_contracts -> 
                    ?INFO("AC: ~p",[E]),
                    nsm_affiliates:write_contract_record(Handler, E);
                affiliates_purchases -> 
                    ?INFO("AP: ~p",[E]),
                    UserId = E#affiliates_purchases.user_id,
                    ContractId = E#affiliates_purchases.contract_id,
                    Index = [{"owner_bin", term_to_binary(UserId)}, {"contract_bin", term_to_binary(ContractId)}],
                    Object = nsm_affiliates:new_object(?PURCHASES_BUCKET, term_to_binary({ContractId, UserId}), E, Index),
                    ok = nsm_affiliates:write_object(Handler, Object, [if_none_match]);
                affiliates_contract_types -> 
                    ?INFO("AfCT: ~p",[E]),
                    nsm_affiliates:write_contract_type_record(Handler, E);
                gifts_categories -> %%%%%%%%%%%%%%%%%%%% gifts 
                    ?INFO("GiftCat: ~p",[E]),
                    Id = E#gifts_category.id,
                    ParentId = E#gifts_category.parent,
                    Obj1 = riak_object:new(<<"gifts_categories">>, term_to_binary(Id), E),
                    Indices = [{"bucket_bin", <<"gifts_categories">>},
                               {"catparent_bin", term_to_binary(ParentId)}],
                    Meta = dict:store(<<"index">>, Indices, dict:new()),
                    Obj2 = riak_object:update_metadata(Obj1, Meta),
                    ok = Handler:put(Obj2, []);
                gift -> 
                    ?INFO("Gift: ~p",[E]),
                    Id = E#gift.id,
                    Cats = E#gift.categories,
                    Obj1 = riak_object:new(<<"gifts">>, term_to_binary(Id), E),
                    Indices = [{"bucket_bin", <<"gifts">>} |
                             [{"categoty_bin", term_to_binary(CatId)} ||
                              CatId <- lists:usort(Cats)]],
                    Meta = dict:store(<<"index">>, Indices, dict:new()),
                    Obj2 = riak_object:update_metadata(Obj1, Meta),
                    ok = Handler:put(Obj2, []);
                transaction -> %%%%%%%%%%%%%%%%%%%% transaction
                    ?INFO("Tx: ~p",[E]),
                        case E of {transaction,Id,T,Am,R,A,C,I} ->
                         Tx = #transaction{id = Id, commit_time = T,  amount = Am, remitter =R, acceptor = A, currency =C, info = I},
                         %add_transaction_to_user(A,Tx);
                         put(Tx);
                         _ -> skip 
                        end;
                membership_purchase ->
                    ?INFO("Mp: ~p",[E]),
                        case E of {membership_purchase,Id,T,User,S,Pkg,Time,Time2,L,Type} ->
                         Mp = #membership_purchase{id = Id, external_id = T,
                                                   user_id = User, state = S,
                                                   membership_package = Pkg, next = undefined,
                                                   prev = undefined, start_time = Time,
                                                   end_time = Time2, state_log = L,
                                                   info = Type},

                         put(Mp);
                         _ -> skip 
                        end;
                _ -> %%%%%%%%%%%%%%%%%%%% all the rest
                    ?INFO("Reg: ~p",[E]),
                    put(E) 
            end
    end || E <- AllEntries].


