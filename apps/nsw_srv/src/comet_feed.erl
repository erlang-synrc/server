-module(comet_feed).

-define(COMET_POOL, feed).

-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("nsm_db/include/user.hrl").
-include_lib("nsm_db/include/feed.hrl").
-include_lib("nsw_srv/include/common.hrl").
-include_lib("nsw_srv/include/setup.hrl").

-include("elements/records.hrl").

-export([start/4]).

%% TODO: add filtering (direct, likes, etc.)
start(Type, FeedId, FeedOwner, CurrentUser) ->
    wf:comet(fun()->
        %% put info to process dictionary, to later examine who is owner
        %% of the process
        put('**process_description**', [comet_feed, FeedOwner, CurrentUser#user.username]),
        ?INFO("(in comet):start ~p feed comet for =~p. Pid=~p ",
              [Type, FeedOwner, self()]),
        case Type of
            user ->
                nsx_util_notification:subscribe_user(FeedOwner, self());
            group ->
                nsx_util_notification:subscribe_group(FeedOwner, self())
        end,
        comet_update(Type, FeedId, FeedOwner, CurrentUser)
    end,  ?COMET_POOL).

comet_update(Type, FeedId, FeedOwner, CurrentUser) ->
    receive
        {delivery, Route, Message} ->
            ?DBG("feed(~p-~p): delivery. Route ~p, Message: ~p",
                 [FeedOwner, CurrentUser, Route, Message]),
            process_delivery({Type, FeedId, FeedOwner, CurrentUser}, Route, Message);
        'INIT' ->
            ?INFO("feed(~p-~p): init received", [FeedOwner, CurrentUser]);
        Unexpected ->
            ?WARNING("feed(~p-~p): unexpected message: ~p",
                     [FeedOwner, CurrentUser, Unexpected])
    end,
    comet_update(Type, FeedId, FeedOwner, CurrentUser).


%%-----------------------------------------------------------------------------
%% ADD
%%-----------------------------------------------------------------------------

process_delivery({_Type, FeedId, Owner, CurrentUser},
                 ["feed", EntryType, EntryOwner, "entry", EntryId, "add"],
                 [From, Destinations, Desc, Medias]) ->
    To = case From of
             Owner ->
                 Destinations;
             _ ->
                 [D || {_, group} = D <- Destinations]
         end,
    ?PRINT({FeedId, CurrentUser#user.direct, CurrentUser#user.feed, To}),
    CurrentUserName = CurrentUser#user.username,
    CurrentUserDirect = CurrentUser#user.direct,
    if
        %% show added message for user who add direct message
        CurrentUserName == From ->
            add_entry(EntryId, FeedId, From, To, Desc, Medias);
        %% owner look to his direct messages feed
        CurrentUserDirect == FeedId ->
            add_entry(EntryId, FeedId, From, To, Desc, Medias);
        %% show messages in group
        EntryType == "group" ->
            add_entry(EntryId, FeedId, From, To, Desc, Medias);
        %% friend post to his feed
        EntryOwner == From ->
            add_entry(EntryId, FeedId, From, To, Desc, Medias);
        %% user gets direct message not looking into direct message feed?
        Owner == EntryOwner ->
            add_notification(From, Desc);
        true ->
            ok
    end;



%%%% system messages
process_delivery({_Type, FeedId, Owner, _},
                 ["feed", _, _, "entry", EntryId, "add_system"],
                 [From, Destinations, Desc, Medias]) ->
    To = case From of
        Owner ->
            Destinations;
        _ ->
            [D || {_, group} = D <- Destinations]
    end,
    add_system_entry(EntryId, FeedId, From, To, Desc, Medias);



process_delivery({Type, FeedId, Owner, _},
                 ["feed", _Type, _WhoShares, "entry", EntryId, "share"],
                 #entry{entry_id = EntryId, raw_description = Desc, media = Medias,
                        to = Destinations, from = From}) ->
    ?PRINT({"SHARE",EntryId }),
    %% FIXME: sharing is like posting to the wall
    case lists:member({Owner, Type}, Destinations) of
         true ->
            skip;
        _ ->
            add_entry(EntryId, FeedId, From, Destinations, Desc, Medias)
    end;

%%-----------------------------------------------------------------------------
%% EDIT
%%-----------------------------------------------------------------------------

process_delivery({_Type, _FeedId, Owner, _},
                 ["feed", _, _WhoEdits, "entry", EntryId, "edit"],
                 [NewText|_]) ->
    ?PRINT({"EDIT", Owner, EntryId }),
    edit_entry(EntryId, NewText);

%%-----------------------------------------------------------------------------
%% COMMENTS
%%-----------------------------------------------------------------------------

process_delivery({_TypeO, FeedId, _Owner, _},
                 ["feed", _Type, _Friend, "comment", CommentId, "add"],
                 [From, EntryId, ParentComment, Content, Medias]) ->
    add_comment(EntryId, FeedId, CommentId, ParentComment, From, Content, Medias);

%%-----------------------------------------------------------------------------
%% DELETE
%%-----------------------------------------------------------------------------

process_delivery(_, ["feed", "group", _Group, "entry", EntryId, "delete"], _) ->
    delete_entry(EntryId);
process_delivery({_, _FeedId, FeedOwner, _},
                 ["feed", "user", Owner, "entry", EntryId, "delete"],
                 [From|_]) ->
    %% this logic shold be the same as in nsm_bg_worker_feed.erl
    case {Owner, From} of
        %% owner of the antry has deleted entry, we will delete it too
        {Owner, Owner} ->
            delete_entry(EntryId);
        %% we are owner of the entry - delete it
        {FeedOwner, _} ->
            delete_entry(EntryId);
        %% one of the friends has deleted some entry from his feed. Ignore
        _ ->
            ok
    end;

%% like
process_delivery({_, _, FeedOwner, _}, ["likes", "user", User, "add_like"], {FeedId, EntryId}) ->
    LikePanelId = element_view_entry:like_panel_id(EntryId),
    {ok, E} = nsm_db:entry_by_id({EntryId, FeedId}),
    {LikeBox, _} = element_view_entry:like_string_and_button_bool(E, FeedOwner, [#one_like{user_id=User, entry_id=EntryId, feed_id=FeedId}]),
    wf:update(LikePanelId, LikeBox),
    wf:flush();

process_delivery(Info, Route, Message) -> % just to avoid nevedomaya yebanaya huynya
    ?WARNING("Unexpected delivery: ~p ~p ~p", [Info, Route, Message]).


delete_entry(EntryId) ->
    wf:wire(#hide {target=EntryId, effect=blind, speed=500}),
    wf:flush().

add_entry(EntryId, FeedId, From, Destinations, Desc, Medias) ->
    Entry0 = #entry{id = {EntryId, FeedId},
                   entry_id = EntryId,
                   created_time = now(),
                   from = From,
                   to = Destinations,
                   description = Desc,
                   raw_description = Desc,
                   media = Medias,
                   feed_id = FeedId},
    Entry = case feedformat:format(Entry0) of
        #entry{}= Formatted ->
            Formatted;
        _ ->
            Entry0
    end,

    VEntry = #view_entry{entry=Entry},
%    wf:wire(#attr{target=add_entry_textbox, attr="value", value=""}), % ? Is this why it empties an input box
    wf:insert_top(feed, VEntry),
    wf:flush().

% still testing this
add_system_entry(EntryId, FeedId, From, Destinations, Desc, Medias) ->
    Entry0 = #entry{id = {EntryId, FeedId},
                   entry_id = EntryId,
                   created_time = now(),
                   from = From,
                   to = Destinations,
                   description = Desc,
                   raw_description = Desc,
                   media = Medias,
                   feed_id = FeedId,
                   type = {user, system}},
    Entry = case feedformat:format(Entry0) of
        #entry{}= Formatted ->
            Formatted;
        _ ->
            Entry0
    end,

    VEntry = #view_entry{entry=Entry},
    wf:insert_top(feed, VEntry),
    wf:flush().

edit_entry(EntryId, NewText) ->
    Entry = feedformat:format(#entry{description = NewText}),
    ElemeId = element_view_entry:entry_body_label_id(EntryId),
    wf:update(ElemeId, Entry#entry.description),
    wf:flush().

add_comment(EntryId, FeedId, CommentId, _ParentComment, From, Content, Medias) ->
    Comment = #comment{id = {CommentId, {EntryId, FeedId}},
                       comment_id = CommentId,
                       author_id = From,
                       content = Content,
                       media = Medias,
                       entry_id = EntryId,
                       create_time = now(),
                       raw_content = Content},
    CommentsPanelId = element_view_entry:comments_panel_id(EntryId),
    CommentTextBox = element_view_entry:comment_textbox_id(EntryId),
    VComment = #view_comment{comment = Comment},
    wf:insert_bottom(CommentsPanelId, VComment),
    wf:wire(#set{target = CommentTextBox, value = ""}),
    wf:wire(#show{target=CommentsPanelId}),
    %wf:wire(#script{script = "upd_scrollers();"}),
    wf:flush().

% for direct messages
add_notification(From, Desc) ->
    wf:insert_top(notification_area, #notice{type=message, title=?_T("New direct message from ") ++ From ++ ?_T("!"),
        body = Desc, delay=5000}),
    wf:flush().
