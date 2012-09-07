%% -*- mode: nitrogen -*-
-module (element_view_entry).
%%FIX: should be called "element_feed_entry"
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("nsm_srv/include/user.hrl").
-include_lib("nsm_srv/include/feed.hrl").
-include("common.hrl").
-include("records.hrl").
-include("setup.hrl").

-define(SYSTEM_MESSAGE_EXPIRES, 600).
-define(SYSTEM_MESSAGE_STAYS_FOR_READING, 20).
-define(LIKERS_TO_SHOW, 5).

reflect() -> record_info(fields, view_entry).


render_element(#view_entry{entry=E, anchor=Anchor, id=Id} = VE) ->
    Type = E#entry.type,
    case Type of
        {system, _Subtype} ->
            element_view_system_entry:render_element(
                #view_system_entry{
                    entry=E,
                    anchor=Anchor,
                    id=Id,
                    type=Type
                }
            );
        {user, normal} ->
            render_normal_element(VE);
        {user, group} ->
            render_normal_element(VE);
        {user, direct} ->
            render_normal_element(VE);
        _ ->
            render_normal_element(VE)
    end.


render_normal_element(#view_entry{entry=E, anchor=Anchor}) ->
    %% Get comment
    Comments = rpc:call(?APPSERVER_NODE,comment,select_by_entry_id,[E#entry.id]),

    %% Get avatar
    {ok, User} = rpc:call(?APPSERVER_NODE, users, get_user,[E#entry.from]),
    Avatar = #image{image = avatar:get_avatar(User, small), style="width:48px;height:48px"},

    %% Get attachments
    ViewMediaPanelId = wf:temp_id(),
    MediaThumb = get_media_thumb(E, ViewMediaPanelId),

    MediaLists = [ 
        #view_media{media=M, target=ViewMediaPanelId, fid=E#entry.entry_id}
        || M  <- E#entry.media 
    ],

    entry_element(E, Comments, Avatar, {MediaThumb, MediaLists}, ViewMediaPanelId, Anchor).


is_slide_attach(#media{type={attachment, "image/jpeg"}}) ->true;
is_slide_attach(#media{type={attachment, "image/png"}})  ->true;
is_slide_attach(#media{type={attachment, "image/gif"}})  ->true;
is_slide_attach(#media{type={attachment, "application/pdf"}})  ->true;
is_slide_attach(_) ->false.

is_link_attach(#media{type=MType}) when element(1,MType)=:=link ->true;
is_link_attach(_) -> false.

is_separate_attach(_) -> true.


get_media_thumb(E, ViewMediaPanelId) ->
    SlideAttachments=[M || M <- E#entry.media, is_slide_attach(M)],
    LinkAttachments =[M || M <- E#entry.media, is_link_attach(M)],
    SeparateAttaches=[M || M <- E#entry.media, is_separate_attach(M),is_link_attach(M)=:=false,is_slide_attach(M)=:=false],
    SlideAttachmentsView = case length(SlideAttachments ++ LinkAttachments) of
        I when is_integer(I),I>0 ->
            NavBody = if
                I*2 > 4 -> [#panel{body=[], class="scroller_prev"},#panel{body=[], class="scroller_next"}];
                true    -> []
            end,
            [
                #panel{style="padding-left:0px", class="scroller_container", body=[
                    #panel{style="float:left;width:34px;min-height:30px", body=NavBody},
                    #panel{body=[
                        #panel{class="modal-content", body=[
                            [
                                #view_media{media=M, target=ViewMediaPanelId, fid=E#entry.entry_id, only_thumb=true} 
                                ||  M <- SlideAttachments
                            ] ++
                            [
                                #view_media{media=M, target=ViewMediaPanelId, fid=E#entry.entry_id, only_thumb=true} 
                                ||  M <- LinkAttachments
                            ]
                        ]}
                    ]}
                ]}
            ];
        _ -> []
    end,
    SeparateAttachesView = case length(SeparateAttaches) of
        J when is_integer(J),J>0 ->
            [
                #panel{body=[
                    #view_media{media=M, target=ViewMediaPanelId, fid=E#entry.entry_id, only_thumb=true} 
                    ||  M <- SeparateAttaches
                ]}
            ];
        _ -> []
    end,
    LinkAttachmentsView = [],
    case LinkAttachmentsView ++ SlideAttachmentsView ++ SeparateAttachesView of
        [] -> #panel{body=[]};
        GMthumbs -> GMthumbs
    end.


entry_element(E, Comments, Avatar, {MediaThumb, MediaLists0}, _TargetMedia, Anchor) ->
    case E#entry.type of 
        {_, system} ->
            MessageSecs = calendar:datetime_to_gregorian_seconds( calendar:now_to_datetime(E#entry.created_time) ),
            NowSecs = calendar:datetime_to_gregorian_seconds( calendar:now_to_datetime(erlang:now()) ),
            case NowSecs - MessageSecs > ?SYSTEM_MESSAGE_EXPIRES of
                true ->
                    [];
                false ->
                    TimeLeft = ?SYSTEM_MESSAGE_EXPIRES - (NowSecs - MessageSecs),
                    TimeLeftAndSomeMore = case TimeLeft < ?SYSTEM_MESSAGE_STAYS_FOR_READING of
                        true -> ?SYSTEM_MESSAGE_STAYS_FOR_READING;
                        _ -> TimeLeft
                    end,
                    Title_URL_Desc = string:tokens(E#entry.description, "|"),
                    case Title_URL_Desc of
                        [URL, UId, TableName, GameType, Rounds, Speed, Mode] ->
                            Title = ?_T("New Table"),
                            Desc1 = ?_TS("Our player $username$, has created ", [{username, UId}]),
                            Link = TableName,
                            Desc2 = " " ++ ?_T("for") ++ " " ++ GameType ++ " " ++ ?_T("game") ++ ". " ++ ?_T("Game") ++ 
                                " " ++ ?_T("specs") ++ ": " ++
                                Rounds ++ " " ++ ?_T("rounds") ++ ", " ++ Speed ++ " " ++ ?_T("speed") ++ ", " ++
                                Mode ++ " " ++ ?_T("mode") ++ ".",
                            #notice{type=system_message, position=left,
                                title=#link{url=URL, text=Title},
                                body=[
                                    Desc1, 
                                    #link{style="font-weight:bold;", url=URL, text=Link},
                                    get_file:replace(Desc2, ?_T("Game"), "<br/>" ++ ?_T("Game") )  
                                ],
                                delay = TimeLeftAndSomeMore * 1000
                            };
                        [Title, Desc] ->
                            #notice{type=message, position=left, title=Title, body=Desc};
                        [Desc] ->
                            #notice{type=message, position=left, title=?_T("System message"), body=Desc}
                    end
            end;
        _ ->
            entry_element_usual(E, Comments, Avatar, {MediaThumb, MediaLists0}, _TargetMedia, Anchor)
    end.

entry_element_usual(E, Comments, Avatar, {MediaThumb, MediaLists0}, _TargetMedia, Anchor) ->
    TempId = E#entry.entry_id,

    AttachmentBox = wf:temp_id(),

    LocalTime = calendar:now_to_local_time(E#entry.created_time),
    Time = site_utils:feed_time_tuple(LocalTime),

    Events = [
        #event {type=mouseover,target=remove, actions=#show {}},
        #event {type=mouseout,target=remove, actions=#hide {}}
    ],
    _ShowAttachment = MediaLists0 /= [],
    MoreAttachment = length(MediaLists0) > 3,

    _ShowMorePostback = wf_event:serialize_event_context({show_more_att, AttachmentBox, MediaLists0}, Anchor, undefined, ?MODULE),

    _MediaLists = case MoreAttachment of
        true ->
            lists:sublist(MediaLists0, 3);
        false ->
            MediaLists0
    end,

    {LikesStr, LikeBtnShow} = case rpc:call(?APPSERVER_NODE, feed, get_entries_likes, [E#entry.entry_id]) of
        []     -> {"", true};
        [#one_like{user_id=Uid}|[]] ->
            {
                #panel{class="like-box", body = case Uid == wf:user() of
                    true ->
                        [
                            "<p>",
                            #link{text=?_T("You"), url=site_utils:user_link(Uid)},
                            #span{text=" " ++ ?_T("like this.")},
                            "</p>"
                        ];
                    false ->
                        [
                            "<p>",
                            #link{text=Uid, url=site_utils:user_link(Uid)},
                            #span{text=" " ++ ?_T("likes this.")},
                            "</p>"
                        ]
                end},
                Uid =/= wf:user()
            };
        OL when is_list(OL) ->
            L = lists:reverse(OL),
            #one_like{user_id=LastUid} = lists:last(L),
            LeftPart = admin:nitrojoin([    %TODO once again, this is a chaos, I have to add a module for this stuff
                    case Uid == wf:user() of
                        true ->
                            #link{text=?_T("You"), url=site_utils:user_link(Uid)};
                        false ->
                            #link{text=Uid, url=site_utils:user_link(Uid)}
                    end
                    || #one_like{user_id=Uid} <- lists:sublist(L, length(L)-1)
            ], ", "),
            Lstr = [
                case length(L) > ?LIKERS_TO_SHOW of
                    true ->
                        Id = wf:temp_id(),
                        [
                            "</p>",
                            #panel{id = Id, style="float:left; margin-right:1.5pt;", body = [        
                                "<p>",
                                #link{text=integer_to_list(length(L)-1) ++ " " ++ ?_T("people"), 
                                    postback={show_all_likers, ["<p>", LeftPart, "</p>"], Id}},
                                "</p>"
                            ]},
                            "<p>"
                        ];
                    false ->
                        LeftPart
                end,
                " ", ?_T("and"), " ",
                case LastUid == wf:user() of
                    true ->
                        #link{text=?_T("You"), url=site_utils:user_link(LastUid)};
                    false ->
                        #link{text=LastUid, url=site_utils:user_link(LastUid)}
                end,
                #span{text=" " ++ ?_T("like this.")}
            ],
            {
                #panel{class="like-box", body=["<p>", Lstr, "</p>"]}, 
                lists:member(wf:user(), [Uid1 || #one_like{user_id=Uid1} <- L]) =:= false
            };
        _  -> {"", true}
    end,

    StO = "<strong class=\"entry-h1\"><a href=\"~s\">~s</a>",
    StC = "</strong>",

    {TitleStr, IsDirectMessage} = case E#entry.type of
        {unknown, unknown} -> {"",false};
        _ ->
            {DmFrom, DmFromLink, IsEditable} = case {E#entry.from, wf:user()} of
                {DMF, DMF} -> {"You", wf:user(), true};
                _ -> {site_utils:username_upper(E#entry.from), E#entry.from, false}
            end,

            case E#entry.to of 
                [] -> {
                    io_lib:format(StO ++ StC,
                        [site_utils:user_link(DmFromLink), DmFrom]), IsEditable};
                    undefined -> {io_lib:format(StO ++ StC,
                        [site_utils:user_link(DmFromLink), DmFrom]), IsEditable};
                _ ->
                   case length(E#entry.to) > 1 of
                        false ->
                            {Name,Type}=lists:nth(1,E#entry.to),
                            {DmTo, DmToLink} = {site_utils:username_upper(Name), Name},
                            case DmFromLink =:= DmToLink of
                                true -> {io_lib:format(StO ++ StC,
                                        [site_utils:user_link(DmFromLink), DmFrom]), IsEditable};
                                false ->
                                    case Type of
                                    user ->
                                         {io_lib:format(StO ++ " to <a href=\"~s\">~s</a>" ++ StC,
                                            [site_utils:user_link(DmFromLink), DmFrom, site_utils:user_link(DmToLink),
                                        case Name =:= wf:user() of true -> "You"; false -> DmTo end]), IsEditable};
                                    group ->
                                        {io_lib:format(StO ++ " to <a href=\"~s\">~s</a>" ++ StC,
                                            [site_utils:user_link(DmFromLink), DmFrom, site_utils:group_link(DmToLink), DmTo]), 
                                            IsEditable}
                                    end
                            end;
                        true ->
                            User  = wf:user(), {
                            io_lib:format(StO ++" to ", [site_utils:user_link(DmFromLink), DmFrom])
                                ++ string:join([ 
                                    begin
                                        {D,_}=Dst,
                                        case Dst of
                                            {D,group} ->io_lib:format("<a href=\"~s\">~s</a>",
                                                [site_utils:group_link(D), site_utils:username_upper(D)]);
                                            _ -> io_lib:format("<a href=\"~s\">~s</a>",
                                                [site_utils:user_link(D), site_utils:username_upper(D)])
                                        end 
                                    end
                                    || Dst <-  E#entry.to, Dst /= {User, user}
                                ], ", ") ++ StC, IsEditable}
                    end
            end
    end,
    EntryBodyId =  entry_body_id(E#entry.entry_id),
    ViewPanelID = wf:temp_id(),
    TextBoxID   = wf:temp_id(),
    Description = case IsDirectMessage of
        true -> #inplace_textbox1 { 
                    text=E#entry.description, eb_id=EntryBodyId, vp_id=ViewPanelID, tb_id=TextBoxID,
                    entry_id=E#entry.entry_id, feed_id=E#entry.feed_id
                };
        _    -> #p{id=entry_body_label_id(E#entry.entry_id), body=E#entry.description}
    end,
    CommentsPanelId = comments_panel_id(E#entry.entry_id),
    CommentRingId   = wf:temp_id(),
    NewCommentBoxId = wf:temp_id(),
    NewCommentTBId  = comment_textbox_id(E#entry.entry_id),
    ViewC           = Comments == [],
    [
        #panel{class="post", id=TempId, actions=Events, body=[
            #panel{class="entry-avatar", body=[
                #link{body=Avatar, url=site_utils:user_link(E#entry.from)}
            ]},
            #panel{class="entity", body=[
                TitleStr,
                Description,
                #panel{class="meta", body=[
                    io_lib:format("<span class=\"entry-time\"> ~s </span>",[Time]),
                    #list{
                        class="list-4", body=[
                            #listitem{body=#link{
                                text=?_T("Comment"), class="clr-1", url="javascript:void(0)", show_if=ViewC,
                                actions=[
                                    #event{type=click, actions=[
                                        #hide{},
                                        #show{target=NewCommentBoxId},
                                        #show{target=CommentRingId}
                                    ]}
                                ]
                            }
                        },
                        #listitem{body=#link{text=?_T("Like"), class="clr-1", postback={like_entry, E, TempId}, show_if=LikeBtnShow}},

                        % user should not share from own feed
                        case (webutils:user_info())#user.feed == E#entry.feed_id of
                            true ->
                                "";
                            false ->
                                % user should not share her own entries
                                case wf:user() == E#entry.from of
                                    true ->
                                        "";
                                    false ->
                                        #listitem{body=#link{text=?_T("Share"), class="clr-2", url="javascript:void(0)", postback={share_entry, E}}}
                                end
                        end,

%PHASE1                        #listitem{body=#link{text=?_T("Hide"),  class="clr-3", url="javascript:void(0)",  postback={hide_entry, E#entry.entry_id, TempId}}},
                        #listitem{body=#link{text=?_T("Edit"), show_if=IsDirectMessage, class="clr-4", url="javascript:void(0)",
                            actions=[ 
                                #event { type=click, actions=[
                                    #hide { target=ViewPanelID },
                                    #show { target=EntryBodyId },
                                    #script { script = wf:f("obj('~s').focus(); obj('~s').select();", [TextBoxID, TextBoxID]) }
                                ]}
                            ]}
                        },
                        #listitem{body=#link{text=?_T("Remove"),class="clr-2", url="javascript:void(0)",
                            postback={remove_entry, E#entry.entry_id, TempId, E#entry.to, E#entry.from},
                            title=?_T("Clicking here will remove an entry from your own and your friends feeds")}
                        }
                    ]}
                ]},
                LikesStr,
                #panel{body=MediaThumb},
                #grid_clear{},
                comments_element(E#entry.entry_id, Comments, Anchor, Avatar, NewCommentBoxId, 
                    CommentsPanelId, CommentRingId, NewCommentTBId)
            ]}
        ]}
    ].

comments_element(EId, Comments, Anchor, Avatar) ->
    comments_element(EId, Comments, Anchor, Avatar, undefined, undefined, undefined, undefined).

comments_element(EId, Comments, Anchor, Avatar, NewCommentBoxId0, CommentsPanelId0, CommentRingId0, NewCommentTBId0) ->
    NewCommentBoxId = case NewCommentBoxId0 of
        undefined -> wf:temp_id()
        ;_        -> NewCommentBoxId0
    end,
    CommentsPanelId = case CommentsPanelId0 of
        undefined -> wf:temp_id()
        ;_        -> CommentsPanelId0
    end,
    _CommentRingId   = case CommentRingId0 of
        undefined -> wf:temp_id()
        ;_        -> CommentRingId0
    end,
    NewCommentTBId  = case NewCommentTBId0 of
        undefined -> wf:temp_id()
        ;_        -> NewCommentTBId0
    end,
    ViewC = Comments == [],
    case ViewC of
        false ->
            wf:wire(#show{target=NewCommentBoxId});
        _ ->
            ok
    end,
    CommentsList = [ #view_comment{comment=Comment} || Comment <- Comments ],
    _CommentsView = case CommentsList of
	[] -> false;
	_  -> true
    end,
    [
        #list{id=CommentsPanelId, class="comment-box", body=[CommentsList]},
        add_comment_box(EId, NewCommentBoxId, CommentsPanelId, Anchor, Avatar, NewCommentTBId)
    ].

add_comment_box(EId, CommentBox, CommentsPanelId, Anchor, Avatar) ->
    add_comment_box(EId, CommentBox, CommentsPanelId, Anchor, Avatar, undefined).
add_comment_box(EId, CommentBox, CommentsPanelId, Anchor, Avatar, NewCommentTBId0) ->
    NewCommentTBId = case NewCommentTBId0 of
        undefined -> wf:temp_id()
        ;_        -> NewCommentTBId0
    end,
    ViewAttachment = wf:temp_id(),
    AttachmentError = wf:temp_id(),
    MSI = lists:concat(["media_storage_id_", ViewAttachment]), %% MSI - Media Storage Id :)
    _ShowAddAtt = wf_event:serialize_event_context({show_add_attachment, EId, ViewAttachment, AttachmentError, MSI},
        Anchor, undefined, ?MODULE),
    #panel{id=CommentBox, style="display:none;", class="comment-form", body=[
        #panel{class="photo-cell", body=#image{image=Avatar#image.image, style="width:32px;height:32px"}},
        #panel{class="textarea", body=[
            #textbox{id=NewCommentTBId, class="textareabox-mkh", style="resize:none;",
                placeholder=?_T("Write a comment..."),
                postback={comment_entry,
                    EId,
                    CommentsPanelId,
                    NewCommentTBId,
                    ViewAttachment,
                    MSI
                }
            }
        ]},
        #button{class="btn-submit", text=?_T("Comment"),
            postback={comment_entry, EId, CommentsPanelId, NewCommentTBId, ViewAttachment, MSI}},
        #grid_clear{},
        #span{id=AttachmentError},
        #panel{id=ViewAttachment},
        #panel{style="clear:both"}
    ]}.


comments_panel_id(EntryId) ->
    EntryId++"_comment_box".
comment_textbox_id(EntryId) ->
    EntryId++"_comment_textbox".
entry_body_id(EntryId)->
    EntryId++"_entry_body_id".
entry_body_label_id(EntryId)->
    element_inplace_textbox1:label_id(entry_body_id(EntryId)).


event({show_more_att, Target,  AttLists}) ->
    wf:update(Target, AttLists);

event({show_add_attachment, EId, ViewAttachment, AttachmentError, MSI}) ->
    dashboard:check_number_of_uploads(AttachmentError, EId) == ok andalso
    begin
        BoxId = wf:temp_id(),
        UploadId = lists:concat(["upload_", BoxId]),
        Body = #panel{style="clear:both", id=BoxId, body=[
            #panel{id=UploadId, body=[
                #upload{tag={comment_att, ViewAttachment, AttachmentError, BoxId, MSI}, show_button=false}
            ]}
        ]},
        wf:insert_bottom(ViewAttachment, Body)
    end.


