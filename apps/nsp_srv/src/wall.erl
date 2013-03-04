-module (wall).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("nsm_db/include/user.hrl").
-include_lib("nsm_db/include/feed.hrl").
-include_lib("nsm_db/include/attachment.hrl").
-include_lib("nsx_config/include/log.hrl").
-include_lib("nsm_db/include/scoring.hrl").
-include_lib("nsm_db/include/membership_packages.hrl").
-include("elements/records.hrl").
-include("gettext.hrl").
-include("setup.hrl").

-define(ENTRY_TEXT_LENGTH, 350).
-define(PAGEAMOUNT, 3).
-define(MAX_NUM_OF_ATTACMNETS_PER_TIME, 3).
-define(TOOLTIP_TIMEOUT, "1500").
-define(GROUPS_ON_DASHBOARD, 10).

title() -> webutils:title(?MODULE).

main() -> #template { file=code:priv_dir(nsp_srv)++"/templates/base.html"}.

body() ->
  {FeedType,TypeDefined} = case wf:q(type) of "user" -> {user,true}; "gr"++XX -> {group,true}; _ -> {user,false} end,
  User = case TypeDefined of 
              true -> case wf:q(id) of undefined -> "0"; A -> A end; 
              false -> wf:user() end,

  ?INFO("Check: ~p",[{FeedType,User,wf:q(FeedType)}]),

  {Exists,Wall,Info} = case FeedType == group of
        true -> {_, GroupInfo} = nsm_groups:get_group(User),
                case GroupInfo of
                     notfound -> {false,[not_found("Group"),undefined]};
                     _ -> {true,[ req_invite(),
                            case nsm_groups:user_has_access(wf:user(), User) of
                                  true -> feed(GroupInfo#group.feed, group, GroupInfo);
                                 false -> hidden_form() end ],GroupInfo} end;
        false -> {_,UserInfo} = nsm_users:get_user(User),
                case UserInfo of
                     notfound -> {false,not_found("User"),undefined};
                     _ -> {true,feed(UserInfo#user.feed, FeedType, UserInfo),UserInfo} end
      end,

  #panel{class="page-content page-canvas", style="overflow:auto;margin-top:20px;", body=[
    "<section id=\"content\">", Wall, "</section>",
    case Exists of 
         true -> #panel{class="aside", body=[case FeedType of
                                                  user -> [ get_ribbon_menu(Info), #panel{id=aside,body=aside(Info)} ];
                                                  _    -> [ group_info(Info), get_members() ] end ]}; 
         false -> "" end 
  ]}.

feed(Fid, Type, Info) ->
  ?INFO("Feed ~p Type ~p Owner ~p", [Fid,Type,Info]),
  [
    #panel{id=notification_area},
    entry_form(Fid, Type, ?MODULE, {add_entry, Fid}),
    #panel{id=feeds_container, class="posts_container_mkh", body=show_feed(Fid, Type, Info)}
  ].

entry_form(FId, Type, Delegate, Postback) ->
  wf:wire(wf:f("objs('add_entry_textbox').bind('keydown change paste',"++
    "function(){"++
      "var $this=objs('add_entry_textbox');" ++
      "setTimeout(function(){" ++
      "var l = parseInt($this.attr('value').length);" ++
      "console.log('lenght: ' + l);" ++
      "if(l>0){" ++
        "objs('sendentry').addClass('enabled').removeClass('disabled');" ++
      "}else{" ++
        "objs('sendentry').removeClass('enabled').addClass('disabled');"
      "}" ++
    "},0);" ++
    "}).bind('keypress',"++
      "function(e){" ++
        "var code = e.keyCode || e.wich;" ++
        "if(code == 13) {" ++
          "if(!e.shiftKey){ ~s return false;}" ++
        "}" ++
        "if(code != 116 && code !=46 && code >40 || code ==32){" ++
          "return $(this).trigger('change').attr('value').length < ~b;" ++
        "}" ++
      "}"
  ");",[site_utils:postback_to_js_string(Delegate, Postback), ?ENTRY_TEXT_LENGTH])),
  Recipients = case  wf:q("filter") of
    "direct" ->
      case wf:q("tu") of
        undefined -> [];
        To -> ["user_"++To, "<b>"++To++"</b>", "<b>"++To++"</b>"]
      end;
    undefined ->
      case wf:q("id") of
        undefined->[];
        Id ->  [atom_to_list(Type) ++ "_" ++ Id, "<b>"++Id++"</b>", "<b>"++Id++"</b>"]
      end;
    _ -> []
  end,
  [
  #panel{class=entry_form, body=[
    #label{text=?_T("To")++":"},
    #textboxlist{id=recipients_list, value=Recipients, placeholder=?_T("Type to receive suggestion.")},
    #panel{id=flashm, body=[]},
    #panel{body=[
      "<span id='guidersaddentrybox'>", #textarea{id=add_entry_textbox, placeholder=?_T("Put your thoughts in here...")},"</span>",
      "<span id='guiderssharebutton'>",
        #button{id=sendentry, postback={add_entry, FId}, text=?_T("Share"), class="submit disabled",
          actions="obj('me').title=\""++?_T("Click here to post your entry to the feed. You can still remove it anytime")++"\""},
      "</span>"
    ]},
    #span{id=attachment_error},
    #panel{class=attachment_thumb,  id=attachment_thumb},
    #panel{class=attachment_box, id=attachment_box},
    "<span id='guidersattachmentbox'>",
      #panel{class=attachpanel, id=attachlinkpanel, body=[
        #link{body=io_lib:format("<strong>~s:</strong> ~s...", [?_T("Add"), ?_T("Image, music, file")]), postback=show_add_attachment}
      ]},
    "</span>"
  ]},
  #span{id=text_length}
  ].

show_feed(undefined, _, _) -> [];
show_feed(Fid, Type, Info) when is_atom(Type) ->
  UserInfo = wf:session(user_info),

  Uid = case Type of
             user -> Info#user.username;
             groupr -> Info#group.username;
             _ -> "system" end,

  Pid = case UserInfo of
       undefined -> self();
       _ -> {ok, P} = comet_feed:start(Type, Fid, Uid, UserInfo),
            wf:state(comet_feed_pid, pid_to_list(P)), P end,


  Node = nsx_opt:get_env(nsx_idgen,game_pool,5000000) div 1000000,
  CheckNode = fun(X) -> lists:foldl(fun(A, Sum) -> A + Sum end, 0, X) rem 3 + 1 end,
  AppNode = case Node of
                 4 -> nsx_opt:get_env(nsm_db,app_srv_node,'app@doxtop.cc');
                 5 -> nsx_opt:get_env(nsm_db,app_srv_node,'app@doxtop.cc');
                 _ -> list_to_atom("app@srv"++integer_to_list(CheckNode(Uid))++".kakaranet.com") end,

  CachePid = rpc:call(AppNode,nsm_bg,pid,[Uid]),

  Answer = case  wf:q("filter") of
    "direct" -> case UserInfo of
                     undefined -> [];
                     _ -> rpc:call(AppNode,nsm_writer,cached_direct,[CachePid, UserInfo#user.direct, ?FEED_PAGEAMOUNT]) end;
    _ -> rpc:call(AppNode,nsm_writer,cached_feed,[CachePid, Fid, ?FEED_PAGEAMOUNT])
  end,
  
  Entries = case Answer of
                 {badrpc,_} -> []; 
                 X -> X end,
  Last = case Entries of
    [] -> [];
    _ -> lists:last(Entries)
  end,
  Pid ! {delivery, check_more, {?MODULE, length(Entries), Last}},
  [
    #panel{id = feed, body=[[#view_entry{entry = E} || E <- Entries]]},%read_entries(Pid, undefined, Fid)},
    #panel{id = more_button_holder, body=[]}
  ].

read_entries(Pid, StartFrom, FeedId)->
  Feed = case StartFrom of
    undefined-> nsm_db:get(feed, FeedId);
    S -> nsm_db:get(entry, {S, FeedId})
  end,
  case Feed of
    {error, notfound} -> [];
    {ok, #feed{}=F} -> traverse_entries(Pid, F#feed.top, ?FEED_PAGEAMOUNT);
    {ok, #entry{prev = E}} -> traverse_entries(Pid, E, ?FEED_PAGEAMOUNT)
  end.

aside(Info) ->
    Fv = get_friends(Info),
    Gv = get_groups(Info),
    [Fv,Gv].

get_friends(undefined) -> [];
get_friends(User) ->
  ?INFO("get_friends: ~p",[User]),
      Msg = case User#user.username == wf:user() of
        true ->
          #panel{class="mark-cell", body=[
            #p{body=["<strong>",?_T("Make new friends on kakaranet."),"</strong>"]},
            #link{url="/members/group/id/kakaranet", class="btn", text=?_T("Find someone!")}
          ]};
        false -> ?_T("User is not subscribed to anyone")
      end,
      Nav = case User#user.username == wf:user() of
        true ->
          #span_b{class="links", body=[
            #link{style="font-size:12pt;",text=?_T("All your friends"), url="/members", id="friendslink",
            title=?_T("You can unsubscribe or write someone private message via this list")}
          ]};
        false ->
          #span_b{class="links", body=[
            #link{style="font-size:12pt;", text=?_T("All friends of ") ++ User#user.username, url="/members/id/"++User#user.username, id="friendslink",
            title=?_T("You can unsubscribe or write someone private message via this list")}
          ]}
      end,
      webutils:get_metalist(User, ?_T("FRIENDS"), nsm_users, list_subscr_usernames, Msg, Nav).

get_groups(undefined) -> [];
get_groups(User) ->
  ?INFO("get_groups: ~p",[User]),
      Groups = case nsm_groups:list_groups_per_user(User#user.username) of
        [] ->
          case User#user.username == wf:user() of
            true -> ?_T("You are currently not in any group");
            false -> ?_TS("$user$ is currently not in any group", [{user, User#user.username}])
          end;
        Gs ->
          UC_GId = lists:sublist(lists:reverse(lists:sort([{nsm_groups:group_members_count(GId), GId} || GId <- Gs])), ?GROUPS_ON_DASHBOARD),
          lists:flatten([
            begin
              case nsm_groups:get_group(GId) of
                {ok, Group} ->
                  GName = Group#group.name,
                  #listitem{body=[
                    #link{body=[GName], style="font-size:12pt;", url=site_utils:group_link(GId)},
                    #span{style="padding-left:4px;", text="(" ++ integer_to_list(UC) ++ ")"}
                  ]};
                _ -> ""
              end
            end
            || {UC, GId} <- UC_GId])
      end,

      #panel{class="box", style="border:0", body=[
        #h3{text=?_T("GROUPS"), class="section-title"},
        #list{class="list-photo list-photo-in", body=[ Groups ]},
        case User#user.username == wf:user() of
          true ->
            #span_b{class="links", body=[
              #link{style="font-size:12pt;", text=?_T("List of all your groups"), url="/groups/of/"++wf:user(), id="groupslink",
                title=?_T("You can unsubscribe a group from this list")}
            ]};
          false-> ""  % here should be all users groups link
        end
      ]}.

new_statistic(SubscribersCount,FriendsCount,CommentsCount,LikesCount,EntriesCount,CheckedUser) ->
    #list{class="list-5", body=[
          #listitem{body= #link{url="/friends/t/subscribtion",
                    text=integer_to_list(SubscribersCount) ++ " " ++ ?_T("subscription") }},
          #listitem{body= #link{url="/friends/t/subscribers",
                    text=integer_to_list(FriendsCount) ++ " " ++ ?_T("subscribers") }},
          #listitem{body= #link{url="/wall/filter/comments/user/" ++ wf:to_list(CheckedUser),
                    text=integer_to_list(CommentsCount) ++ " " ++ ?_T("comments") }},
          #listitem{body=#link{url="/wall/filter/like/user/" ++ wf:to_list(CheckedUser),
                    text=integer_to_list(LikesCount) ++ " " ++ ?_T("likes") }},
          #listitem{body=#link{url="/wall/filter/like/user/" ++ wf:to_list(CheckedUser),
                    text=integer_to_list(EntriesCount) ++ " " ++ ?_T("entries") }} ]}.

get_ribbon_menu() -> get_ribbon_menu(wf:user()).
get_ribbon_menu(User) ->
    ?INFO("get_ribbon_menu: ~p",[User]),
    CheckedUser = case {wf:q(id),wf:user()} of
        {CU,CU} -> undefined;
        {CUA,CUB}   -> CUA
    end,
    IsSubscribedUser = case CheckedUser of
        undefined -> undefined;
        _         -> feed:is_subscribed_user(wf:user(), CheckedUser)
    end,
%    User = case CheckedUser of
%        undefined -> webutils:user_info();
%        _         -> {ok, Usr} = nsm_users:get_user(CheckedUser), Usr
%    end,
    SubscribersCount = feed:user_subscription_count(User#user.username),
    FriendsCount = feed:user_friends_count(User#user.username),
    CommentsCount = feed:get_comments_count(User#user.username),
    LikesCount = feed:get_user_likes_count(User#user.username),
    EntriesCount = feed:get_entries_count(User#user.username),
    Scores = scoring:score_entries(User#user.username),
    SNum = integer_to_list(length(Scores)),
    SPoints = integer_to_list(lists:sum([S#scoring_record.score_points || S <- Scores])),
    NewDirectMessages=false,
    BlockedUsers = case wf:user() of
                        undefined -> [];
                        _ -> nsm_users:get_blocked_users(wf:user()) end,
    BlockUnblock = case User of
        undefined -> [];
        _ -> case lists:member(CheckedUser, BlockedUsers) of
                true ->
                    #panel{id="blockunblock", class="center",
                        body=#link{text=?_T("Unblock this user"),url="javascript:void(0)",
                            postback={unblock, CheckedUser}
                        }
                    };
                _   ->
                    #panel{id="blockunblock", class="center",
                        body=#link{text=?_T("Block this user"), url="javascript:void(0)",
                            postback={block, CheckedUser}
                        }
                    }
             end
    end,
   
    Admin = nsm_acl:check_access(wf:user(), {feature, admin}),

    Affiliate = case Admin of
	            allow -> 
                        case nsm_affiliates:is_existing_affiliate(User#user.username) of
                            true -> #link{body=[#image{image="/images/ico-04.gif", style="width:27px;height:34px"},
                                                 ?_T("This user is an affiliate")]};
                            false -> #link{body=[#image{image="/images/ico-04.gif", style="width:27px;height:34px"},
                                                 ?_T("Make this user affiliate")], postback={make_affiliate, User#user.username}}
                        end;
	            _ -> []
            end,

    case ((Admin == allow) or (CheckedUser == undefined)) and (User#user.status /= ok) of
	true -> wf:update(notification_area, verification_notification());
	false -> skip end,

    ?INFO("ribbon2: ~p ~p",[CheckedUser, IsSubscribedUser]),

    MenuTail = case {CheckedUser, IsSubscribedUser} of
        {undefined, undefined} ->
                [ #list{class="list-6", body=[
                        #listitem{body=#link{url="/wall", 
                                             body=[ #image{image="/images/ico-04.gif", style="width:27px;height:34px"},
                                                           ?_T("My Feed")]}},
                        #listitem{body=#link{url="/wall/filter/direct", 
                                             body=[ #image{image="/images/ico-05.gif", style="width:27px;height:34px"},
                                                           ?_T("Direct Messages")]}}   ]},
                    new_statistic(SubscribersCount,FriendsCount,CommentsCount,LikesCount,EntriesCount,CheckedUser) ];

        {_, true}  -> [#link{text=?_T("Unsubscribe"), url="#", class="btn-abone btn-abone-2", postback={unsubscribe, CheckedUser}},
                       BlockUnblock,
                       #list{class="list-6", body=[#listitem{body=Affiliate}]},
                       new_statistic(SubscribersCount,FriendsCount,CommentsCount,LikesCount,EntriesCount,CheckedUser) ];
        {_, false} -> [#link{text=?_T("Subscribe"), url="#", class="btn-abone", postback={subscribe, CheckedUser}},
                       BlockUnblock,
                       new_statistic(SubscribersCount,FriendsCount,CommentsCount,LikesCount,EntriesCount,CheckedUser) ];
                 _ -> []
    end,
    [
        #panel{class="top-box", body=[
            case nsm_accounts:user_paid(element(2, User) ) of
                true -> #panel{class="paid_user_avatar_photo", body=[#image{image=webutils:get_user_avatar(element(2, User) ,"big"), 
                           style="height:150px; width:150px;"}], style="margin-left:19px;"};
                _ -> #panel{class="photo", body=[#image{image=webutils:get_user_avatar(element(2, User) ,"big"), 
                           style="height:150px; width:150px;"}], style="margin-left:19px;"}
            end
        ]},
        #h3{class="title-box", text=wf:to_list(User#user.name) ++ " " ++ wf:to_list(User#user.surname)},
        #panel{class="block", body=[
            #panel{class="gallery-game", body=[
                #panel{class="slider-container", body=[
                 #panel{class="slider-content", body=[
                  #panel{class="slider", body=[
                    #list{body=[
                        #listitem{body=#image{image="/images/img-013.gif", style="width:53px;height:26px"}},
                        #listitem{body=io_lib:format("<span>~s</span><strong>~s</strong>",[?_T("Num"), SNum])},
                        #listitem{body=io_lib:format("<span>~s</span><strong>~s</strong>",[?_T("Points"), SPoints])}
                    ]}
                  ]}
                 ]}
                ]}
            ]},
            MenuTail
        ]}
    ].

traverse_entries(_, undefined, _) -> [];
traverse_entries(_, _, 0) -> [];
traverse_entries(Pid, Next, Count)->
  case nsm_db:get(entry, Next) of
    {error, notfound}->[];
    {ok, R}->
      Pid ! {delivery, show_entry, R},
      [R | traverse_entries(Pid, R#entry.prev, Count-1)]
  end.

user_blocked_message(U) ->
    #panel{style="font: 1em Arial,Helvetica,sans-serif;font-size: 14px;font-weight: bold;", body=[
        io_lib:format("You have blocked ~s, so all of ~s's posts and comments are invisible to you. ", [U, U]),
        #link{text="Un-block", url="javascript:void(0)", postback={unblock_load, U, get_page_number()}}
    ]}.

start_upload_event({entry_att, BoxId}) ->
    AttThumb = #panel{class="view_media_other_attachment", body=#panel{class=loading_spiner}},
    wf:wire(#hide{target=lists:concat(["upload_", BoxId]) }),
    wf:insert_top(BoxId, AttThumb);

start_upload_event({comment_att, _, BoxId, _}) ->
    AttThumb = #panel{class="view_media_other_attachment", body=#panel{class=loading_spiner}},
    wf:wire(#hide{target=lists:concat(["upload_", BoxId]) }),
    wf:insert_top(BoxId, AttThumb);

start_upload_event({comment_att, _,_,BoxId,_}) ->
    AttThumb = #panel{class="view_media_other_attachment", body=#panel{class=loading_spiner}},
    wf:wire(#hide{target=lists:concat(["upload_", BoxId]) }),
    wf:insert_top(BoxId, AttThumb).

finish_upload_event({comment_att, _, AttachmentError, BoxId, MediaStorageId}, OrigFile, LocalFile, _Node) ->
    User = wf:session(user_info),
    case feed_attachment:process_uploaded_file(User#user.username, User#user.feed, OrigFile, LocalFile) of
        {error, Error} ->
            attachment_error(AttachmentError, ?_T(Error)),
            wf:remove(BoxId);
        {ok, Att} ->
            wf:update(BoxId, #span{class=view_media_other_attachment, text=OrigFile}),
            %wf:wire("upd_parent_to_float('"++ wf:to_list(BoxId) ++"');"),
            ThisMedia = create_media(Att),
            Medias = case wf:state(MediaStorageId) of
			 undefined -> [];
			 Other -> Other
		     end,
            NewMedias = [ ThisMedia | Medias ],
            wf:state(MediaStorageId, NewMedias)
    end;

finish_upload_event({entry_att, BoxId}, OrigFile, LocalFile, _Node) ->
    UploadLimit = nsm_db:get_config("storage/upload_limit", 31457280),
    case length(string:tokens(OrigFile, ".")) == 1 of
    true ->
        attachment_error(?_T("Sorry, can't upload binary files")),
        wf:remove(BoxId);
    false ->
        case filelib:file_size(LocalFile) > UploadLimit of
        true ->
            attachment_error(?_TS("Sorry, $n$ Mbytes is the upload limit here", [{n, UploadLimit/1048576}])),
            wf:remove(BoxId);
        false ->
            User = wf:session(user_info),
            case feed_attachment:process_uploaded_file(User#user.username, User#user.feed, OrigFile, LocalFile) of
                {error, Error} ->
                    attachment_error(?_T(Error)),
                    wf:remove(BoxId);
                {ok, Att} ->
                    case Att#attachment.thumb of
                        undefined -> ok;
                        Thumb ->
                            wf:insert_bottom(attachment_thumb,
                                             #image{image=Thumb, class=upload_attachment_thumb})
                    end,
                    wf:update(BoxId, #span{class=view_media_other_attachment, text=OrigFile}),
                    wf:wire("upd_parent_to_float('"++ wf:to_list(BoxId) ++"');"),
                    ThisMedia = create_media(Att),
                    Medias = wf:state(medias),
                    NewMedias = [ ThisMedia | Medias ],
                    wf:state(medias, NewMedias)
            end
        end
    end.

create_media(Att) ->
    #media{id = Att#attachment.id,
	   title = Att#attachment.name,
	   width = 130,
	   height = 130,
	   url = Att#attachment.file,
	   type = {attachment, Att#attachment.type},
	   thumbnail_url = Att#attachment.thumb}.

%% from view_user

event({subscribe,_,_,_}=Event) ->
    friends:event(Event);
event({unsubscribe,_,_,_}=Event) ->
    friends:event(Event);

event({make_affiliate, User}) ->
    nsx_msg:notify(["affiliates", "user", User, "create_affiliate"], {}),
    wf:redirect("");

% from view_group

event({show_group_edit, Group}) ->
    wf:update(simple_panel, show_editgroup_content(Group)),
    wf:wire(simple_lightbox, #show{});

event(hide_group) ->
    wf:wire(simple_lightbox, #hide{});

event({approve, Who}) ->
    GId = wf:q(id),
    Owner = wf:user(),
    nsm_groups:approve_request(Who, GId, Owner),
%    Res = nsm_groups:join_group(GId,Who),
    wf:replace(incoming_requests, incoming_requests()),
    wf:wire(simple_lightbox, #hide{});

event({reject, Who}) ->
    GId = wf:q(id),
    Owner = wf:user(),
    nsm_groups:reject_request(Who, GId, Owner),
%    nsx_msg:notify(["subscription", "user", User, "reject_invite_to_group"], {GId, Who, ?_T("Sorry")}),
    wf:replace(incoming_requests, incoming_requests()),
    wf:wire(simple_lightbox, #hide{});

event(update_group) ->
    GId = wf:q(id),
    NewUId = wf:q(group_username),
    NewName = wf:q(group_name),
    NewDesc = wf:q(group_desc),
    NewOwner = wf:q(group_owner),
    NewPublicity = wf:q(group_publicity),
    case nsm_users:get_user(NewOwner) of
        {ok, _} ->
            nsx_msg:notify(["db", "group", GId, "update_group"], 
                {wf:user(), NewUId, NewName, NewDesc, NewOwner, NewPublicity}),
            wf:update(group_info_name, wf:q(group_name)),
            wf:update(group_info_publicity, wf:q(group_publicity)),
            wf:update(group_info_owner, wf:q(group_owner)),
            wf:update(group_info_description, wf:q(group_desc)),
            wf:wire(simple_lightbox, #hide{});
        {error, _} ->
            wf:wire(#alert{text=?_TS("User '$username$' does not exist!", [{username, NewOwner}]) })
    end;

event(join_group) ->
    GId = wf:q(id),
    User = wf:user(),
    Rpc = nsm_groups:join_group(GId,User),
    io:format("Join_group result = ~p~n", [Rpc]),
    Replace = case Rpc of
        {ok, joined} -> joined();
        {ok, requested} -> requested();
        {error, already_sent} -> already_sent();
        {error, not_found} -> msg_error();
        _ -> msg_error()
    end,
    wf:replace(join_notice, Replace);
%    wf:wire("location.reload()");

event({invite_act, WhoName, GId}) ->
    wf:update(simple_panel, invition_form(WhoName, GId)),
    wf:wire(simple_lightbox, #show{});

event({leave_group, Group}) when is_record(Group, group) ->
    wf:update(simple_panel, leave_group_form(Group)),
    wf:wire(simple_lightbox, #show{});

event({do_leave, GId}) ->
    User = wf:user(),
    nsx_msg:notify(["subscription", "user", User, "leave_group"], {GId}),
    wf:wire(simple_lightbox, #hide{}),
    wf:redirect(?_U("/wall"));

event(Event) ->
    case wf:user() of
	undefined -> wf:redirect_to_login(?_U("/login"));
        User      -> inner_event(Event, User)
    end.

inner_event(account, _) ->
    wf:redirect(?_U("/login"));

inner_event(show_add_attachment, _) ->
    check_number_of_uploads(attachment_error) == ok andalso
    begin
        TempId = wf:temp_id(),
        UploadId = lists:concat(["upload_", TempId]),
        Body = #panel{style="clear:both", id=TempId, body=[
            #panel{id=UploadId, body=[
                #upload{tag={entry_att, TempId}, show_button=false}
            ]}
        ]},
        wf:insert_bottom(attachment_box, Body)
    end;

inner_event({add_entry, _}, User) ->
  UserDashboard = wf:state(feed_owner),
  DashboardOwner = case UserDashboard of
    {_Type, Name} -> Name;
    _ -> User
  end,
  OrigDesc = wf:q(add_entry_textbox),
  reset_number_of_uploads(),
  DescBin = list_to_binary(OrigDesc),
  DescRunes = unicode:characters_to_list(DescBin),

  case DescRunes of
    "" -> ok;
    _Error when is_tuple(_Error) -> ok; %% corrupted utf-8 string
    _ ->
      DescTruncated = string:substr(DescRunes,1,?ENTRY_TEXT_LENGTH),
      RawDescBin = unicode:characters_to_binary(DescTruncated),
      RawDesc = binary_to_list(RawDescBin),
      Desc = wf:html_encode(RawDesc, true),

      Medias = case wf:state(medias) of
        undefined -> [];
        PreMedias when is_list(PreMedias) -> mkh_clear_list(PreMedias, []);
        _ -> []
      end,
      wf:state(medias, []),
      wf:update(attachment_thumb, []),
      wf:update(attachment_box, []),

      post_entry(DashboardOwner, Desc, Medias),

      nsx_msg:notify(["feed", "user", wf:user(), "count_entry_in_statistics"], {}),

      wf:update(text_length, ""),
      wf:wire(#attr{target=add_entry_textbox, attr="value", value=""})
  end;

inner_event({comment_entry, EntryTrueId, _CommentsPanelId, SourceElementId, _ViewAtt, MSI}, User) ->
    UserDashboard = wf:state(feed_owner),
    {OwnerType, DashboardOwner} = case UserDashboard of
        {T, Name} ->
            {T, Name};
        _ ->
            {user, wf:user()}
    end,
    reset_number_of_uploads(EntryTrueId),
    Value = wf:q(SourceElementId),
    case Value of
        "" -> ok;
        _ ->
            Medias = case wf:state(MSI) of
                undefined -> [];
                Other -> lists:reverse(Other)
            end,
            wf:state(MSI, []),
            nsx_msg:notify(["feed", "user", wf:user(), "count_comment_in_statistics"], {}),  
            nsx_msg:notify([feed, OwnerType, DashboardOwner, comment, utils:uuid_ex(), add],
                [User, EntryTrueId, undefined, Value, Medias])
    end;

inner_event({remove_entry, EId, PanelId, ETo, From}, _) ->
    wf:wire(#confirm { text=?_T("Do you want to remove this entry?"),
                       postback={remove_entry, EId, PanelId, ETo, From, true} });

inner_event({remove_entry, EId, _PanelId, _ETo, From, true}, _) ->
    CurrentUser = wf:user(),
    {Type, Owner} = case wf:state(feed_owner) of
        {T, Name} -> {T, Name};
        _ -> {user, CurrentUser}
    end,
    case Type of
      _Any when From == CurrentUser ->
        nsx_msg:notify([feed, Type, Owner, entry, EId, delete], [From]);
      group ->
        case nsm_groups:user_is_owner(CurrentUser, Owner) of
          true -> nsx_msg:notify([feed, Type, Owner, entry, EId, delete], [From]);
          _ -> ok
        end;
      user when CurrentUser == Owner ->
        nsx_msg:notify([feed, Type, Owner, entry, EId, delete], [From]);
      Type ->
        ?INFO("Remove entry of unknown type: ~p canceled.~n", [Type])
    end;

inner_event({show_all_likers, LeftPart, Id}, _) ->
    wf:update(Id, LeftPart);

inner_event({like_entry, E, LikeBtnId}, User) ->
    {Type, Owner} = case wf:state(feed_owner) of
        {T, O} ->
            {T, O};
        _ ->
            {user, User}
    end,
    nsx_msg:notify(["likes", Type, Owner, "add_like"], {User, E}),
    wf:replace(LikeBtnId, []);

inner_event({share_entry, Entry, ShareBtnId}, User) ->
    wf:replace(ShareBtnId, ""),
    Type = user,
    nsx_msg:notify([feed, Type, User, entry, utils:uuid_ex(), share], Entry),
     % it is not good design to issue id in web part, but we need it both in comet and db
    wf:wire(#alert{text=?_T("It will now appear in your feed.")});

inner_event({unsubscribe, UserUid}, User) ->
    nsx_msg:notify(["subscription", "user", User, "remove_subscribe"], {UserUid}),
    wf:wire("location.reload(true);");

inner_event({subscribe, UserUid}, User) ->
    nsx_msg:notify(["subscription", "user", User, "subscribe_user"], {UserUid}),
    wf:wire("location.reload(true);");

inner_event({set_user_status, Status}, User) ->
    nsx_msg:notify(["subscription", "user", User, "set_user_game_status"], {Status});

inner_event({set_user_status}, User) ->
    nsx_msg:notify(["subscription", "user", User, "set_user_game_status"], {wf:q(user_status)});

inner_event({block, CheckedUser}, User) ->
    nsx_msg:notify(["subscription", "user", User, "block_user"], {CheckedUser}),
    wf:update(blockunblock, #link{text=?_T("Unblock this user"), url="javascript:void(0)", postback={unblock, CheckedUser}}),
    wf:update(feed, user_blocked_message(CheckedUser));

inner_event({unblock, CheckedUser}, User) ->
    nsx_msg:notify(["subscription", "user", User, "unblock_user"], {CheckedUser}),
    wf:update(blockunblock, #link{text=?_T("Block this user"), url="javascript:void(0)", postback={block, CheckedUser}}),
    Fid = webutils:user_info(User, feed),
    show_feed(Fid, user, User#user.username);

inner_event({unblock_load, CheckedUser, _Offset}, User) ->
    nsx_msg:notify(["subscription", "user", User, "unblock_user"], {CheckedUser}),
    wf:update(blockunblock, #link{text=?_T("Block this user"), url="javascript:void(0)", postback={block, CheckedUser}}),
    Fid = webutils:user_info(User, feed),
    show_feed(Fid, user, User#user.username);

inner_event(notice_close, _) ->
    wf:update(notification_area, []);

inner_event(notice_resend, _User) ->
    UserInfo = webutils:user_info(),
    Mail = UserInfo#user.email,
    VerificationCode = UserInfo#user.verification_code,
    {Subject, PlpainText} = mail_construction:verification(Mail, VerificationCode),
    nsx_msg:notify_email(Subject, PlpainText, Mail),
    wf:update(notification_area, [#notice{type=message, title=?_T("Verification letter sent"),
                        body = ?_T("Please check your mailbox."), delay=2000}]);

inner_event(Other, _) -> webutils:event(Other).

mkh_clear_list([], A)           -> A;
mkh_clear_list(undefined, A)    -> A;
mkh_clear_list([H|T], A) -> mkh_clear_list(T, [H] ++ A).

get_page_number() ->
    case wf:q("p") of
        undefined -> 1;
        Ofs       ->
            try list_to_integer(Ofs) of
                Ofs1 -> Ofs1
            catch
               _:_   -> 1
            end
    end.

more_entries(Entry) ->
  Fid = webutils:user_info(feed),
  Pid = list_to_pid(wf:state(comet_feed_pid)),
  Entrs = read_entries(Pid, element(1,Entry#entry.id), Fid),
  Last = case Entrs of
    [] -> [];
     _ -> lists:last(Entrs)
  end,
  Pid ! {delivery, check_more, {?MODULE, length(Entrs), Last}}.

textboxlist_event(SearchTerm) ->
  DataU = case nsm_users:list_subscr(wf:user()) of
    [] -> [];
    Sub -> [{Who, user} || #subs{whom = Who} <- Sub]
  end,
  DataG = case nsm_groups:list_groups_per_user(wf:user()) of
    [] -> [];
    Gs ->
      [case nsm_groups:get_group(GId) of
        {ok, Group} -> {Group#group.name, group};
        {error, notfound}-> []
      end || GId <- Gs]
  end,
  Data = lists:filter(fun({E,_}) -> string:str(string:to_lower(E), string:to_lower(SearchTerm)) > 0 end, lists:flatten(DataU ++ DataG)),
  List = [{array, [
    list_to_binary([atom_to_list(T), "_", string:to_lower(L)]),
    list_to_binary(L),
    list_to_binary("<b>" ++ L ++ "</b>"),
    list_to_binary("<img src=\""
      ++ avatar:get_avatar_by_username(string:to_lower(L), tiny)
      ++ "\" style='float:left;width:30px;height:30px;margin:0 5px 0 0;'/>"
      ++ L)] }
  || {L,T} <- Data],
  mochijson2:encode({array, List}).

inplace_textbox_event(_, Value, {undefined, undefined}) -> Value;
inplace_textbox_event(_, Value, {EntryId, _FeedId}) ->
    Route = [feed, user, wf:user(), entry, EntryId, edit],
%    ?PRINT({wf:user(), Route, nsm_mq_lib:list_to_key(Route)}),
    nsx_msg:notify(Route, [Value]),
    Value.

attachment_error(Text) ->  attachment_error(attachment_error, Text).
attachment_error(AttachmentErrorBox, Text) ->  wf:update(AttachmentErrorBox, #notice{type=error, delay=1800, body=Text}).

check_number_of_uploads(ErrorBox) ->  check_number_of_uploads(ErrorBox, main_form).
check_number_of_uploads(ErrorBox, Entry) ->
    Key = {Entry, num_of_attached},
    NA = case wf:state(Key) of
        undefined ->
            0;
        NAttached ->
            NAttached
    end,

    if
        NA < ?MAX_NUM_OF_ATTACMNETS_PER_TIME ->
            wf:state(Key, NA+1),
            ok;
        true ->
            attachment_error(ErrorBox, ?_TS("You can't upload more than $number$ files at once",
                [{number, ?MAX_NUM_OF_ATTACMNETS_PER_TIME}])),
            error
    end.

reset_number_of_uploads() ->  reset_number_of_uploads(main_form).
reset_number_of_uploads(Entry) ->  wf:state({Entry, num_of_attached}, 0).

verification_notification() ->
    #notice{type=message, position=left, title=?_T("Verification required"),
        body=[
            #p{body=?_T("Email address must be verified!")},
            #button{class="fb_continue", postback=notice_resend, text=?_T("Resend verification")},
            #button{class="fb_continue", postback=notice_close,  text=?_T("Close")}
    ]}.

has_group_destination([]) -> false;
has_group_destination([H | T]) ->
  case H of
    {_, group} -> true;
    _ -> has_group_destination(T)
  end.

post_entry(DashboardOwner, Desc, Medias) ->
  User = wf:user(),
  % please consult me before changing logic here <maxim@synrc.com>
  Recipients= [
    case lists:prefix("user_", E) of
      true-> {lists:nthtail(length("user_"), E), user};
      false ->
        case lists:prefix("group_", E) of
          true -> {lists:nthtail(length("group_"),E), group};
          false -> []
        end
    end
  || E <- string:tokens(lists:flatten(wf:qs(recipients_list)), ",")],

  Destinations = case Recipients of
    [] -> [{DashboardOwner, user}];
    D -> D
  end,

  case has_group_destination(Destinations) of    % to avoid doubles when posting to group from own feed
    true -> Destinations2 = lists:delete({User, user}, Destinations);
    false -> Destinations2 = Destinations
  end,

  [begin
    Route = [feed, Type, To, entry, utils:uuid_ex(), add],
    ?PRINT({To, Route, nsm_mq_lib:list_to_key(Route)}),
    nsx_msg:notify(Route, [User, Destinations, Desc, Medias])
  end || {To, Type} <- Destinations2].

%% @doc Decode/encode term to/from base64 string
encode_term(Term) ->
    Binary = term_to_binary(Term),
    base64:encode(Binary).

decode_term(String) ->
    Bianry = base64:decode(String),
    binary_to_term(Bianry).

s_T(String) ->
    matchmaker:s_T(String). % this just makes sure nothing in text will break JS

%%%%%%%%%%%%%%%%%% GROUP BEHAAVIOR

req_invite() ->
    GId = wf:q(id),
    UId = wf:user(),
    case {nsm_groups:group_publicity(GId), nsm_groups:group_user_type(UId, GId)} of
        {_, member} -> [];
        {public, _} -> join_form();
        {private, req} -> requested();
        {private, reqrejected} -> req_rejected_form(GId);
        {_, _} -> req_invite_form()
    end.

%TODO
join_form() ->
    [
        #panel{id=join_notice, class="join-notice", body=[
                ?_T("Like this group? "),
                #cool_button{postback=join_group, text=?_T("Join it NOW!")},
                #grid_clear{}
            ]}
    ].

joined() ->
    [
        #panel{id=join_notice, class="join-notice", body=[
                ?_T("You are now member of this group!"),
                #grid_clear{}
            ]}
    ].
already_sent() ->
    [
        #panel{id=join_notice, class="join-notice", body=[
                ?_T("You have alrady requested in past"),
                #grid_clear{}
            ]}
    ].
requested() ->
    [
        #panel{id=join_notice, class="join-notice", body=[
                ?_T("Request sent."),
                #grid_clear{}
            ]}
    ].
msg_error() ->
    [
        #panel{id=join_notice, class="join-notice", body=[
                ?_T("Error, try again later"),
                #grid_clear{}
            ]}
    ].

%TODO
req_invite_form() ->
    [
        #panel{id=join_notice, class="join-notice", body=[
                ?_T("Like this group? "),
                #cool_button{postback=join_group, text=?_T("Request an invitation!")},
                #grid_clear{}
            ]}
    ].

req_rejected_form(GId) ->
    [
        #panel{id=join_notice, class="join-notice", body=[
                ?_T("Your request have been rejected "),
                #cool_button{postback={do_leave, GId}, text=?_T("Leave group")},
                #grid_clear{}
            ]}
    ].

%TODO
hidden_form() ->
    [
        #panel{class="form-001", body=[
                ?_T("This is private group, only members can see updates."),
                #panel{style="height:10px;clear:both"}
            ]}
    ].

not_found(String) ->
    [
        #panel{class="form-001", body=[
                ?_T(String ++ " not found"),
                #panel{style="height:10px;clear:both"}
            ]}
    ].

get_members() ->
    GId = wf:q(id),
    UId = wf:user(),
    [
        case nsm_groups:user_is_owner(UId, GId) of
            true -> incoming_requests();
            _ -> []
        end,
        case nsm_groups:user_has_access(UId, GId) of
            true ->  webutils:get_members(GId);
            false -> []
        end
    ].

incoming_requests() ->
    GId = wf:q(id),
    MemberTypes = nsm_groups:list_group_members_with_types(GId),
    Requests = [
        begin
            RealName = nsm_users:user_realname(UId),
            #listitem{body=[
                #link{ text = RealName, postback={invite_act, UId, GId} }
            ]}
        end
    || {UId, UType} <- MemberTypes, UType == req ],
    case Requests of
        [] ->
            [];
        _ ->
            [#panel{class="box", id=incoming_requests, body=[
               #h3{text=?_T("Invite requests")},
               #list{class="list-photo", body=[ Requests ]}
            ]}]
    end.

group_info(Group) ->
  UId = wf:user(),
  GId = wf:q(id),
%  case nsm_groups:get_group(GId) of
%    {error, notfound} -> [];
%    {ok, Group} ->
      CTime = Group#group.created,
      {D,_H} = calendar:now_to_local_time(CTime),
      Date = io_lib:fwrite("~b/~b/~b", tuple_to_list(D)),

      Ava = webutils:get_group_avatar(Group#group.username, "big"),

      Description = case nsm_groups:user_has_access(UId, GId) of
        true -> #span{id=group_info_description, style="font-size:11pt;", text=Group#group.description};
        false -> []
      end,
      MemberCount = Group#group.users_count,
      Membership = case nsm_groups:user_in_group(UId, GId) of
        true ->
          case Group#group.username of
            "kakaranet" -> ""; 
            _ ->
              case MemberCount of
                1 -> "";
                _ ->
                  #link{text=?_T("Leave group"), postback={leave_group, Group}, id="leavegrouplink",
                    style="padding-left:17px; font-weight:bold; font-size:1.1em;",
                    title=?_T("You may unsubscribe from group messages this way.
                              You can also subscribe back later if you wish")}
              end
          end;
        false -> ""
      end,
      GroupTitle = case Group#group.name of
        []-> Group#group.username;
        Name -> Name
      end,

      #panel{class="box user-info", body=[
        #h3{id=group_info_name, text=GroupTitle, class="section-title", style="background:#4ec3f1;"},
        Description,
        #br{},
        #br{},
        #panel{class=img, body=#image{image=Ava}},
        #list{class=user_info, body=[
          #listitem{body=[?_T("Publicity")++": ",
            #span{id=group_info_publicity, text=
              case Group#group.publicity of
                public -> ?_T("Public group");
                private -> ?_T("Private group")
              end
            }]},
          #listitem{body=[?_T("Created")++": ",#span{text=Date}]},
          #listitem{body=[?_T("Owner")++": ",#span{id=group_info_owner, text=Group#group.owner}]},
          #listitem{body=[?_T("Members")++": ",#span{text=integer_to_list(MemberCount)}]}
        ]},
        Membership,
        group_edit_form(Group),
        #br{},
        #br{}
      ]}.
%  end.

user_in_group() ->
    GId = wf:q(id),
    Members = nsm_groups:list_group_members(GId),
    [   ?_T("Users in this group:"),
         #panel{style="text-align: left;", body=[view_user(Members)]}
    ].

view_user(Users) ->
    [#panel{body=site_utils:user_vcard(Who)} || Who <- Users ].

group_edit_form(Group) ->
    Owner = Group#group.owner,
    case wf:user() of
        Owner -> [#br{}, 
                 #link{text=?_T("Group settings"), postback={show_group_edit, Group}, 
                       style="padding-left:17px; font-weight:bold; font-size:1.1em;"} ];
        _ -> []
    end.

show_editgroup_content(Group) ->
    Title = #h1{class = "head", text = ?_T("Group settings")},
    Settings = editgroup_form(Group),
    Body = [Title,
            #panel{class=holder, body=
             [Settings,
              #br{},
              #cool_button{postback=update_group, text=?_T("Update settings")},
              #cool_button{postback=hide_group, text=?_T("Cancel")},
              #grid_clear{}
           ]}],
    webutils:lightbox_panel_template(simple_lightbox, Body).

editgroup_form(Group) ->
    [#panel { class="group-settings", body = [
        #grid_4 { body=[
            #panel{
                body = [
                    #label{style="float:left;", text = ?_T("Group username") ++ ": "},
                    #label{id = group_username, style="font-weight:bold;", text=Group#group.username}
            ]},            
            #label{text = ?_T("Group name")},
            #panel{class = "text",
                body = [#textbox{id = group_name, text=Group#group.name}]},
            #label{text = ?_T("Group owner")},
            #panel{class = "text",
                body = [#textbox{id = group_owner, text=Group#group.creator}]}
        ]},
        #grid_4 { body=[
            #label{text = ?_T("Group description")},
            #panel{class = "textarea", style="height:74px;",
                body = [#textarea{id = group_desc, text=Group#group.description, style="resize:none; height:60px;"}]},
            #panel{class = "error", body=[
                #label{text = "", class="error", id="update_error"}
            ]},
            #label{text = ?_T("Publicity")},
            #panel{class="sel", body=[
                #dropdown{class="cs-3", id=group_publicity, value=Group#group.publicity, options=[
                    #option{text=?_T("Public group"), value=public},
                    #option{text=?_T("Private group"), value=private}
                ]}
            ]}
        ]},
        #grid_clear{}
    ]}].

invition_form(Who, GId) ->
    Title = #h1{class = "head", text = ?_TS("Invite request from $user$", [{user, Who}])},
    Body = [Title,
            #panel{class=holder, body=
             [#cool_button{postback={approve, Who}, text=?_T("Approve")},
              #cool_button{postback={reject, Who}, text=?_T("Reject")},
              #cool_button{postback=hide_group, text=?_T("Decide later")},
              #grid_clear{}
           ]}],
    webutils:lightbox_panel_template(simple_lightbox, Body).

leave_group_form(Group) ->
    Title = #h1{class = "head", text = ?_TS("Leave group $group$", [{group, Group#group.name}])},
    Body = [Title,
            #panel{class=holder, body=
             [?_T("Are you sure to leave this group?"), #br{}, #br{},
              #cool_button{postback={do_leave, Group#group.username}, text=?_T("Yes, i'm leaving")},
              #cool_button{postback=hide_group, text=?_T("No, i'm staying")},
              #grid_clear{}
           ]}],
    webutils:lightbox_panel_template(simple_lightbox, Body).

api_event(Name, Tag, Data)-> webutils:api_event(Name, Tag, Data).

