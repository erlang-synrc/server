-module (dashboard).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("nsm_db/include/user.hrl").
-include_lib("nsm_db/include/feed.hrl").
-include_lib("nsm_db/include/attachment.hrl").
-include_lib("nsx_config/include/log.hrl").

-include("elements/records.hrl").
-include_lib("nsm_db/include/membership_packages.hrl").

-include("gettext.hrl").
-include("setup.hrl").

-define(ENTRY_TEXT_LENGTH, 350).
-define(PAGEAMOUNT, 3).
-define(MAX_NUM_OF_ATTACMNETS_PER_TIME, 3).
-define(TOOLTIP_TIMEOUT, "1500").

title() -> webutils:title(?MODULE).

main() ->
  case wf:user() of
    undefined -> wf:redirect(?_U("/login"));
    _User ->
      case wf_context:page_module() of
        dashboard -> [];
        _ -> webutils:js_for_main_authorized_game_stats_menu()
      end,
      case webutils:guiders_ok("dashboard_guiders_shown") of
        true -> guiders_script();
        false -> ok
      end,
      case wf_context:page_module() of
        dashboard ->
          #template { file=code:priv_dir(nsp_srv)++"/templates/base.html"};
        _ ->
          #template { file=code:priv_dir(nsp_srv)++"/templates/bare.html"}
      end
  end.

body() ->
  #panel{class="page-content page-canvas", style="overflow:auto;margin-top:20px;", body=[
    "<section id=\"content\">", feed(), "</section>",
    #panel{class="aside", body=[
      webutils:get_ribbon_menu(),
      #panel{id=aside,body=dashboard:aside()}
    ]}
  ]}.

feed() ->
  UserInfo = webutils:user_info(),
  NotVerified = UserInfo#user.status == not_verified,
  BuySuccess = wf:q(buy) == "success",
  InternalError = wf:q('__submodule__') == "internal_error",

  if NotVerified ->
      %% show notification about email verification
      wf:update(notification_area, verification_notification());
    BuySuccess ->
      case buy:package() of
        undefined -> ?WARNING("buy success received, but there are no package information in session");
        Package -> wf:update(notification_area, buy_success_notification(Package))
      end;
    InternalError -> wf:update(notification_area, internal_error_notification());
    true -> ok
  end,

  FId = webutils:user_info(feed),
  [
    #panel{id=notification_area},
    entry_form(FId),
    #panel{id=feeds_container, class="posts_container_mkh", body=show_feed(FId)}
  ].

entry_form(FId) -> entry_form(FId, ?MODULE, {add_entry, FId}).

entry_form(FId, Delegate, Postback) ->
%  wf:wire(
%    wf:f("objs('add_entry_textbox')
%      .bind('keyup keydown change', function(){
%        var $this=objs('add_entry_textbox');
%        var l = parseInt($this.attr('value').length);
%        if(l > 0){
%          objs('sendentry').css('background','url(/images/grn-shr-btn.png) no-repeat');
%          objs('sendentry').css('cursor','pointer');
%        }
%        if(l <= 0){
%          objs('sendentry').css('background','url(/images/gre_shr_btn.png) no-repeat');
%          objs('sendentry').css('cursor','default');
%        }
%        objs('text_length').text((l>1000)?(~b-l):'');
%      })
%      .bind('keypress', function(e){
%        var code = e.keyCode || e.which;
%        if (code == 13) {
%          if (!e.shiftKey) {~s; return false;}  // send postback
%        }
%        if (code != 116 && code != 46 && code > 40 || code == 32){
%          return $(this).trigger('change').attr('value').length < ~b // deny only text keys
%        }
%      })",
%      [?ENTRY_TEXT_LENGTH, site_utils:postback_to_js_string(Delegate, Postback), ?ENTRY_TEXT_LENGTH]
%  )),
  [
  #panel{class=entry_form, body=[
    #label{text=?_T("To")++":"},
    #panel{class=autocompletelist, body=#textboxlist{id=recipients_list}},
    #panel{id=flashm, body=[]},
    #panel{body=[
      "<span id='guidersaddentrybox'>", #textarea{id=add_entry_textbox, placeholder=?_T("Put your thoughts in here...")},"</span>",
      "<span id='guiderssharebutton'>",
        #button{id="sendentry", postback={add_entry, FId}, text=?_T("Share"), class="submit",
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

show_feed(undefined) -> [];
show_feed(Fid) ->
  {ok, Pid} = comet_feed:start(user, Fid, wf:user(), wf:session(user_info)),
  wf:state(comet_feed_pid, pid_to_list(Pid)),
%  X = dashboard:aside(),
  spawn(fun()->
%    Pid ! {delivery, aside, X},
    Entrs = dashboard:read_entries(Pid, undefined, Fid),
    Last = case Entrs of
      [] -> [];
      _ -> lists:last(Entrs)
    end,
    Pid ! {delivery, check_more, {?MODULE, length(Entrs), Last}}
  end),
  [
    #panel{id = feed, body=[]},
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

aside() ->
    Fv = webutils:get_friends(),
    Gv = webutils:get_groups(),
    [Fv,Gv].

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
    nsx_msg:notify([feed, Type, User, entry, utils:uuid_ex(), share], Entry), % it is not good design to issue id in web part, but we need it both in comet and db
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

%inner_event({direct_message_to, CheckedUser}, _) ->
    %autocomplete_select_event({struct, [{<<"id">>, CheckedUser},{<<"value">>, CheckedUser}]} , CheckedUser),
    %wf:wire("set_focus_to_search()"),
%    ok;

inner_event({block, CheckedUser}, User) ->
    nsx_msg:notify(["subscription", "user", User, "block_user"], {CheckedUser}),
    wf:update(blockunblock, #link{text=?_T("Unblock this user"), url="javascript:void(0)", postback={unblock, CheckedUser}}),
    wf:update(feed, user_blocked_message(CheckedUser));

inner_event({unblock, CheckedUser}, User) ->
    nsx_msg:notify(["subscription", "user", User, "unblock_user"], {CheckedUser}),
    wf:update(blockunblock, #link{text=?_T("Block this user"), url="javascript:void(0)", postback={block, CheckedUser}}),
    Fid = webutils:user_info(User, feed),
    show_feed(Fid);

inner_event({unblock_load, CheckedUser, _Offset}, User) ->
    nsx_msg:notify(["subscription", "user", User, "unblock_user"], {CheckedUser}),
    wf:update(blockunblock, #link{text=?_T("Block this user"), url="javascript:void(0)", postback={block, CheckedUser}}),
    Fid = webutils:user_info(User, feed),
    show_feed(Fid);

%inner_event({remove_select_event, Value}, _) ->
%    case wf:session(autocomplete_list_values) of
%        undefined -> ok
%        ;L        -> wf:session(autocomplete_list_values, lists:delete(Value, L)), ok
%    end;

%inner_event({add_to_event, "MyFeed"}, _) ->
%    Owner = case  wf:state(feed_owner) of
%        undefined ->
%            {wf:user(), user};
%        {Type, Name} ->
%            {Name, Type}
%    end,
%    Value = encode_term(Owner),
%    autocomplete_select_event({struct, [{<<"id">>, 1 },{<<"value">>, Value}]} , direct_to);

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
    ?INFO("Page Number"),
    case wf:q("p") of
        undefined -> 1;
        Ofs       ->
            try list_to_integer(Ofs) of
                Ofs1 -> Ofs1
            catch
               _:_   -> 1
            end
    end.

is_direct_message_page() ->
    case wf:q("filter") of
        "direct" -> true
        ;_       -> false
    end.

more_entries(Entry) ->
  Fid = webutils:user_info(feed),
  Pid = list_to_pid(wf:state(comet_feed_pid)),
  Entrs = read_entries(Pid, element(1,Entry#entry.id), Fid),
  Pid ! {delivery, check_more, {?MODULE, length(Entrs), lists:last(Entrs)}}.

textboxlist_event(SearchTerm) ->
  DataU = case nsm_users:list_subscr(wf:user ()) of
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
    list_to_binary("<b>"++L++"</b>"),
    list_to_binary("<b>" ++ L++ "</b>")] }|| {L,T} <- Data],
  mochijson2:encode({array, List}).

inplace_textbox_event(_, Value, {undefined, undefined}) ->
    Value;
inplace_textbox_event(_, Value, {EntryId, _FeedId}) ->
    Route = [feed, user, wf:user(), entry, EntryId, edit],
    ?PRINT({wf:user(), Route, nsm_mq_lib:list_to_key(Route)}),
    nsx_msg:notify(Route, [Value]),
    Value.

attachment_error(Text) ->
    attachment_error(attachment_error, Text).

attachment_error(AttachmentErrorBox, Text) ->
    wf:update(AttachmentErrorBox, #notice{type=error, delay=1800, body=Text}).

check_number_of_uploads(ErrorBox) ->
    check_number_of_uploads(ErrorBox, main_form).

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

reset_number_of_uploads() ->
    reset_number_of_uploads(main_form).

reset_number_of_uploads(Entry) ->
    wf:state({Entry, num_of_attached}, 0).

verification_notification() ->
    #notice{type=message, position=left, title=?_T("Verification required"),
        body=[
            #p{body=?_T("Email address must be verified!")},
            #button{class="fb_continue", postback=notice_resend, text=?_T("Resend verification")},
            #button{class="fb_continue", postback=notice_close,  text=?_T("Close")}
    ]}.


internal_error_notification() ->
    #notice{type = message, delay = 5000, title = ?_T("Something strange happend"),
        body = ?_T("Something strange happend, we really sorry.")}.


buy_success_notification(Package) ->
    PurchaseDetails = #list{body=[
        #listitem{text=[?_T("Package number"),":", wf:to_list(Package#membership_package.no)]},
        #listitem{text=[?_T("Quota"),":", wf:to_list(Package#membership_package.quota)]},
        #listitem{text=[?_T("Price"),":", wf:to_list(Package#membership_package.amount), " TL"]}
    ]},
    [
        #notice{type=message, position=left, title=?_T("Thank you for buying our package."),
            body=[#panel{body=[
                #p{body=?_T("Purchase details")++":"},
                PurchaseDetails
            ]},
                #button{class="fb_continue", postback=notice_close,  text=?_T("Close")}]
        },
        "<!-- Google Code for order Conversion Page -->
        <script type='text/javascript'>
        /* <![CDATA[ */
        var google_conversion_id = 1008605414;
        var google_conversion_language = 'tr';
        var google_conversion_format = '2';
        var google_conversion_color = 'ffffff';
        var google_conversion_label = 'zEe1CPKo1AMQ5rH44AM';
        var google_conversion_value = 0;
        /* ]]> */
        </script>
        <script type='text/javascript' src='https://www.googleadservices.com/pagead/conversion.js'>
        </script>
        <noscript>
        <div style='display:inline;'>
        <img height='1' width='1' style='border-style:none;' alt='' src='https://www.googleadservices.com/pagead/conversion/1008605414/?value=0&amp;label=zEe1CPKo1AMQ5rH44AM&amp;guid=ON&amp;script=0'/>
        </div>
        </noscript>"
    ].

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

% guiders script for dashboard
guiders_script() ->
    wf:wire("
        guiders.createGuider({
            buttons: [
                {name: '"++s_T("No, thanks")++"', onclick: guiders.hideAll},
                {name: '"++s_T("Continue")++"', onclick: guiders.next}
            ],
            description: '"++s_T("Here you can share your thoughts, interesting links, music and pictures with the public. You can make friends, join different groups or even create your own group. Here, let us show you everything in detail.")++"',
            id: 'guider_10',
            next: 'guider_20',
            overlay: true,
            title: '"++s_T("Welcome to Kakaranet dashboard")++"'
        }).show();

        guiders.createGuider({
            attachTo: '#guiderssharebutton',
            position: 9,
            buttons: [
                {name: '"++s_T("<< Back")++"', onclick: guiders.prev},
                {name: '"++s_T("Continue")++"', onclick: guiders.next}
            ],
            description: '"++s_T("This is the main share button. You press it and all you have written in a text area will be sent to your friends dashboards. Along with the attachments of course.")++"',
            id: 'guider_20',
            next: 'guider_30',
            overlay: false,
            xButton: true,
            title: '"++s_T("Share button")++"'
        });

        guiders.createGuider({
            attachTo: '#guidersaddentrybox',
            position: 6,
            buttons: [
                {name: '"++s_T("<< Back")++"', onclick: guiders.prev},
                {name: '"++s_T("Continue")++"', onclick: guiders.next}
            ],
            description: '"++s_T("And this is the text area for writing. You can also put youtube, vimeo or other links in here.")++"',
            id: 'guider_30',
            next: 'guider_40',
            overlay: false,
            xButton: true,
            title: '"++s_T("Text area")++"'
        });

        guiders.createGuider({
            attachTo: '#guidersattachmentbox',
            position: 6,
            buttons: [
                {name: '"++s_T("<< Back")++"', onclick: guiders.prev},
                {name: '"++s_T("Continue")++"', onclick: guiders.next}
            ],
            description: '"++s_T("You can also add files to your entry. Right now the limit is 3 files and 30 Mbytes per post. And yes, uploading big files might take a little lime, so please be patient with this.")++"',
            id: 'guider_40',
            next: 'guider_50',
            overlay: false,
            xButton: true,
            title: '"++s_T("Attachment box")++"'
        });

        guiders.createGuider({
            attachTo: '#guidersmyfeed',
            position: 11,
            buttons: [
                {name: '"++s_T("<< Back")++"', onclick: guiders.prev},
                {name: '"++s_T("Continue")++"', onclick: guiders.next}
            ],
            description: '"++s_T("Your feed combines all the messages from your friends, groups along with your own entries. Also, when you press small \"share\" button on some other entry, it also goes to your feed")++"',
            id: 'guider_50',
            next: 'guider_60',
            overlay: false,
            xButton: true,
            title: '"++s_T("Your feed")++"'
        });

        guiders.createGuider({
            attachTo: '#guidersdirectmessages',
            position: 7,
            buttons: [
                {name: '"++s_T("<< Back")++"', onclick: guiders.prev},
                {name: '"++s_T("Continue")++"', onclick: guiders.next}
            ],
            description: '"++s_T("Sometimes you might want to have a private conversations with someone. Direct messages are the right tool for this. They do not go in anyones feed, instead they are only available from here.")++"',
            id: 'guider_60',
            next: 'guider_70',
            overlay: false,
            xButton: true,
            title: '"++s_T("Direct messages")++"'
        });

        guiders.createGuider({
            attachTo: '#guidersstatistics',
            position: 3,
            buttons: [
                {name: '"++s_T("<< Back")++"', onclick: guiders.prev},
                {name: '"++s_T("Continue")++"', onclick: guiders.next}
            ],
            description: '"++s_T("This shows your activity on Kakaranet. It does not mean much, but it still good to know, how much people are reading you.")++"',
            id: 'guider_70',
            next: 'guider_80',
            overlay: false,
            xButton: true,
            title: '"++s_T("Statistics")++"'
        });

        guiders.createGuider({
            attachTo: '#guidersfriends',
            position: 3,
            buttons: [
                {name: '"++s_T("<< Back")++"', onclick: guiders.prev},
                {name: '"++s_T("Continue")++"', onclick: guiders.next}
            ],
            description: '"++s_T("As you play games or browse groups, you might find people you want to be friends with. In technical terms this means automatically showing what they are willing to share in your feed.")++"',
            id: 'guider_80',
            next: 'guider_90',
            overlay: false,
            xButton: true,
            title: '"++s_T("Friends")++"'
        });

        guiders.createGuider({
            attachTo: '#guidersgroups',
            position: 3,
            buttons: [
                {name: '"++s_T("<< Back")++"', onclick: guiders.prev},
                {name: '"++s_T("Continue")++"', onclick: guiders.next}
            ],
            description: '"++s_T("Groups are communities of people with common interests. Groups can be public and private. You can join any public group and see its entry right in your feed. You need an invitation to join someones private group though. And also you can create your own group if you like to.")++"',
            id: 'guider_90',
            next: 'guider_100',
            overlay: false,
            xButton: true,
            title: '"++s_T("Groups")++"'
        });

        guiders.createGuider({
            buttons: [
                {name: '"++s_T("You are welcome")++"', onclick: guiders.hideAll}
            ],
            description: '"++s_T("Thank you for going through this guide. Hope this was helpful. If you have any problem, feel free to use our support service.")++"',
            id: 'guider_100',
            overlay: true,
            title: '"++s_T("Thank you!")++"'
        });
    ").

api_event(Name, Tag, Data)->
  webutils:api_event(Name, Tag, Data).
