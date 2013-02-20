%% -*- mode: nitrogen -*-
-module (view_group).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("nsm_db/include/user.hrl").
-include_lib("nsm_db/include/feed.hrl").
-include("gettext.hrl").
-include("setup.hrl").
-include("elements/records.hrl").
-include_lib("nsx_config/include/log.hrl").

main() -> dashboard:main().

title() -> webutils:title(?MODULE).

body() ->
  GId = wf:q(id),
  UId = wf:user(),
  {_, Group} = nsm_groups:get_group(GId),
  #panel{class="page-content page-canvas", style="overflow:auto;margin-top:20px;", body=[
    "<section id=\"content\">",
      case Group of
        notfound -> [no_group()];
        _ -> [
          req_invite(),
          case nsm_groups:user_has_access(UId, GId) of
            true -> dashboard:feed(group, GId);
            false -> hidden_form()
          end
        ]
      end,
    "</section>",
    #panel{class="aside", body=[
      #panel{id=aside,body=[
        group_info(),
        get_members()
      ]}
    ]}
  ]}.

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

no_group() ->
    [
        #panel{class="form-001", body=[
                ?_T("Group not found"),
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

group_info() ->
  UId = wf:user(),
  GId = wf:q(id),
  case nsm_groups:get_group(GId) of
    {error, notfound} -> [];
    {ok, Group} ->
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
            "kakaranet" -> ""; %PUBLIC BETA One can not unsubscribe from kakaranet for now. 
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
        #h3{id=group_info_name, text=GroupTitle},
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
      ]}
  end.

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
        Owner -> [#br{}, #link{text=?_T("Group settings"), postback={show_group_edit, Group}, style="padding-left:17px; font-weight:bold; font-size:1.1em;"} ];
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
    ?INFO("Invition form: ~p ~p", [Who, GId]),
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

api_event(Name, Tag, Args)->
  webutils:api_event(Name, Tag, Args).

event({show_group_edit, Group}) ->
    wf:update(simple_panel, show_editgroup_content(Group)),
    wf:wire(simple_lightbox, #show{});

event(hide_group) ->
    wf:wire(simple_lightbox, #hide{});

event({approve, Who}) ->
    GId = wf:q(id),
    Owner = wf:user(),
    ?INFO("Approve: ~p to ~p",[Who,GId]),
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
    wf:redirect(?_U("/dashboard"));

event(Other) ->
    dashboard:event(Other).

textboxlist_event(SearchTerm)-> dashboard:textboxlist_event(SearchTerm).
more_entries(Entry) -> dashboard:more_entries(Entry).

finish_upload_event(X1, X2, X3, X4) ->
    dashboard:finish_upload_event(X1, X2, X3, X4).

inplace_textbox_event(Tag, Value, FeedEntry) ->
    dashboard:inplace_textbox_event(Tag, Value, FeedEntry).

start_upload_event({entry_att, BoxId}) ->
    dashboard:start_upload_event({entry_att, BoxId}).
