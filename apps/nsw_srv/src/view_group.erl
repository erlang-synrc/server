%% -*- mode: nitrogen -*-
-module (view_group).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("nsm_srv/include/user.hrl").
-include_lib("nsm_srv/include/feed.hrl").
-include("gettext.hrl").
-include("setup.hrl").
-include("elements/records.hrl").
-include_lib("alog/include/alog.hrl").

main() ->
    case wf:user() /= undefined of
        true  -> main_authorized();
        false -> wf:redirect_to_login(?_U("/login"))
    end.

group_info(all) ->
    GId = wf:q(id),
    case get(group_info) of
        {GId, Info} ->
            Info;
        _ ->
            Info = rpc:call(?APPSERVER_NODE,nsm_groups,get_group,[GId]),
            put(group_info, {GId, Info}),
            Info
    end;
group_info(exists) ->
    Info = group_info(all),
    is_record(Info, group);
group_info(description) ->
    Info = group_info(all),
    Info#group.description;
group_info(username) ->
    Info = group_info(all),
    Info#group.username;
group_info(name) ->
    Info = group_info(all),
    Info#group.name;
group_info(publicity) ->
    Info = group_info(all),
    Info#group.publicity;
group_info(creator) ->
    Info = group_info(all),
    Info#group.creator;
group_info(created) ->
    Info = group_info(all),
    Info#group.created;
group_info(feed) ->
    Info = group_info(all),
    Info#group.feed;
group_info(owner) ->
    Info = group_info(all),
    Info#group.owner;
group_info(access_level) ->
    GId = wf:q(id),
    User = wf:user(),
    case get(group_access_level) of
        {GId, User, AccessLevel} -> AccessLevel;
        _ ->
            AccessLevel = rpc:call(?APPSERVER_NODE,nsm_groups,user_access,[GId,User]),
            put(group_access_level, {GId, User, AccessLevel}),
            AccessLevel
    end;
group_info(member) ->
    GId = wf:q(id),
    case {wf:user(), get(group_members)} of
        {undefined, _} -> false;
        {User, {GId, Members}} ->
            lists:any(fun({A,_})->A==User;(A)->A==User end, Members);
        {User, _} ->
            case get(group_member) of
                {GId, User, Member} -> Member;
                _ ->
                    Member = rpc:call(?APPSERVER_NODE,nsm_groups,user_inside,[GId,User]),
                    put(group_member, {GId, User, Member}),
                    Member
            end
    end;
group_info(membership) ->
    GId = wf:q(id),
    case get(group_membership) of
        {GId, Membership} -> Membership;
        _ ->
            Membership = rpc:call(?APPSERVER_NODE,nsm_groups,list_group_membership,[GId]),
            put(group_membership, {GId, Membership}),
            Membership
    end;
group_info(members) ->
    GId = wf:q(id),
    case get(group_members) of
        {GId, Members} -> Members;
        _ ->
            Members = rpc:call(?APPSERVER_NODE,nsm_groups,list_user_in_group,[GId]),
            put(group_members, {GId, Members}),
            Members
    end.


main_authorized() ->
    dashboard:main_authorized().

title() -> webutils:title(?MODULE).

body() ->
    #template{file=code:priv_dir(nsw_srv)++"/templates/view-group.html"}.

has_access() ->
    case group_info(exists) of
        false ->
            false;
        true ->
            case {group_info(publicity),group_info(member)} of
                {public, _} -> true;
                {moderated, _} -> true;
                {private, true} -> true;
                _ -> false
            end
    end.

content() ->
    case group_info(exists) of
        true -> [
            req_invite(),
            case has_access() of
                true -> feed_form();
                false -> hidden_form()
            end
        ];
        false -> [ no_group() ]
    end.


req_invite() ->
    case {group_info(publicity),group_info(member)} of
        {_, true} -> [];
        {public, false} -> join_form();
        {_, false} -> req_invite_form()
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

feed_form() ->
    FId  = group_info(feed),
    wf:state(feed_owner, {group, wf:q(id)}),
    [
     #panel{body=dashboard:entry_form(FId, dashboard, {add_entry, FId})},
     #grid_clear{},
     #panel{id=attachment_box},
     #grid_clear{},
     #panel{body=view_feed()}
    ].

view_feed() ->
    FId = group_info(feed),
    UId = group_info(username),
    Entries = rpc:call(?APPSERVER_NODE, nsm_db, entries_in_feed, [FId, ?FEED_PAGEAMOUNT]),
    comet_feed:start(group, FId, UId, wf:session(user_info)),
    webutils:view_feed_entries(?MODULE, ?FEED_PAGEAMOUNT, Entries).


get_members() ->
    [
        case group_info(access_level) of
            moder -> incoming_invites();
            admin -> incoming_invites();
            _ -> []
        end,
        case has_access() of
            true ->  webutils:get_members(group_info(username));
            false -> []
        end
    ].

incoming_invites() ->
    Members = group_info(membership),
    Invites = [
        #listitem{body=[
            #link{ text = WhoName, postback={invite_act, WhoName, Who} }
        ]}
    || #group_member_rev{ who = Who, who_name = WhoName, type=Type } <- Members, Type == invreq ],
    case Invites of  %PUBLIC DEMO No requests - no block
        [] ->
            [];
        _ ->
            [#panel{class="box", id=incoming_invites, body=[
               #h3{text=?_T("Invite requests"), style="letter-spacing:0px;"},
               #list{class="list-photo", body=[ Invites ]}
            ]}]
    end.

group_info() ->
    case group_info(exists) of
        false -> [];
        true ->
            CTime = group_info(created),
            {D,_H} = calendar:now_to_local_time(CTime),
            Date = io_lib:fwrite("~b/~b/~b", tuple_to_list(D)),
            Info = group_info(all),

            Ava = webutils:get_group_avatar(Info#group.username, "big"),

            Description = case has_access() of
                true -> #span{id=group_info_description, style="font-size:11pt;", text=Info#group.description};
                false -> []
            end,
            Membership = case group_info(member) of
                true -> 
                    case Info#group.username of
                        "kakaranet" ->
                            ""; %PUBLIC BETA One can not unsubscribe from kakaranet for now. 
                        _ ->
                            #link{text=?_T("Leave group"), postback={leave_group, Info}, id="leavegrouplink", 
                                style="padding-left:17px; font-weight:bold; font-size:1.1em;",
                                title=?_T("You may unsubscribe from group messages this way. 
                                    You can also subscribe back later if you wish")}
                    end;
                false ->
                    %TODO:
                    %#listitem{body=[?_T("$N$ of your friends are members")]}
                    ""
            end,

            #panel{class="box user-info", body=[
                #h3{id=group_info_name, style="letter-spacing:0px;", text=Info#group.name},
                Description,
                #br{},
                #br{},
                #panel{class=img, body=#image{image=Ava}},
            #list{class=user_info, body=[
                    #listitem{body=[?_T("Publicity")++": ",#span{id=group_info_publicity, text=Info#group.publicity}]},
                    #listitem{body=[?_T("Created")++": ",#span{text=Date}]},
                    #listitem{body=[?_T("Owner")++": ",#span{id=group_info_owner, text=Info#group.owner}]},
                    #listitem{body=[?_T("Members")++": ",#span{text=integer_to_list(rpc:call(?APPSERVER_NODE,nsm_groups, get_members_count, [Info#group.username]))}]}
                ]},
                Membership,
                group_edit_form(Info#group.owner),
                #br{},
                #br{}
           ]}
    end.

user_in_group() ->
    Members = group_info(members),
    [?_T("Users in this group:"),
     #panel{style="text-align: left;",
            body=[view_user(Members)]}].



big() ->
    [
    ].

view_user(Users) ->
    [ #panel{body=site_utils:user_vcard(Who)}
      || Who <- Users ].

group_edit_form(Owner) ->
    case wf:user() of
        Owner -> [#br{}, #link{text=?_T("Group settings"), postback=show_group_edit, style="padding-left:17px; font-weight:bold; font-size:1.1em;"} ];
        _ -> []
    end.

show_editgroup_content() ->
    Title = #h1{class = "head", text = ?_T("Group settings")},
    Settings = editgroup_form(),
    Body = [Title,
            #panel{class=holder, body=
             [Settings,
              #br{},
              #cool_button{postback=update_group, text=?_T("Update settings")},
              #cool_button{postback=hide_group, text=?_T("Cancel")},
              #grid_clear{}
           ]}],
    webutils:lightbox_panel_template(simple_lightbox, Body).

editgroup_form() ->
    [#panel { class="group-settings", body = [
        #grid_4 { body=[
            #panel{
                body = [
                    #label{style="float:left;", text = ?_T("Group username") ++ ": "},
                    #label{id = group_username, style="font-weight:bold;", text=group_info(username)}
            ]},            
            #label{text = ?_T("Group name")},
            #panel{class = "text",
                body = [#textbox{id = group_name, text=group_info(name)}]},
            #label{text = ?_T("Group owner")},
            #panel{class = "text",
                body = [#textbox{id = group_owner, text=group_info(creator)}]}
        ]},
        #grid_4 { body=[
            #label{text = ?_T("Group description")},
            #panel{class = "textarea", style="height:74px;",
                body = [#textarea{id = group_desc, text=group_info(description), style="resize:none; height:60px;"}]},
            #panel{class = "error", body=[
                #label{text = "", class="error", id="update_error"}
            ]},
            #label{text = ?_T("Publicity")},
            #panel{class="sel", body=[
                #dropdown{class="cs-3", id=group_publicity, value=group_info(publicity), options=[
                    #option{text=?_T("Public group"), value=public},
                    #option{text=?_T("Private group"), value=private}
                ]}
            ]}
        ]},
        #grid_clear{}
    ]}].

invition_form(WhoName, Who) ->
    io:format("Invition form: ~p ~p~n", [WhoName, Who]),
    Title = #h1{class = "head", text = ?_TS("Invite request from $user$", [{user, WhoName}])},
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

event({change_language,SL}) ->  %PUBLIC BETA this is here just to fix not working language selector bug ASAP
    webutils:event({change_language, SL});

event(show_group_edit) ->
    wf:update(simple_panel, show_editgroup_content()),
    wf:wire(simple_lightbox, #show{});

event(hide_group) ->
    wf:wire(simple_lightbox, #hide{});

event({approve, Who}) ->
    GId = wf:q(id),
    User = wf:user(),
    Rpc = rpc:call(?APPSERVER_NODE,nsm_groups,invite_user,[GId, User, Who]),
    io:format("Approve result=~p~n", [Rpc]),
    wf:replace(incoming_invites, incoming_invites()),
    wf:wire(simple_lightbox, #hide{});

event({reject, Who}) ->
    GId = wf:q(id),
    User = wf:user(),
    Rpc = rpc:call(?APPSERVER_NODE,nsm_groups,reject_invite,[GId, User, Who, "Sorry"]),
    io:format("Rpc = ~p~n", [Rpc]),
    wf:replace(incoming_invites, incoming_invites()),
    wf:wire(simple_lightbox, #hide{});

event(update_group) ->
    GId = wf:q(id),
    NewUId = case {wf:q(group_username),group_info(username)} of
        {UId,UId} -> undefined;
        {UId,_} -> UId
    end,
    NewName = case {wf:q(group_name),group_info(name)} of
        {Name,Name} -> undefined;
        {Name,_} -> Name
    end,
    NewDesc = case {wf:q(group_desc),group_info(description)} of
        {Desc,Desc} -> undefined;
        {Desc,_} -> Desc
    end,
    NewOwner = case {wf:q(group_owner),group_info(owner)} of
        {Owner,Owner} -> Owner; %PUBLIC BETA we need to check if user exists, therefore it shouldn't been unknown
        {Owner,_} -> Owner
    end,
    NewPublicity = case {wf:q(group_publicity),group_info(publicity)} of
        {Publicity,Publicity} -> undefined;
        {Publicity,_} -> Publicity
    end,
    case rpc:call(?APPSERVER_NODE, users, get_user, [{username, NewOwner}]) of
        {ok, _} ->
            Rpc = rpc:call(?APPSERVER_NODE,nsm_groups,update_group,[GId, wf:user(), NewUId, NewName, NewDesc, NewOwner, NewPublicity]),
            case {Rpc,NewUId} of
                {ok, _} -> 
                    wf:update(group_info_name, wf:q(group_name)),
                    wf:update(group_info_publicity, wf:q(group_publicity)),
                    wf:update(group_info_owner, wf:q(group_owner)),
                    wf:update(group_info_description, wf:q(group_desc)),
                    wf:wire(simple_lightbox, #hide{});
                {{error,Reason},_} ->
                    wf:wire(#alert{text=?_TS("Error: '$reason$'!", [{reason, Reason}]) });
                _ ->
                    wf:wire(#alert{text=?_T("Error for unknown reason!") })
            end;
        {error, _} ->
            wf:wire(#alert{text=?_TS("User '$username$' does not exist!", [{username, NewOwner}]) })
    end;

event(join_group) ->
    GId = wf:q(id),
    User = wf:user(),
    Rpc = rpc:call(?APPSERVER_NODE,nsm_groups,join_group,[GId,User]),
    io:format("Join_group result = ~p~n", [Rpc]),
    Replace = case Rpc of
        {ok, joined} -> joined();
        {ok, requested} -> requested();
        {error, already_sent} -> already_sent();
        {error, not_found} -> msg_error();
        _ -> msg_error()
    end,
    wf:replace(join_notice, Replace),
    wf:wire("location.reload()");

event({invite_act, WhoName, Who}) ->
    wf:update(simple_panel, invition_form(WhoName, Who)),
    wf:wire(simple_lightbox, #show{});

event({leave_group, Group}) when is_record(Group, group) ->
    wf:update(simple_panel, leave_group_form(Group)),
    wf:wire(simple_lightbox, #show{});

event({do_leave, GId}) ->
    User = wf:user(),
    Rpc = rpc:call(?APPSERVER_NODE,nsm_groups,leave_group,[GId,User]),
    io:format("Do_leave result = ~p~n", [Rpc]),
    % case Rpc
    wf:wire(simple_lightbox, #hide{}),
    wf:redirect("");

event(Other) ->
    dashboard:event(Other).

%% when more button presed
on_more_entries({EntryId, FeedId}, Count) ->
   rpc:call(?APPSERVER_NODE, nsm_db, entries_in_feed, [FeedId, EntryId, Count]).

finish_upload_event(X1, X2, X3, X4) ->
    dashboard:finish_upload_event(X1, X2, X3, X4).

autocomplete_enter_event(SearchTerm, _Tag) -> dashboard:autocomplete_enter_event(SearchTerm, _Tag).
autocomplete_select_event(SI , _Tag) -> dashboard:autocomplete_select_event(SI, _Tag).

%autocomplete_enter_event(SearchTerm, search_member) ->
%    Members = group_info(members),
%    List = [ {struct,[{id, User}, {label, User}, {value, User}]}
%             || UserStr <- Members, string:str(string:to_lower(UserStr),string:to_lower(SearchTerm))>0, User <- [ list_to_binary(UserStr) ] ],
%    mochijson2:encode(List).

inplace_textbox_event(Tag, Value, FeedEntry) ->
    dashboard:inplace_textbox_event(Tag, Value, FeedEntry).

rpc(Mod, Fun, Args) ->
    rpc:call(?APPSERVER_NODE, Mod, Fun, Args).

%PUBLIC BETA this too. I should merge this two things: dashboard and view_group to one entity
start_upload_event({entry_att, BoxId}) ->
    dashboard:start_upload_event({entry_att, BoxId}).
