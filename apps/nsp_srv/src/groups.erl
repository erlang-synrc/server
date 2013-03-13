%% -*- mode: nitrogen -*-
-module (groups).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("nsm_db/include/user.hrl").
-include_lib("nsm_db/include/feed.hrl").
-include_lib("nsx_config/include/log.hrl").

-include("elements/records.hrl").

-include("gettext.hrl").
-include("setup.hrl").

-define(GROUPPERPAGE, 8).

main() ->
    case wf:user() /= undefined of
        true  -> main_authorized();
        false -> wf:redirect_to_login(?_U("/login"))
    end.

main_authorized() ->
%  user_counter:regroups(),
  Tooltip  = wf:q(tooltip),
  if
    Tooltip /= undefined ->
      tooltip_content(Tooltip);
    true ->
      #template { file=code:priv_dir(nsp_srv) ++ "/templates/base.html" }
  end.

title() -> webutils:title(?MODULE).

get_page_from_path(PI) ->
    case re:run(PI, "^([^/]+)/(.+)") of
        {match,[_,{B,L},{B1,L1}]} -> {string:substr(PI, B+1, L), string:substr(PI, B1+1, L1)};
        _ -> default
    end.

body() -> [
  #panel{class="page-content", body=webutils:quick_nav()},
  "<section id=\"main\">",
    content(1),
    %pagenator_buttons(),
    #panel{class="side-col", body=[
      top_adv(),
      get_popular_group_container(),
      active_members()
    ]},
  "</section>",

  "<div id=\"add_group_dialog\" title=\"Create a new group\">",
    "<form id=\"add_new_group_form\">",
      "<fieldset>",
        "<div>",
          "<div>Group name</div>",
          "<div><input name=\"group_name\" id=\"group_name\" class=\"required\"></div>",
        "</div>",
        "<div>",
          "<div>Description</div>",
          "<div>",
            "<textarea id=\"group_description\" class=\"required\"></textarea>",
          "</div>",
        "</div>",
        "<div>",
          "<div>Private</div>",
          "<div>",
            "<select id=\"group_type\">",
              "<option value=\"false\">Public</option>",
              "<option value=\"true\">Private</option>",
            "</select>",
          "</div>",
        "</div>",
        "<div id=\"gc_error_label\"></div>",
      "</fieldset>",
    "</form>",
  "</div>"
  ].


tooltip_content(_GName) ->
    #template{file=code:priv_dir(nsp_srv)++"/templates/tooltip.html"}.

content(PageNumber) ->
    Anchor = wf_context:anchor(), ValidationGroup = wf_context:event_validation_group(),
    Postback_js = wf_event:generate_postback_script(search_group, Anchor, ValidationGroup, ?MODULE, undefined),
    wf:wire(wf:f("objs('search_textbox')"
             ".bind('keyup keydown change', function()"
             "{var $this=objs('search_textbox');var l = parseInt($this.attr('value').length);"
             " if(l > 0){objs('sendsearch').css('background','url(/images/grn-shr-btn.png) no-repeat');objs('sendsearch').css('cursor','pointer');}"
             " if(l <= 0){objs('sendsearch').css('background','url(/images/gre_shr_btn.png) no-repeat');objs('sendsearch').css('cursor','default');}"
             " })"
             ".bind('keypress', function(e)"
             "{var code = e.keyCode || e.which;"
             " if (code == 13) { if (!e.shiftKey) {~s; return false;}}" %% send postback
             " if (code != 116 && code != 46 && code > 40 || code == 32) return $(this).trigger('change').attr('value').length < ~b" %% deny only text keys
             "})",
             [Postback_js, 100])),
    wf:wire(wf:f("ReloadPostback='~s';",[wf_event:serialize_event_context({page, PageNumber}, undefined, undefined, groups)])),
    [
        #panel{class="main-col", body=[
%            #panel{class="top-space", body=[
%PHASE1                #panel{class="search-cell", body=[
%                    #form{body=[
%                        #panel{class="text", body=#textbox{id="search_textbox"}},
%                        #button{class="btn-submit-mkh", id="sendsearch", postback={search_group, 1}, text=?_T("Search")}
%                    ]}
%                ]},
                #h1{text=?_T("Groups"),class="section-title",style=
			"width:200px;height:43px;padding-left:20px;margin-top:-24px;margin-bottom:25px;text-transform:uppercase;"},
%PHASE1                #panel{id="create_new_group", class="group-create-new-container", body=#link{
%                    url="javascript:void(0)",
%                    actions=#event{type=click, actions=[
%                        #script{script="clear_form_values();$(\"#add_group_dialog\").dialog(\"open\");"}
%                    ]},
%                    text=?_T("Create a group")}}
 %           ]},
            #panel{id="groups_content", body=[
                inner_content(PageNumber)
            ]}
        ]}
    ].

inner_content(PageNumber) ->
    {GroupsView, AllCount} = get_group_rows(PageNumber),
    case is_list(GroupsView) of
        true ->
            #span{text=GroupsView};
        false ->
            {_, _, _, _, _, _, _, _, _, _, GroupsViewList} = GroupsView,

            NextButton = if
                length(GroupsViewList) < ?GROUPPERPAGE
                     -> #listitem{body=#link{text=">", url="javascript:void(0)", class="inactive"}};
                true -> #listitem{body=#link{text=">", postback={page, PageNumber + 1}}}
            end,
            PrevButton = case PageNumber of
                I when is_integer(I),I>1 -> #listitem{body=#link{text="<", postback={page, PageNumber - 1}}};
                _                        -> #listitem{body=#link{text="<", url="javascript:void(0)", class="inactive"}}
            end,
            [
                GroupsView,
                case AllCount > ?GROUPPERPAGE of
                    false -> [];
                    true ->
                        #panel{class="paging-2", body=[
                        #panel{class="center", body=[
                            #list{body=[
                                    PrevButton,
                                    [
                                        case N of 
                                            PageNumber ->
                                                #listitem{body=#link{class="inactive", url="javascript:void(0)", text=io_lib:format("~b",[N])}};
                                            _ ->
                                                #listitem{body=#link{postback={page, N}, text=io_lib:format("~b",[N])}}
                                        end
                                        || N <- lists:seq(1, AllCount div ?GROUPPERPAGE + 1)
                                    ],
                                    NextButton
                                ]}
                            ]}
                        ]}
                end
            ]
    end.

%searched_content(PageNumber, Content) ->
%    GroupsView = [group_row(X) || X <- split_subs(Content, [])],
%    NextButton = if
%        length(GroupsView) < ?GROUPPERPAGE/2
%             -> #listitem{body=#link{text=">", url="javascript:void(0)", class="inactive"}};
%        true -> #listitem{body=#link{text=">", postback={search_group, PageNumber + 1}}}
%   end,
%    PrevButton = case PageNumber of
%        I when is_integer(I),I>1 -> #listitem{body=#link{text="<", postback={search_group, PageNumber - 1}}};
%        _                        -> #listitem{body=#link{text="<", url="javascript:void(0)", class="inactive"}}
%    end,
%    [
%        #panel{body=[GroupsView]},
%        #panel{class="paging-2", body=[
%        #panel{class="center", body=[
%            #list{body=[
%                    PrevButton,
%                    #listitem{body=#link{class="inactive", url="javascript:void(0)", text=io_lib:format("~b",[PageNumber])}},
%                    NextButton
%                ]}
%            ]}
%        ]}
%    ].

get_group_rows() -> get_group_rows(1).
get_group_rows(Page) ->
    Offset = (Page - 1) * ?GROUPPERPAGE + 1,
    case wf:q("of") of
        undefined ->
            All = lists:sort(fun(#group{created=T1}, #group{created=T2}) -> T2 =< T1 end, user_counter:groups()),
                                                                                          %nsm_groups:get_all_groups()),
            {group_row(lists:sublist(All, Offset, ?GROUPPERPAGE)), length(All)};
        UId ->
            case nsm_groups:list_groups_per_user(UId) of
                [] ->
                    {?_T("You are not subscribed to anyone"), 0};
                Full ->
                    GroupsAndNoGroupsFull = [begin {_, Group} = nsm_groups:get_group(GId), Group end || GId <- Full],
                    GroupsFull = [G || G <- GroupsAndNoGroupsFull, G /= notfound],
                    SortedFull = lists:sort(fun(#group{created=T1}, #group{created=T2}) -> T2 =< T1 end, GroupsFull),
                    Sub = lists:sublist(SortedFull, Offset, ?GROUPPERPAGE),
                    {group_row(Sub), length(Full)}
            end
    end.

split_subs([], A) -> A;
split_subs(L, A)  when length(L) =< 2 -> A ++ [L];
split_subs(L, A)  ->
    {L2, L3} = lists:split(2, L),
    split_subs(L3, A ++ [L2]).

group_row(GL) when is_list(GL)->
    #list{class="group-list-mkh", style="font-size:10pt;", body=[ show_group_ul(X) || X <- GL]};
group_row(GL) ->
    #list{class="group-list-mkh", style="font-size:10pt;", body=show_group_ul(GL)}.

show_group_ul(#group{username=GName, description=GDesc, name=GFineName, users_count=GroupMembersCount}) ->
    GFullName = case GFineName of
      [] -> GName;
      Name -> Name
    end,
    show_group_ul_view(GName, GFullName, GDesc, GroupMembersCount);
show_group_ul(GId) ->
    {ok, Group} = nsm_groups:get_group(GId),
    show_group_ul(Group).

show_group_ul_view(GName, GFullName, GDesc, GroupMembersCount) ->
    RealGroupMembersCount = case GroupMembersCount of   % patch for freshly created groups
        {error, notfound} -> 1;
        _ -> GroupMembersCount
    end,

    #listitem{class="group-item", body=[
        #panel{class="img", body=#link{url=site_utils:group_link(GName),
            body=#image{image=webutils:get_group_avatar(GName, "big"), style="width:96px; height:96px"}}},
        #panel{class="descr", style="overflow:hidden;", body=[
            #link{url=site_utils:group_link(GName), body=#h3{text=GFullName}},
            io_lib:format("<p>~s</p>", [GDesc]),
            io_lib:format("<p><i>~p ~s</i></p>", [RealGroupMembersCount, ?_T("members")])
        ]}
    ]}.


view_user_groups(UId) ->
    case nsm_groups:list_groups_per_user(UId) of
        [] ->
            ?_T("You are currently not in any group");
        Groups ->
            Source =
               [begin
                    {ok, Group} = nsm_groups:get_group(GId),
                    GName = Group#group.name,
                    [#link{text=GName, url=lists:concat([?_U("/view/group"), "/id/", GName])}, #br{}]
                end
                || GId <- Groups ],
            lists:flatten(Source)
    end.

get_popular_group_container() ->
    #panel{ class="box", body=[
        #h3{text=?_T("Popular groups")},
        #list{class="list-photo list-photo-in", body=[
            [begin              
                case nsm_groups:get_group(GId) of
                    {ok, Group} ->
                        GroupName = Group#group.name,
                        GroupUsers = Group#group.users_count,
                        #listitem{body=
                            [#link{url=site_utils:group_link(GId), body=io_lib:format("~s", [GroupName])},
                             #span{style="padding-left:4px;", text = io_lib:format("(~b)", [GroupUsers])}
                            ]};
                     _ -> ""
                 end
             end
             || GId <-nsm_groups:get_popular_groups()]
        ]}
    ]}.


active_members() ->
    ActiveUsers = user_counter:active_users_top(),
    #panel{class="cell-space", body=[
        #h3{text=?_T("Active members")},
        #list{class="soc-users", body=[
            #listitem{body=#link{url=site_utils:user_link(Uid),
                body=#image{image=webutils:get_user_avatar(Uid, "small"), style="width:52px;height:52px", class=
                    case Paid of
                        true -> "paid_user_avatar";
                        _ -> ""
                    end
                }, style="width:52px;height:52px"}}
            || {Uid, Paid} <- ActiveUsers
        ]}
    ]}.


% new group block
show_new_group_content() ->
    Title = #h1{class = "head", text = ?_T("Create your own group")},
    Settings = new_group_settings(),
    Body = [Title,
            #panel{class=holder, body=
             [Settings,
              #br{},
              #cool_button{postback=create_new_group, text=?_T("Create")},
              #cool_button{postback=hide_new_group_edit, text=?_T("Cancel")},
              #grid_clear{}
           ]}],
    webutils:lightbox_panel_template(simple_lightbox, Body).

new_group_settings() ->
    [#panel { class="group-settings", body = [
        #grid_4 { body=[
            #label{text = ?_T("Short name")},
            #panel{class = "text",
                body = [#textbox{id = new_group_username, placeholder=?_T("gunsandbutter"), style="overflow:auto;"}]},
            #label{text = ?_T("Full name")},
            #panel{class = "text",
                body = [#textbox{id = new_group_name, placeholder=?_T("Guns and Butter"), style="overflow:auto;"}]}
        ]},
        #grid_4 { body=[
            #label{text = ?_T("What it will be all about")},
            #panel{class = "textarea", style = "height:70px;",
                body = [#textarea{id = new_group_desc, placeholder=?_T("Guns, butter, everything in between"), 
                                  style="resize:vertical; height:70px; "}]},
            #panel{class = "error", body=[
                #label{text = "", class="error", id="update_error"}
            ]},
            #checkbox { id = new_group_is_public, text=?_T("I want to let everyone post to my group"), checked=true, style="float:left;" }
        ]},
        #grid_clear{}
    ]}].

event(show_new_group_edit) ->
    ?INFO("Got to new group form creation"),
    wf:update(simple_panel, show_new_group_content()),
    wf:wire(simple_lightbox, #show{});

event(hide_new_group_edit) ->
    wf:wire(simple_lightbox, #hide{});

event(create_new_group) ->
    PGId = wf:q(new_group_username),
    GName = wf:q(new_group_name),
    GDesc = wf:q(new_group_desc),
    GPublic = wf:q(new_group_is_public),
    GPublicity = case GPublic of
        "on" -> public;
        _ -> private
    end,    
    GId = site_utils:validate_group_id(PGId),
    ?INFO("New group: ~p ~p ~p ~p",[GId, GName, GDesc, GPublicity]),
    AllGroups = [G#group.username || G <- user_counter:groups()],%nsm_groups:get_all_groups()],
    case GId of 
        "" ->
            wf:wire(#alert{text=?_T("Group should have some kind of shortname")});
        _ ->
            case lists:member(GId, AllGroups) of
                true ->
                    wf:wire(simple_lightbox, #hide{}),
                    wf:wire(#alert{text=?_T("Group with this shortname already exists! Maybe, you should check it out.")});
                false ->
                    case nsm_users:get_user({username, GId}) of
                        {ok, _} ->
                            wf:wire(#alert{text=?_TS("User '$username$' exist!", [{username, GId}]) });
                        {error, _} ->
%                            nsm_groups:create_group_directly_to_db(wf:user(), GId, GName, GDesc, GPublicity),
                            nsx_msg:notify(["system", "create_group"], {wf:user(), GId, GName, GDesc, GPublicity}),
                            wf:wire(#alert{text=?_T("New group created!")}),
                            user_counter:regroups(),
                            wf:redirect("")
                    end
            end
    end;
% create new group block ends

event({change_language,SL}) ->  %PUBLIC BETA this is here just to fix not working language selector bug ASAP. It is hardly a best solution
    webutils:event({change_language, SL});


event(Event) ->
    case wf:user() of
	undefined -> wf:redirect_to_login(?_U("/login"));
        User      -> inner_event(Event, User)
    end.

inner_event({page, N}, _) ->
    ActialNumber = if
        N < 1 -> 1;
        true  -> N
    end,
    wf:update(groups_content, [ inner_content(ActialNumber) ]);

%inner_event({search_group, Page}, _) ->    % we should implement some kind of search, but it's not there yet
%    SearchStr = wf:q("search_textbox"),
%    Searched = case nsm_groups:find_group(SearchStr, Page, ?GROUPPERPAGE) of
%        [] -> #panel{body=?_T("We could not find any groups matching the search") ++ " \"" ++ SearchStr ++ "\""};
%        L  -> searched_content(Page, L)
%    end,
%    wf:wire(wf:f("ReloadPostback='~s';",[wf_event:serialize_event_context({search_group, Page}, undefined, undefined, groups)])),
%    wf:update(groups_content, Searched);

inner_event({subscribe, User1, GName, SUId}, _User) ->
    nsx_msg:notify(["subscription", "user", User1, "add_to_group"], {GName, User1, member}),
    wf:update(SUId, #link{url="javascript:void(0)", text=?_T("Unsubscribe"), postback={unsubscribe, User1, GName, SUId}});

inner_event({unsubscribe, User1, GName, SUId}, _User) ->
    nsx_msg:notify(["subscription", "user", User1, "remove_from_group"], {GName}),
    wf:update(SUId, #link{url="javascript:void(0)", text=?_T("Subscribe"), postback={subscribe, User1, GName, SUId}});


inner_event({delete, GName}, User) ->
    ?PRINT({"DELETE OWN GROUP", GName, User}),
    Group = nsm_groups:get_group(GName),
    case Group#group.creator =:= User of
        false -> ok
        ;_    ->
            nsx_msg:notify([db, group, GName, remove_group], []),
            wf:wire("reload_current_content();")
    end;

inner_event(Any, _)->
    webutils:event(Any).

top_adv() ->
    #panel{class="mark-cell", body=[
        io_lib:format("<h3><span class=\"mdl\">~s</span><span class=\"large\">~s</span></h3>",
            [?_T("Sende Bir"), ?_T("Grup Kur!")]),
        io_lib:format("<p><strong>~s</strong></p>",
            [?_T("This would of been great, wouldn't it! Go on, try it!")]),
        #link{
            url="javascript:void(0)", class="btn", postback=show_new_group_edit,
            text=?_T("Create a group")}
    ]}.

api_event(Name, Tag, Args) ->
  webutils:api_event(Name, Tag, Args).
