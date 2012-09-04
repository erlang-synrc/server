%% -*- mode: nitrogen -*-
-module (create_game).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("nsm_srv/include/user.hrl").
-include_lib("nsm_srv/include/feed.hrl").
-include_lib("nsm_srv/include/table.hrl").
-include_lib("elements/records.hrl").

-include("gettext.hrl").
-include("loger.hrl").
-include("setup.hrl").

%% THIS PAGE ARE TO BE DELETED

-define(CREATE_GAME_SETTINGS_KEY, {game_settings_create, wf:state(session_id)}).

main() ->
    case (catch check_requirements()) of
	ok ->
	    wf:state(state_element, []),

	    try
		%% checking that session have proper settings value
		Settings = wf:session(?CREATE_GAME_SETTINGS_KEY),
		_GameType = game_okey = proplists:get_value(game, Settings)
	    catch
		error:E when E==function_clause orelse element(1, E)==badmatch ->
		    %% reset to default if it's not
		    wf:session(?CREATE_GAME_SETTINGS_KEY, [{game, game_okey}, {rounds, 20}])
	    end,

	    UId = webutils:user_info(username),

	    wf:state(user_in_groups, rpc:call(?APPSERVER_NODE,groups,list_group_per_user,[UId])),

	    wf:state(users_subscribe, rpc:call(?APPSERVER_NODE,users,list_subscription,[UId])),

	    main_authorized();
	{redirect, login} ->
	    wf:redirect_to_login(?_U("/login"));
	{redirect, Url} ->
	    wf:redirect(Url);
	_ -> ""
    end.

check_requirements() ->
    case wf:user() /= undefined of
        true  -> ok;
        false -> throw({redirect, login})
    end,
    case wf:q('__submodule__') of
	"okey" -> ok;
	Other ->
	    ?ERROR("Hacking attempt? game_type='~w'\n", [Other]),
	    throw({redirect, ?_U("/dashboard")})
    end,
    case wf:q(csid) of
	undefined ->
	    C = wf:temp_id(),
	    throw({redirect, lists:concat([?_U("/create-game"), "/", wf:q('__submodule__'), "/csid/", C])});
	Sid ->
	    wf:state(session_id, Sid)
    end,
    ok.

main_authorized() -> #template { file=code:priv_dir(nsw_srv)++"/templates/bare.html" }.

title() -> ?_T("Game Page").

body() ->
    #container_12 {
      body=[#grid_4{alpha=true, omega=true, class="border-form", body=criteria_box()},
            #grid_8{omega=true, class="border-form", body=setting_box()},
            #grid_12{alpha=true, omega=true, class="", body=saved_tables()}
           ]}.


saved_tables() ->
    UId = wf:user(),
    Tables = rpc:call(?APPSERVER_NODE,table_manager,get_save_tables,[UId]),
    #table{class=view_table_table,
           rows=[#tablerow{class=view_table_header,
                           cells=[#tableheader{text=?_T("Table name")},
                                  #tableheader{text=?_T("Create data")},
                                  #tableheader{text=?_T("Options")},
                                  #tableheader{text=?_T("Actions")}
                                 ]},
                 lists_game(Tables)]}.

lists_game(Tables) ->
    [ begin
          Url = lists:concat([?_U("/view-table"), "/", wf:q('__submodule__'), "/saveid/", Id]),
          LoadGame = webutils:new_window_js(Url),
          LocalTime = calendar:now_to_local_time(CTime),
          Time = site_utils:local_time_to_text(LocalTime),
	  Options = site_utils:textify_settings(Settings),
          RowId = wf:temp_id(),
          #tablerow{id=RowId, cells=[#tablecell{id=tableName, body=Name},
                                   #tablecell{id=tableCreateTime, body=Time},
                                   #tablecell{id=table_options, body=Options},
                                   #tablecell{id=tableActions,
                                              body=[#button{class="nice_button",
                                                           text=?_T("Start"),
                                                           postback=load_game,
                                                           actions=#event
                                                           {type=click,
                                                            actions=#script{script=LoadGame}}},

                                                    #button{class="nice_button",
                                                           text=?_T("Remove"),
                                                           postback={delete_save_game, Id},
                                                           actions=#event
                                                           {type=click,
                                                            actions=#hide{target=RowId}}}]

                                              }]}
      end || #save_game_table{name = Name,
                              id = Id,
			      settings = Settings,
                              create_time = CTime} <- Tables ].




criteria_box() ->
    UId = wf:user(),
    {Date,_} = calendar:now_to_local_time(now()),
    Time = site_utils:date_to_text(Date),

    Url = lists:concat([?_U("/view-table"), "/", wf:q('__submodule__'), "/id/", wf:state(session_id)]),
    CreateGame = webutils:new_window_js(Url),
    TableName = ?_TS("$username$ table, $date$", [{username, UId}, {date, Time}]),
    Textbox = #textbox{id=table_name,
                       class="nice_textbox",
                       style="width: 100%;",
                       postback=check_required,
                       placeholder=?_T("Table name"),
		       text=TableName
		      },
    update_setting({set, {table_name, TableName}}), % to apply default value of table_name
    wf:wire(Textbox#textbox.anchor, #event { type=keyup, postback=check_required }),

    Body =
        [#frame{title=?TXT("Criteria"),
                close_icon="/images/box_close.png",
                close_action=undefined,
                body=#droppable{id=criteria_box,
                                accept_groups=[speed,
                                               game_mode,
                                               additional,
                                               group,
                                               friend,
                                               personal],
                                tag=criteria,
                                class=".create_game_criteria_box",
                                body=Textbox}
               },

         #frame{close_icon="/images/box_close.png",
                close_action=undefined,
                body=#panel{class="create_game_top",
                            body=[#flash{id=point_info},
                                  #button{text=?_T("Create"),
                                          class="nice_button disable",
                                          id=create_game,
                                          style="cursor: pointer;",
                                          postback=create_game,
                                          actions=#event{type=click,
                                                         actions=#script{script=CreateGame}}},
                                  #button{text=?_T("<< Back"),
                                          class="nice_button",
                                          style="cursor: pointer;",
                                          postback=exit}
                                 ]}
               }],
    wf:wire(#attr{target=create_game,
                  attr=disabled,
                  value=true}),
    Body.



setting_box() ->
    #panel{body=[#accordion{
                    tab=[#accordion_tab{title=?_T("Game settings"),
                                        body=tab_game_setting()},
                         #accordion_tab{title=?_T("Group settings"),
                                        body=tab_group_setting()},
                         #accordion_tab{title=?_T("Friend settings"),
                                        body=tab_friend_setting()},
                         #accordion_tab{title=?_T("Personal settings"),
                                        body=tab_personal_setting()}]}]
          }.


tab_game_setting() ->

    wf:wire(#attr{target=coupled_cb, attr="disabled", value="true"}),
    wf:wire(#attr{target=gosterge_finish_cb, attr="disabled", value="true"}),
    wf:wire(#attr{target=private_cb, attr="disabled", value="true"}),
    wf:wire(#attr{target=slang_cb, attr="disabled", value="true"}),
    wf:wire(#attr{target=observers_cb, attr="disabled", value="true"}),
    wf:wire(normal, #tooltip_wizard{content=?_T("Drag me to the left to select game speed!"), title=?_T("Game speed")}),
    wf:wire(standard, #tooltip_wizard{content=?_T("Drag me to the left to select game mode!"), title=?_T("Game mode")}),
    wf:wire(sets_text, #tooltip_wizard{content=?_T("Drag me to the left to select number of set!"), title=?_T("Sets")}),
    wf:wire(create_game, #tooltip_wizard{content=?_T("Now you can create your game!"), my="bottom center"}),
    wf:wire(#event{postback=settings_to_criteria}),
    tab_game_setting_elements().


tab_game_setting_elements() ->
    Format = slider_text_format(sets),
    [#panel{class="create_game_frame",
            body=[#span{text=?_T("Speed"), class="table_manager_group_name"},
                  #droppable{body=[#draggable_new {group=speed,
                                                   tag={speed, fast},
                                                   class="create_game_criteria_item",
                                                   revert=invalid,
                                                   id=fast,
                                                   group_disable=[fast, normal, slow],
                                                   body=?_T("Fast")},

                                   #draggable_new {group=speed,
                                                   tag={speed, normal},
                                                   class="create_game_criteria_item",
                                                   revert=invalid,
                                                   id=normal,
                                                   group_disable=[fast, normal, slow],
                                                   body=?_T("Normal")},

                                   #draggable_new {group=speed,
                                                   tag={speed, slow},
                                                   revert=invalid,
                                                   class="create_game_criteria_item",
                                                   id=slow,
                                                   group_disable=[fast, normal, slow],
                                                   body=?_T("Slow")}]}]},

     #panel{class="create_game_frame",
            body=[#span{text=?_T("Game type"), class="table_manager_group_name"},
                  #droppable{body=[#draggable_new {group=game_mode,
                                                   tag={game_mode, standard},
                                                   revert=invalid,
                                                   class="create_game_criteria_item",
                                                   id=standard,
                                                   group_disable=[color,
                                                                  evenodd,
                                                                  standard,
                                                                  countdown],
                                                   body=?_T("Standard")},

                                   #draggable_new {group=game_mode,
                                                   tag={game_mode, color},
                                                   class="create_game_criteria_item",
                                                   revert=invalid,
                                                   id=color,
                                                   group_disable=[color,
                                                                  evenodd,
                                                                  standard,
                                                                  countdown],
                                                   body=?_T("Color")},

                                   #draggable_new {group=game_mode,
                                                   tag={game_mode, evenodd},
                                                   class="create_game_criteria_item",
                                                   revert=invalid,
                                                   id=evenodd,
                                                   group_disable=[color,
                                                                  evenodd,
                                                                  standard,
                                                                  countdown],
                                                   body=?_T("Even/Odd")},

                                   #draggable_new {group=game_mode,
                                                   tag={game_mode, countdown},
                                                   class="create_game_criteria_item",
                                                   revert=invalid,
                                                   id=countdown,
                                                   group_disable=[color,
                                                                  evenodd,
                                                                  standard,
                                                                  countdown,
								  sets_text
								 ],
						   criteria_for=[{gosterge_finish_cb,gosterge_finish}], %% element, tag of #checkbox{id=gosterge_finish_cb}
                                                   body=?_T("Countdown from 10")},
                                   #br{},

                                   #checkbox{class="create_game_criteria_item",
                                             id=coupled_cb,
                                             postback={cb_change, ?_T("Coupled"), coupled_cb, coupled},
                                             text=?_T("Coupled game")},

                                   #checkbox{class="create_game_criteria_item",
                                             id=gosterge_finish_cb,
                                             postback={cb_change, ?_T("Gosterge finish"), gosterge_finish_cb, gosterge_finish},
                                             text=?_T("Gosterge finish")}


                                  ]},
                  #table{rows=#tablerow{cells=[#tablecell{style="width: 70px",
                                                          body=#draggable_new{group=additional,
                                                                              tag={sets, undefined},
                                                                              revert=invalid,
                                                                              class="create_game_criteria_item",
                                                                              id=sets_text,
                                                                              group_disable=[sets_text, {slider, sets_slider}],
                                                                              body=wf:f(Format,["1"])}},
                                               #tablecell{style="width: 150px",
                                                          body=#slider{id=sets_slider,
                                                                       style="margin-left: 15px; width: 100px;",
                                                                       target=sets_text,
                                                                       value=1,
                                                                       min=1,
                                                                       max=4,
                                                                       text=wf:f("'"++Format++"'", %js code
										 ["' + ~s + '"])}}]}}
                 ]},

     #panel{class="create_game_frame",
            body=[#span{text=?_T("Additional options"), class="table_manager_group_name"},
                  #checkbox{class="create_game_criteria_item",
                            id=robots_cb,
                            postback={cb_change, ?_T("Disallow robots"), robots_cb, deny_robots},
                            text=?_T("Table can contain only players, not robots.")},
                  #br{},
                  #checkbox{class="create_game_criteria_item",
                            id=private_cb,
                            postback={cb_change, ?_T("Private table"), private_cb, private},
                            text=?_T("Private table, only for invited friends.")},
                  #br{},
                  #checkbox{class="create_game_criteria_item",
                            id=slang_cb,
                            postback={cb_change, ?_T("Slang is accepted"), slang_cb, slang},
                            text=?_T("Slang is accepted")},
                  #br{},
                  #checkbox{class="create_game_criteria_item",
                            id=observers_cb,
                            postback={cb_change, ?_T("Observers are accepted"), observers_cb, observers},
                            text=?_T("Observers are accepted")}
                 ]}
    ].

%% this function used to get info about settings needed in event(settings_to_criteria)
criteria_elements() ->
    tab_game_setting_elements()++tab_personal_setting()++tab_group_setting()++tab_friend_setting().

drop_event({{age, undefined}, Id, _, DisGroup, CriteriaFor}, DropTag) when is_list(DisGroup)  ->
    MinAge = wf:to_integer(wf:q(age_slider_values_min)),
    MaxAge = wf:to_integer(wf:q(age_slider_values_max)),
    Body = wf:f(slider_text_format(age), [wf:to_list(MinAge), wf:to_list(MaxAge)]),
    drop_event({ {age, [MinAge, MaxAge]}, Id, Body, DisGroup, CriteriaFor}, DropTag);

%% sliders
drop_event({{sets, undefined}, Id, _, DisGroup, CriteriaFor}, DropTag) when is_list(DisGroup) ->
    Value = wf:to_integer(wf:q(sets_slider_value)),
    Body = wf:f(slider_text_format(sets), [wf:to_list(Value)]),
    drop_event({ {sets, Value}, Id, Body, DisGroup, CriteriaFor}, DropTag);


drop_event({Tag, Id, Body, DisGroup, CriteriaFor}, DropTag) when is_list(DisGroup)  ->
    %% ?PRINT([{Tag, Id, Body, DisGroup, CriteriaFor}, DropTag]),
    case DropTag of
        criteria ->
	    add_element_to_criteria_box(Tag, Id, Body, DisGroup, CriteriaFor),
	    update_setting({add, Tag});
        _ ->
            ok
    end.


add_element_to_criteria_box(Tag, Id, Body, DisGroup, CriteriaFor) ->
    case Tag of
	{game_mode,countdown} ->
	    [ begin
		  wf:wire(#attr{target=ElemId, attr="disabled", value=""})
	      end || {ElemId,_} <- CriteriaFor],
	    remove_checkbox_from_area(sets_text, sets);
	_ -> ok
    end,
    TempId = wf:temp_id(), {FromId, _} = Tag,
    Element = #panel{id=TempId,
		     body=[#label{text=Body,
				  class="create_game_criteria_item_label"},
			   #link{postback={remove_criteria_item, TempId, Id, DisGroup, CriteriaFor, Tag},
				 class="ui-icon ui-icon-circle-close create_game_criteria_item_close"}],
		     class=wf:f("create_game_criteria_item from_~s", [FromId])},

    wf:insert_bottom("criteria_box", Element),
    wf:wire(Id, #add_class { class=create_game_criteria_item_select }),
    [ begin
	  state_element({lock, Dis, Id})
      end || Dis <- DisGroup ],
    ok.

add_checkbox_to_criteria_box(Body, Id, Tag) ->
    TempId = wf:f("~s_tag", [Tag]),
    Element = #panel{id=TempId,
		     body=[#label{text=Body,
				  class="create_game_criteria_item_label"},
			   #link{text="",
				 postback={uncheck_cb, TempId, Id, Tag},
				 class="ui-icon ui-icon-circle-close create_game_criteria_item_close"}],
		     class=wf:f("create_game_criteria_item from_~s", [Tag])},
    wf:insert_bottom(criteria_box, Element),
    Script = wf:f("objs(\"~s\").attr('checked', true).addClass(\"~s\");", [Id, "create_game_criteria_item_select"]),
    wf:wire(Script),
    ok.

tab_group_setting() ->
    Groups = wf:state(user_in_groups),
    View = view_groups(Groups),
    [#panel{class="create_game_groups_box",
            body=[#textbox{id=groups_filter_name,
                          class="create_game_groups_filter_textbox"},
                  #panel{class="create_game_groups_list",
                         id=groups_list,
                         body=View}]
            }].

view_groups(Groups) ->
    [ begin
	  {ok, H} = wf:hex_encode(lists:concat(["group", Name])),
          TempId = wf:to_list(H),
          #draggable_new {group=group,
                          tag={group, Name},
                          revert=invalid,
                          class="create_game_group_item",
                          id=TempId,
                          group_disable=[TempId],
                          body=Name}
      end || #group_member{group = Name} <- Groups ].

tab_friend_setting() ->
    Groups = wf:state(users_subscribe),
    View = view_friends(Groups),
    [#panel{class="create_game_groups_box",
            body=[#textbox{id=friends_filter_name,
                           class="create_game_groups_filter_textbox"},
                  #panel{class="create_game_groups_list",
                         id=friends_list,
                         body=View}]
           }].

view_friends(Groups) ->
    [ begin
	  {ok, H} = wf:hex_encode(lists:concat(["user", Name])),
          TempId = wf:to_list(H),
          #draggable_new {group=friend,
                          tag={user, Name},
                          revert=invalid,
                          class="create_game_group_item",
                          id=TempId,
                          group_disable=[TempId],
                          body=Name}
      end || #subscription{whom = Name} <- Groups ].

tab_personal_setting() ->
    AgeFormat=slider_text_format(age),
    [#panel{class="create_game_frame", style="text-align: center",
            body=[#span{text=?_T("Optional settings"), class="table_manager_group_name"},
                  #table{style="width: 100%",
                         rows=[#tablerow{cells=[#tablecell{style="width: 120px",
                                                           body=#draggable_new{group=personal,
                                                                               tag={age, undefined},
                                                                               revert=invalid,
                                                                               class="create_game_criteria_item",
                                                                               id=age_text,
                                                                               group_disable=[age_text, {slider, age_slider}],
                                                                               body=wf:f(AgeFormat, ["18","50"])}},
                                                #tablecell{body=#slider{range = true, id=age_slider,
                                                                        target=age_text,
                                                                        values=[{min,18}, {max, 50}],
                                                                        text=wf:f("'"++AgeFormat++"'", %js code
										 ["' + ~s + '","' + ~s + '"])}}]},
                               #tablerow{cells=[#tablecell{body=?_T("Gender")},
                                                #tablecell{body=
                                                               [#draggable_new{group=personal,
                                                                               tag={sex, male},
                                                                               revert=invalid,
                                                                               class="create_game_criteria_item",
                                                                               id=sex_male,
                                                                               group_disable=[sex_female, sex_male],
                                                                               body=?_T("Male")},
                                                                #draggable_new{group=personal,
                                                                               tag={sex, female},
                                                                               revert=invalid,
                                                                               class="create_game_criteria_item",
                                                                               id=sex_female,
                                                                               group_disable=[sex_female, sex_male],
                                                                               body=?_T("Female")}]}
                                               ]}
                               %% ,
                               %% #tablerow{cells=[#tablecell{body=?_T("")},
                               %%                  #tablecell{body=#draggable_new{group=personal,
                               %%                                                 disabled=true,
                               %%                                                 tag={location, not_implemented},
                               %%                                                 revert=invalid,
                               %%                                                 class="create_game_criteria_item",
                               %%                                                 id=location,
                               %%                                                 group_disable=[location],
                               %%                                                 body=?_T("City")}]
                              ]}]}].

check_required(Setting) ->
    BaseRequired = [table_name, game, game_mode, speed, sets, rounds],
    Required = case proplists:get_value(game_mode, Setting) of
		   countdown ->
		       BaseRequired -- [sets];
		   _ -> BaseRequired
	       end,
    Check = [ lists:keymember(Req, 1, Setting) || Req <- Required ],
    Rounds = wf:to_integer(proplists:get_value(rounds, Setting, 0)),
    Sets = wf:to_integer(proplists:get_value(sets, Setting, 0)),
    case lists:usort(Check) of
        [true] ->
            case site_utils:table_per_user_point(wf:user(), Sets, Rounds) of
                true ->
                    wf:remove(point_info),
                    wf:wire(create_game, #remove_class { class=disable }),
                    wf:wire(#attr{target=create_game,
                        attr=disabled,
                        value=""}),
                    true;
                false ->
                    wf:flash(point_info, ?_T("You don't have enough points to play!")),
                    wf:wire(create_game, #add_class { class=disable }),
                    wf:wire(#attr{target=create_game,
                        attr=disabled,
                        value=true}),
                    false
            end;
        _ ->
            wf:remove(point_info),
            wf:wire(create_game, #add_class { class=disable }),
            wf:wire(#attr{target=create_game,
                attr=disabled,
                value=true}),
                    false
    end.

event({uncheck_cb, El, Id, Tag}) ->
    wf:remove(El),
    wf:wire(#attr{target=Id,
                  attr="checked",
                  value=""}),
    update_setting({remove, {Tag, true}});

event({cb_change, Body, Id, Tag}) ->
    Val = wf:q(Id),
    On = Val == "on" andalso Val /= undefined,
    TempId = wf:f("~s_tag", [Tag]),
    case On of
        true ->
	    add_checkbox_to_criteria_box(Body, Id, Tag),
            update_setting({set, {Tag, true}});
        false ->
            event({uncheck_cb, TempId, Id, Tag})
    end;

event(create_game) ->
    UId = wf:user(),
    Settings = wf:session(?CREATE_GAME_SETTINGS_KEY),
    rpc:call(?APPSERVER_NODE,table_manager,save_table,[UId, Settings]),

    wf:redirect(?_U("/dashboard"));

event(load_game) ->
    wf:redirect(?_U("/dashboard"));

event({delete_save_game, Id}) ->
    rpc:call(?APPSERVER_NODE,table_manager,delete_save_table,[Id]);

event(exit) ->
    wf:redirect(lists:concat([?_U("/join-game"), "/",  wf:q('__submodule__')]));
event(settings_to_criteria) ->
    Settings = wf:session(?CREATE_GAME_SETTINGS_KEY),
    Elements = criteria_elements(),
    [ begin
	  Tag = case Tag0 of
		    {sets, _} -> {sets, undefined};
		    {age,  _} -> {age, undefined};
		    _ -> Tag0
		end,
	  case site_utils:traverse_criteria_elements(Elements, Tag) of
	      %% draggable option
	      {ok, #draggable_new{tag = Tag, id=Id, body=Body, group_disable=DisGroup, criteria_for=CriteriaFor}} ->
		  case Tag == Tag0 of
		      false -> %% slider
			  SliderId = proplists:get_value(slider, DisGroup),
			  {SliderKey, Value} = Tag0,
			  Args = case Value of
				     [Min, Max] ->
					 wf:wire(wf:f("$(\".wfid_~s\").slider(\"option\", \"values\", [~b, ~b]);", [SliderId, wf:to_integer(Min), wf:to_integer(Max)])),
					 [Min, Max];
				     I ->
					 wf:wire(wf:f("$(\".wfid_~s\").slider(\"value\", ~b);", [SliderId, wf:to_integer(I)])),
					 [wf:to_list(I)]
				 end,
			  NewBody = wf:f(slider_text_format(SliderKey), Args),
			  add_element_to_criteria_box(Tag, Id, NewBody, DisGroup, CriteriaFor);
		      true ->
			  add_element_to_criteria_box(Tag, Id, Body, DisGroup, CriteriaFor)
		  end;
	      %% checkbox
	      {ok, #checkbox{postback={cb_change, Body, Id, CBTag}}} ->
		  add_checkbox_to_criteria_box(Body, Id, CBTag);
	      %% options set by other ways (like game_type set by wf:q('__submodule__'))
	      _ ->
		  ignore
	  end
      end || Tag0 <-
		 Settings
    ],
    wf:wire(#tooltip_wizard_update{}),
    check_required(Settings),
    ok;

event(check_required) ->
    Setting0 = wf:session(?CREATE_GAME_SETTINGS_KEY),

    Text = wf:q(table_name),

    TableNameNew = {table_name, Text},
    TableNameSet = lists:keyfind(table_name, 1, Setting0),

    case TableNameSet of
        false ->
            case Text of
                "" -> empty;
                _ -> update_setting({set, TableNameNew})
            end;
        _ ->
            case Text of
                "" -> update_setting({remove, TableNameSet});
                _ ->
                    update_setting({remove, TableNameSet}),
                    update_setting({set, TableNameNew})
            end
    end,
    Setting = wf:session(?CREATE_GAME_SETTINGS_KEY),

    check_required(Setting);

event({remove_criteria_item, Id, DragId, DisGroup, CriteriaFor, Tag}) ->
    [ begin
	  wf:wire(#attr{target=ElemID, attr="disabled", value="disabled"}),
	  remove_checkbox_from_area(ElemID, CTag)
      end || {ElemID, CTag} <- CriteriaFor ],
    [ state_element({unlock, Dis, DragId}) || Dis <- DisGroup ],

    wf:wire(DragId, #remove_class { class=create_game_criteria_item_select }),
    wf:remove(Id),
    update_setting({remove, Tag});
event(Other) ->
    webutils:event(Other).



state_element({lock, Element, By}) ->
    case Element of
	{slider, E} ->
	    wf:wire(wf:f("$(\".wfid_~s\").slider(\"disable\");", [E]));
	E ->
	    wf:wire(wf:f("$(\".wfid_~s\").draggable(\"disable\");", [E]))
    end,
    State = wf:state(state_element),
    NewState = [{Element, By} | State],
    wf:state(state_element, NewState),
    {ok, lock};

state_element({unlock,sets_text,".wfid_countdown"}) ->
    wf:wire(wf:f("$('.wfid_~s').draggable('enable');", [sets_text])),
    {ok, unclock};

state_element({unlock, Element, By}) ->
    State = wf:state(state_element),
    NewState = lists:delete({Element, By}, State),
    wf:state(state_element, NewState),
    case lists:keytake(Element, 1, NewState) of
        false ->
	    case Element of
		{slider, E} ->
		    wf:wire(wf:f("$(\".wfid_~s\").slider(\"enable\");", [E]));
		E ->
		    wf:wire(wf:f("$(\".wfid_~s\").draggable(\"enable\");", [E]))
	    end,
            {ok, unlock};
        _ ->
            {error, can_not_unlock}
    end.

update_setting({remove, Setting}) ->
    Old = wf:session(?CREATE_GAME_SETTINGS_KEY),
    {Key, _Val} = Setting,
    New = proplists:delete(Key, Old),
    check_required(New),
    wf:wire(#tooltip_wizard_update{}),
    wf:session(?CREATE_GAME_SETTINGS_KEY, New),
    New;

update_setting({add, Setting}) ->
    Old = wf:session(?CREATE_GAME_SETTINGS_KEY),
    New = [Setting | lists:delete(Setting, Old)],
    check_required(New),
    wf:wire(#tooltip_wizard_update{}),
    wf:session(?CREATE_GAME_SETTINGS_KEY, New),
    New;

update_setting({set, Setting}) ->
    {Key, _Val} = Setting,
    Old = wf:session(?CREATE_GAME_SETTINGS_KEY),
    New = [Setting | proplists:delete(Key, Old)],
    check_required(New),
    wf:wire(#tooltip_wizard_update{}),
    wf:session(?CREATE_GAME_SETTINGS_KEY, New),
    New.

get_setting(Key) ->
    SessionName = wf:state(session_name),
    Setting = wf:session(SessionName),
    proplists:get_value(Key, Setting).


remove_checkbox_from_area(Element, Tag) ->
    %% remove element from criteria area (see event({remove_criteria_item, ...)
    wf:remove(wf:f(".from_~s", [Tag])),
    ElementState = wf:state(state_element),
    case proplists:get_value(Element, ElementState) of
	undefined -> % nothing checked or this is checkbox
	    wf:wire(#attr{target=Element,
			  attr="checked",
			  value=""});
	SetsTextId -> % something was selected
	    wf:wire(SetsTextId, #remove_class { class=create_game_criteria_item_select })
    end,
    %% remove sets_text tag from settings (see update_setting/1)
    Old = wf:session(?CREATE_GAME_SETTINGS_KEY),
    New = proplists:delete(Tag, Old),
    wf:session(?CREATE_GAME_SETTINGS_KEY, New).

slider_text_format(sets) ->
    ?_TS("Set: $setsize$", [{setsize,"~s"}]); %%"
slider_text_format(age) ->
    ?_TS("Age: $fromage$ - $toage$", [{fromage,"~s"},{toage,"~s"}]). %%"


