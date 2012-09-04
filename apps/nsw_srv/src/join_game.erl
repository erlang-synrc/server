%% -*- mode: nitrogen -*-
-module (join_game).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("nsm_srv/include/user.hrl").
-include_lib("nsm_srv/include/feed.hrl").
-include_lib("nsm_srv/include/table.hrl").
-include_lib("elements/records.hrl").

-include("gettext.hrl").
-include("setup.hrl").

% THIS PAGE ARE TO BE DELETED

main() ->

    case wf:user() /= undefined of
        true  ->
            wf:session("game_setting_play", [ {game, [{game, game_okey}]} ]),
            wf:session("state_element", []),

            UId = webutils:user_info(username),

            wf:session("user_in_groups", rpc:call(?APPSERVER_NODE,groups,list_group_per_user,[UId])),

            wf:session("users_subscribe", rpc:call(?APPSERVER_NODE,users,list_subscription,[UId])),


            main_authorized();
        false -> wf:redirect_to_login(?_U("/login"))
    end.

main_authorized() -> #template { file=code:priv_dir(nsw_srv)++"/templates/bare.html" }.

title() -> ?_T("Game Page").


comet_update() ->
    case wf:user() of
	undefined -> % user logged of
	    wf:redirect_to_login(?_U("/login"));
	_UId ->
	    wf:update(view_table, filter_table()),
	    wf:flush(),
	    user_counter:wf_update_me(),
	    timer:sleep(3000),
	    comet_update()
    end.



body() ->
    wf:comet(fun() -> comet_update() end),
    wf:wire(setting_box, #hide{}),
    wf:wire(view_table_filter, #hide{}),
    wf:state(filter_visible, false),
    [#container_12 {
        body=[#grid_9{alpha=true, class="border-form",
                      id=top_box,
                      body=view_table_box()},

              #grid_3{ omega=true, class="border-form",
                       body=button_box()},


              #grid_5{alpha=true, class="border-form view_table_filter",
                      id=view_table_filter,
                      body=[]},

              #grid_7{omega=true, class="border-form",
                      id=setting_box,
                      body=settings_box()}
           ]}].



logo() ->
    [
     #link{url=?_U("/dashboard"), body=#image{image="/images/logo.png", style="float: left; margin-top: 25px;"}}
    ].

button_box() ->
    Lucky = webutils:new_window_js( lists:concat([?_U("/client"), "/", wf:q('__submodule__')]), game_okey_window),
    %% CreateGame = webutils:new_window_js("./create_game_new"),

    #panel{class="play_game_top", body=[#button{text=?_T("Create a game"),
                                                class="nice_button",
                                                style="cursor: pointer;",
                                                postback=to_create_game},

                                        #button{text=?_T("I'm feeling lucky!"),
                                                class="nice_button",
                                                style="cursor: pointer;",
                                                actions=#event{type=click,
                                                               actions=#script
                                                               {script=Lucky}}},

                                        #label{text=?_T("or")},

                                        #link{text=?_T("Filter tables..."),
                                              id=show_filter_button,
                                              style="cursor: pointer;",
                                              postback=show_filter_table}

                                       ]}.


criteria_box() ->
    #frame{title=?_T("Criteria"),
           close_icon="/images/box_close.png",
           close_action=none,
           body=[#droppable{id=criteria_box,
                            accept_groups=[speed,
                                           game_mode,
                                           additional,
                                           group,
                                           friend,
                                           personal],
                            tag=criteria,
                            class=".join_game_criteria_box",
                            body=[]}
                 ]}.

settings_box() ->
    #frame{title=?_T("Settings"), close_icon="/images/box_close.png",
           close_action=none,
           body=[#accordion{tab=[#accordion_tab{title=?_T("Game settings"),
                                                body=tab_game_setting()},
                                 #accordion_tab{title=?_T("Group settings"),
                                                body=tab_group_setting()},
                                 #accordion_tab{title=?_T("Friend settings"),
                                                body=tab_friend_setting()},
                                 #accordion_tab{title=?_T("Personal settings"),
                                                body=tab_personal_setting()}]}
                ]}.

view_table_box() ->
    [#frame{title=?_T("Tables"), close_icon="/images/box_close.png", close_action=none,
            body=[#grid_clear{},
                  #panel{id=view_table,
                         body=filter_table()}]},
     #panel{id=info_table}].

tab_game_setting() ->
    wf:wire(#attr{target=coupled_cb, attr="disabled", value="true"}),
    wf:wire(#attr{target=private_cb, attr="disabled", value="true"}),
    wf:wire(#attr{target=slang_cb, attr="disabled", value="true"}),
    wf:wire(#attr{target=observers_cb, attr="disabled", value="true"}),
    [#panel{class="create_game_frame",
            body=[#span{text=?_T("Speed"), class="table_manager_group_name"},
                  #droppable{body=[#draggable_new {group=speed,
                                                   tag={speed, fast},
                                                   class="create_game_criteria_item",
                                                   revert=invalid,
                                                   id=fast,
                                                   group_disable=[fast],
                                                   body=?_T("Fast")},

                                   #draggable_new {group=speed,
                                                   tag={speed, normal},
                                                   class="create_game_criteria_item",
                                                   revert=invalid,
                                                   id=normal,
                                                   group_disable=[normal],
                                                   body=?_T("Normal")},

                                   #draggable_new {group=speed,
                                                   tag={speed, slow},
                                                   revert=invalid,
                                                   class="create_game_criteria_item",
                                                   id=slow,
                                                   group_disable=[slow],
                                                   body=?_T("Slow")}]}]},

     #panel{class="create_game_frame",
            body=[#span{text=?_T("Game type"), class="table_manager_group_name"},
                  #droppable{body=[#draggable_new {group=game_mode,
                                                   tag={game_mode, standard},
                                                   revert=invalid,
                                                   class="create_game_criteria_item",
                                                   id=standard,
                                                   group_disable=[standard],
                                                   body=?_T("Standard")},

                                   #draggable_new {group=game_mode,
                                                   tag={game_mode, color},
                                                   class="create_game_criteria_item",
                                                   revert=invalid,
                                                   id=color,
                                                   group_disable=[color],
                                                   body=?_T("Color")},

                                   #draggable_new {group=game_mode,
                                                   tag={game_mode, evenodd},
                                                   class="create_game_criteria_item",
                                                   revert=invalid,
                                                   id=evenodd,
                                                   group_disable=[evenodd],
                                                   body=?_T("Even/Odd")},

                                   #draggable_new {group=game_mode,
                                                   tag={game_mode, countdown},
                                                   class="create_game_criteria_item",
                                                   revert=invalid,
                                                   id=countdown,
                                                   group_disable=[countdown],
                                                   body=?_T("Countdown from 10")},
                                   #br{},
                                   #checkbox{class="create_game_criteria_item",
                                             id=coupled_cb,
                                             postback={cb_change, ?_T("Coupled"), coupled_cb, coupled},
                                             text=?_T("Coupled game")},

                                   #checkbox{class="create_game_criteria_item",
                                             id=gosterge_finish_cb,
                                             postback={cb_change, ?_T("Gosterge finish"), gosterge_finish_cb, gosterge_finish},
                                             text=?_T("Gosterge finish")}                                  ]}]},
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
                            postback={cb_change, ?_T("Observers are accepted"),
                                      observers_cb, observers},
                            text=?_T("Observers are accepted")}
                 ]}

    ].

drop_event({{age, undefined}, Id, _, DisGroup}, DropTag) when is_list(DisGroup)  ->
    MinAge = wf:to_integer(wf:q(age_slider_values_min)),
    MaxAge = wf:to_integer(wf:q(age_slider_values_max)),
    Body = ?_TS("Age: $minage$ - $maxage$", [{minage,MinAge}, {maxage, MaxAge}]),
    drop_event({ {age, [MinAge, MaxAge]}, Id, Body, DisGroup}, DropTag);

drop_event({Tag, Id, Body, DisGroup, _CriteriaFor}, DropTag) when is_list(DisGroup)  ->
    case DropTag of
        criteria ->
            TempId = wf:temp_id(),
            Element = #panel{id=TempId,
                             body=[#label{text=Body,
                                          class="create_game_criteria_item_label"},
                                   #link{text="",
                                         postback={remove_criteria_item, TempId, Id, DisGroup, Tag},
                                         class="ui-icon ui-icon-circle-close create_game_criteria_item_close"}],
                             class="create_game_criteria_item"},

            wf:insert_bottom(undefined, Element),
            Script = wf:f("$(\"~s\").addClass(\"~s\");", [Id, "create_game_criteria_item_select"]),
            wf:wire(Script),
            [ begin
                  wf:wire(wf:f("$(\".wfid_~s\").draggable(\"disable\");", [Dis])),
                  state_element({lock, Dis, Id})
              end || Dis <- DisGroup ],
            update_setting({add, Tag});

        _ ->
            ok
    end.

tab_group_setting() ->
    Groups = wf:session("user_in_groups"),
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
          TempId = wf:temp_id(),
          #draggable_new {group=group,
                          tag={group, Name},
                          revert=invalid,
                          class="create_game_group_item",
                          id=TempId,
                          group_disable=[TempId],
                          body=Name}
      end || #group_member{group = Name} <- Groups ].

tab_friend_setting() ->
    Groups = wf:session("users_subscribe"),
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
          TempId = wf:temp_id(),
          #draggable_new {group=friend,
                          tag={user, Name},
                          revert=invalid,
                          class="create_game_group_item",
                          id=TempId,
                          group_disable=[TempId],
                          body=Name}
      end || #subscription{whom = Name} <- Groups ].

tab_personal_setting() ->
    LocationUser = webutils:user_info(location),
    CityList = webutils:city_list(),
    City = webutils:list_to_options(CityList, LocationUser),
    CityDrag =
        case LocationUser of
            undefined ->
                #draggable_new{group=personal,
                               tag={location, undefined},
                               revert=invalid,
                               class="create_game_criteria_item",
                               id=location,
                               disabled=true,
                               group_disable=[],
                               body=?_T("City: Not defined")};
            LocationUser  ->
                #draggable_new{group=personal,
                               tag={location, LocationUser},
                               revert=invalid,
                               class="create_game_criteria_item",
                               id=location,
                               group_disable=[],
                               body=?_TS("City: $city$", [{city, LocationUser}])}
        end,

    AgeFormat=?_TS("Age: $fromage$ - $toage$", [{fromage,"~s"},{toage,"~s"}]),
    [#panel{class="create_game_frame", style="text-align: center",
            body=[#span{text=?_T("Age limit"), class="table_manager_group_name"},
                  #draggable_new{group=personal,
                                 tag={age, undefined},
                                 revert=invalid,
                                 class="create_game_criteria_item",
                                 id=age_text,
                                 group_disable=[],
                                 body=wf:f(AgeFormat, ["18","50"])},
                  #slider{range = true, id=age_slider,
                          target=age_text,
                          values=[{min,18}, {max, 50}],
                          text=wf:f("'"++AgeFormat++"'", %js code
				    ["' + ~s + '","' + ~s + '"])}]},

     #panel{class="create_game_frame", style="text-align: center",
            body=[#span{text=?_T("Gender"), class="table_manager_group_name"},
                  #draggable_new{group=personal,
                                 tag={sex, male},
                                 revert=invalid,
                                 class="create_game_criteria_item",
                                 id=sex_male,
                                 group_disable=[sex_female],
                                 body=?_T("Male")},
                  #draggable_new{group=personal,
                                 tag={sex, female},
                                 revert=invalid,
                                 class="create_game_criteria_item",
                                 id=sex_female,
                                 group_disable=[sex_male],
                                 body=?_T("Female")}
                 ]},
     #panel{class="create_game_frame", style="text-align: center",
            body=[#span{text=?_T("Location"), class="table_manager_group_name"},
                  #panel{id=city_drag,
                         body=CityDrag},
                  #br{},

                  #dropdown{options=City,
                            id=select_city,
                            postback=select_city,
                            class="nice_dropdown"}]}


    ].


event({info, {Target, TId}}) ->
    {ok, TableSettings} = case Target of
                              table ->
                                  {ok, Table} = rpc:call(?APPSERVER_NODE,table_manager,get_table,[TId]),
                                  {ok, rpc:call(?APPSERVER_NODE,table_manager,game_table_to_settings,[Table])};
                              save_table -> rpc:call(?APPSERVER_NODE,table_manager,get_save_table_setting,[TId])
                          end,
    Info = webutils:table_info(TableSettings),
    wf:update(info_table, #dialog{body=Info});



event({delete_table, TId}) ->
    rpc:call(?APPSERVER_NODE,table_manager,delete_table,[TId]);

event({delete_saved_table, TId}) ->
    rpc:call(?APPSERVER_NODE,table_manager,delete_save_table,[TId]);

event(select_city) ->
    NotDefined =
        #draggable_new{group=personal,
                       tag={location, undefined},
                       revert=invalid,
                       class="create_game_criteria_item",
                       id=location,
                       disabled=true,
                       group_disable=[],
                       body=?_T("City: Not defined")},

    Drag =
        case wf:q(select_city) of
            undefined ->
                NotDefined;
            "undefined" ->
                NotDefined;
            City ->
                #draggable_new{group=personal,
                               tag={location, City},
                               revert=invalid,
                               class="create_game_criteria_item",
                               id=location,
                               group_disable=[],
                               body=?_TS("City: $city$", [{city, City}])}
        end,
    wf:update(city_drag, Drag);





event({uncheck_cb, El, Id, Tag}) ->
    wf:remove(El),
    wf:wire(#attr{target=Id,
                  attr="checked",
                  value=""}),
    update_setting({remove, {Tag, true}});

event({cb_change, Body, Id, Tag}) ->
    Val = wf:q(Id),
    On = Val == "on" andalso Val /= undefined,
    TempId = list_to_atom(lists:flatten(io_lib:format("~s_tag", [Tag]))),
    case On of
        true ->
            Element = #panel{id=TempId,
                             body=[#label{text=Body,
                                          class="create_game_criteria_item_label"},
                                   #link{text="",
                                         postback={uncheck_cb, TempId, Id, Tag},
                                         class="ui-icon ui-icon-circle-close create_game_criteria_item_close"}],
                             class="create_game_criteria_item"},
            wf:insert_bottom(criteria_box, Element),
            Script = wf:f("$(\"~s\").addClass(\"~s\");", [Id, "create_game_criteria_item_select"]),
            wf:wire(Script),
            update_setting({add, {Tag, true}});
        false ->
            event({uncheck_cb, TempId, Id, Tag})
    end;


event(to_create_game) ->
    wf:redirect(lists:concat([?_U("/create-game"), "/", wf:q('__submodule__')]));

event(logout) ->
    wf:logout(),
    wf:redirect_to_login(?_U("/login"));

event({remove_criteria_item, Id, DragId, DisGroup, Tag}) ->
    [ state_element({unlock, Dis, DragId}) || Dis <- DisGroup ],

    wf:wire(wf:f("$(\"~s\").removeClass(\"~s\");",
                  [DragId, "create_game_criteria_item_select"])),

    wf:remove(Id),
    update_setting({remove, Tag});

event(show_filter_table) ->
    case wf:state(filter_visible) of
        false ->
            wf:wire(setting_box, #show{speed=500, effect=blind}),
            wf:wire(top_box, #show{speed=500, effect=slide}),
            wf:wire(view_table_filter, #show{speed=500, effect=blind}),
            wf:update(show_filter_button, ?_T("Close filter")),
            wf:update(top_box, criteria_box()),
            wf:update(view_table_filter, view_table_box()),
            wf:state(filter_visible, true);
        true ->
            wf:wire(setting_box, #hide{speed=500, effect=blind}),
            wf:wire(top_box, #show{speed=500, effect=slide}),
            wf:wire(view_table_filter, #hide{speed=500, effect=blind}),
            wf:update(show_filter_button, ?_T("Filter tables...")),
            wf:update(top_box, view_table_box()),
            wf:update(view_table_filter, []),
            wf:state(filter_visible, false)
    end;



event(Other) ->
    webutils:event(Other).



state_element({lock, Element, By}) ->
    State = wf:session("state_element"),
    NewState = [{Element, By} | State],
    wf:session("state_element", NewState),
    {ok, lock};

state_element({unlock, Element, By}) ->
    State = wf:session("state_element"),
    NewState = lists:delete({Element, By}, State),
    wf:session("state_element", NewState),
    case lists:keytake(Element, 1, NewState) of
        false ->
            wf:wire(wf:f("$(\".wfid_~s\").draggable(\"enable\");", [Element])),
            {ok, unclock};
        _ ->
            {error, can_not_unlock}
    end.




update_setting({remove, {Key, _Value} = Setting}) ->
    Old = wf:session("game_setting_play"),
    NewParam =
        case proplists:get_value(Key, Old) of
            Current ->
                case lists:delete(Setting, Current) of
                    [] ->
                        lists:delete({Key, Current}, Old);
                    Param ->
                        New0 = lists:delete({Key, Current}, Old),
                        [{Key, Param} | New0]
                end
        end,
    wf:session("game_setting_play", NewParam),
    io:fwrite("Update setting (remove): ~p~n~n", [NewParam]),
    wf:update(view_table, filter_table());

update_setting({add, {Key, _Value} = Setting}) ->
    Old = wf:session("game_setting_play"),
    NewParam =
        case proplists:get_value(Key, Old) of
            undefined ->
                Param = {Key, [Setting]},
                [Param | Old];
            Current ->
                Param = {Key, lists:flatten([Setting | Current])},
                New0 = lists:delete({Key, Current}, Old),
                [Param | New0]
        end,
    wf:session("game_setting_play", NewParam),
    io:fwrite("Update setting (add): ~p~n~n", [NewParam]),
    wf:update(view_table, filter_table()).




show_table(Tables) ->
    case Tables of
        [] ->
            #panel{style="text-align: center",
                   body=#h3{text=?_T("You can create a game or join a game")} };
        _ ->
            #table{class=view_table_table,
                   rows=[#tablerow{class=view_table_header,
                                   cells=[#tableheader{text=?_T("Table name")},
                                          #tableheader{text=?_T("Owner")},
                                          #tableheader{text=?_T("Actions")}
                                         ]},
                         [ begin
                           RowId = wf:temp_id(),
                           RemoveActions = #event{type=click,
                                                  actions=#hide{target=RowId}},
			       Info =
				   case InfoPostback of
				       {info, _} ->
					   #button{id=showInfo,
						    postback=InfoPostback,
						    text=?_T("Info")};
				       _ -> []
				   end,
			       JoinOrCrate =
				   case Action of
				       {join, Act} ->
					   #button{id=joinTable,
						     actions=Act,
						     show_if=ViewPerPoint,
						     text=?_T("Join")};
				       {create, Act} ->
					   #button{id=joinTable,
						     actions=Act,
						     text=?_T("Create")};
				       _ -> []
				   end,
                               DeleteTable = #button{id=deleteTable,
                                                     postback=DeleteAction,
                                                     show_if=UserOwner,
                                                     actions=RemoveActions,
                                                     text=?_T("Remove")},
			       #tablerow{id=RowId,
					 cells=[#tablecell{class=view_table_name,
							   text=TableNameLabel,
							   id=tableNameLabel},
						#tablecell{class=view_table_owner,
							   text=OwnerLabel,
							   id=ownerLabel},
						#tablecell{class=view_table_action,
							   body=[Info, JoinOrCrate, DeleteTable]}]}
			   end
                           || [TableNameLabel,
                               OwnerLabel,
                               InfoPostback,
                               Action,
                               ViewPerPoint,
                               UserOwner,
                               DeleteAction] <- Tables ]
                        ]}
    end.

filter_table() ->
    UId = wf:user(),
    {ok, Tables0} = rpc:call(?APPSERVER_NODE,table_manager,filter_per_user,[UId]),
    Tables = convert_to_map(Tables0),
    MyTables0 = rpc:call(?APPSERVER_NODE,table_manager,get_save_tables,[UId]),
    MyTables = convert_to_map2(MyTables0),
    show_table(Tables++MyTables).


convert_to_map(Data) ->
    Setting = wf:session("game_setting_play"),
    UId = wf:user(),
    [ begin
          Url = lists:concat([?_U("/view-table"), "/id/", TId]),
          Script = webutils:new_window_js(Url),
          Action = #event{type=click,
                          actions=#script{script=Script}},
          ViewPerPoint = site_utils:table_per_user_point(UId,
							 Sets,
							 Rounds),
          UserOwner = UId == Owner,
        [Name,
         Owner,
         {info, {table, TId}},
         {join, Action},
         ViewPerPoint,
         UserOwner,
         {delete_table, TId}]
      end || #game_table{name = Name,
                         id = TId,
                         sets = Sets,
                         rounds = Rounds,
                         owner = Owner} = Tab <- Data,
             filter_table(Setting, Tab) ].

convert_to_map2(Data) ->
    User = wf:user(),
    Setting = wf:session("game_setting_play"),
    [ begin
          Url = lists:concat([?_U("/view-table"), "/", wf:q('__submodule__'), "/saveid/", TId]),
          Script = webutils:new_window_js(Url),
          Action = #event{type=click,
                          actions=#script{script=Script}},
          Rounds = wf:to_integer(proplists:get_value(rounds, Settings, 20)),
          Sets = wf:to_integer(proplists:get_value(sets, Settings, 1)),
          ViewPerPoint = site_utils:table_per_user_point(User, Sets, Rounds),
          [Name,
           User,
           {info, {save_table, TId}},
           {create, Action},
           ViewPerPoint,
           User,
           {delete_saved_table, TId}
          ]
      end || #save_game_table{name = Name,
                              settings = Settings,
			      id = TId} = Tab <- Data,filter_table(Setting, Tab) ].


filter_table(Setting, Table) ->
    TableSetting = filter_table_get_settings(Table),
    lists:all(fun(Set) ->
                      member(Set, TableSetting)
              end, Setting).

filter_table_get_settings(#save_game_table{} = Table) ->
    rpc:call(?APPSERVER_NODE,table_manager,save_game_table_to_settings,[Table]);
filter_table_get_settings(#game_table{} = Table) ->
    rpc:call(?APPSERVER_NODE,table_manager,game_table_to_settings,[Table]).



member({age, AgeLists}, TableSetting) ->
    CheckAge =
        fun([SMin, SMax], [TMin, TMax]) ->
                TMin >= SMin andalso TMax =< SMax
        end,


    case lists:keysearch(age, 1, TableSetting) of
        {value, {age, undefined}} ->
            false;
        {value, {age, TAge}} ->
            lists:any(fun({_, SAge}) ->
                              CheckAge(SAge, TAge)
                      end, AgeLists)
    end;

member({Key, Set}, TableSetting) ->
    case proplists:get_value(Key, TableSetting) of
        undefined ->
            false;
        Setting ->
            lists:member({Key, Setting}, Set)
    end.


