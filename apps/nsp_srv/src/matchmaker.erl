-module(matchmaker).
-author('Maxim Sokhatsky <maxim@synrc.com>').
-compile(export_all).

-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("nsm_db/include/user.hrl").
-include_lib("nsm_db/include/config.hrl").
-include_lib("nsm_db/include/feed.hrl").
-include_lib("nsm_db/include/table.hrl").
-include_lib("stdlib/include/qlc.hrl").

-include("elements/records.hrl").
-include("loger.hrl").
-include("setup.hrl").
-include("gettext.hrl").

-define(TABLE_UPDATE_INTERVAL, 10000).
-define(TABLE_UPDATE_QUANTUM, 3000).

route() -> ["game_name"].
title() -> ?_T("Matchmaker").

main() ->
  wf:state(buttons, green),

  case wf:user() of
    undefined -> [];
    User ->
      webutils:add_script("/nitrogen/jquery.paginatetable.js"),
      webutils:add_raw("<link href='/nitrogen/guiders-js/guiders-1.2.8.css' rel='stylesheet'>
        <script src='/nitrogen/guiders-js/guiders-1.2.8.js'></script>"),
      case webutils:guiders_ok("matchmaker_guiders_shown") of
        true -> guiders_script();
        false -> []
      end,
      add_game_settings_guiders(),
      SavedSettings = case wf:session({wf:q(game_name), wf:user()}) of
        undefined ->
          case wf:q(game_name) of
            "tavla" -> [{game, game_tavla}];
            "okey" -> [{game, game_okey}];
            _ -> [{game, game_okey}]
          end ++ [{table_name, table_name(default)}];
        Settings -> Settings
      end,
      wf:session({wf:q(game_name), wf:user()}, SavedSettings),
      wf:state(user_paid, nsm_accounts:user_paid(User))
  end,
  #template { file=code:priv_dir(nsp_srv)++"/templates/bare.html" }.


body() ->
  wf:state(buttons, green),
  %?INFO("Matchmaker User: ~p game: ~p",[wf:user(),?_U(wf:q(game_name))]),
  GameName = case wf:q(game_name) of 
    undefined -> "okey";
    Name -> Name 
  end,
  case wf:user() of
    undefined -> [];
    User ->
      X = rpc:call(?GAMESRVR_NODE,game_manager,get_lucky_table,[list_to_atom("game_"++GameName)]),
      wf:state(lucky,X),

      ui_update_buttons(),
      UId = webutils:user_info(username),
      wf:state(user_in_groups, nsm_groups:list_groups_per_user(UId)),
      wf:state(users_subscribe, nsm_users:list_subscr(UId)),
      ui_paginate(),

      PagePid = self(),
      wf:comet(fun() -> PagePid ! {comet_started,self()}, comet_update() end),
      receive {comet_started,Pid} -> wf:state(comet_pid,Pid) end
  end,

  [
    "<div class=\"list-top-photo-h\" style=\"margin-bottom: -30px;\">",
      webutils:get_hemen_nav(matchmaker),
    "</div>",
    #section{class="create-area", body=#section{class="create-block", body=[
      matchmaker_submenu(),
      #panel{id=rules_container, body=[]},
      #article{class="article1", body=[
        #panel{id=matchmaker_main_container, class="head", body=matchmaker_show_tables()},
        #panel{id=matchmaker_slide_area, class="slide-area"},
        #panel{id=tables, body=ui_get_tables()}
      ]},
      #panel{class="matchmaker-table-pager paging", body=[
        #panel{class="center", body=[
          #list{body=[#listitem{body=[#link{class="prevPage", text="<"}]}]},
          "<ul class='pageNumbers'></ul>",
          #list{body=[#listitem{body=[#link{class="nextPage", text=">"}]}]}
        ]}
      ]},
      #panel{id=info_table}
    ]}}
  ].

table_name(default) ->
    UId = wf:user(),
    {Date,_} = calendar:now_to_local_time(now()),
    Time = site_utils:date_to_text(Date),
    TableName = ?_TS("$username$ table, $date$ ", [{username, UId}, {date, Time}]),
    lists:flatten(TableName).

matchmaker_submenu() ->
  B = #span{style="font-weight:bold"},
  [
  #list{class="steps-list", body=[
    #listitem{class="submenu item1", body=[
      "<span id='guiderscreateblock'>",
        #link{postback={show,create_game}, text=?_T("CREATE"),
          actions=ac_hide_main_container()},
          B#span{class="ttl", text=?_T("Create Your Game")},
          B#span{text=?_T("You can create your own table and start to earn gift points.")},
      "</span>"]},
    #listitem{class="submenu item2", body=[
      "<span id='guidersjoinblock'>",
        #link{postback={show,join_game}, text=?_T("JOIN"),
          actions=ac_hide_main_container()},
          B#span{class="ttl", text=?_T("Join An Existing Game")},
          B#span{text=?_T("Start to gain gift points right now. Join to an existing follow's game.")},
      "</span>"]},
    #listitem{class="submenu item3",id=play_button_panel, body=[el_inside_play()]}]}
  ].

el_inside_play() ->
     LuckyAction =
        case wf:state(lucky) of
             [#game_table{id = GaId}] ->
                 IdStr = integer_to_list(GaId),
                 wf:session(IdStr, IdStr),
                 URL = lists:concat([?_U("/client"),"/",wf:q(game_name),"/id/", GaId]),
                 #event{type=click, actions=webutils:new_window_js(URL)};
             _ -> []
        end,
     B = #span{style="font-weight:bold"},
     [
      "<span id='guidersplayblock'>",
      #link{text=?_T("PLAY"), actions=LuckyAction},
      B#span{class="ttl", text=?_T("I Am Feeling Lucky")},
      B#span{text=?_T("You have no chance to get any gift points. Fast game only.")},
      "</span>"
     ].

matchmaker_show_create(Tag) ->
    ThisClass = case wf:state(buttons) of
        green -> "row_green";
        _ -> "row"
    end,
    #panel{class=criteria, body=[
        #singlerow{cells=[
            #tablecell{body=[
                "<span id='guiderscriteria'><nobr>",
                case Tag of
                    create -> #h2{text=wf:q(game_name) ++ " " ++ ?_T("Selected Option")};
                    _ -> #h2{text=wf:q(game_name) ++ " " ++?_T("Selected Option")}
                end,
                "</nobr></span>",
                #br{},
                #link{text=?_T("Game Rules"), class="matchmaker_game_rules", postback=show_game_rules, style="margin-left:0px;"}
            ]},
            #tablecell{body=[
                #panel{class=area, body=[
                    #list{id=criteria_field, class=ThisClass, body=""},
                    "<span id='guiderstab1createbutton' style='float:right; text-align:center;'>",
                    case Tag of
                        create -> el_create_game_button();
                        _ -> ""
                    end,
                    "</span>",
                    #link{text=?_T("Clear Options"), postback=clear_selection, class="matchmaker_clear_selection"}
                ]}
            ]}
        ]}
    ]}.

ui_paginate() ->

    CD10 = is_option_present(game_mode, countdown),
%    ?INFO("CD10: ~p",[CD10] ),
    case CD10 of true -> wf:wire(gosterge_placeholder, #show{}); _ -> wf:wire(gosterge_placeholder, #hide{}) end,

    wf:wire("$('.view_table_table').paginateTable({ rowsPerPage: 10, pager: '.matchmaker-table-pager', maxPageNumbers:20 }).find('tr:nth-child(2n)').addClass('color1');").

ui_table_name() ->
  ThisClass = case wf:state(buttons) of
    green -> "set-table-name_green";
    _ -> "set-table-name"
  end,
  [
        "<span id='guiderstab1tablename'>",
        "</span>",
        #h3{text=?_T("Table Name")},
        #panel{class="table-name", body=[
            #textbox{id=table_name,
                placeholder=?_T("Table name"),
                text=table_name(default)},
            "<span id='guiderstab1set' style='float:left;'>",
            #link{text=?_T("Set"), class=ThisClass, postback={tag, {table_name, textbox}}},
            "</span>"
        ]}
  ].

ui_game_speed() ->
    ThisClass = case wf:state(buttons) of
        green -> "list1_green size1";
        _ -> "list1 size1"
    end,
    [
        "<span id='guidersitem1'>",
        "<span id='guiderstab1gamespeed'>",
        "</span>",
        #h3{text=?_T("Game Speed")},
        "</span>",
        #list{class=ThisClass, body=[ #listitem{body=X} || X <- [
            construct_id(#link{text=?_T("Fast"), postback={tag,{speed,fast}}}),
            construct_id(#link{text=?_T("Normal"), postback={tag,{speed,normal}}}),
            construct_id(#link{text=?_T("Slow"), postback={tag,{speed,slow}}})
        ]]}
    ].

ui_game_type() ->
    ThisClass = case wf:state(buttons) of
        green -> "list1_green";
        _ -> "list1"
    end,
    GameType =
        case wf:q(game_name) of
            "okey" ->
                [{?_T("Standard"), standard},
                 {?_T("Even/Odd"), evenodd},
                 {?_T("Color"), color},
                 {?_T("Countdown from 10"), countdown}];
            "tavla" ->
                [{?_T("Standard"),  standard} %,
%                 {?_T("Pair"),      paired}
                ];
            _ ->
                [{?_T("Standard"), standard},
                 {?_T("Even/Odd"), evenodd},
                 {?_T("Color"), color},
                 {?_T("Countdown from 10"), countdown}]

        end,
    [
        "<span id='guidersitem2'>",
        "<span id='guiderstab1gametype'>",
        "</span>",
        #h3{text=?_T("Game Type")},
        "</span>",
        #list{class=ThisClass, body=[ #listitem{body=construct_id(#link{text=Text, postback={tag,{game_mode,Value}}})}
                                               || {Text, Value} <- GameType ]}
    ].

ui_double_game() ->
    ThisClass = case wf:state(buttons) of
        green -> "list1_green size1 form1";
        _ -> "list1 size1 form1"
    end,
    DoubleFactor = [{2, "x2"}, {4, "x4"}, {6, "x6"}, {8,"x8"}, {10, "x10"}],
    [
     "<span id='guiderstab1double'>",
     "</span>",
     #h3{text=?_T("Double quota, kakush and game points to")},
     #list{class=ThisClass, body=[#listitem{
            body=construct_id(#link{
                text=wf:to_list(Text),  postback={tag,{double_points, Factor}}}
        )} || {Factor, Text} <- DoubleFactor]}].

ui_rounds() ->
    ThisClass = case wf:state(buttons) of
        green -> "list1_green";
        _ -> "list1"
    end,
    Rounds =
	case wf:q(game_name) of
	    "okey"  -> [10,20,40,60,80];
	    "tavla" -> [3,5,7];
	    _  -> [10,20,40,60,80]
	end,
    [
        "<span id='guidersitem3'>",
        "<span id='guiderstab1rounds'>",
            "</span>",
        #h3{text=?_T("Rounds")},
        "</span>",
        #list{class=ThisClass, body=[ 
            #listitem{body=construct_id(#link{text=wf:to_list(X), class=ui_rounds_btn, postback={tag,{rounds,X}}})}
            || X <- Rounds]
        }
    ].



ui_checkboxes() ->
    ui_checkboxes(tabs).

ui_checkboxes(Section) ->
    GameName = case wf:q(game_name) of undefined -> "okey"; X -> X end,


    Settings = wf:session({GameName, wf:user()}),
    %_Countdown = proplists:get_value(game_mode, Settings, undefined),
    Checkboxes = [
        "<span id='guidersitem4'>",
        "<span id='guiderstab1paired'>",
        "</span>",
        case wf:q(game_name) of
            "okey" ->
                #panel{id=gosterge_placeholder, body=
                    construct_id(#checkbox{class="chk", postback={tag,{gosterge_finish,true}},
                        text=?_T("Gosterge finish"), value=?_T("Gosterge finish")})
                };
            _ -> ""
        end,
        "</span>"
    ],
    case Section of
	join -> #panel{class="choose-form", body=[ #panel{class="row", body=X} || X <- Checkboxes ]};
	_ ->    #panel{class="form1", body=Checkboxes }
    end.

ui_add_checkboxes() ->
    Checkboxes = [
                  construct_id(#checkbox{class="chk", postback={tag,{deny_robots,true}},
                                         value=?_T("Deny Robots"),
                                         text=?_T("Table can contain only players, not robots")}),
                  construct_id(#checkbox{class="chk", postback={tag,{private,true}}, % TODO: friends_only?
                                         value=?_T("Private"), text=?_T("Private table, only friends")}),
                  construct_id(#checkbox{class="chk", postback={tag,{slang,true}},
                                         value=?_T("Slang"), text=?_T("Slang is accepted")}),
                  construct_id(#checkbox{class="chk", postback={tag,{robots_replacement_disallowed,true}},
                                         value=?_T("Disallow Replace Robots"),
                                         text=?_T("Players can't replace robots if no free seats")}),
                  case wf:state(user_paid) of
                      true ->
                          construct_id(#checkbox{class="chk", postback={tag,{paid_only,true}},
                                                 value=?_T("Paid"), text=?_T("Only paid members")});
                      _ ->
                          []
                  end
                 ],
    #panel{class="form1", body=[ 
        "<span id='guiderstab1additional'>",
        "</span>",
        #h3{text=?_T("Additional options")},
		[#panel{class="row", body=X} || X <- Checkboxes ] 
    ]}.

matchmaker_show_tables() ->
    GameName = case wf:q(game_name) of undefined -> "okey"; X -> X end,
    TableFilter = [
        #panel{class="item item1", body=ui_game_speed() },
        #panel{class="item item2", body=ui_game_type() },
        #panel{id = ui_rounds, class="item item3", body=ui_rounds() },
        #panel{class="options", style="height:61px;", body=[
            #panel{style="margin-top:-32px; margin-bottom:-6px;", body=
              case wf:user() of
                undefined-> [];
                _U -> ui_checkboxes(join)
              end},
                "<span id='guidersdetailedsettings'>",
                    #link{body=?_T("Detailed Settings"), postback={show, join_game_detailed},
                       actions=ac_hide_main_container(), class="cancel", style=""},
                 "</span>"]}],
    [
        #singlerow { cells=[
            #tablecell{body=#h2{text=GameName ++ " " ++ ?_T("Selected Option")}},
            #tablecell{body=#link{text=?_T("Game Rules"), class="matchmaker_game_rules", postback=show_game_rules}}]},
        #panel{class="items", body=TableFilter}
    ].


el_create_game_button() ->
    GameName = case wf:q(game_name) of undefined -> "okey"; X -> X end,

    wf:state(session_id, rpc:call(?GAMESRVR_NODE,id_generator,get_id,[])),
    Url = lists:concat([?_U("/view-table/"), ?_U(GameName),"/id/",wf:state(session_id)]),
    Settings = wf:session({GameName, wf:user()}),
    wf:session(wf:state(session_id), Settings),
    JSPostback = site_utils:postback_to_js_string(?MODULE, create_game),
    CreateAction = #event{type=click, actions="if (!objs('create_button').hasClass('disable')) {"++webutils:new_window_js(Url)++";"++JSPostback++";};"},
    [
        "<nobr>",
        #link{id=create_button, class="create btn-create", actions=CreateAction, text=?_T("CREATE"), style="width:120px;"}, 
        "</nobr>"
    ].

construct_id(#link{postback={tag,Tag}} = A) -> A#link{id=construct_id(Tag)};
construct_id(#cool_button{postback={tag,Tag}} = A) -> A#cool_button{id=construct_id(Tag)};
construct_id(#checkbox{postback={tag,Tag}} = A) -> A#checkbox{id=construct_id(Tag)};
construct_id({Key, Val}) -> site_utils:simple_pickle({Key, Val}).

modified_base64_encode(B) -> m_b64_e(base64:encode(B), <<>>).
m_b64_e(<<>>, Acc) -> Acc;
m_b64_e(<<$+, Rest/binary>>, Acc) -> m_b64_e(Rest, <<Acc/binary, $->>);
m_b64_e(<<$/, Rest/binary>>, Acc) -> m_b64_e(Rest, <<Acc/binary, $_>>);
m_b64_e(<<$=, Rest/binary>>, Acc) -> m_b64_e(Rest, Acc);
m_b64_e(<<H,  Rest/binary>>, Acc) -> m_b64_e(Rest, <<Acc/binary, H>>).

ui_get_tables() -> Tables = get_tables(), show_table(Tables).

retrieve_tables(Setting, UId, GameType,Convert) ->
    Pid = spawn(matchmaker, process_tables, [Setting,UId,GameType,Convert]),
    Pid ! {self(), get},
    receive {Pid,Msg} -> Msg end.

process_tables(Setting, UId,GameType,Convert) ->
    receive
        {From, get} -> 
            
            Tables0 = [ begin
                         A =
                         nsm_queries:map_reduce(nsm_queries,
                                                get_single_tables,
                                                [Setting,UId,GameType,Convert,X]),
                         A end ||
                      X <- [more_other,nomore_other,three_other,two_other,one_other,
                            more_own,  nomore_own,  three_own,  two_own,  one_own]],

              Tables = lists:flatten(Tables0),


    Filtered = filter_tables(Tables,UId,GameType,Setting,Convert),


    From ! {self(), Filtered},stop end.

get_tables() -> get_tables(convert).

get_tables(Convert) -> 
    GameName = case wf:q(game_name) of undefined -> "okey"; X -> X end,

    Setting = wf:session({GameName,wf:user()}),
    UId = wf:user(),
    retrieve_tables(Setting, UId, GameName, Convert).

filter_tables(QLC,UId,GameFSM,Setting,Convert) ->

    GetPropList = fun(Key,Setngs) -> 
                   case Setngs of
                        undefined -> undefined;
                        _Else -> proplists:get_value(Key, Setngs)
                   end end,

    FilterAllUsers = case GetPropList(users, Setting) of
        undefined -> [];
        {multiple, ManyUsers} -> ManyUsers;
        SingleUser -> [SingleUser]
    end,


    FilterAnyUser = case GetPropList(group, Setting) of
        undefined -> [];
        GroupId -> 
            [GUId || GUId <- nsm_groups:list_group_members(GroupId)]
    end,


    FilteredQLC1 = lists:filter(
        fun(OneTable) ->
             TableUsers = OneTable#game_table.users,
            AllFilterOk = (FilterAllUsers==[]) orelse
                (lists:usort( [lists:member(OFU, TableUsers) || OFU <- FilterAllUsers] ) == [true]),
            AnyFilterOk = (FilterAnyUser==[]) orelse
                (lists:usort( [lists:member(OTU, FilterAnyUser) || OTU <- TableUsers] ) =/= [false]),
            AllFilterOk andalso AnyFilterOk
        end, QLC),

%    FilteredQLC2 = [GT || GT = #game_table{users = Users} <- FilteredQLC1, Users /= []],
    F = fun(T = #game_table{id = TId}, {TAcc, IdAcc}) ->
                case lists:member(TId, IdAcc) of
                    true -> {TAcc, IdAcc};
                    false -> {[T | TAcc], [TId | IdAcc]}
                end
        end,
    {FilteredQLC3, _} = lists:foldl(F, {[], []}, FilteredQLC1),
%    FilteredQLC4= lists:reverse(FilteredQLC3),
    FilteredQLC4= FilteredQLC3,
    case Convert of convert -> convert_to_map(FilteredQLC4, Setting,UId,GameFSM); _ -> FilteredQLC4 end.

convert_to_map(Data,_Setting,UId,GameFSM) ->
    [ begin Url = lists:concat([?_U("/view-table/"),GameFSM,"/id/", TId]),
            Script = webutils:new_window_js(Url),
            Action = #event{type=click, actions=#script{script=Script}},
            UserOwner = UId == Owner,
            [ Name,
              Owner,
              {info, {table, TId}},
              {join, Action},
              UserOwner,
              Users,
              {delete_table, TId, ProcId},
              GameState,
              RobotsReplacementAllowed ]
      end || #game_table{ name = Name,
                          id = TId,
                          rounds = _Rounds,
                          users = Users,
                          game_process = ProcId,
                          owner = _Owner,
                          creator = Owner,
                          game_state = GameState,
                          robots_replacement_allowed = RobotsReplacementAllowed
                         } = _Tab <- Data ].



list_users(Users) -> [ " " ++ case User of robot -> "robot"; User -> User end ++ " " || User <- Users].
list_users_links(Users, Owner) ->
    Usrs = [ " " ++ case User of robot -> "robot"; 
              Owner ->  
                  io_lib:format("<strong class=\"author\" style='font-size:14px;'><a href=\"~s\">~s</a></strong>",
                        [site_utils:user_link(User), User]);
              User -> 
                  io_lib:format("<strong class=\"author\"><a href=\"~s\">~s</a></strong>",
                        [site_utils:user_link(User), User])
              end ++ "" || User <- Users],
    ling:join(Usrs,",").

show_table(Tables) ->
  GameName = case wf:q(game_name) of undefined -> "okey"; Xx -> Xx end,

  wf:update(play_button_panel, el_inside_play()),
  case Tables of
    [] ->
      #panel{style="text-align: center", body=#h4{text=?_T("You can create a game or join a game")} };
    _ ->
      #table{class="view_table_table article-table", style="width:100%", rows=[
        begin
          {info, {_, TId}} = InfoPostback,
          Zone = TId div 1000000,
          WebSrv = "public@srv" ++ integer_to_list(Zone) ++ ".kakaranet.com",
          NodeAtom = case Zone of
            4 -> nsx_opt:get_env(nsm_db,web_srv_node,'web@doxtop.cc');
            _ -> list_to_atom(WebSrv)
          end,
          %{ok, WholeTable}
          Res = rpc:call(NodeAtom,view_table,get_table,[TId,wf:state(table)]),

          case Res of
            {ok,WholeTable} ->
              MaxUsers = case GameName of 
                "tavla" ->
                  case WholeTable#game_table.tournament_type of
                    paired -> 10;
                    paired_lobby -> 10;
                    _ -> 2
                  end;
                "okey" -> 4 
              end,
              RealUsers = case GameName of
                "tavla" ->
                  case WholeTable#game_table.tournament_type of
                    paired -> WholeTable#game_table.users;
                    _ -> Users
                  end;
                "okey" -> Users
              end,
              GameType = WholeTable#game_table.game_type,
              FreeSeatsNum = if GameType == game_okey,
                                GameState == started,
                                RobotsReplacementAllowed -> MaxUsers - length([U || U <- RealUsers, U=/=robot]);
                              true -> MaxUsers - length(RealUsers)
              end,
              TMode = matchmaker:game_mode_to_text(WholeTable#game_table.game_mode) 
                ++ " {"++atom_to_list(WholeTable#game_table.tournament_type)++"} " 
                ++ integer_to_list(TId),
              TSpeed = matchmaker:game_speed_to_text(WholeTable#game_table.game_speed),
              TRoundsOrNot = case WholeTable#game_table.rounds of
                undefined -> "";
                1 -> "";
                M -> ", "++integer_to_list(M) ++ " " ++ ?_T("rounds")
              end,
              TDoubleOrNot = case WholeTable#game_table.double_points of
                1 -> "";
                N -> ", x"++integer_to_list(N)
              end,
              RowId = wf:temp_id(),
              RemoveActions = #event{type=click, actions=#hide{target=RowId}},
              Info = case InfoPostback of
                {info, _} -> #link{id=showInfo, postback=InfoPostback, text=?_T("Info")};
                _ -> []
              end,
              JoinOrCrate = case Action of
                {join, Act} ->
                  IsMember = case wf:user() of
                    undefined -> false;
                    User -> lists:member(list_to_binary(User), Users)
                  end,
%%                            ?INFO("User: ~p, GameState: ~p, IsMember: ~p, Real users num: ~p, MaxUsers: ~p",
%%                                  [wf:user(), GameState, IsMember, length(RealUsers), MaxUsers]),
                  if GameState == started andalso IsMember;
                     FreeSeatsNum > 0 ->
                     #link{id=joinTable, actions=Act, show_if=wf:user()=/=undefined, text=?_T("Join"), class="join-button"};
                    true -> ""
                  end;
                {create, Act} ->
                  #link{id=joinTable, actions=Act, text=?_T("Create")};
                _ -> []
              end,
              DeleteTable = #link{id=deleteTable,
                postback=DeleteAction,
                show_if=UserOwner andalso wf:user()=/=undefined,
                actions=RemoveActions,
                text=?_T("Remove")
              },

              Buttons = #list{style="float:right;", body=[
                #listitem{body=X} || X <-
                  lists:duplicate(FreeSeatsNum, #image{image="/images/free.png"}) ++ [Info, JoinOrCrate, DeleteTable]
              ]},


              #tablerow{id=RowId, cells=[
                #tablecell{ class=cell1, body=[
                    TMode ++ ", " ++ TSpeed ++ TRoundsOrNot ++ TDoubleOrNot ++ 
                    "<br>" ++ ?_T("Players:") ++ " " ++ list_users_links(RealUsers, OwnerLabel) ++ " "
                  ],
                  id=tableNameLabel},
                #tablecell{ class=cell3, body = ["<nobr>", Buttons, "</nobr>"]}
              ]};

              X ->% ?INFO("Matchmaker #game_table rpc:call failed: ~p",[X]),
                ""
          end
        end
        || [_TableNameLabel,
            OwnerLabel,
            InfoPostback,
            Action,
            UserOwner,
            Users,
            DeleteAction,
            GameState,
            RobotsReplacementAllowed] <- Tables
      ]}
    end.

check_required(Setting) ->

    Required = case proplists:get_value(game_mode, Setting) of
        countdown -> [table_name, game, game_mode, speed];
        _ -> [table_name, game, game_mode, speed, rounds]
    end,

    Check = [case proplists:get_value(Req, Setting) of
                   undefined -> false;
                   {multiple, _} -> false;
                    _ -> true end || Req <- Required ],

    case lists:usort(Check) of
         [true] -> wf:remove(point_info),
                   wf:wire(create_button, #remove_class { class=disable });
              _ -> wf:remove(point_info),
                   wf:wire(create_button, #add_class { class=disable }) end.


%% don't put this function to process_setting/1, it can cause cycling
check_depended({game_mode, countdown}) ->
    ?INFO("CD10 pressed"),
    wf:wire(ui_rounds, #hide{}),
    wf:wire("objs('.for_rounds').remove()"), %% criteria_box
    wf:wire("objs('.ui_rounds_btn').parent('li').removeClass('active');"),
    wf:wire(gosterge_placeholder, #show{}); 

check_depended({game_mode, _}) ->
    wf:wire(ui_rounds, #show{}),
    wf:wire(gosterge_placeholder, #hide{});

check_depended(_) -> ok.

settings_box() -> settings_box(create).
settings_box(_Tag) ->
  case wf:user() of
    undefined -> [];
    _User ->
    ThisClass = case wf:state(buttons) of
        green -> "slide-up_green";
        _ -> "slide-up"
    end,
    [#tabs{tabs = [
      {?_T("Game Settings"), tab_game_setting()},
      {?_T("Group Settings"), tab_group_setting()},
      {?_T("Friend Settings"), tab_friend_setting()},
      {?_T("Personal Settings"), tab_personal_setting()}
    ]},
     "<span id='guiderstab1hide' style='float: right; margin-top:-50px;'>",
     "</span>",
     case wf:state(buttons) of
        green -> #link{class=ThisClass, postback={show,join_game}, text=?_T("Hide"), actions=ac_hide_main_container()};
        _ -> ""
     end
    ]
  end.

tab_game_setting()->
  #panel{body=[
    case wf:state(buttons) =:= green of
      false -> ui_table_name();
      _ -> "" 
    end,
    ui_game_speed(),
    #panel{class="two-cols", body=[
      #panel{class=left,body=ui_game_type()},
      #panel{id=ui_rounds, class="right", body=ui_rounds()}
    ]},
    ui_checkboxes(),
    ui_double_game(), 
    ui_add_checkboxes(),
    add_game_settings_guiders()
  ]}.

tab_group_setting() ->
  ThisClass = case wf:state(buttons) of
    green -> "list1_green";
    _ -> "list1"
  end,
  Groups = case wf:state(user_in_groups) of
    undefined-> [];
    Grps -> Grps
  end,
  View = [
    case nsm_groups:get_group(GId) of
      {ok, Group} -> #listitem{body=construct_id(#link{text=Group#group.name, postback={tag,{group, Group#group.name}}})};
      _ -> []
    end || GId <- Groups ],
  #panel{class="create-game-groups-box", body=[
    "<span id='guidersgroupfiltername'>",
    #textbox{id=groups_filter_name, actions=js_options_filter(groups_list), class="create-game-groups-filter-textbox"},
    #panel{class="create-game-groups-list", id=groups_list, body=#list{class=ThisClass, body=View}},
    "</span>"
  ]}.

tab_friend_setting() ->
  ThisClass = case wf:state(buttons) of
    green -> "list1_green";
    _ -> "list1"
  end,
  Users = case wf:state(users_subscribe) of
    undefined -> [];
    Us -> Us
  end,

  View = [ #listitem{body=construct_id(#link{text=Name, postback={tag,{users, Name}}})} || #subs{whom = Name} <- Users ],
  #panel{class="create-game-friends-box", body=[
    #textbox{id=friends_filter_name, actions=js_options_filter(friends_list), class="create-game-friends-filter-textbox"},
    #panel{class="create-game-friends-list", id=friends_list, body=#list{class=ThisClass, body=View}}
  ]}.

slider_text_format(sets) ->
    ?_TS("Set: $setsize$", [{setsize,"~s"}]); %%"
slider_text_format(age) ->
    ?_TS("Age: $fromage$ - $toage$", [{fromage,"~s"},{toage,"~s"}]). %%"

tab_personal_setting() ->
    ThisClass = case wf:state(buttons) of
        green -> "list1_green";
        _ -> "list1"
    end,
    AgeFormat=slider_text_format(age),
    [#panel{class="create_game_frame", style="text-align: center",
            body=[
            #h3{text=?_T("Age")},
            #slider{range = true, id=age_slider, min=18,
                postback={?MODULE, {tag, {age, slider}}},
                values=[{min,18}, {max,100}],
                text=wf:f("'"++AgeFormat++"'", %js code
                    ["' + ~s + '","' + ~s + '"])
            },
            #panel{
                id=age_numbers,
                body="18–100"
            },
            #h3{text=?_T("Gender")},
            #list{class=ThisClass, body=[ #listitem{body=X} || X <- [
                                          construct_id(#link{text=?_T("Male"), postback={tag,{sex, male}}}),
                                          construct_id(#link{text=?_T("Female"), postback={tag,{sex, female}}})
               ]]}
        ]}
    ].

js_options_filter(OptionsPanelId) when is_list(OptionsPanelId) ->
    "objs('me').keyup(function() {
        var filter = new RegExp($(this).val(), 'i');
        objs('" ++ OptionsPanelId ++ "').find('a').each(function() {
                console.log(this); var $this = $(this);
                if ($this.text().search(filter) < 0) { $this.parent().hide() } else { $this.parent().show() }
        })
    });";
js_options_filter(OptionsPanelId) -> js_options_filter(wf:to_list(OptionsPanelId)).

ui_update_buttons() ->
    GameName = case wf:q(game_name) of undefined -> "okey"; X -> X end,

    Settings = wf:session({GameName,wf:user()}),
    ui_update_buttons(Settings).

ui_update_buttons(undefined)->
  ok;
ui_update_buttons(Settings) ->
    wf:update(criteria_field, ""),
    [begin
        case Setting of
            {Key, {multiple, Elems}} ->
                [ ui_button_select({Key, V}) || V <- Elems];

            {Key, Elem} ->
                ui_button_select({Key, Elem})
        end,
        check_depended(Setting) end || Setting <- Settings ],

    check_required(Settings),
    ok.

ui_button_select({game, _}) -> ignore;
ui_button_select({table_name, Value}) -> wf:set(table_name, Value);
ui_button_select({Key, Value}) ->

    Id = construct_id({Key, Value}),
    SpanElement =
        case Key of
             age -> Format = slider_text_format(age),
                    Text = wf:f(Format, [ wf:to_list(X) || X <- Value ]),
                    #span{text=Text};
             _ ->   #span{actions="var e=objs('"++Id++"'); objs('me').text( e.text() ? e.text() : e.attr('value') )"}
        end,

    CriteriaElement = #listitem{id="for_"++Id, class="for_"++wf:to_list(Key), body=["<em>", SpanElement,
                        #link{text="X", postback={tag, {Key, Value}}}, "</em>"]},

    wf:insert_bottom(criteria_field, CriteriaElement),

    case is_checkbox(Key) of
         true -> wf:wire(#attr{target=Id, attr="checked", value="checked"});
         _ when Key == age -> case Value of
                                   [Min, Max] -> wf:wire(wf:f("$(\".wfid_~s\").slider(\"values\", [~b, ~b]);",
                                                    [age_slider, wf:to_integer(Min), wf:to_integer(Max)]));
                                   _ -> ignore
                              end;
         _ -> JSId = wf:js_escape(wf:to_list(Id)), wf:wire("objs('"++JSId++"').parent('li').addClass('active');")
    end.

ui_button_deselect({Key, Value}) ->
    Id = construct_id({Key, Value}),
    wf:remove("for_"++Id), %% criteria_box
    JSId = wf:js_escape(wf:to_list(Id)),
    case is_checkbox(Key) of
         true -> wf:wire("objs('"++JSId++"').attr('checked', false);");
            _ -> wf:wire("objs('"++JSId++"').parent('li').removeClass('active');")
    end.

is_checkbox(Key) ->
    case wf:state(user_paid) of
        true ->  lists:member(Key, [paired_game, deny_robots, private, slang, deny_observers, paid, robots_replacement_disallowed]);
        false -> lists:member(Key, [paired_game, deny_robots, private, slang, deny_observers, robots_replacement_disallowed])
    end.

is_option_present(Key, Value) ->
    GameName = case wf:q(game_name) of undefined -> "okey"; X -> X end,

    Settings = wf:session({GameName, wf:user()}),
    proplists:is_defined(Key, Settings) andalso Value == proplists:get_value(Key, Settings).

comet_update() -> comet_update(empty).
comet_update(State) ->
  case wf:user() of
       undefined -> wf:redirect_to_login(?_U("/login"));
       _UId -> timer:sleep(?TABLE_UPDATE_QUANTUM),
%            X = rpc:call(?GAMESRVR_NODE,game_manager,get_lucky_table,[list_to_atom("game_"++?_U(wf:q(game_name)))]),
%            wf:state(lucky,X),
            garbage_collect(self()),
            Tables = get_tables(),
            wf:update(tables, show_table(Tables)),
            ui_paginate(),
            wf:flush(),
            comet_update(State)
 end.

%comet_update() ->
%    wf:state(comet_pid,self()),
%    receive 
%        Z -> Tables = get_tables(),
%             wf:update(tables, show_table(Tables)),
%             ui_paginate(),
%             wf:flush()
%    end,
%    comet_update().

can_be_multiple(age)   -> false;
can_be_multiple(users) -> true;
can_be_multiple(_Key)  -> false.

api_event(Name, Tag, Args) -> webutils:api_event(Name, Tag, Args).

event(Event) ->
    case wf:user() of
    undefined -> u_event(Event);%wf:redirect_to_login(?_U("/login"));
    _User -> u_event(Event),
             X = wf:state(comet_pid),
             X ! ping_comet_to_refresh_table
    end.

ac_hide_main_container() -> #event{type=click, actions="objs('matchmaker_main_container').hide();"}.
ac_show_main_container() -> wf:wire("objs('matchmaker_main_container').show();"),
                            wf:wire("objs('matchmaker_slide_area').show();").

u_event({show,create_game}) ->
    u_event(clear_selection),
    wf:state(buttons, yellow),
    wf:update(matchmaker_main_container, matchmaker_show_create(create)),
    wf:update(matchmaker_slide_area, settings_box()),
    wf:wire("objs('tabs').tabs()"),
    ac_show_main_container(),
    ui_update_buttons();

u_event({show,join_game}) ->
    u_event(clear_selection),
    wf:state(buttons, green),
    wf:update(matchmaker_main_container, matchmaker_show_tables()),
    wf:update(matchmaker_slide_area, ""),
    ac_show_main_container(),
    ui_update_buttons(); 

u_event({show,join_game_detailed}) ->
    wf:update(matchmaker_main_container, matchmaker_show_create(none)),
    wf:update(matchmaker_slide_area, settings_box()),
    wf:wire("objs('tabs').tabs()"),
    ac_show_main_container(),
    ui_update_buttons();

u_event({tag, {age, slider}}) ->
    MinAge = wf:to_integer(wf:q(age_slider_values_min)),
    MaxAge = wf:to_integer(wf:q(age_slider_values_max)),
    wf:update(age_numbers, integer_to_list(MinAge) ++ "–" ++ integer_to_list(MaxAge)),
    process_setting({age, [MinAge, MaxAge]});

u_event({tag, {table_name, textbox}}) ->
    TableName = case wf:q(table_name) of
                     undefined -> table_name(default);
                     String -> case string:strip(String) of
                                    "" -> table_name(default);
                                    TN -> TN
                               end
    end,
    process_setting({table_name, TableName});

u_event(create_game) ->
  case wf:user() of
    undefined -> ok;
    UId ->
      GameName = case wf:q(game_name) of undefined -> "okey"; X -> X end,

      Settings = wf:session({GameName, wf:user()}),
      wf:session({GameName, UId},Settings),
      URL = ?_U(lists:concat(["/matchmaker/", GameName])),
      wf:redirect(URL),

      SRound = case proplists:get_value(rounds,Settings) of
        undefined -> "no";
        I -> integer_to_list(I)
      end,
      Desc = lists:flatten( URL ++ "|" ++ UId ++ "|" ++ proplists:get_value(table_name,Settings) ++ "|" ++ GameName ++ "|" ++
        SRound ++ "|" ++ atom_to_list(proplists:get_value(speed,Settings)) ++ "|" ++
        atom_to_list(proplists:get_value(game_mode,Settings))),
      webutils:post_user_system_message(Desc)
  end;

u_event({info, {Target, TId}}) ->
    {ok, TableSettings} = case Target of
        table ->
            Zone = TId div 1000000,
            WebSrv = "public@srv" ++ integer_to_list(Zone) ++ ".kakaranet.com",
            NodeAtom = case Zone of
                            4 -> nsx_opt:get_env(nsm_db, web_srv_node, 'web@doxtop.cc');
                            _ -> list_to_atom(WebSrv)
                       end,
            {ok, Table} = rpc:call(NodeAtom,view_table,get_table,[TId,wf:state(table)]),
            ?INFO("INFO: ~p",[{TId,Table}]),
            {ok,Table};
        save_table -> table_manager:get_save_table_setting(TId)
    end,
    Info = webutils:table_info(TableSettings),
    wf:update(info_table, #dialog{body=Info});

u_event({delete_table, TId, ProcId}) ->
    GameName = case wf:q(game_name) of undefined -> "okey"; X -> X end,

    Zone = TId div 1000000,
    GameNode = list_to_atom("game@srv"++integer_to_list(Zone)++".kakaranet.com"),
    NodeAtom = case Zone of
                    4 -> nsx_opt:get_env(nsm_db, web_srv_node, 'web@doxtop.cc');
                    _ -> GameNode
               end,
    A = rpc:call(NodeAtom,game_manager,destroy_game,[case GameName of "tavla" -> tavla_sup; _ -> okey_sup end, TId]),
    ?INFO("Table Deleted ~p",[A]);

u_event(clear_selection) ->
    GameName = case wf:q(game_name) of undefined -> "okey"; X -> X end,

    case wf:session({GameName, wf:user()}) of
      undefined -> ok;
      OldSettings -> 
    [
        u_event({tag, lists:nth(N, OldSettings)})
        || N <- lists:seq(3, length(OldSettings))
    ]
    end;

u_event(show_game_rules) ->
    GameName = case wf:q(game_name) of undefined -> "okey"; X -> X end,

    case GameName of
        "okey" ->
            wf:update(rules_container, [
                #link{text=?_T("Hide"), class="matchmaker_game_rules", style="float:right;", postback=hide_rules},
                #br{},
                case site_utils:detect_language() of
                    "tr" ->
                        #template{file=code:priv_dir(nsp_srv)++"/templates/matchmaker_rules_okey_tr.html"};
                    "en" ->
                        #template{file=code:priv_dir(nsp_srv)++"/templates/matchmaker_rules_okey_en.html"}
                end,
                #br{},
                #link{text=?_T("Hide"), class="matchmaker_game_rules", style="float:right;", postback=hide_rules},
                #br{},
                #br{},
                #br{}
        ]);
        "tavla" ->
            wf:wire("
                guiders.createGuider({
                    buttons: [
                        {name: '"++s_T("Ok")++"', onclick: guiders.hideAll},
                    ],
                    description: '"++s_T("Tables is a general name given to a class of board games similar to backgammon, played on a board with two rows of 12 vertical markings called \"points\". Players roll dice to determine the movement of pieces. Tables games are among the oldest known board games, and many variants are played throughout the world.")++"',
                    id: 'tavla_okey',
                    overlay: true,
                    title: '"++s_T("Tavla Rules")++"'
                }).show();
            ")
    end;

u_event({tag, {_Key, _Value} = Setting}) -> check_depended(Setting), process_setting(Setting);
u_event({tab_selected, ID}) -> show_tab_guiders(ID);
u_event(hide_rules) -> wf:update(rules_container, []);
u_event(Other) -> webutils:event(Other).

process_setting({Key, Value} = Setting) ->
  case wf:user() of 
    undefined -> ok;
    User ->
      GameName = case wf:q(game_name) of undefined -> "okey"; X -> X end,

      OldSettings = wf:session({GameName, User}),

      OldValues = case proplists:get_value(Key, OldSettings) of
                     undefined -> [];
                     {multiple, ValueList} -> ValueList;
                     OldValue -> [OldValue] end,

      NewValues = case lists:member(Value, OldValues) of
                     false -> ui_button_select(Setting),
                              case can_be_multiple(Key) of
                                   true -> [Value | OldValues];
                                   false -> [ ui_button_deselect({Key, V}) || V <- OldValues ], [Value] end;
                     true -> ui_button_deselect(Setting), lists:delete(Value, OldValues) end,

      NewSettings = case NewValues of
                       [] -> proplists:delete(Key, OldSettings);
                       List -> NewValue = case List of
                                               [Elem] -> Elem;
                                               _ -> {multiple, List} end,
                               lists:keystore(Key, 1, OldSettings, {Key, NewValue}) end,

      wf:session({GameName, User}, NewSettings),
      check_required(NewSettings),
      ?INFO("Settings: ~p",[NewSettings]),
      ok
  end.

game_mode_to_text(Type) when is_atom(Type) -> game_mode_to_text(atom_to_list(Type));
game_mode_to_text(Type) ->
   case Type of
        "standard" -> ?_T("Standard");
        "evenodd" -> ?_T("Even/Odd");
        "color" -> ?_T("Color");
        "countdown" -> ?_T("Countdown from 10");
        "paired" -> ?_T("Pair");
        "kakaratavla" -> ?_T("Kakara Tavla");
        undefined -> ?_T("Unknown");
        "undefined" -> ?_T("Unknown");
        _ -> "?"
    end.

game_speed_to_text(Speed) when is_atom(Speed) -> game_speed_to_text(atom_to_list(Speed));
game_speed_to_text(Speed) ->
    case Speed of 
        "fast" -> ?_T("Fast");
        "normal" -> ?_T("Normal");
        "slow" -> ?_T("Slow");
        _ -> ?_T("Unknown")
    end.

split(String, Separator) ->
    Pos = string:str(String, Separator),
    case Pos of
        0 -> [String];
        _ -> [string:left(String, Pos-1)] ++ split(string:right(String, length(String) - length(Separator) - Pos + 1), Separator)
    end.

join(String, Separator) -> string:join(String, Separator).
replace(String, Dirt, Icecream) -> join(split(String, Dirt), Icecream).
s_T(String) -> replace(replace(?_T(String), "\"", "\\\""), "\'", "\\\'"). 

make_guider(Show,Title,Desc,Buttons,Id,Next,Overlay,XButton,Attach,Pos) ->
    " guiders.createGuider({ " ++
    case Attach of none -> " "; Y -> "attachTo: '#" ++ atom_to_list(Y) ++ "', " end ++
        "buttons: [" ++ ling:join([" { name: '" ++ Text ++ "', onclick: "  ++
                  case Click of hide -> "guiders.hideAll";
                                next -> "guiders.next";
                                prev -> "guiders.prev" end ++ " }" || {Text,Click}<-Buttons],",") ++ "], " ++
    case Pos of none -> " "; Z -> "position: " ++ integer_to_list(Z) ++ ", " end ++
       "description: '"  ++ Desc ++ "', " ++ "id: '"  ++ atom_to_list(Id) ++ "', " ++
    case Next of none -> " "; X -> "next: '" ++ atom_to_list(X) ++ "', " end ++
    case Overlay of true -> "overlay: true"; false -> "overlay: false" end ++ ", " ++
    case XButton of true -> "xButton: true, "; false -> " " end ++ 
    " title: '" ++ Title ++ "' })" ++ case Show of show -> ".show();"; _ -> ";" end.

guiders_script() ->
    StdButtons = [{s_T("<< Back"),prev},{s_T("Continue"),next}],
    Guiders = [
        make_guider(show,s_T("Welcome to Kakaranet matchmaker"), s_T("Welcome to matchmaker notice."), [{s_T("No, thanks"),hide},{s_T("Continue"),next}],guider_10,guider_20,true,false,none,none),
        make_guider(hide,s_T("Create"), s_T("Create description."), StdButtons,guider_20,guider_30,false,true,guiderscreateblock,3),
        make_guider(hide,s_T("Join"), s_T("Join description."), StdButtons,guider_30,guider_40,false,true,guidersjoinblock,3),
        make_guider(hide,s_T("Play"), s_T("Join description."), StdButtons,guider_40,guider_50,false,true,guidersplayblock,9),
        make_guider(hide,s_T("Game Speed"), s_T("Game Speed description."), StdButtons,guider_50,guider_60,false,true,guidersitem1,12),
        make_guider(hide,s_T("Game Type"), s_T("Game Type description."), StdButtons,guider_60,guider_70,false,true,guidersitem2,12),
        make_guider(hide,s_T("Rounds"), s_T("Rounds description."), StdButtons,guider_70,guider_80,false,true,guidersitem3,12),
        make_guider(hide,s_T("Attributes"), s_T("Attributes description."), StdButtons,guider_80,guider_100,false,true,guidersitem4,12),
        make_guider(hide,s_T("Detailed settings"), s_T("Detailed settings description."), [{s_T("<< Back"),prev}],guider_100,guider_110,false,true,guidersdetailedsettings,9)],
    wf:wire(Guiders).

add_game_settings_guiders() ->
  StdButtons = [{s_T("<< Back"), prev}, {s_T("Continue"), next}],
  FinButtons = [{s_T("<< Back"), prev}, {s_T("Ok"), hide}],
  case webutils:guiders_ok("matchmaker_tab_1_guiders_shown") of
    false -> "";
    true -> [
      "<script>guiders.hideAll();",
      make_guider(show, s_T("Criteria"), s_T("Criteria description"), [{s_T("Continue"),next}], guider_110, guider_150, false, true, guiderscriteria, 12),
      make_guider(hide, s_T("Detailed game speed"), s_T("Detailed game speed description"), StdButtons, guider_150, guider_160, false, true, guiderstab1gamespeed, 11),
      make_guider(hide, s_T("Detailed game type"), s_T("Detailed game type description"), StdButtons, guider_160, guider_170, false, true, guiderstab1gametype, 11),
      make_guider(hide, s_T("Detailed paired"), s_T("Detailed paired description"), StdButtons, guider_170, guider_180, false, true, guiderstab1paired, 12),
      make_guider(hide, s_T("Detailed rounds"), s_T("Detailed rounds description"), StdButtons, guider_180, guider_185, false, true, guiderstab1rounds, 11),
      make_guider(hide, s_T("Double quota"), s_T("Double quota description"), StdButtons, guider_185, guider_190, false, true, guiderstab1double, 11),
      make_guider(hide, s_T("Additional options"), s_T("Additional options description"), StdButtons, guider_190, guider_195, false, true, guiderstab1additional, 11),
      make_guider(hide, s_T("Hide"), s_T("Hide button description"), FinButtons, guider_195, guider_199, false, true, guiderstab1hide, 1),
      "</script>"
    ]
  end.



show_tab_guiders(ID) ->
    case ID of
        "tab_2" -> case webutils:guiders_ok("matchmaker_tab_2_guiders_shown") of
                false -> ok;
                true -> wf:wire(["guiders.hideAll;", make_guider(show, s_T("Group Settings"), s_T("Group settings description."), 
                                                     [{s_T("Ok"), hide}], guider_200, guider_210, false, true, none, none)]) end;
        "tab_3" -> case webutils:guiders_ok("matchmaker_tab_3_guiders_shown") of
                false -> ok;
                true -> wf:wire(["guiders.hideAll;", make_guider(show, s_T("Friend Settings"), s_T("Friend settings description."),
                                                     [{s_T("Ok"), hide}], guider_300, guider_310, false, true, none, none)]) end;
        "tab_4" -> case webutils:guiders_ok("matchmaker_tab_4_guiders_shown") of
                false -> ok;
                true -> wf:wire(["guiders.hideAll;", make_guider(show, s_T("Personal Settings"), s_T("Personal settings description."), 
                                                     [{s_T("Ok"), hide}], guider_400, guider_410, false, true, none, none)]) end;
        _ -> ok
    end.
