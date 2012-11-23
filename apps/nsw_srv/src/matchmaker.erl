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
  ?INFO("Matchmaker User: ~p game: ~p",[wf:user(),?_U(wf:q(game_name))]),
%     Settings = wf:session({wf:q(game_name), wf:user()}),
%     Game = proplists:get_value(game, Settings),

  case wf:q(game_name) of
    "okey" -> ok;
    "tavla" -> ok;
    _ -> wf:redirect(?_U("/dashboard"))
  end,

  case wf:user() of
    undefined -> webutils:redirect_to_ssl("login");
    User ->        
      webutils:add_script("/nitrogen/jquery.paginatetable.js"),
      webutils:add_raw("<link href='/nitrogen/guiders-js/guiders-1.2.8.css' rel='stylesheet'>
      <script src='/nitrogen/guiders-js/guiders-1.2.8.js'></script>"),
      case webutils:guiders_ok("matchmaker_guiders_shown") of
        true -> guiders_script();
        false -> []
      end,
      add_game_settings_guiders(),
      SS = case wf:session({wf:q(game_name), wf:user()}) of
        undefined -> case wf:q(game_name) of
                      "tavla" -> [{game, game_tavla}];
                      "okey" -> [{game, game_okey}]
                     end ++ [{table_name, table_name(default)}];
        Settings -> Settings
      end,
      wf:session({wf:q(game_name), wf:user()}, SS),
      wf:state(user_paid, nsm_accounts:user_paid(User)),
      #template { file=code:priv_dir(nsw_srv)++"/templates/bare.html" }
  end.

body() ->

            X = rpc:call(?GAMESRVR_NODE,game_manager,get_lucky_table,[list_to_atom("game_"++?_U(wf:q(game_name)))]),
            wf:state(lucky,X),

  ui_update_buttons(),
  UId = webutils:user_info(username),
  wf:state(user_in_groups, nsm_groups:list_groups_per_user(UId)),
  wf:state(users_subscribe, nsm_users:list_subscr(UId)),
  ui_paginate(),
  wf:comet(fun() -> comet_update() end),


  [#section{class="create-area", body=#section{class="create-block", body=[
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
  ]}}].

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
                "</span>"
            ]},
            #listitem{class="submenu item2", body=[
                "<span id='guidersjoinblock'>",
                #link{postback={show,join_game}, text=?_T("JOIN"),
				    actions=ac_hide_main_container()},
                B#span{class="ttl", text=?_T("Join An Existing Game")},
                B#span{text=?_T("Start to gain gift points right now. Join to an existing follow's game.")},
                "</span>"
            ]},
            #listitem{class="submenu item3",id=play_button_panel, body=[
                el_inside_play()
            ]}
	    ]}
    ].

el_inside_play() ->
     Settings = wf:session({wf:q(game_name), wf:user()}),
     Game = proplists:get_value(game, Settings),
%     X = rpc:call(?GAMESRVR_NODE,game_manager,get_lucky_table,[Game]),
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
    ?INFO("CD10: ~p",[CD10] ),
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
		   [{?_T("Standard"),	  standard},
		    {?_T("Even/Odd"),	  evenodd},
		    {?_T("Color"),	  color},
		    {?_T("Countdown from 10"), countdown}];
	       "tavla" ->
		   [{?_T("Standard"),  standard},
		    {?_T("Pair"),      paired},
		    {?_T("Kakara Tavla"),    kakaratavla}]
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
	    "tavla" -> [3,5,7]
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

    Settings = wf:session({wf:q(game_name), wf:user()}),
    _Countdown = proplists:get_value(game_mode, Settings, undefined),
    Checkboxes = [
        "<span id='guidersitem4'>",
        "<span id='guiderstab1paired'>",
        "</span>",
%        construct_id(#checkbox{class="chk", postback={tag,{paired_game,true}}, text=?_T("Paired"), value=?_T("Paired")}),
        case wf:q(game_name) of
            "okey" ->  % this is how it should be
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
		  construct_id(#checkbox{class="chk", postback={tag,{deny_observers,true}},
					 value=?_T("No observers"), text=?_T("I don't accept observers")}),
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
    TableFilter = [
		   #panel{
		      class="item item1",
		      body=ui_game_speed()
		   },
		   #panel{
		      class="item item2",
		      body=ui_game_type()
		   },
		   #panel{
              id = ui_rounds,
		      class="item item3",
		      body=ui_rounds()
		   },
%		   #panel{
%		      class="item item4", style="width:64px;",
%		      body=ui_checkboxes(join)
%		   },
		   #panel{
		      class="options", style="height:61px;",
		      body=[
%                "<span id='guidersgamebutton'>",
%			     el_create_game_button(),
%                 "</span>",
                 #panel{
                    style="margin-top:-32px; margin-bottom:-6px;",
       		        body=ui_checkboxes(join)
                 },
                 "<span id='guidersdetailedsettings'>",
		         #link{body=?_T("Detailed Settings"), postback={show, join_game_detailed},
			       actions=ac_hide_main_container(), class="cancel", style=""},
                 "</span>"
		       ]}
		  ],
    [
        #singlerow { cells=[
            #tablecell { 
                body=#h2{text=wf:q(game_name) ++ " " ++ ?_T("Selected Option")}
            },
            #tablecell { 
                body=#link{text=?_T("Game Rules"), class="matchmaker_game_rules", postback=show_game_rules}
            }
        ]},
        #panel{class="items", body=TableFilter}
    ].


el_create_game_button() ->
    {_, _, Sid} = now(),
    wf:state(session_id, Sid),
    Url = lists:concat([?_U("/view-table/"), ?_U(wf:q(game_name)),"/id/",wf:state(session_id)]),
    Settings = wf:session({wf:q(game_name), wf:user()}),
    wf:session(wf:state(session_id), Settings),
    JSPostback = site_utils:postback_to_js_string(?MODULE, create_game),
    %% create only when link is not disabled
    CreateAction = #event{type=click, actions="if (!objs('create_button').hasClass('disable')) {"
					      ++webutils:new_window_js(Url)++";"++JSPostback++";};"},
    [
        "<nobr>",   % had to tweek CSS here. It's complicated
        #link{id=create_button, class="create btn-create", actions=CreateAction, text=?_T("CREATE"), style="width:120px;"}, 
        "</nobr>"
    ].

construct_id(#link{postback={tag,Tag}} = A) ->
    A#link{id=construct_id(Tag)};
construct_id(#cool_button{postback={tag,Tag}} = A) ->
    A#cool_button{id=construct_id(Tag)};
construct_id(#checkbox{postback={tag,Tag}} = A) ->
    A#checkbox{id=construct_id(Tag)};
construct_id({Key, Val}) ->
    site_utils:simple_pickle({Key, Val}).

modified_base64_encode(B) -> m_b64_e(base64:encode(B), <<>>).
m_b64_e(<<>>, Acc) -> Acc;
m_b64_e(<<$+, Rest/binary>>, Acc) -> m_b64_e(Rest, <<Acc/binary, $->>);
m_b64_e(<<$/, Rest/binary>>, Acc) -> m_b64_e(Rest, <<Acc/binary, $_>>);
m_b64_e(<<$=, Rest/binary>>, Acc) -> m_b64_e(Rest, Acc);
m_b64_e(<<H,  Rest/binary>>, Acc) -> m_b64_e(Rest, <<Acc/binary, H>>).

ui_get_tables() ->
  Tables = get_tables(),
  show_table(Tables).

qlc_to_game_table(TN2,I,R,G,S,M,O,A,GP) ->
   #game_table{name = TN2, id = I, rounds = R, age_limit = A, game_type = G, 
               game_speed = S, game_mode = M, owner = O, game_process = GP}.

retrieve_tables(Setting, UId, GameType,Convert) ->
    Pid = spawn(matchmaker, process_tables, [Setting,UId,GameType,Convert]),
    Pid ! {self(), get},
    receive {Pid,Msg} -> Msg end.

process_tables(Setting, UId,GameType,Convert) ->
  receive
    {From, get} -> 
      Tables = nsm_queries:map_reduce(nsm_queries,get_single_tables,[Setting,UId,GameType,Convert]),
      Filtered = filter_tables(Tables,UId,GameType,Setting,Convert),
      From ! {self(), Filtered},stop end.

get_tables() -> get_tables(convert).

get_tables(Convert) -> 
    Setting = wf:session({wf:q(game_name),wf:user()}),
    UId = wf:user(),
    retrieve_tables(Setting, UId, wf:q(game_name), Convert).

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
            AllFilterOk = (FilterAllUsers==[]) or 
                (lists:usort( [lists:member(OFU, TableUsers) || OFU <- FilterAllUsers] ) == [true]),
            AnyFilterOk = (FilterAnyUser==[]) or 
                (lists:usort( [lists:member(OTU, FilterAnyUser) || OTU <- TableUsers] ) =/= [false]),
            AllFilterOk and AnyFilterOk
        end, QLC),

    FilteredQLC2 = lists:usort(fun (A, B) -> 
            A#game_table.id =< B#game_table.id
        end, FilteredQLC1),

    FilteredQLC3 = lists:sort(fun (A, B) ->
            A#game_table.timestamp =< B#game_table.timestamp
        end, FilteredQLC2),
   
    case Convert of convert -> convert_to_map(FilteredQLC3, Setting,UId,GameFSM); _ -> FilteredQLC3 end.

convert_to_map(Data,_Setting,UId,GameFSM) ->
    [ begin Url = lists:concat([?_U("/view-table/"),GameFSM,"/id/", TId]),
            Script = webutils:new_window_js(Url),
            Action = #event{type=click, actions=#script{script=Script}},
            UserOwner = UId == Owner,
            [ Name,
              Owner,
              {info, {table, TId}},
              {join, Action},
              ViewPerPoint,
              UserOwner,
              Users,
              {delete_table, TId, ProcId} ]
      end || #game_table{ name = Name,
                          id = TId,
                          rounds = _Rounds,
                          users = Users,
                          game_process = ProcId,
                          owner = _Owner,
                          creator = Owner} = _Tab <- Data ].



list_users(Users) -> [ " " ++ case User of robot -> "robot"; User -> User end ++ " " || User <- Users].
list_users_links(Users, Owner) ->
    [ " " ++ case User of robot -> "robot"; 
              Owner ->  
                  io_lib:format("<strong class=\"author\" style='font-size:14px;'><a href=\"~s\">~s</a></strong>",
                        [site_utils:user_link(User), User]);
              User -> 
                  io_lib:format("<strong class=\"author\"><a href=\"~s\">~s</a></strong>",
                        [site_utils:user_link(User), User])
              end ++ " " || User <- Users].

show_table(Tables) ->
    %% update i'm feeling lucky
    wf:update(play_button_panel, el_inside_play()),
    case Tables of
        [] ->
            #panel{style="text-align: center", body=#h4{text=?_T("You can create a game or join a game")} };
        _ ->
            #table{class="view_table_table article-table", style="width:100%", rows=[
                begin
                    {info, {_, TId}} = InfoPostback,
                    {ok, WholeTable} = view_table:get_table(TId),
                    MaxUsers = case wf:q(game_name) of 
                        "tavla" -> case WholeTable#game_table.tournament_type of
                            paired -> 10;
                            paired_lobby -> 10;
                            _ -> 2
                        end;
                        "okey" -> 4 
                    end,
                    RealUsers = case wf:q(game_name) of 
                        "tavla" -> case WholeTable#game_table.tournament_type of
                            paired -> WholeTable#game_table.users;
                            _ -> Users
                        end;
                        "okey" -> Users 
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
                        {info, _} ->
                            #link{id=showInfo,
                                postback=InfoPostback,
                                text=?_T("Info")
                            };
                        _ -> []
                    end,
                    JoinOrCrate = case Action of
                        {join, Act} ->
                            case length(RealUsers) of
                                MaxUsers -> "";
                                _ ->
                                    #link{id=joinTable,
                                        actions=Act,
                                        text=?_T("Join"),
                                        class="join-button"
                                    }
                            end;
                        {create, Act} ->
                            #link{id=joinTable,
                                actions=Act,
                                text=?_T("Create")
                            };
                        _ -> []
                    end,
                    DeleteTable = #link{id=deleteTable,
                        postback=DeleteAction,
                        show_if=UserOwner,
                        actions=RemoveActions,
                        text=?_T("Remove")
                    },

                    Buttons = #list{style="float:right;", body=[
                        #listitem{body=X} || X <- 
                            [#image{image="/images/free.png"} || _N <- lists:seq(1,MaxUsers-length(RealUsers))] ++ 
                            [Info, JoinOrCrate, DeleteTable]
                    ]},
                    #tablerow{id=RowId, cells=[
                        #tablecell{ class=cell1,
                            body=[
                                TMode ++ ", " ++ TSpeed ++ TRoundsOrNot ++ TDoubleOrNot ++ 
                                " (" ++ list_users_links(RealUsers, OwnerLabel) ++ ") "
                            ],
                            id=tableNameLabel
                        },
                        #tablecell{ class=cell3,
                            body = [
                                "<nobr>", Buttons, "</nobr>"
                            ]
                        }
                    ]}
                end
                || [_TableNameLabel,
                    OwnerLabel,
                    InfoPostback,
                    Action,
                    ViewPerPoint,
                    UserOwner,
                    Users,
                    DeleteAction] <- Tables
            ]}
    end.

check_required(Setting) ->
    Required = case proplists:get_value(game_mode, Setting) of
        countdown ->
            [table_name, game, game_mode, speed];
        _ ->
            [table_name, game, game_mode, speed, rounds]
    end,
    Check = [case proplists:get_value(Req, Setting) of
		 undefined -> false;
		 {multiple, _} -> false;
		 _ -> true
	     end || Req <- Required ],
%    Rounds = wf:to_integer(case proplists:get_value(rounds, Setting, 0) of
%				{multiple, _} -> 0;
%				R -> R
%			   end),
%    Sets = 0,
    case lists:usort(Check) of
 	[true] ->
	    %case site_utils:table_per_user_point(wf:user(), Sets, Rounds) of
	%	true ->
		    wf:remove(point_info),
		    wf:wire(create_button, #remove_class { class=disable });
	%	false ->
	%	    wf:flash(point_info, ?_T("You don't have enough points to play!")),
	%	    wf:wire(create_button, #add_class { class=disable })
	%    end;
	_ ->
	    wf:remove(point_info),
	    wf:wire(create_button, #add_class { class=disable })
    end.


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
    ].

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
    Groups = wf:state(user_in_groups),
    View = [
            begin
                {ok, Group} = nsm_groups:get_group(GId),
                Name = Group#group.name,
                #listitem{body=construct_id(#link{text=Name, postback={tag,{group, Name}}})}
            end
	     || GId <- Groups ],
    #panel{class="create-game-groups-box",       
	   body=[
         "<span id='guidersgroupfiltername'>",
         #textbox{id=groups_filter_name,
			  actions=js_options_filter(groups_list),
			  class="create-game-groups-filter-textbox"},
		 #panel{class="create-game-groups-list",
			id=groups_list,
			body=#list{class=ThisClass, body=View}},
         "</span>"
         ]
	  }.

tab_friend_setting() ->
    ThisClass = case wf:state(buttons) of
        green -> "list1_green";
        _ -> "list1"
    end,
    Users = wf:state(users_subscribe),
    View = [ #listitem{body=construct_id(#link{text=Name, postback={tag,{users, Name}}})}
	     || #subs{whom = Name} <- Users ],
    #panel{class="create-game-friends-box",
	   body=[#textbox{id=friends_filter_name,
			  actions=js_options_filter(friends_list),
			  class="create-game-friends-filter-textbox"},
		 #panel{class="create-game-friends-list",
			id=friends_list,
			body=#list{class=ThisClass, body=View}}]
	  }.

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
	objs('"++OptionsPanelId++"').find('a').each(function() {
		console.log(this);
		var $this = $(this);
		if ($this.text().search(filter) < 0) {
			$this.parent().hide()
		} else {
			$this.parent().show()
		}
	})
    });";
js_options_filter(OptionsPanelId) ->
    js_options_filter(wf:to_list(OptionsPanelId)).

ui_update_buttons() ->
    Settings = wf:session({wf:q(game_name),wf:user()}),
    ui_update_buttons(Settings).

ui_update_buttons(Settings) ->
    wf:update(criteria_field, ""),
    [begin
        case Setting of
            {Key, {multiple, Elems}} ->
                [ ui_button_select({Key, V}) || V <- Elems];

            {Key, Elem} ->
                ui_button_select({Key, Elem})
        end,
        %% check if need to make some depended changes
        check_depended(Setting)
    end || Setting <- Settings ],

    check_required(Settings),
    ok.

ui_button_select({game, _}) ->
    ignore;
ui_button_select({table_name, Value}) ->
    wf:set(table_name, Value);
ui_button_select({Key, Value}) ->
    Id = construct_id({Key, Value}),
    SpanElement =
	case Key of
	    age ->
		Format = slider_text_format(age),
		Text = wf:f(Format, [ wf:to_list(X) || X <- Value ]),
		#span{text=Text};
	    _ ->
		#span{actions="var e=objs('"++Id++"');"
		      "objs('me').text( e.text() ? e.text() : e.attr('value') )"}
	end,
    CriteriaElement =
	#listitem{id="for_"++Id, class="for_"++wf:to_list(Key),
	       body=["<em>", SpanElement,
	             #link{text="X", postback={tag, {Key, Value}}},
	            "</em>"]},
    wf:insert_bottom(criteria_field, CriteriaElement), %% criteria_box
    case is_checkbox(Key) of
	true ->
	    wf:wire(#attr{target=Id, attr="checked", value="checked"});
	_ when Key == age ->
	    case Value of
		[Min, Max] ->
		wf:wire(wf:f("$(\".wfid_~s\").slider(\"values\", [~b, ~b]);",
			[age_slider, wf:to_integer(Min), wf:to_integer(Max)]));
		_ -> ignore
	    end;
	_ ->
	    JSId = wf:js_escape(wf:to_list(Id)),
	    wf:wire("objs('"++JSId++"').parent('li').addClass('active');")
    end.

ui_button_deselect({Key, Value}) ->
    Id = construct_id({Key, Value}),
    wf:remove("for_"++Id), %% criteria_box
    JSId = wf:js_escape(wf:to_list(Id)),
    case is_checkbox(Key) of
	true ->
	    wf:wire("objs('"++JSId++"').attr('checked', false);");
	_ ->
	    wf:wire("objs('"++JSId++"').parent('li').removeClass('active');")
    end.


is_checkbox(Key) ->
    case wf:state(user_paid) of
        true ->
            lists:member(Key, [paired_game, deny_robots, private, slang, deny_observers, paid]);
        false ->
            lists:member(Key, [paired_game, deny_robots, private, slang, deny_observers])
    end.

is_option_present(Key, Value) ->
    Settings = wf:session({wf:q(game_name), wf:user()}),
    proplists:is_defined(Key, Settings) andalso Value == proplists:get_value(Key, Settings).


comet_update() -> comet_update(empty).
comet_update(State) ->
  case wf:user() of
    undefined -> webutils:redirect_to_ssl("login");
    _UId -> timer:sleep(?TABLE_UPDATE_QUANTUM),
            X = rpc:call(?GAMESRVR_NODE,game_manager,get_lucky_table,[list_to_atom("game_"++?_U(wf:q(game_name)))]),
            wf:state(lucky,X),
            garbage_collect(self()),
            Tables = get_tables(),
            wf:update(tables, show_table(Tables)),
            ui_paginate(),
            wf:flush(),
            comet_update(State)

  end.

%% We should leave that in case when we want that number of options can be selected in same time.
can_be_multiple(age)   -> false;
can_be_multiple(users) -> true;
can_be_multiple(_Key)  -> false.

api_event(Name, Tag, Args) ->
  fb_utils:api_event(Name, Tag, Args).

event(Event) ->
  case wf:user() of
    undefined -> webutils:redirect_to_ssl("login");
    _User -> u_event(Event)
  end.

ac_show_main_container() ->
    wf:wire("objs('matchmaker_main_container').show();"),
    wf:wire("objs('matchmaker_slide_area').show();").

ac_hide_main_container() ->
    #event{type=click, actions="objs('matchmaker_main_container').hide();"}.

u_event({show,create_game}) ->
    u_event(clear_selection),
    wf:state(buttons, yellow),
    ?INFO("u_event create_game"),
    wf:update(matchmaker_main_container, matchmaker_show_create(create)),
    wf:update(matchmaker_slide_area, settings_box()),
    wf:wire("objs('tabs').tabs()"),
    ac_show_main_container(),
    ui_update_buttons();

u_event({show,join_game}) ->
    u_event(clear_selection),
    wf:state(buttons, green),
    ?INFO("u_event join_game"),
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
    TableName =
	case wf:q(table_name) of
	     undefined -> table_name(default);
	     String ->
		case string:strip(String) of
		    "" -> table_name(default);
		    TN -> TN
		end
	end,
    process_setting({table_name, TableName});

u_event({tag, {_Key, _Value} = Setting}) ->
    check_depended(Setting),
    process_setting(Setting);

u_event(create_game) ->
    UId = wf:user(),
    Settings = wf:session({wf:q(game_name), wf:user()}),
    wf:session({wf:q(game_name), UId},Settings),
    URL = ?_U(lists:concat(["/matchmaker/", wf:q(game_name)])),
    wf:redirect(URL),

    Desc = lists:flatten( URL ++ "|" ++ UId ++ "|" ++ proplists:get_value(table_name,Settings) ++ "|" ++ wf:q(game_name) ++ "|" ++
        integer_to_list(proplists:get_value(rounds,Settings)) ++ "|" ++ atom_to_list(proplists:get_value(speed,Settings)) ++ "|" ++
        atom_to_list(proplists:get_value(game_mode,Settings))),
    webutils:post_user_system_message(Desc);


u_event({info, {Target, TId}}) ->
    {ok, TableSettings} = case Target of
        table ->
            {ok, Table} = view_table:get_table(TId),
            ?INFO("INFO: ~p",[{TId,Table}]),
            {ok,Table};
        save_table -> table_manager:get_save_table_setting(TId)
    end,
    Info = webutils:table_info(TableSettings),
    wf:update(info_table, #dialog{body=Info});

u_event({delete_table, TId, ProcId}) ->
    ?INFO("delete table: ~p",[{delete_table, TId, ProcId}]),
%    case ProcId of undefined -> ok; _ -> ProcId ! {unreg, {p,l,ProcId}} end,
    ?INFO(" *** delete"),
    exit(ProcId, {shutdown,and_forget});
%    table_manager:delete_table(TId);

u_event({delete_saved_table, TId}) ->
    ?INFO(" *** delete saved");
%    table_manager:delete_save_table(TId);

u_event({tab_selected, ID}) ->
    ?INFO("u_event"),
    show_tab_guiders(ID);

u_event(clear_selection) ->
    OldSettings = wf:session({wf:q(game_name), wf:user()}),
    [
        u_event({tag, lists:nth(N, OldSettings)})
        || N <- lists:seq(3, length(OldSettings))
    ];

u_event(show_game_rules) ->
    case wf:q(game_name) of
        "okey" ->
            wf:update(rules_container, matchmaker_rules:okey_rules());
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

u_event(hide_rules) ->
%    wf:wire(simple_lightbox, #hide{});
    wf:update(rules_container, []);

u_event(Other) ->
    ?INFO("u_event other: ~p",[Other]),
    webutils:event(Other).

process_setting({Key, Value} = Setting) ->
    OldSettings = wf:session({wf:q(game_name), wf:user()}),

    OldValues =
	case proplists:get_value(Key, OldSettings) of
	    undefined -> [];
	    {multiple, ValueList} -> ValueList;
	    OldValue -> [OldValue]
	end,

    NewValues =
	case lists:member(Value, OldValues) of
	    false -> %% new value
		ui_button_select(Setting),
		case can_be_multiple(Key) of
		    true -> [Value | OldValues];
		    false ->
			[ ui_button_deselect({Key, V}) || V <- OldValues ],
			[Value]
		end;
	    true -> %% old value
		ui_button_deselect(Setting),
		lists:delete(Value, OldValues)
	end,

    NewSettings =
	case NewValues of
	    [] ->
		proplists:delete(Key, OldSettings);
	    List ->
		NewValue =
		    case List of
			[Elem] -> Elem;
			_ -> {multiple, List}
		    end,
		lists:keystore(Key, 1, OldSettings, {Key, NewValue})
	end,

    wf:session({wf:q(game_name), wf:user()}, NewSettings),
    check_required(NewSettings),
    ?INFO("Update setting: ~p~n~n", [NewSettings]),
    wf:session(time_left_to_update, ?TABLE_UPDATE_QUANTUM),
    ok.

% text from atoms. I want in to be here, though it is not used, for not to get these things scattered all over the code
game_mode_to_text(Type) when is_atom(Type) ->
    game_mode_to_text(atom_to_list(Type));
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

game_speed_to_text(Speed) when is_atom(Speed) ->
    game_speed_to_text(atom_to_list(Speed));
game_speed_to_text(Speed) ->
    case Speed of 
        "fast" -> ?_T("Fast");
        "normal" -> ?_T("Normal");
        "slow" -> ?_T("Slow");
        _ -> ?_T("Unknown")
    end.

% guiders scripts for matchmaker. I had to separate them into linear part and tab part.
split(String, Separator) ->
    Pos = string:str(String, Separator),
    case Pos of
        0 ->
            [String];
        _ ->
            [string:left(String, Pos-1)] ++ split(string:right(String, length(String) - length(Separator) - Pos + 1), Separator)
    end.

join(String, Separator) ->
    string:join(String, Separator).

replace(String, Dirt, Icecream) ->
    join(split(String, Dirt), Icecream).

s_T(String) ->
    replace(replace(?_T(String), "\"", "\\\""), "\'", "\\\'"). 

guiders_script() ->
    wf:wire("
        guiders.createGuider({
            buttons: [
                {name: '"++s_T("No, thanks")++"', onclick: guiders.hideAll},
                {name: '"++s_T("Continue")++"', onclick: guiders.next}
            ],
            description: '"++s_T("Welcome to matchmaker notice.")++"',
            id: 'guider_10',
            next: 'guider_20',
            overlay: true,
            title: '"++s_T("Welcome to Kakaranet matchmaker")++"'
        }).show();

        guiders.createGuider({
            attachTo: '#guiderscreateblock',
            position: 3,
            buttons: [
                {name: '"++s_T("<< Back")++"', onclick: guiders.prev},
                {name: '"++s_T("Continue")++"', onclick: guiders.next}
            ],
            description: '"++s_T("Create description.")++"',
            id: 'guider_20',
            next: 'guider_30',
            overlay: false,
            xButton: true,
            title: '"++s_T("Create")++"'
        });

        guiders.createGuider({
            attachTo: '#guidersjoinblock',
            position: 3,
            buttons: [
                {name: '"++s_T("<< Back")++"', onclick: guiders.prev},
                {name: '"++s_T("Continue")++"', onclick: guiders.next}
            ],
            description: '"++s_T("Join description.")++"',
            id: 'guider_30',
            next: 'guider_40',
            overlay: false,
            xButton: true,
            title: '"++s_T("Join")++"'
        });

        guiders.createGuider({
            attachTo: '#guidersplayblock',
            position: 9,
            buttons: [
                {name: '"++s_T("<< Back")++"', onclick: guiders.prev},
                {name: '"++s_T("Continue")++"', onclick: guiders.next}
            ],
            description: '"++s_T("Play description.")++"',
            id: 'guider_40',
            next: 'guider_50',
            overlay: false,
            xButton: true,
            title: '"++s_T("Play")++"'
        });

        guiders.createGuider({
            attachTo: '#guidersitem1',
            position: 12,
            buttons: [
                {name: '"++s_T("<< Back")++"', onclick: guiders.prev},
                {name: '"++s_T("Continue")++"', onclick: guiders.next}
            ],
            description: '"++s_T("Game speed description.")++"',
            id: 'guider_50',
            next: 'guider_60',
            overlay: false,
            xButton: true,
            title: '"++s_T("Game Speed")++"'
        });

        guiders.createGuider({
            attachTo: '#guidersitem2',
            position: 12,
            buttons: [
                {name: '"++s_T("<< Back")++"', onclick: guiders.prev},
                {name: '"++s_T("Continue")++"', onclick: guiders.next}
            ],
            description: '"++s_T("Game type description.")++"',
            id: 'guider_60',
            next: 'guider_70',
            overlay: false,
            xButton: true,
            title: '"++s_T("Game Type")++"'
        });

        guiders.createGuider({
            attachTo: '#guidersitem3',
            position: 12,
            buttons: [
                {name: '"++s_T("<< Back")++"', onclick: guiders.prev},
                {name: '"++s_T("Continue")++"', onclick: guiders.next}
            ],
            description: '"++s_T("Rounds description.")++"',
            id: 'guider_70',
            next: 'guider_80',
            overlay: false,
            xButton: true,
            title: '"++s_T("Rounds")++"'
        });

        guiders.createGuider({
            attachTo: '#guidersitem4',
            position: 12,
            buttons: [
                {name: '"++s_T("<< Back")++"', onclick: guiders.prev},
                {name: '"++s_T("Continue")++"', onclick: guiders.next}
            ],
            description: '"++s_T("Attributes description.")++"',
            id: 'guider_80',
            next: 'guider_100',
            overlay: false,
            xButton: true,
            title: '"++s_T("Attributes")++"'
        });

        guiders.createGuider({
            attachTo: '#guidersdetailedsettings',
            buttons: [
                {name: '"++s_T("<< Back")++"', onclick: guiders.prev}
            ],
            position: 9,
            description: '"++s_T("Detailed settings description.")++"',
            id: 'guider_100',
            next: 'guider_110',
            overlay: false,
            xButton: true,
            title: '"++s_T("Detailed settings")++"'
        });
    ").

add_game_settings_guiders() ->
    case webutils:guiders_ok("matchmaker_tab_1_guiders_shown") of
        false ->
            "";
        true ->
        "<script>
        guiders.hideAll();
        ////////// detailed settings tab 1
        guiders.createGuider({
            attachTo: '#guiderscriteria',
            buttons: [{name: '"++s_T("Continue")++"', onclick: guiders.next}],
            position: 12,
            description: '"++s_T("Criteria description")++"',
            id: 'guider_110',
            next: 'guider_130',
            overlay: false,
            xButton: true,
            title: '"++s_T("Criteria")++"'
        }).show();

        guiders.createGuider({  // do we want it here?
            attachTo: '#guiderstab1createbutton',
            buttons: [
                {name: '"++s_T("<< Back")++"', onclick: guiders.prev},
                {name: '"++s_T("Continue")++"', onclick: guiders.next}
            ],
            position: 12,
            description: '"++s_T("Detailed settings create description")++"',
            id: 'guider_120',
            next: 'guider_130',
            overlay: false,
            xButton: true,
            title: '"++s_T("Detailed settings create")++"'
        });

        guiders.createGuider({
            attachTo: '#guiderstab1tablename',
            buttons: [
                {name: '"++s_T("<< Back")++"', onclick: guiders.prev},
                {name: '"++s_T("Continue")++"', onclick: guiders.next}
            ],
            position: 11,
            description: '"++s_T("Table name description")++"',
            id: 'guider_130',
            next: 'guider_140',
            overlay: false,
            xButton: true,
            title: '"++s_T("Table name")++"'
        });

        guiders.createGuider({
            attachTo: '#guiderstab1set',
            buttons: [
                {name: '"++s_T("<< Back")++"', onclick: guiders.prev},
                {name: '"++s_T("Continue")++"', onclick: guiders.next}
            ],
            position: 1,
            description: '"++s_T("Table name set description")++"',
            id: 'guider_140',
            next: 'guider_150',
            overlay: false,
            xButton: true,
            title: '"++s_T("Table name set")++"'
        });

        guiders.createGuider({
            attachTo: '#guiderstab1gamespeed',
            buttons: [
                {name: '"++s_T("<< Back")++"', onclick: guiders.prev},
                {name: '"++s_T("Continue")++"', onclick: guiders.next}
            ],
            position: 11,
            description: '"++s_T("Detailed game speed description")++"',
            id: 'guider_150',
            next: 'guider_160',
            overlay: false,
            xButton: true,
            title: '"++s_T("Detailed game speed")++"'
        });

        guiders.createGuider({
            attachTo: '#guiderstab1gametype',
            buttons: [
                {name: '"++s_T("<< Back")++"', onclick: guiders.prev},
                {name: '"++s_T("Continue")++"', onclick: guiders.next}
            ],
            position: 11,
            description: '"++s_T("Detailed game type description")++"',
            id: 'guider_160',
            next: 'guider_170',
            overlay: false,
            xButton: true,
            title: '"++s_T("Detailed game type")++"'
        });

        guiders.createGuider({
            attachTo: '#guiderstab1paired',
            buttons: [
                {name: '"++s_T("<< Back")++"', onclick: guiders.prev},
                {name: '"++s_T("Continue")++"', onclick: guiders.next}
            ],
            position: 12,
            description: '"++s_T("Detailed paired description")++"',
            id: 'guider_170',
            next: 'guider_180',
            overlay: false,
            xButton: true,
            title: '"++s_T("Detailed paired")++"'
        });

        guiders.createGuider({
            attachTo: '#guiderstab1rounds',
            buttons: [
                {name: '"++s_T("<< Back")++"', onclick: guiders.prev},
                {name: '"++s_T("Continue")++"', onclick: guiders.next}
            ],
            position: 11,
            description: '"++s_T("Detailed rounds description")++"',
            id: 'guider_180',
            next: 'guider_185',
            overlay: false,
            xButton: true,
            title: '"++s_T("Detailed rounds")++"'
        });

        guiders.createGuider({
            attachTo: '#guiderstab1double',
            buttons: [
                {name: '"++s_T("<< Back")++"', onclick: guiders.prev},
                {name: '"++s_T("Continue")++"', onclick: guiders.next}
            ],
            position: 11,
            description: '"++s_T("Double quota description")++"',
            id: 'guider_185',
            next: 'guider_190',
            overlay: false,
            xButton: true,
            title: '"++s_T("Double quota")++"'
        });

        guiders.createGuider({
            attachTo: '#guiderstab1additional',
            buttons: [
                {name: '"++s_T("<< Back")++"', onclick: guiders.prev},
                {name: '"++s_T("Continue")++"', onclick: guiders.next}
            ],
            position: 11,
            description: '"++s_T("Additional options description")++"',
            id: 'guider_190',
            next: 'guider_195',
            overlay: false,
            xButton: true,
            title: '"++s_T("Additional options")++"'
        });

        guiders.createGuider({
            attachTo: '#guiderstab1hide',
            buttons: [
                {name: '"++s_T("<< Back")++"', onclick: guiders.prev},
                {name: '"++s_T("Ok")++"', onclick: guiders.hideAll}
            ],
            position: 1,
            description: '"++s_T("Hide button description")++"',
            id: 'guider_195',
            next: 'guider_199',
            overlay: false,
            xButton: true,
            title: '"++s_T("Hide button options")++"'
        });

        </script>"
    end.



show_tab_guiders(ID) ->
    case ID of
        "tab_2" ->
            case webutils:guiders_ok("matchmaker_tab_2_guiders_shown") of
                false ->
                    ok;
                true ->
                    wf:wire("                        
                        guiders.hideAll;
                        guiders.createGuider({
                            attachTo: '#guidersgroupfiltername',    // this wouldn't work
                            buttons: [
                                {name: '"++s_T("Ok")++"', onclick: guiders.hideAll},
                            ],
                            description: '"++s_T("Group settings description.")++"',
                            id: 'guider_200',
                            next: 'guider_210',
                            overlay: true,
                            title: '"++s_T("Group Settings")++"'
                        }).show();
                    ")
            end;
        "tab_3" ->
            case webutils:guiders_ok("matchmaker_tab_3_guiders_shown") of
                false ->
                    ok;
                true ->
                    wf:wire("
                        guiders.hideAll;
                        guiders.createGuider({
                            buttons: [
                                {name: '"++s_T("Ok")++"', onclick: guiders.hideAll},
                            ],
                            description: '"++s_T("Friend settings description.")++"',
                            id: 'guider_300',
                            next: 'guider_310',
                            overlay: true,
                            title: '"++s_T("Friend Settings")++"'
                        }).show();
                    ")
            end;
        "tab_4" ->
            case webutils:guiders_ok("matchmaker_tab_4_guiders_shown") of
                false ->
                    ok;
                true ->
                    wf:wire("
                        guiders.hideAll;
                        guiders.createGuider({
                            buttons: [
                                {name: '"++s_T("Ok")++"', onclick: guiders.hideAll},
                            ],
                            description: '"++s_T("Personal settings description.")++"',
                            id: 'guider_400',
                            next: 'guider_410',
                            overlay: true,
                            title: '"++s_T("Personal Settings")++"'
                        }).show();
                    ")
            end;
        _ ->
            ok
    end.
