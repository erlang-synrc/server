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

-define(TABLE_UPDATE_INTERVAL, 5000).
-define(TABLE_UPDATE_QUANTUM, 100).

route() -> ["game_name"].

main() ->
    wf:state(buttons, green),
    case wf:user() /= undefined of
        true  -> main_authorized0();
        false -> wf:redirect_to_login(?_U("/login"))
    end.

main_authorized0() ->
    webutils:add_script("/nitrogen/jquery.paginatetable.js"),
    webutils:add_raw("
        <link href='/nitrogen/guiders-js/guiders-1.2.8.css' rel='stylesheet'>
        <script src='/nitrogen/guiders-js/guiders-1.2.8.js'></script>
    "),
    case wf:q(style) of
	"fb" ->  #template { file=code:priv_dir(nsw_srv)++"/templates/facebook.html" };
	_ ->     #template { file=code:priv_dir(nsw_srv)++"/templates/bare.html" }
    end.

title() -> ?_T("Matchmaker").

body() ->
    case (catch check_requirements()) of
	ok ->
	    try
		Settings = wf:session({q_game_type(),wf:user()}),
		ui_update_buttons(case Settings of undefined -> 
			case q_game_type() of
			    "tavla" ->
				[{game, game_tavla}];%, {rounds, 3}, {speed,normal}, {game_mode, standard}];
			    "okey" ->
				[{game, game_okey}]%, {rounds, 20}, {speed,normal}, {game_mode, standard}]
			end ++ [{table_name, table_name(default)}]; _ -> Settings end),
 		case proplists:get_value(game, Settings) of
		   game_okey   -> ok;
		   game_tavla  -> ok
		end,
                wf:session({q_game_type(),wf:user()}, Settings)
	    catch
		error:E when E==function_clause orelse element(1, E)==badmatch ->
		    %% reset to default settings if it's not set or wrong
                    FromCookies = wf:session({q_game_type(),wf:user()}),
                    ?INFO("FromCookies: ~p",[FromCookies]),
		    DefaultSettings = case FromCookies of
                        undefined ->
			case q_game_type() of
			    "tavla" ->
				[{game, game_tavla}];%, {rounds, 3}, {speed,normal}, {game_mode, standard}];
			    "okey" ->
				[{game, game_okey}]%, {rounds, 20}, {speed,normal}, {game_mode, standard}]
			end ++ [{table_name, table_name(default)}];
                       _ -> FromCookies
                    end,
		    wf:session({q_game_type(),wf:user()}, DefaultSettings),
		    ui_update_buttons()
	    end,
	    UId = webutils:user_info(username), 
	    wf:state(user_in_groups, rpc:call(?APPSERVER_NODE,nsm_groups,list_group_per_user,[UId])),
	    wf:state(users_subscribe, rpc:call(?APPSERVER_NODE,nsm_users,list_subscr,[UId])), 
	    main_authorized();
	{redirect, login} ->
	    wf:redirect_to_login("/");
	{redirect, Url} ->
	    wf:redirect(Url);
	_ -> ""
    end.

table_name(default) ->
    UId = wf:user(),
    {Date,_} = calendar:now_to_local_time(now()),
    Time = site_utils:date_to_text(Date),
    TableName = ?_TS("$username$ table, $date$ ", [{username, UId}, {date, Time}]),
    lists:flatten(TableName).

q_game_type() ->
    wf:q(game_name).

check_requirements() ->
    case wf:user() /= undefined of
	true  -> ok;
	false -> throw({redirect, login})
    end,
    case q_game_type() of
	"okey" -> ok;
	"tavla" -> ok;
	Other ->
	    ?WARNING("Hacking attempt? game_type=~p\n", [Other]),
	    throw({redirect, ?_U("/dashboard")})
    end,
    case wf:q(csid) of
	undefined ->
	    {_, _, C} = now(),
	    throw({redirect, lists:concat([?_U("/matchmaker"), "/", ?_U(q_game_type()), "/csid/", C,
					   case wf:q(style) of "fb" -> "/style/fb";_->"" end])});
	Sid ->
	    wf:state(session_id, Sid)
    end,
    ok.

main_authorized() ->
    Tables = #panel{id=tables, body=ui_get_tables()},
    Pager = "<div class='matchmaker-table-pager paging'><div class=\"center\">"
	"<ul><li><a href=\"#\" class=\"prevPage\">&lt;</a></li></ul>"
	"<ul class='pageNumbers'></ul>"
	"<ul><li><a href=\"#\" class=\"nextPage\">&gt;</a></li>"
	"</ul></div></div>",
    ui_paginate(),

    wf:comet(fun() -> comet_update() end),
    % guiders
    case webutils:guiders_ok("matchmaker_guiders_shown") of
        true ->
            guiders_script();
        false ->
            "" 
    end,
    [
        #section{class="create-area", body=#section{class="create-block",
            body=[
                matchmaker_submenu(),
                #article{class="article1",
                    body=[
                        #panel{id=matchmaker_main_container, class="head", body=matchmaker_show_tables()},
                        #panel{id=matchmaker_slide_area, class="slide-area"},
                        Tables
                ]},
                Pager,
                view_table_box()
    ]}}].

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
   %%    GameId = ensure_lucky_game(Game),
%%     UId = wf:user(),
%%     Settings = wf:session({q_game_type(),wf:user()}),
%%     Game = proplists:get_value(game, Settings),
%%     LTs = get_lucky_tables2(UId, Game),
%%     Table =
%%         case connect_to_lucky(LTs) of
%%             {ok, T} ->
%%                 T;
%%             error ->
%%                 Params
%%                 T = create_lucky_table(),
%%                 
%%         end,

    RealTableId = "1000001",
    URL = lists:concat([?_U("/client"),"/",q_game_type(),"/id/", RealTableId]), 
    LuckyAction = #event{type=click, actions=webutils:new_window_js(URL)},
    B = #span{style="font-weight:bold"},
    [
        "<span id='guidersplayblock'>",
        #link{postback=lucky_play_button, text=?_T("PLAY"), actions=LuckyAction},
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
                    create -> #h2{text=q_game_type() ++ " " ++ ?_T("Selected Option")};
                    _ -> #h2{text=q_game_type() ++ " " ++?_T("Selected Option")}
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
	   case q_game_type() of
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
	case q_game_type() of
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
    Checkboxes = [
        "<span id='guidersitem4'>",
        "<span id='guiderstab1paired'>",
        "</span>",
        construct_id(#checkbox{class="chk", postback={tag,{paired_game,true}},
            text=?_T("Paired"), value=?_T("Paired")}),
        case q_game_type() of
            okey ->
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
					 value=?_T("No observers"), text=?_T("I don't accept observers")})
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
			       actions=ac_hide_main_container(), class="cancel"},
                 "</span>"
		       ]}
		  ],
    [
        #singlerow { cells=[
            #tablecell { 
                body=#h2{text=q_game_type() ++ " " ++ ?_T("Selected Option")}
            },
            #tablecell { 
                body=#link{text=?_T("Game Rules"), class="matchmaker_game_rules", postback=show_game_rules}
            }
        ]},
        #panel{class="items", body=TableFilter}
    ].


el_create_game_button() ->

    Url = lists:concat([?_U("/view-table/"), ?_U(q_game_type()),"/id/",wf:state(session_id)]),
    Settings = wf:session({q_game_type(),wf:user()}),
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
    receive {From, get} -> From ! {self(),matchmaker:get_tables2(Setting,UId,GameType,Convert)},stop end.

get_tables() -> get_tables(convert).

get_tables(Convert) -> 
    Setting = wf:session({q_game_type(),wf:user()}),
    UId = wf:user(),
    retrieve_tables(Setting,UId,q_game_type(),Convert).

get_tables2(Setting,UId,GameFSM,Convert) ->

    GetPropList = fun(Key,Setngs) -> 
                   case Setngs of
                        undefined -> undefined;
                        _Else -> proplists:get_value(Key, Setngs)
                   end end,

    Rounds = GetPropList(rounds, Setting),
    GameType = GetPropList(game_mode, Setting),
    Speed = GetPropList(speed, Setting),
    Game = GetPropList(game, Setting),
    Lucky = false,

    FilterAllUsers = case GetPropList(users, Setting) of
        undefined -> [];
        {multiple, ManyUsers} -> ManyUsers;
        SingleUser -> [SingleUser]
    end,

    FilterAnyUser = case GetPropList(group, Setting) of
        undefined -> [];
        GroupId -> 
            [GMR#group_member_rev.who || GMR <- rpc:call(?APPSERVER_NODE,nsm_groups,list_group_membership,[GroupId])]
    end,

    MaxUsers = case GameFSM of "tavla" -> 2; "okey" -> 4 end,

    Check = fun(Param,Value) -> 
                   case Param of
                        undefined -> true;
                        _Else -> Param == Value
                   end end,

    Cursor = fun(Id,FilterFree,FilterUser) ->
                qlc:cursor(qlc:q([V || {{_,_,_K},_,V=#game_table{creator=C,
                                                   rounds=R, game_type=G,
                                                   users=U, game_speed=S,
                                                   game_mode=GT,
                                                   feel_lucky = L}} <- gproc:table(props),
                           FilterFree(MaxUsers - length(U)),
                           FilterUser(C,Id),
                           Check(Game,G),
                           Check(Speed,S),
                           Check(GameType,GT),
                           Check(Rounds,R),
                           Check(Lucky, L)])
                )
    end,
    OneAvailable   = fun(N) -> N == 1 end,
    TwoAvailable   = fun(N) -> N == 2 end,
    ThreeAvailable = fun(N) -> N == 3 end,
    MoreAvailable  = fun(N) -> N > 3 end,
    NotAvailable   = fun(N) -> N == 0 end,
    Others         = fun(IterUser,CurrentUser) -> IterUser =/= CurrentUser end,
    Own            = fun(IterUser,CurrentUser) -> IterUser == CurrentUser end,

    OneLeftListOther = qlc:next_answers(Cursor(UId, OneAvailable, Others), 10),
    OneLeftListOwn = qlc:next_answers(Cursor(UId, OneAvailable, Own), 10),
    TwoLeftListOther = qlc:next_answers(Cursor(UId, TwoAvailable, Others), 10),
    TwoLeftListOwn = qlc:next_answers(Cursor(UId, TwoAvailable, Own), 10),
    ThreeLeftListOther = qlc:next_answers(Cursor(UId, ThreeAvailable, Others), 10),
    ThreeLeftListOwn = qlc:next_answers(Cursor(UId, ThreeAvailable, Own), 10),
    MoreLeftListOther = qlc:next_answers(Cursor(UId, MoreAvailable, Others), 10),
    MoreLeftListOwn = qlc:next_answers(Cursor(UId, MoreAvailable, Own), 10),
    NoMoreLeftListOther = qlc:next_answers(Cursor(UId, NotAvailable, Others), 50),
    NoMoreLeftListOwn = qlc:next_answers(Cursor(UId, NotAvailable, Own), 10),

%%     OneLeftList = qlc:next_answers(qlc:cursor(qlc:q([V || {{_,_,_K},_,V=#game_table{creator=C,
%%                                                    rounds=R, game_type=G, owner=O,
%%                                                    users=U, game_speed=S,
%%                                                    game_mode=GT, feel_lucky = L}}
%%                     <- gproc:table(props),
%%                    length(U) == (MaxUsers-1), C =/= UId,
%%                    Check(C,O),
%%                    Check(Game,G),
%%                    Check(Speed,S),
%%                    Check(GameType,GT),
%%                    Check(Lucky,L),
%%                    Check(Rounds,R)
%%                   ])), 10),
%%
%%     TwoLeftList = case GameFSM of
%%          "tavla" -> [];
%%          "okey" -> qlc:next_answers(qlc:cursor(qlc:q([V || {{_,_,_K},_,V=#game_table{creator=C,
%%                                                    rounds=R, game_type=G, owner=O,
%%                                                    users=U, game_speed=S,
%%                                                    game_mode=GT, feel_lucky = L}}
%%                     <- gproc:table(props),
%%                    length(U) == (MaxUsers-2), C =/= UId,
%%                    Check(C,O),
%%                    Check(Game,G),
%%                    Check(Speed,S),
%%                    Check(GameType,GT),
%%                    Check(Lucky,L),
%%                    Check(Rounds,R)])),10)
%%     end,
%%
%%     ThreeLeftList = case GameFSM of
%%          "tavla" -> [];
%%          "okey" -> qlc:next_answers(qlc:cursor(qlc:q([V || {{_,_,_K},_,V=#game_table{creator=C,
%%                                                    rounds=R, game_type=G, owner=O,
%%                                                    users=U, game_speed=S,
%%                                                    game_mode=GT, feel_lucky = L}}
%%                    <- gproc:table(props),
%%                    length(U) == (MaxUsers-3), C =/= UId,
%%                    Check(C,O),
%%                    Check(Game,G),
%%                    Check(Speed,S),
%%                    Check(GameType,GT),
%%                    Check(Lucky,L),
%%                    Check(Rounds,R)])), 10)
%%     end,
%%
%%     Own = qlc:next_answers(qlc:cursor(qlc:q([V || {{_,_,_K},_,V=#game_table{creator=C,
%%                                                    rounds=R, game_type=G, owner=O,
%%                                                    users=_U, game_speed=S,
%%                                                    game_mode=GT, feel_lucky = L}}
%%                    <- gproc:table(props),
%%                    C == UId,
%%                    Check(C,O),
%%                    Check(Game,G),
%%                    Check(Speed,S),
%%                    Check(GameType,GT),
%%                    Check(Lucky,L),
%%                    Check(Rounds,R)])), 10),
%%
%%     Rest = qlc:next_answers(qlc:cursor(qlc:q([ValRest || {{_,_,_KeyRest},_,ValRest=#game_table{creator=C,
%%                                                    rounds=R, game_type=G, owner=O,
%%                                                    users=U, game_speed=S,
%%                                                    game_mode=GT, feel_lucky = L}}
%%                    <- gproc:table(props),
%%                    length(U) == MaxUsers, C =/= UId,
%%                    Check(C,O),
%%                    Check(Game,G),
%%                    Check(Speed,S),
%%                    Check(GameType,GT),
%%                    Check(Lucky,L),
%%                    Check(Rounds,R)])), 50),

%%     QLC = OneLeftList ++ TwoLeftList ++ ThreeLeftList ++ Own ++ Rest,
    QLC = OneLeftListOwn ++ OneLeftListOther ++
          TwoLeftListOwn ++ TwoLeftListOther ++
          ThreeLeftListOwn ++ ThreeLeftListOther ++
          MoreLeftListOwn ++ MoreLeftListOther ++
          NoMoreLeftListOwn ++ NoMoreLeftListOther,

%    ?INFO("~w:get_tables2 QLC = ~w", [?MODULE, QLC]), 

    FilteredQLC = lists:filter(
        fun(OneTable) ->
            TableUsers = OneTable#game_table.users,
            AllFilterOk = (FilterAllUsers==[]) or 
                (lists:usort( [lists:member(OFU, TableUsers) || OFU <- FilterAllUsers] ) == [true]),
            AnyFilterOk = (FilterAnyUser==[]) or 
                (lists:usort( [lists:member(OTU, FilterAnyUser) || OTU <- TableUsers] ) =/= [false]),
            AllFilterOk and AnyFilterOk
        end, QLC),

    case Convert of convert -> convert_to_map(FilteredQLC,Setting,UId,GameFSM); _ -> FilteredQLC end.

convert_to_map(Data,_Setting,UId,GameFSM) ->
    [ begin Url = lists:concat([?_U("/view-table/"),GameFSM,"/id/", TId]),
            Script = webutils:new_window_js(Url),
            Action = #event{type=click, actions=#script{script=Script}},
            ViewPerPoint = site_utils:table_per_user_point(UId, 0, Rounds),
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
                          rounds = Rounds,
                          users = Users,
                          game_process = ProcId,
                          owner = _Owner,
                          creator = Owner} = _Tab <- Data ].



get_lucky_tables2(_UId, Game) ->
    Lucky = true,

    Check = fun(Param,Value) ->
                   case Param of
                        undefined -> true;
                        _Else -> Param == Value
                   end end,

    Cursor = fun() ->
                qlc:cursor(qlc:q([V || {{_,_,_K},_,V=#game_table{game_type=G,
                                                   users=U,
                                                   feel_lucky = L}} <- gproc:table(props),
                           lists:member(robot, U),
                           Check(Game, G),
                           Check(Lucky, L)])
                )
    end,

    Tables = qlc:next_answers(Cursor(), 10),

    ?INFO("~w:get_lucky_tables2 Tables = ~w", [?MODULE, Tables]),
    Tables.



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
    MaxUsers = case q_game_type() of "tavla" -> 2; "okey" -> 4 end,
    case Tables of
        [] ->
            #panel{style="text-align: center", body=#h4{text=?_T("You can create a game or join a game")} };
        _ ->
            #table{class="view_table_table article-table", style="width:100%", rows=[
                begin
                    {info, {_, TId}} = InfoPostback,
                    {ok, WholeTable} = view_table:get_table(TId),
                    TMode = matchmaker:game_mode_to_text(WholeTable#game_table.game_mode),
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
                            case length(Users) of
                                MaxUsers -> "";
                                _ ->
                                    #link{id=joinTable,
                                        actions=Act,
                                        show_if=ViewPerPoint,
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
                            [#image{image="/images/free.png"} || _N <- lists:seq(1,MaxUsers-length(Users))] ++ 
                            [Info, JoinOrCrate, DeleteTable]
                    ]},
                    #tablerow{id=RowId, cells=[
                        #tablecell{ class=cell1,
                            body=[
                                "\""++TableNameLabel++"\"" ++ " (" ++ list_users_links(Users, OwnerLabel) ++ ") " ++
                                TMode ++ ", " ++ TSpeed ++ TRoundsOrNot ++ TDoubleOrNot
                            ],
                            id=tableNameLabel
                        },
                        #tablecell{ class=cell3,
                            body = Buttons
                        }
                    ]}
                end
                || [TableNameLabel,
                    OwnerLabel,
                    InfoPostback,
                    Action,
                    ViewPerPoint,
                    UserOwner,
                    Users,
                    DeleteAction] <- lists:usort(fun (A, B) -> 
                            {_, {_, An}} = lists:nth(3, A),
                            {_, {_, Bn}} = lists:nth(3, B),
                            An =< Bn
                        end, Tables)
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
    Rounds = wf:to_integer(case proplists:get_value(rounds, Setting, 0) of
				{multiple, _} -> 0;
				R -> R
			   end),
    Sets = 0,
    case lists:usort(Check) of
 	[true] ->
	    case site_utils:table_per_user_point(wf:user(), Sets, Rounds) of
		true ->
		    wf:remove(point_info),
		    wf:wire(create_button, #remove_class { class=disable });
		false ->
		    wf:flash(point_info, ?_T("You don't have enough points to play!")),
		    wf:wire(create_button, #add_class { class=disable })
	    end;
	_ ->
	    wf:remove(point_info),
	    wf:wire(create_button, #add_class { class=disable })
    end.


%% don't put this function to process_setting/1, it can cause cycling
check_depended({game_mode, countdown}) ->
    %% disable rounds
    wf:wire(ui_rounds, #hide{}),
    wf:wire("objs('.for_rounds').remove()"), %% criteria_box
    wf:wire("objs('.ui_rounds_btn').parent('li').removeClass('active');"),

    %% enable gosterge_finish
    wf:wire(gosterge_placeholder, #show{});

check_depended({game_mode, _}) ->
    %% enable rounds
    wf:wire(ui_rounds, #show{}),

    %% disable gosterge
    case is_option_present(gosterge_finish, true) of
        true ->
            process_setting({gosterge_finish, true}); %% toggle setting
        false ->
            ok
    end,
    wf:wire(gosterge_placeholder, #hide{});

check_depended(_) -> ok.

settings_box() -> settings_box(create).

settings_box(_Tag) ->
    ThisClass = case wf:state(buttons) of
        green -> "slide-up_green";
        _ -> "slide-up"
    end,
    GameSettings = #panel{body=[
        case wf:state(buttons) =:= green of false -> ui_table_name(); _ -> "" end,
        ui_game_speed(),
        #panel{class="two-cols",
            body=[#panel{class=left,body=ui_game_type()},
                #panel{id=ui_rounds, class="right", body=ui_rounds()}]},
        ui_checkboxes(),
        ui_double_game(),
        ui_add_checkboxes(),
        add_game_settings_guiders()
    ]},
    %% TODO: use real tabs
    GroupSettings = tab_group_setting(),
    FriendSettings = tab_friend_setting(),
    PersonalSettings = tab_personal_setting(),
    [#tabs{
	 tabs = [
	  {?_T("Game Settings"), GameSettings},
	  {?_T("Group Settings"), GroupSettings},
	  {?_T("Friend Settings"), FriendSettings},
	  {?_T("Personal Settings"), PersonalSettings}
	 ]},
     "<span id='guiderstab1hide' style='float: right; margin-top:-50px;'>",
     "</span>",
     case wf:state(buttons) of
        green -> #link{class=ThisClass, postback={show,join_game}, text=?_T("Hide"), actions=ac_hide_main_container()};
        _ -> ""
     end
    ].

view_table_box() ->
    [#panel{id=info_table}].

tab_group_setting() ->
    ThisClass = case wf:state(buttons) of
        green -> "list1_green";
        _ -> "list1"
    end,
    Groups = wf:state(user_in_groups),
    View = [ #listitem{body=construct_id(#link{text=Name, postback={tag,{group, Name}}})}
	     || #group_member{group = Name} <- Groups ],
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
    Settings = wf:session({q_game_type(),wf:user()}),
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
    lists:member(Key, [paired_game, gosterge_finish, deny_robots, private, slang, deny_observers]).

is_option_present(Key, Value) ->
    Settings = wf:session({q_game_type(),wf:user()}),
    proplists:is_defined(Key, Settings) andalso Value == proplists:get_value(Key, Settings).


comet_update() ->
    comet_update(empty).
comet_update(State) ->
    case wf:user() of
	undefined -> % user logged of
	    wf:redirect_to_login("/");
	_UId ->
	    timer:sleep(?TABLE_UPDATE_QUANTUM),

        TimeLeft = wf:session(time_left_to_update),
        if 
            TimeLeft == undefined ->
                NewState = State,
                wf:session(time_left_to_update, ?TABLE_UPDATE_INTERVAL);
            TimeLeft =< 0 ->
                garbage_collect(self()),
	            Tables = get_tables(),
	            %% used to reduce traffic and send updates only when they needed
	            NewState = erlang:md5(term_to_binary(Tables)),
	            case NewState of
		        State ->
		            nothing_changed;
		        _ ->
%                    [ garbage_collect(Pid) || Pid <- processes() ],
		            wf:update(tables, show_table(Tables)),
		            ui_paginate(),
		            wf:flush()
	            end,
	            user_counter:wf_update_me(),
                wf:session(time_left_to_update, ?TABLE_UPDATE_INTERVAL);
            true ->
                NewState = State,  
                wf:session(time_left_to_update, TimeLeft - ?TABLE_UPDATE_QUANTUM)
        end,

	    comet_update(NewState)
    end.

%% We should leave that in case when we want that number of options can be selected in same time.
can_be_multiple(age)   -> false;
can_be_multiple(users) -> true;
can_be_multiple(_Key)  -> false.


event(Event) ->
    case wf:user() of
	undefined ->
	    wf:redirect_to_login("/");
	_User ->
	    u_event(Event)
    end.

ac_show_main_container() ->
    wf:wire("objs('matchmaker_main_container').slideDown('fast');"),
    wf:wire("objs('matchmaker_slide_area').hide().slideDown('slow');").

ac_hide_main_container() ->
    #event{type=click, actions="objs('matchmaker_main_container').slideUp('fast');"}.

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
    Settings = wf:session({q_game_type(),wf:user()}),
    wf:session({q_game_type(),UId},Settings),
    URL = ?_U(lists:concat(["/matchmaker/", q_game_type()])),
    wf:redirect(URL),

    Desc = lists:flatten( URL ++ "|" ++ UId ++ "|" ++ proplists:get_value(table_name,Settings) ++ "|" ++ q_game_type() ++ "|" ++
        integer_to_list(proplists:get_value(rounds,Settings)) ++ "|" ++ atom_to_list(proplists:get_value(speed,Settings)) ++ "|" ++
        atom_to_list(proplists:get_value(game_mode,Settings))),
    webutils:post_user_system_message(Desc);


%% XXX: 
u_event(lucky_play_button) ->
    wf:session("108","1000001"),
    wf:session("1000001","1000001"),
    wf:redirect(?_U(lists:concat(["/matchmaker/"++q_game_type()])));

u_event({info, {Target, TId}}) ->
    {ok, TableSettings} = case Target of
        table ->

%            {ok, Table} = rpc:call(?APPSERVER_NODE,table_manager,get_table,[TId]),
            {ok, Table} = view_table:get_table(TId),
            ?INFO("INFO: ~p",[{TId,Table}]),
            {ok,Table};
           % {ok, rpc:call(?APPSERVER_NODE,table_manager,game_table_to_settings,[Table])};
        save_table -> rpc:call(?APPSERVER_NODE,table_manager,get_save_table_setting,[TId])
    end,
    Info = webutils:table_info(TableSettings),
    wf:update(info_table, #dialog{body=Info});

u_event({delete_table, TId, ProcId}) ->
    ?INFO("delete table: ~p",[{delete_table, TId, ProcId}]),
    case ProcId of undefined -> ok; _ -> ProcId ! {unreg, {p,g,ProcId}} end,
    ?INFO(" *** delete"),
    rpc:call(?APPSERVER_NODE,table_manager,delete_table,[TId]);

u_event({delete_saved_table, TId}) ->
    ?INFO(" *** delete saved"),
    rpc:call(?APPSERVER_NODE,table_manager,delete_save_table,[TId]);

u_event({tab_selected, ID}) ->
    ?INFO("u_event"),
    show_tab_guiders(ID);

u_event(clear_selection) ->
    OldSettings = wf:session({q_game_type(),wf:user()}),
    [
        u_event({tag, lists:nth(N, OldSettings)})
        || N <- lists:seq(3, length(OldSettings))
    ];

u_event(show_game_rules) ->
    case q_game_type() of
        "okey" ->
            Body = [
                #panel{class=holder, style="max-height:600px; ", body=[
                    #h1{text="OKEY OYUNLARI HAKKINDA", style="font-size:26px; margin-bottom:14px;"},
                    #h1{text="OYUNU OYNAMA", style="font-size:20px; margin-bottom:10px;"},
         
                    #table { rows=[
                        #tablerow { cells=[
                            #tablecell { style="vertical-align:top; text-align:right; font-weight:bold; width:130px; padding:5px;", text="Gösterme taşı:"},
                            #tablecell { style="padding:5px; text-align:left; ", body="Taşlar dağıtıldıktan sonra sağ alt köşede \"&#8239;gösterme&#8239;\" bölümüne açılan taş. "
                                            ++ "Elinde göstermesi olan gösterme taşının üzerini çift tıklayarak oyundaki (&#8239;varsa&#8239;) gösterme puanını kazanır. "
                                            ++ "Gösterme ilk el taş çekilmeden yapılmalıdır. Taş çekildikten sonra gösterme yapılamaz." }
                        ]},
                        #tablerow { cells=[
                            #tablecell { style="vertical-align:top; text-align:right; font-weight:bold; width:130px; padding:5px;", text="Okey taşı:"},
                            #tablecell { style="padding:5px; text-align:left; ", body="Göstermenin bir sayı yukarısı (&#8239;aynı renk olmak şartı ile&#8239;) okey taşıdır. Bu taş joker niteliğinde olup elde eksik olan istenilen renkteki sayı yerine kullanılır. "
                                            ++ "her el iki adet taş okey taşı yerine geçer. (&#8239;okey sayılan taşın yerine kullanılmak üzere 2 tane sahte okey bulunur&#8239;)."}
                        ]},
                        #tablerow { cells=[
                            #tablecell { style="vertical-align:top; text-align:right; font-weight:bold; width:130px; padding:5px;", text="Açar taşlar:"},
                            #tablecell { style="padding:5px; text-align:left; ", body="aynı renkten sıralı hale gelmesi, (&#8239;örn. Yeşil 1, 2, 3, 4, sarı 7, 8, 9, kırmızı 10, 11, 12 gibi&#8239;) 1sayılı taş seride 13 ten sonrada kullanılabilir (&#8239;11,12,13,1 gibi&#8239;) "
                                            ++ "aynı sayıların farklı renklerden bir seri uluşturması (&#8239;sarı- yeşil-kırmızı 2'li gibi&#8239;) "
                                            ++ "eldeki taşların tamamının (&#8239;14 tane&#8239;) açarlı gruplar oluşturması oyun bitme şartıdır. "
                                            ++ "Bu gruplar 4 &ndash; 4 &ndash; 3 &ndash; 3 lü olabileceği gibi 5 &ndash; 3 &ndash; 3 &ndash; 3 lü de veya 14 taşı açar hale gelen 5 &ndash; 5 &ndash; 4 te olabilir. Seri taşlar arasında boşluk olmamalıdır. "
                                            ++ "sürükleyip bırakarak oyunu bitirir."}
                        ]},
                        #tablerow { cells=[
                            #tablecell { style="vertical-align:top; text-align:right; font-weight:bold; width:130px; padding:5px;", text="Çift açmak:"},
                            #tablecell { style="padding:5px; margin-top:5px; text-align:left; ", body="Aynı renk ve sayıdan olmak üzere 7 çift taş da açar seri oluşturur. (&#8239;sahte okey çifti de dahildir&#8239;). Buna \" ÇİFT AÇMAK \" denir. "
                                            ++ "Çift olan taşlar yanyana dururken farklı çiftleri en az bir boşluk bırakarak dizmek gerekir. "
                                            ++ "15. taş çekilen taşların üzerine konarak oyun bitti bilgisi verilir."}
                        ]},
                        #tablerow { cells=[
                            #tablecell { style="vertical-align:top; text-align:right; font-weight:bold; width:130px; padding:5px;", text="Okey atmak:"},
                            #tablecell { style="padding:5px; text-align:left; ", body="Taşını açan yere oyun bitme taşı olarak \"okey\"i atarsa, oyundaki açma puanını değil okey atma puanını alır. "}
                        ]}
                    ]},

                    #h1{text="OYUN ÇEŞİTLERİ", style="font-size:20px;"},

                    #table { rows=[
                        #tablerow { cells=[
                            #tablecell { style="vertical-align:top; text-align:right; font-weight:bold; width:130px; padding:5px;", text="Standart:"},
                            #tablecell { style="padding:5px; text-align:left; ", body=[
                                #panel{ class="affiliates-box", style="font-size:12px; margin-left:0px; margin-top:-8px; margin-bottom:-2px; padding-top:3px; padding-bottom:3px;", body=[
                                    #table { rows=[
                                        #tablerow { cells=[
                                            #tablecell { style="padding:3px;", text="Gösterme"},
                                            #tablecell { style="padding:3px; text-align:center;", text="1"},
                                            #tablecell { style="padding:3px;", text="Puan"}
                                        ]},
                                        #tablerow { cells=[
                                            #tablecell { style="padding:3px;", text="Açma"},
                                            #tablecell { style="padding:3px; text-align:center;", text="3"},
                                            #tablecell { style="padding:3px;", text="Puan"}
                                        ]},
                                        #tablerow { cells=[
                                            #tablecell { style="padding:3px;", text="Okey atma"},
                                            #tablecell { style="padding:3px; text-align:center;", text="6"},
                                            #tablecell { style="padding:3px;", text="Puan"}
                                        ]},
                                        #tablerow { cells=[
                                            #tablecell { style="padding:3px;", text="Çift açma"},
                                            #tablecell { style="padding:3px; text-align:center;", text="6"},
                                            #tablecell { style="padding:3px;", text="Puan"}
                                        ]},
                                        #tablerow { cells=[
                                            #tablecell { style="padding:3px;", text="Çiftte okey atma"},
                                            #tablecell { style="padding:3px; text-align:center;", body="&nbsp;12&nbsp;"},
                                            #tablecell { style="padding:3px;", text="Puan"}
                                        ]}
                                    ]}
                                ]}
                            ]}
                        ]},
                        #tablerow { cells=[
                            #tablecell { style="vertical-align:top; text-align:right; font-weight:bold; width:130px; padding:5px;", text="Tek-çift:"},
                            #tablecell { style="padding:5px; text-align:left; ", body="Gösterme nin tek yada çift olmasına göre puanların belirlendiği oyun şeklidir. "
                                        ++ "Tek sayılı gösterme olduğunda standart oyun puanları geçerli olur. "
                                        ++ "Çift sayılı gösterme olduğunda standart oyun puanlarının 2 katı kabul edilir."}
                        ]},
                        #tablerow { cells=[
                            #tablecell { style="vertical-align:top; text-align:right; font-weight:bold; width:130px; padding:5px;", text="Renkli:"},
                            #tablecell { style="padding:5px; text-align:left; ", body="Gösterme taşının rengine göre puanlamanın yapıldığı oyundur. "
                                        ++ "Gösterme SARI-MAVİ tek sayı olursa STANDART OYUN PUANLARI. "
                                        ++ "Gösterme SARI-MAVİ çift sayı ve KIRMIZI-SİYAH tek sayı olursa 2 katı puan hesaplanır. "
                                        ++ "Gösterme KIRMIZI-SİYAH çift sayı olursa 4 katı puan hesaplanır."}
                        ]},
                        #tablerow { cells=[
                            #tablecell { style="vertical-align:top; text-align:right; font-weight:bold; width:130px; padding:5px;", text="Çanak puanı:"},
                            #tablecell { style="padding:5px; text-align:left; ", body="Tek-çift ve renkli oyunlar \"çanak\" lıdır. "
                                        ++ "\"Çanak\", oyun içi bonus puanların toplandığı yerdir. Açılışta oyun puanı sistem tarafından çanağa yazılır. "
                                        ++ "Oyun içinde yapılmayan gösterme puanları çanağa eklenir. Okey atan, çift açan, 8 taş yapan, renk açanlar açma puanlarına ilave olarak. "
                                        ++ "Çanak içindeki birikmiş puanı da kazanırlar. "
                                        ++ "Boşalan çanağa, sistem tarafından oyun puanı yeniden eklenir. Böylece çanak hiçbir el için boş kalmamış olur."}
                        ]},                  
                        #tablerow { cells=[
                            #tablecell { style="vertical-align:top; text-align:right; font-weight:bold; width:130px; padding:5px; padding-top:10px;", text="10' dan düşmeli oyun:"},
                            #tablecell { style="padding:5px; padding-top:10px; text-align:left; ", body=[
                                #panel{ class="affiliates-box", style="font-size:12px; margin-left:0px; margin-top:-8px; padding-top:3px; padding-bottom:3px;", body=[
                                    #table { rows=[
                                        #tablerow { cells=[
                                            #tableheader { style="padding:3px;", text="Oyuna her oyuncu 10 puan ile başlar"},
                                            #tableheader { style="padding:3px;", text=""}
                                        ]},
                                        #tablerow { cells=[
                                            #tablecell { style="padding:3px;", text="Gösterme"},
                                            #tablecell { style="padding:3px;", text="1"}
                                        ]},
                                        #tablerow { cells=[
                                            #tablecell { style="padding:3px;", text="Açma"},
                                            #tablecell { style="padding:3px;", text="2"}
                                        ]},
                                        #tablerow { cells=[
                                            #tablecell { style="padding:3px;", text="Okey atma"},
                                            #tablecell { style="padding:3px;", text="4"}
                                        ]},
                                        #tablerow { cells=[
                                            #tablecell { style="padding:3px;", text="Çift açma"},
                                            #tablecell { style="padding:3px;", text="4"}
                                        ]},
                                        #tablerow { cells=[
                                            #tablecell { style="padding:3px;", text=""},
                                            #tablecell { style="padding:3px;", body=["olmak üzere puan düşümü yapılır. ", #br{}, "Puanını ilk önce bitiren oyuncu oyunu kazanır."]}
                                        ]}                                         
                                    ]}
                                ]}
                            ]}
                        ]}
                    ]},

                    

                    #h1{text="FARKLI OYUN BİTİRME ÇEŞİTLERİ", style="font-size:20px; margin-bottom:4px;"},

                    #panel{style="padding:10px 30px;", 
                            text="Ayrıca Tek-çift ve Renkli oyunlar için geçerli aşağıda açıklanan 
                                özel açma şekilleri de bulunur ve ikramiyeli puan hesabı yapılır."},

                    #table { rows=[
                        #tablerow { cells=[
                            #tablecell { style="vertical-align:top; text-align:right; font-weight:bold; width:130px; padding:5px;", text="Renk açmak:"},
                            #tablecell { style="padding:5px; text-align:left; ", body="Açmanın eldeki taşların tamamının aynı renkten taşlarla yapılması esasına dayanır. Normal oyundaki gibi okeyler eksik taşların yerine kullanılabilir. "
                                        ++ "Oyuna esas puanlamanın 8 katı açma puanı olarak uygulanır. Bu durumda okey atılırsa 16 katı uygulanır."}
                        ]},
                        #tablerow { cells=[
                            #tablecell { style="vertical-align:top; text-align:right; font-weight:bold; width:130px; padding:5px;", text="Renkten çift açmak:"},
                            #tablecell { style="padding:5px; text-align:left;", body="Bütün çift taşların aynı renkten olması hali. Renk açmanın iki katı puan uygulanır. Okey taşları kullanılabilir. "}
                        ]},
                        #tablerow { cells=[
                            #tablecell { style="vertical-align:top; text-align:right; font-weight:bold; width:130px; padding:5px;", text="Sekiz taş:"},
                            #tablecell { style="padding:5px; text-align:left; ", body="Oyun esnasında, oyun taşları içinde herhangi bir taşın 8 tanesini birden ıstakada toplamak demektir. "
                                        ++ "Örn. 8 tane 4'lü gibi. "
                                        ++ "8 taşı toplayan oyuncu ekranındaki \"8 Taş\" düğmesini tıkladığında eli açıp açmamasına bakılmaksızın oyun puanının 4 katı puanı ve "
                                        ++ "çanakta biriken puanı ödül olarak kazanır. "}
                        ]}
                    ]},

                    #panel{style="padding:10px 30px;", text="Oyun ve puanlama seçenekleri masa açılışında set edilir. Masa açılışında herhangi bir oyun seçilmez ise standart oyun yüklenir."},

                    #table { rows=[
                        #tablerow { cells=[
                            #tableheader { style="padding:5px;", text="" },
                            #tableheader { style="padding:5px;", text="" },
                            #tableheader { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="Tek-çift", colspan=2 },
                            #tableheader { style="padding:5px; background-color:#ddbbff; text-align:center;", text="Renkli", colspan=2 },
                            #tableheader { style="padding:5px;", text="" },
                            #tableheader { style="padding:5px;", text="" }
                        ]},
                        #tablerow { cells=[
                            #tableheader { style="padding:5px;", text="" },
                            #tableheader { style="padding:5px; background-color:#ddddff; text-align:center;", text="standart" },
                            #tableheader { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="tek" },
                            #tableheader { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="çift" },
                            #tableheader { style="padding:5px; background-color:#ddbbff; text-align:center;", body=["sarı-mavi ÇİFT", #br{}, "siyah-kırmızı TEK"] },
                            #tableheader { style="padding:5px; background-color:#ddbbff; text-align:center;", text="Siyah kırmızı ÇİFT" },
                            #tableheader { style="padding:5px; background-color:#ddffdd; text-align:center;", text="10'dan düşmeli" },
                            #tableheader { style="padding:5px; text-align:center;", text="Çanakta biriken" }
                        ]},
                        #tablerow { cells=[
                            #tableheader { style="padding:5px;", text="Gösterme" },
                            #tablecell { style="padding:5px; background-color:#ddddff; text-align:center;", text="1" },
                            #tablecell { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="1" },
                            #tablecell { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="2" },
                            #tablecell { style="padding:5px; background-color:#ddbbff; text-align:center;", text="2" },
                            #tablecell { style="padding:5px; background-color:#ddbbff; text-align:center;", text="4" },
                            #tablecell { style="padding:5px; background-color:#ddffdd; text-align:center;", text="1" },
                            #tablecell { style="padding:5px; text-align:center;", body="&mdash;" }
                        ]},
                        #tablerow { cells=[
                            #tableheader { style="padding:5px;", text="Açma" },
                            #tablecell { style="padding:5px; background-color:#ddddff; text-align:center;", text="3" },
                            #tablecell { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="3" },
                            #tablecell { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="6" },
                            #tablecell { style="padding:5px; background-color:#ddbbff; text-align:center;", text="6" },
                            #tablecell { style="padding:5px; background-color:#ddbbff; text-align:center;", text="12" },
                            #tablecell { style="padding:5px; background-color:#ddffdd; text-align:center;", text="2" },
                            #tablecell { style="padding:5px; text-align:center;", body="&mdash;" }
                        ]},
                        #tablerow { cells=[
                            #tableheader { style="padding:5px;", text="Okey atma" },
                            #tablecell { style="padding:5px; background-color:#ddddff; text-align:center;", text="6" },
                            #tablecell { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="6" },
                            #tablecell { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="12" },
                            #tablecell { style="padding:5px; background-color:#ddbbff; text-align:center;", text="12" },
                            #tablecell { style="padding:5px; background-color:#ddbbff; text-align:center;", text="24" },
                            #tablecell { style="padding:5px; background-color:#ddffdd; text-align:center;", text="4" },
                            #tablecell { style="padding:5px; text-align:center;", body="açana" }
                        ]},
                        #tablerow { cells=[
                            #tableheader { style="padding:5px;", text="Taş çifti" },
                            #tablecell { style="padding:5px; background-color:#ddddff; text-align:center;", text="6" },
                            #tablecell { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="6" },
                            #tablecell { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="12" },
                            #tablecell { style="padding:5px; background-color:#ddbbff; text-align:center;", text="12" },
                            #tablecell { style="padding:5px; background-color:#ddbbff; text-align:center;", text="24" },
                            #tablecell { style="padding:5px; background-color:#ddffdd; text-align:center;", text="4" },
                            #tablecell { style="padding:5px; text-align:center;", body="açana" }
                        ]},
                        #tablerow { cells=[
                            #tableheader { style="padding:5px;", text="8 taş" },
                            #tablecell { style="padding:5px; background-color:#ddddff; text-align:center;", text="yok" },
                            #tablecell { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="12" },
                            #tablecell { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="24" },
                            #tablecell { style="padding:5px; background-color:#ddbbff; text-align:center;", text="24" },
                            #tablecell { style="padding:5px; background-color:#ddbbff; text-align:center;", text="48" },
                            #tablecell { style="padding:5px; background-color:#ddffdd; text-align:center;", text="yok" },
                            #tablecell { style="padding:5px; text-align:center;", body="yapana" }
                        ]},
                        #tablerow { cells=[
                            #tableheader { style="padding:5px;", text="Renk açma" },
                            #tablecell { style="padding:5px; background-color:#ddddff; text-align:center;", text="yok" },
                            #tablecell { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="24" },
                            #tablecell { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="48" },
                            #tablecell { style="padding:5px; background-color:#ddbbff; text-align:center;", text="48" },
                            #tablecell { style="padding:5px; background-color:#ddbbff; text-align:center;", text="96" },
                            #tablecell { style="padding:5px; background-color:#ddffdd; text-align:center;", text="yok" },
                            #tablecell { style="padding:5px; text-align:center;", body="açana" }
                        ]},
                        #tablerow { cells=[
                            #tableheader { style="padding:5px;", text="Renkten Taş çifti" },
                            #tablecell { style="padding:5px; background-color:#ddddff; text-align:center;", text="yok" },
                            #tablecell { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="48" },
                            #tablecell { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="96" },
                            #tablecell { style="padding:5px; background-color:#ddbbff; text-align:center;", text="96" },
                            #tablecell { style="padding:5px; background-color:#ddbbff; text-align:center;", text="192" },
                            #tablecell { style="padding:5px; background-color:#ddffdd; text-align:center;", text="yok" },
                            #tablecell { style="padding:5px; text-align:center;", body="açana" }
                        ]},
                        #tablerow { cells=[
                            #tableheader { style="padding:5px;", text="Renkten okey atma" },
                            #tablecell { style="padding:5px; background-color:#ddddff; text-align:center;", text="yok" },
                            #tablecell { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="48" },
                            #tablecell { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="96" },
                            #tablecell { style="padding:5px; background-color:#ddbbff; text-align:center;", text="96" },
                            #tablecell { style="padding:5px; background-color:#ddbbff; text-align:center;", text="192" },
                            #tablecell { style="padding:5px; background-color:#ddffdd; text-align:center;", text="yok" },
                            #tablecell { style="padding:5px; text-align:center;", body="açana" }
                        ]},
                        #tablerow { cells=[
                            #tableheader { style="padding:5px;", text="Renkten Taş çifti okey atma" },
                            #tablecell { style="padding:5px; background-color:#ddddff; text-align:center;", text="yok" },
                            #tablecell { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="96" },
                            #tablecell { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="192" },
                            #tablecell { style="padding:5px; background-color:#ddbbff; text-align:center;", text="192" },
                            #tablecell { style="padding:5px; background-color:#ddbbff; text-align:center;", text="384" },
                            #tablecell { style="padding:5px; background-color:#ddffdd; text-align:center;", text="yok" },
                            #tablecell { style="padding:5px; text-align:center;", body="açana" }
                        ]},
                        #tablerow { cells=[
                            #tableheader { style="padding:5px;", text="Yanlış açan" },
                            #tablecell { style="padding:5px; background-color:#ddddff; text-align:center;", text="-9" },
                            #tablecell { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="-9" },
                            #tablecell { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="-18" },
                            #tablecell { style="padding:5px; background-color:#ddbbff; text-align:center;", text="-18" },
                            #tablecell { style="padding:5px; background-color:#ddbbff; text-align:center;", text="-36" },
                            #tablecell { style="padding:5px; background-color:#ddffdd; text-align:center;", text="-2" },
                            #tablecell { style="padding:5px; text-align:center;", body="&mdash;" }
                        ]},
                        #tablerow { cells=[
                            #tableheader { style="padding:5px;", text="Yanlış açışta diğerleri" },
                            #tablecell { style="padding:5px; background-color:#ddddff; text-align:center;", text="3" },
                            #tablecell { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="3" },
                            #tablecell { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="6" },
                            #tablecell { style="padding:5px; background-color:#ddbbff; text-align:center;", text="6" },
                            #tablecell { style="padding:5px; background-color:#ddbbff; text-align:center;", text="12" },
                            #tablecell { style="padding:5px; background-color:#ddffdd; text-align:center;", text="yok" },
                            #tablecell { style="padding:5px; text-align:center;", body="&mdash;" }
                        ]},
                        #tablerow { cells=[
                            #tableheader { style="padding:5px;", text="Çanak boşalınca çanağa eklenen" },
                            #tablecell { style="padding:5px; background-color:#ddddff; text-align:center;", text="yok" },
                            #tablecell { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="4" },
                            #tablecell { style="padding:5px; background-color:#ffbbcc; text-align:center;", text="8" },
                            #tablecell { style="padding:5px; background-color:#ddbbff; text-align:center;", text="8" },
                            #tablecell { style="padding:5px; background-color:#ddbbff; text-align:center;", text="16" },
                            #tablecell { style="padding:5px; background-color:#ddffdd; text-align:center;", text="yok" },
                            #tablecell { style="padding:5px; text-align:center;", body="&mdash;" }
                        ]}
                    ]},



                    #singlerow{style="margin: 20px;", cells=[
                        #tablecell{
                            body="", style="width:272px;"
                        },
                        #tablecell{
                            body=#cool_button{text=?_T("Ok"), postback=hide_rules, style="display:block;"}
                        }
                    ]},
                    #grid_clear{}
                ]}
            ],    
            wf:update(simple_panel, webutils:lightbox_panel_template(simple_lightbox, Body)),
            wf:wire(simple_lightbox, #show{});
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
    wf:wire(simple_lightbox, #hide{});

u_event(Other) ->
    ?INFO("u_event other: ~p",[Other]),
    webutils:event(Other).


process_setting({Key, Value} = Setting) ->
    OldSettings = wf:session({q_game_type(),wf:user()}),

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

    wf:session({q_game_type(),wf:user()}, NewSettings),
    check_required(NewSettings),
    io:fwrite("Update setting: ~p~n~n", [NewSettings]),
    wf:session(time_left_to_update, ?TABLE_UPDATE_QUANTUM),
    %wf:update(tables, ui_get_tables()),
    %ui_paginate(),
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
        "kakaratavla" -> ?_T("Kakara Tavla")
    end.

game_speed_to_text(Speed) when is_atom(Speed) ->
    game_speed_to_text(atom_to_list(Speed));
game_speed_to_text(Speed) ->
    case Speed of 
        "fast" -> ?_T("Fast");
        "normal" -> ?_T("Normal");
        "slow" -> ?_T("Slow")
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
