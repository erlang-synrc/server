-module (view_table).
-author('Maxim Sokhatsky <maxim@synrc.com>').
-compile(export_all).

-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("nsm_db/include/user.hrl").
-include_lib("nsm_db/include/feed.hrl").
-include_lib("nsm_db/include/table.hrl").
-include_lib("nsm_db/include/config.hrl").
-include_lib("elements/records.hrl").
-include_lib("stdlib/include/qlc.hrl").

-define(MAX_CHAT_LENGTH, 1024). % 1024 bytes
-define(INTERACT_TIMEOUT, 10000). % 10 sec

-include("gettext.hrl").
-include("loger.hrl").
-include("setup.hrl").

route() -> ["game_name"].

main() ->
    case wf:user() /= undefined of
        true  -> main_authorized();
        false -> wf:redirect_to_login("/")
    end.

main_authorized() ->
    case wf:q(id) of

	"lucky" ->

            AllTables = matchmaker:get_tables(raw),
            case erlang:length(AllTables) of 
		 0 -> wf:state(table_id, {error, table_not_found});
                 _ -> First = lists:nth(1,AllTables), 
                      ?INFO("Lucky Table: ~p",[AllTables]),
                      MaxUser = proplists:get_value(max_users, game_requirements(First)),
                      CurrentUser = length(First#game_table.users),
                      case CurrentUser < MaxUser of
                           true ->  wf:state(table, First),
                                    wf:state(joined, true),
		                    wf:state(table_id, First#game_table.id);
                           false -> wf:state(table_id, {error, all_tables_are_full})
                      end
            end,

            main_template();
	    
	_ ->

            User = webutils:user_info(),
  
            ?INFO("Settings Key: ~p: ",[{wf:q(game_name),wf:user()}]),
            ?INFO("TableId: ~p: ",[wf:q(id)]),
            Id = case wf:session({wf:to_integer(wf:q(id)),wf:user()}) of
                      undefined -> wf:to_integer(wf:q(id));
                      CachedId -> CachedId
                 end,

            Settings = table_settings(),

            ?INFO("ViewTable Settings: ~p",[Settings]),
            Tables = get_tables(Id),
            case Tables of
                [] ->   

                           case table_manager:create_table(User, Settings) of
                           {ok, Table} ->
                                ?INFO("Table_manager CREATE: ~p",[Table]),
                                wf:session({wf:to_integer(wf:q(id)),wf:user()},Table#game_table.id), 
                                wf:state(table, Table),
                                wf:state(joined, true),
                                wf:state(table_id, Table#game_table.id);
                           {error, Error} ->
                                ?INFO("Table_manager ERROR!!!"),
                                wf:state(joined, false),
                                wf:state(table_id, {error, Error})
                           end;
                 [H |_] -> ?INFO("TableId: ~p: ~p",[Id,Tables]),
                           Creator = H#game_table.creator,
                           Private = H#game_table.private,
                           FriendCheck = nsm_users:is_user_subscr(Creator, User#user.username),
                           if Private andalso FriendCheck == false ->
                                  wf:state(joined, false),
                                  wf:state(table_id, {error, not_a_friend});
                              true ->
                                  wf:state(table, H),
                                  wf:state(joined, true),
                                  wf:state(table_id, Id)
                           end
            end,

            main_template()
    end.

main_template() ->
    Id = wf:state(table_id),
    case wf:state(table) of
        #game_table{game_state = started, id = GameId} when is_integer(Id) -> %% Game is already started => Run flex client
            wf:session(integer_to_list(Id), integer_to_list(GameId)),
            ?INFO("GameId: ~p, Id: ~p",[GameId,Id]),
            Url = lists:concat([?_U("/client"), "/", ?_U(wf:q(game_name)), "/id/", Id]),
            ?INFO("Starting Game: ~p",[Url]),
            wf:wire("window.started=true;"),
            wf:session({Id,wf:user()},undefined),
            wf:redirect(Url);

        _ -> #template { file=code:priv_dir(nsp_srv)++"/templates/700.html" }
    end.

title() -> ?_T("Game Page").

body() ->
    PageTitle = case wf:state(table_id) of
        {error, _} -> ?_T("Info");
        Id -> ?INFO("body"),
               case get_table(Id,wf:state(table)) of
	            {ok, Table} when not is_list(Table) -> Table#game_table.name;
		    _ -> ?_T("Error") end
    end,
    ["<header><div class=\"block\">"
	"<strong class=\"logo vcard\">", webutils:logo(), "</strong>"
     "</div></header>"]
    ++
    [ #section{ body=[ #h3{class="table-name", text=PageTitle},
		       table_box(),
                       #lightbox{id=lightbox, style="display: none;", body=delete_table_info() } ]} ].

table_settings() ->
    case wf:session({wf:q(game_name), wf:user()}) of
        undefined -> [];
        X -> X
    end.


delete_table_info() ->
    CloseAction = #event{type=click, actions=#script{script="window:close()"}},
    #panel{id=lightbox_panel,
           body=webutils:lightbox_panel_template(
		"lightbox",
		[#h1{text=?_T("Error")},
		 #panel{id=lightboxmsg}
		], CloseAction)}.

table_box() ->
    Joined = wf:state(joined),
    case wf:state(table_id) of
	{error, _A} -> wf:session({wf:q(id),wf:user()},undefined),
                       #panel{style="width: 720px;", body=table({error, _A}, Joined)};
	Id ->
            {ok,Table} = get_table(Id,wf:state(table)),
            case Table of [] -> 
                          wf:session({wf:q(id),wf:user()},undefined),
                          ?INFO("Table Null ~p",[Id]),
                          #panel{style="width: 720px;", body=table({error, table_not_found}, Joined)};
            _ ->
	    update_table_info(Table), 
            ?INFO("Session: ~p",[wf:session({wf:q(id),wf:user()})]),
            MaxUser = proplists:get_value(max_users, game_requirements(Table)),
            CurrentUser = length(Table#game_table.users),
            case CurrentUser < MaxUser of
                 true ->  List = lists:any(fun(A) -> A =:= wf:user() end, Table#game_table.users),
                          case List of 
                               true -> wf:session({wf:q(id),wf:user()},undefinded),
                                      #panel{style="width: 720px;", body=table({error, already_joined}, Joined)}; 
                               false ->
                               case wf:session({Id,wf:user()}) of
                                    undefined ->  ?INFO("Start New Comet"),
                                                  start_pre_comet_process(Id);
                                     _else -> ?INFO("Process Already Created"),
                                             leave_table(wf:user(),Id),
		                             start_pre_comet_process(Id,skip)
                               end,
  			       #panel{style="width: 720px;", body=table(Id, Joined)}
                          end;
                          
                 false -> wf:session({Id,wf:user()},undefined),
                          #panel{style="width: 720px;", body=table({error, too_much_users}, Joined)}
            end
            end
    end.

table({error, Error}, _) ->
    ?INFO(Error),
     Msg = case Error of
	       already_joined ->
		    ?_T("You are already in this table.");
	       table_not_found ->
		    ?_T("Table not found");
	       all_tables_are_full ->
		    ?_T("Sorry, all tables are full, you can create one yourself!");
               game_type_missing ->
                    ?_T("Game type missing");
               table_name_conflict ->
                   ?_T("You've already created table with that name.");
               too_much_users ->
		   ?_T("This table is full. Try another one.");
               quota_hard_limit ->
                   ?_T("Quota hard limit reached");
               not_a_friend ->
                   ?_T("The table is private. You're not allowed to join...")
           end,
    [#grid_8{id=error_info, prefix=1, alpha=true, body=[Msg]},
     #grid_clear{}
    ];

table(Id, _Joined=true) ->
    ?INFO("table joined"),
    table_ok(Id);

table(Id, _Joined=false) ->
    ?INFO("table not joined yet"),
%    UId = wf:user(),
%    Table = wf:state(table),
%    {ok, User} = nsm_users:get_user(UId),

%    Options = case wf:q(lucky) of
%                  "true" ->
%                      [feellucky];
%                  _ ->
%                      []
%              end,
    table_ok(Id).

%   case table_manager:join_table(User, Id, Options) of
%	ok ->
%	    table_ok(Id);
%	{error, Error} ->
%	    {ok, Pid} = sync_with_pre_comet(),
%	    exit_from_pre_comet(Pid), %% if error, comet wont really start
%	    case Error of
%	    end,
%	    [#grid_8{id=error_info, prefix=1, alpha=true, body=[Msg]},
%	     #grid_clear{}
%	    ]
%   end.
%   table_ok(Id).

sync_with_pre_comet() ->
    receive {comet_started, Pid} -> {ok, Pid}
    after ?INTERACT_TIMEOUT -> timeout 
    end.

really_start_comet(Pid) ->
    Pid ! {self(), really_start_comet}.

exit_from_pre_comet(Pid) ->
    Pid ! {self(), exit}.

table_ok(Id) ->
    {ok, Pid} = sync_with_pre_comet(),
    really_start_comet(Pid), %% comet will be really started
    User = webutils:user_info(),
    LeavePostback = {leave_table, User#user.username, Id},

    % cool way to create JavaScript code which does postback call
    Anchor = wf_context:anchor(), ValidationGroup = wf_context:event_validation_group(),
    Postback_js = wf_event:generate_postback_script(LeavePostback, Anchor, ValidationGroup, ?MODULE, undefined),

    % normally postback should be called by Nitrogen.$event_loop()
    % running in main thread, but in our case nothing outside of
    % unload() function will run, so we should do
    % Nitrogen.$event_loop() to handle our postback this

    wf:wire(wf:f("jQuery(window).unload( function () { if(!window.started) { ~s; Nitrogen.$event_loop();} });", 
                       [Postback_js])),

    ?INFO("table_ok"),
    {Ret, Table} = get_table(Id,wf:state(table)),
    case Ret of
        ok -> Owner = Table#game_table.owner,
              [ #panel { class="user-list", body=#panel{id=user_list, body=?_T("Loading...")}},
                #panel { class="chat-wrapper", 
                         body=[ #panel{class="chat-header", body=?_T("Chat")},
                                #panel{class="container", body= [
                                         #panel   { id=chat_history, 
                                                    class="chat-history", 
                                                    style="overflow:auto; height: 170px;" },
                                         #textbox { id=message_text_box, class="chat-text", next=chat_send_button },
                                         #button  { id=chat_send_button, 
                                                    class="chat-send",
                                                    style="width: 75px;margin-right: 0;", 
                                                    text=?_T("Send"), 
                                                    postback=chat }
		                ] }
	                      ] },

                #panel{class="table-info", body=[ #panel{class="panel-info", id=info, body=["Loading..."] },
                                                  #panel{id=action_button, body=action_button(Owner, Table)}
                                                ] },
                #grid_clear{}
              ];
	error -> #game_table{}
   end.

action_button(Id) ->
    ?INFO("action button"),
    {ok, Table} = get_table(Id,wf:state(table)),
    Owner = Table#game_table.owner,
    action_button(Owner, Table).

action_button(Owner, Table) ->
    AdminButton = wf:user() == Owner,
    TId = Table#game_table.id,
    _AllowRobot = case Table#game_table.deny_robots == false of
        true -> AdminButton == true;
        false -> false
    end,
    #panel{class="contrtol-buttons", body=[
        #panel{class="button leave", body=#link{text=?_T("Leave table"), postback={leave_table, wf:user(), TId} }},
        #panel{class="button start", show_if = AdminButton, body=#link{text=?_T("Start game"), 
                postback=start_game, actions=[
                    #event{type=click, actions=#script{script="window.started=true;"}}
        ]}}
    ]}.

start_pre_comet_process(Id) -> start_pre_comet_process(Id, noskip).
start_pre_comet_process(Id, Skip) ->
    WebPid = self(),
    ?INFO("start pre comet "),
    Ret = get_table(Id,wf:state(table)),
    Table = case Ret of {ok,T} -> T; (_) -> #game_table{} end,
    Settings = table_settings(),
    TableId = wf:state(table_id),
    GameType = Table#game_table.game_type,
    case Skip of 
         skip ->  wf:comet_global(fun() ->
			    WebPid ! {comet_started, self()},
			    put(user, webutils:user_info()),
			    put(game_type, GameType),
                            ?INFO("skip"),
			    receive
				{WebPid, really_start_comet} ->
                                    ?INFO("INIT: ~p",[Table]),
                                    gproc:reg({p,l,self()},Table),
				    table_info(Table); %% really start comet
				{WebPid, exit} -> %% explicit exit
				    exit("No need in comet")
			    after ?INTERACT_TIMEOUT -> %% exit by timeout (WebPid must be dead or not intrested in comet)
				    exit("No one really wants to start comet")
			    end
		   end, {chat_table, Id});
         (_) ->   wf:comet_global(fun() ->
                            ?INFO("view table comet process"),
			    wf:session({table_comet_pid,Id}, self()),
			    WebPid ! {comet_started, self()},
			    put(user, webutils:user_info()),
			    put(game_type, GameType),
                            Tab1 = case get_table(Id,wf:state(table)) of 
                                 undefined -> #game_table{creator = wf:user()}; 
                                 [] -> #game_table{creator = wf:user()}; {ok,A}-> A end,
                            TableName = proplists:get_value(table_name, Settings, "no table"),
                            Rounds = proplists:get_value(rounds, Settings, 1),
                            GameMode = proplists:get_value(game_mode, Settings, Table#game_table.game_mode),
                            GameSpeed = proplists:get_value(speed, Settings, normal),
                            FeelLucky = proplists:get_value(feel_lucky, Settings, false),
                            TourType = case GameMode of 
                                paired -> paired_lobby;
                                _ -> simple
                            end,
                            GProcTable = Table#game_table{game_process = self(),
                                                          id = TableId,
                                                          age_limit = [crypto:rand_uniform(20,30), crypto:rand_uniform(31,40)],
                                                          game_mode = GameMode,
                                                          game_speed = GameSpeed,
                                                          feel_lucky = FeelLucky,
                                                          owner = wf:user(),
                                                          creator = Tab1#game_table.creator,
                                                          rounds = Rounds,
                                                          name = TableName,
                                                          tournament_type = TourType
                                                         },

                            wf:state(table, GProcTable),
                            ?INFO("GProc Registration from Comet: ~p",[GProcTable]),

                            gproc:reg({p,l,self()},GProcTable),

			    receive
				{WebPid, really_start_comet} ->
                                    Tables = get_tables(Id),

                                    [ begin 
                                            {ok,User1} = nsm_users:get_user(wf:user()),
                                            ?INFO("join ~p to table ~p owner ~p",[wf:user(), Id, Table2#game_table.owner]), 
                                            Table2#game_table.game_process ! {join, User1, Table2} % XXX
                                      end || Table2 <- Tables],
    
                                    [ begin 
                                      {ok,User1} = nsm_users:get_user(Table3#game_table.owner),
                                      ?INFO("join user ~p to table ~p owner ~p",[User1#user.username, Id,
                                                            GProcTable#game_table.owner]), 
                                      self() ! {join, User1, GProcTable}
                                      end || Table3 <- Tables, Table3#game_table.owner /= wf:user() ],

				    table_info(GProcTable); %% really start comet
				{WebPid, exit} -> %% explicit exit
				    exit("No need in comet")
			    after ?INTERACT_TIMEOUT -> %% exit by timeout (WebPid must be dead or not intrested in comet)
				    exit("No one really wants to start comet")
			    end
		    end, {chat_table, Id})
    end.

table_info(Table) ->
    Id = Table#game_table.id,
    process_flag(trap_exit, true),
    receive
	{'EXIT', _Pid, _} -> 
             Tables = get_tables(wf:state(table_id)),
             ?INFO("EXIT signal"),
             [ try T#game_table.game_process ! {leave, wf:user(), T} after ok end || T <- Tables ], 
             leave_table(wf:user(),Table);
        {start_game} -> start_game(), table_info(Table);
        {launch_client, GameId, HumanReadableGameName} -> redirect_to_flex(GameId, HumanReadableGameName);
        {add_robot} -> add_robot(), table_info(Table);
        {kick, User} -> kick_user(User);
        {unreg, Key} -> gproc:unreg(Key), table_info(Table);
        {join, User, Tab} -> join_user(User,Tab);
        {chat, User, ChMessage} -> chat_new_msg(User, ChMessage), wf:flush(), table_info(Table);
        {leave, User, Tab} -> leaving(User,Tab), table_info(Tab);
        {leave_table, User, Id} -> leave_table(User,Id);
        {change_owner, User} ->
             ?INFO("changing creator from ~p to ~p",[Table#game_table.creator,User]),
             NewTable = Table#game_table{creator=User,users=lists:delete(Table#game_table.creator,Table#game_table.users)},
             gproc:set_value({p,l,self()},NewTable),
             update_table_info(NewTable),
             wf:update(action_button, action_button(Id)),
             table_info(NewTable);
	'INIT'       -> table_info(Table);
	{'JOIN', _}  -> table_info(Table);
	{'LEAVE', _} -> table_info(Table);
	Any ->
	    ?PRINT({unknown_message, Any}),
	    table_info(Table)
    after 3000 ->
	?MODULE:table_info(Table)
    end.

chat_info(Info) ->
    Terms = #panel{class="info", body = Info},
    update_table_chat(Terms).

chat_user_in(Username) ->
    Terms = #panel{class="user join",
		   body = [ ?_TS("User $username$ connected.", [{username, Username}]) ]},
    update_table_chat(Terms).

change_owner(Username) ->
    Terms = #panel{ class="user new_owner", 
                    body = [ ?_TS("User $username$ was appointed as the owner.", [{username, Username}]) ]},
    update_table_chat(Terms).

delete_table() ->
    Terms = #panel{ class="info important", 
                    body = [ ?_T("This table has been destroyed") ]},
    update_table_chat(Terms).

chat_user_out(Username) ->
    Terms = #panel{ class="user left", 
                    body = [ ?_TS("User $username$ has left.", [{username, Username}]) ]},
    update_table_chat(Terms).

chat_new_msg(User, Message) ->
    Terms = #panel{ class="chat",
		    body = [ #span{class="username", text = User#user.username}, ":&nbsp;", #span{text = Message} ]},
    update_table_chat(Terms).

update_table_chat(Terms) ->
    wf:insert_bottom(chat_history, Terms),
    wf:wire("obj('chat_history').scrollTop = obj('chat_history').scrollHeight;"),
    %% wf:flush(),
    ok.

show_row(Label, Info) ->
    ShowIf = fun(I) ->
                     case I of
                         undefined -> false;
                         ""  -> false;
                         <<"">> -> false;
                         _ -> true
                     end
             end,
    #panel{show_if=ShowIf(Info),
              body=[#h5 {text=Label},
                     #panel {body=Info}]}.

get_tables(Id) ->
  nsm_queries:map_reduce(game_manager,get_tables,[Id]).

qlc_id(Id) ->
    qlc:e(qlc:q([Val || {{_,_,_Key},_,Val=#game_table{gameid = _GameId, id = _Id, 
                            owner = _Owner, creator = _Creator}} <- 
             gproc:table(props), Id == _Id])).

qlc_id_creator(Id,Creator,Owner) ->
    qlc:e(qlc:q([Val || {{_,_,_Key},_,Val=#game_table{gameid = _GameId, id = _Id, 
                            owner = _Owner, creator = _Creator}} <- 
             gproc:table(props), Id == _Id, Creator == _Creator, Owner ==_Owner])).

get_table(Id, Owner) -> get_table_raw(Id, qlc_id(Id) ++ rpc:call(?GAMESRVR_NODE,game_manager,qlc_id,[Id]), Owner).
get_table(Id, Creator,Owner) -> get_table_raw(Id, qlc_id_creator(Id,Creator,Owner) ++
  rpc:call(?GAMESRVR_NODE,game_manager,qlc_id_creator,[Id,Creator,Owner]), undefined).

get_table_raw(_Id, QLC,StateTab) ->
    Tables = case StateTab  of
       undefined -> QLC;
       _ -> [StateTab]
    end,

    case length(Tables) =:= 1 of % 
        true -> [Table] = Tables,
		{ok,Table};
	false -> 
                case StateTab of
		     undefined when length(Tables) > 0 -> [H|_T]=Tables, {ok,H};
		     undefined -> {ok,[]};
		     _Else -> ?INFO("Cached Table in Session ProcId: ~p",
                                   [{StateTab#game_table.game_process,
                                   StateTab#game_table.id}]),{ok,StateTab}
                 end
    end.

leaving(User, Table) ->
    Message = ?_TS("User ($user$) leaving",[{user, User}]),
    Users = Table#game_table.users,
    NewUsers = lists:delete(User,Users),
    NewTable = Table#game_table{users=NewUsers},
    wf:state(table,NewTable),
    ?INFO("leaving ~p in owner table ~p left: ~p",[User,Table#game_table.owner, NewUsers]),
    gproc:set_value({p,l,NewTable#game_table.game_process},NewTable),
    chat_info(Message),
    update_table_info(NewTable),
    wf:flush().

leave_table(User, Id) ->
    wf:session({wf:to_integer(wf:q(id)),User},undefined), 
    Tables = get_tables(Id),
    ?INFO("leave table ~p ~p",[User,Tables]),
    [ begin 
    case T#game_table.owner == User of
         true ->  ?INFO("CLOSE ME"), 
                  gproc:unreg({p,l,self()}),
                  wf:update(info, ?_T("You have left from table")),
		  wf:flush(),
                  Users = T#game_table.users,
                  case length(Users) > 0 of 
                       true -> [H|_] = Users, 
                               {ok, Tab} = get_table(Id,User,H),
                               Tab#game_table.game_process ! {change_owner, H},
                               ?INFO("Change Ownership of ~p sent to ~p",[H,Tab#game_table.game_process]);
		       false-> nothing
                  end,     
		  stop;
         false -> ?INFO("DO NOTHING")
                  %chat_user_out(User#user.username),
                  %update_table_info(T),
    end
    end || T <- Tables ],
%    MyUser = get(user),
    wf:wire("window.close();").

join_user(User,Table) ->
    ?INFO("Comet Join: ~p",[{User#user.username,Table#game_table.id,Table#game_table.creator,Table#game_table.owner}]),
    Users = Table#game_table.users,
    List = lists:any(fun(A) -> A =:= User#user.username end, Users),
    ?INFO("Join Attempt User ~p to ~p ~p",[User#user.username,Table#game_table.users, 
                                         case List of true -> " was rejected"; false -> "going to be successful" end]),
    MaxUser = proplists:get_value(max_users, game_requirements(Table)),
    CurrentUser = length(Table#game_table.users),
    case CurrentUser < MaxUser of
         true ->  
                  NewTable2 = case List of 
                     false -> chat_user_in(User#user.username),
                              NewTable = Table#game_table{users=[User#user.username|Users]},
                              gproc:set_value({p,l,NewTable#game_table.game_process},NewTable),
                              ?INFO("Can Join: ~p User ~p to ~p",[not List,User#user.username,NewTable#game_table.users]),
                              wf:state(table,NewTable),
                              update_table_info(NewTable),
                              NewTable;
                     true ->  Table
                  end,
                  table_info(NewTable2);
         false -> table_info(Table)
    end.

add_robot() ->
    Id = wf:state_default(table_id, undefined),
    ?INFO("add robot"),
    case get_table(Id,wf:state(table)) of
         {ok, Table} ->
              MaxUser = proplists:get_value(max_users, game_requirements(Table)),
              CurrentUser = length(Table#game_table.users),
              case CurrentUser < MaxUser of
                   true ->
                       Users = Table#game_table.users,
                       NewTable = Table#game_table{users=[robot|Users]},
                       ?INFO("add robot Table: ~p",[{Table#game_table.game_process,NewTable}]),
                       wf:state(table,NewTable),
                       gproc:set_value({p,l,NewTable#game_table.game_process},NewTable),
                       update_table_info(NewTable);
                   false ->
                       chat_info(#span{class=error, text= ?_T("There are too many players.")})
              end;
          _ ->
              wf:wire(#alert { text=?_T("Error") })
    end.

kick_user(UserName) ->
    case wf:user() of
	undefined ->
	    wf:redirect_to_login(?_U("/login"));
	CUser ->
	    Id = wf:state(table_id),
 	    {ok, Table} = get_table(Id,wf:state(table)),
	    case Table#game_table.owner of
		CUser ->
		    case UserName of
			robot ->
			    Users = Table#game_table.users,
                            NewUsers = lists:delete(robot,Users),
                            NewTable = Table#game_table{users=NewUsers},
                            ?INFO("kick robot: ~p",[NewUsers]),
                            wf:state(table,NewTable),
                            gproc:set_value({p,l,NewTable#game_table.game_process},NewTable),
                            update_table_info(NewTable),
                            table_info(Table);
			CUser ->
                            Message =  ?_T("You where kicked."),
                            wf:session({Id,wf:user()},undefined),
                            wf:update(lightboxmsg, #span{text=Message}),
                            wf:wire(lightbox, #show{}),
                            wf:flush(),
      	                    stop;
                        Player ->
			    case nsm_users:get_user(UserName) of
			    {ok, _UserToLeave} ->
                                    Message = ?_TS("User ($user$) was kicked by ($owner$).",[{user, Player},{owner, CUser}]),
	        		    Users = Table#game_table.users,
                                    NewUsers = lists:delete(UserName,Users),
                                    NewTable = Table#game_table{users=NewUsers},
                                    ?INFO("kick user: ~p",[NewUsers]),
                                    wf:state(table,NewTable),
                                    gproc:set_value({p,l,NewTable#game_table.game_process},NewTable),
                               	    chat_info(Message),
                                    update_table_info(NewTable),
                 	      	    wf:flush(),
			            table_info(Table);
                		_ ->
	                	    chat_info(?_T("Error: user not found."))
		           end
		     end;
	    _ -> %% handle only targets
                 ok
	end
    end.

redirect_to_flex(GameId, HumanReadableGameName) ->
%    NewTable = wf:state(table),
    Id = wf:state(table_id),
%    InGame = wf:session(integer_to_list(Id)),
    wf:session(integer_to_list(Id), integer_to_list(GameId)),
    ?INFO("GameId: ~p, Id: ~p",[GameId,Id]),
    Url = lists:concat([?_U("/client"), "/", ?_U(HumanReadableGameName), "/id/", Id]),
    ?INFO("Starting Game: ~p",[Url]),
    wf:wire("window.started=true;"),
    wf:session({Id,wf:user()},undefined),
    wf:redirect(Url),
    wf:flush(),
    exit(kill_comet).

start_game() ->
%    Id = wf:state(table_id),
    Table = wf:state(table),
    io:fwrite("Table ID: ~p~n",[Table]),
    ?INFO("start game"),
    Owner = Table#game_table.owner,
    CUser = wf:user(),
    MaxUsers = proplists:get_value(max_users, game_requirements(Table)),
    MinUsers = proplists:get_value(min_users, game_requirements(Table)),
    CurrentUser = length(Table#game_table.users),
    ?INFO("StartGame: Owner ~p wf:User ~p (~p of ~p) ~p",[Owner,CUser,CurrentUser,MaxUsers,Table]),
    if Table#game_table.game_type == game_okey andalso (CurrentUser =< MaxUsers);
       (CurrentUser >= MinUsers) andalso (CurrentUser =< MaxUsers) ->
            case Table#game_table.creator of
                 CUser ->
                    io:fwrite("creating table: ~n", []),
                    UsersIdsAsBinaries = [ binarize_name(X) || X <- Table#game_table.users ],
                    Params = table_manager:game_table_to_settings(Table),
                    io:fwrite("params: ~p~n", [Params]),
                    GSPId = rpc:call(?GAMESRVR_NODE,
                                        game_manager,
                                        create_standalone_game,  %% Replace next line to use new standalone tables implementation
%%                                        create_table,
                                        [Table#game_table.game_type, Params, UsersIdsAsBinaries]),
                    ?INFO("GameManager Create Table Pid: ~p",[GSPId]),
                    case GSPId of
                        {ok, GaId, _GamePid} when is_integer(GaId) ->
                            chat_info(?_T("starting game...")),
                            wf:state(table, Table#game_table{gameid = GaId, game_state = started}),
                            Tables = get_tables(wf:state(table_id)),
                            ?INFO("Launch Flex: ~p",[Tables]),
                            [ Table2#game_table.game_process ! {launch_client, GaId, wf:q(game_name)}
                                      || Table2 <- Tables];
                        _Any ->
                            chat_info(#span{class=error, text= ?_T("Error in communication with game server")}),
                            wf:flush()
                    end;
                _ ->
%                    chat_info(#span{class=error, text= ?_T("Only owner can start the game")}),
                    update_table_info(Table)
            end;
        true ->
            chat_info(#span {class=error, text=?_T("You can't start game with this number of users.")}),
            update_table_info(Table)
    end.

update_table_info(undefined) -> ok;
update_table_info(ATable) ->
    Table = case ATable of
        unknown -> wf:update(lightboxmsg, #span{text=?_T("This table has been destroyed")}),
                   wf:wire(lightbox, #show{});
        Tables when is_list(ATable) ->
                   ?INFO("update_table_info:709 ~p",[Tables]),
                   [H|_T] = lists:foreach(fun(A)-> A#game_table.owner == wf:user() end, Tables),
                   H;
        Tab -> Tab
    end,
    CUser = wf:user(),
    OwnerName = Table#game_table.creator,
    GameType = Table#game_table.game_type,
    IsOwner = CUser == OwnerName,
    MaxUser = proplists:get_value(max_users, game_requirements(Table)),
    CurrentUser = length(Table#game_table.users),
    CurrentState = io_lib:fwrite("~b/~b", [CurrentUser, MaxUser]),
    ?INFO("Max Users: ~p",[MaxUser]),
    ?INFO("Current Users: ~p",[CurrentUser]),
    RobotsAllowed = Table#game_table.deny_robots == false,
    UsersList = [ html_user_info(Name,IsOwner,false,OwnerName,GameType) || Name <- Table#game_table.users ]
                 ++ case IsOwner andalso RobotsAllowed of
		           true -> [ html_user_info(robot,IsOwner,true,OwnerName,GameType) 
                                     || _  <- lists:seq(1,MaxUser-CurrentUser) ];
			   false -> ""
                    end, 
    ElUsersList = #list{body = UsersList},
    AgeLimit = case site_utils:as_str(Table#game_table.age_limit) of
        "undefined" -> 
            ?_T("Unknown");
        [From|[To|[]]] -> 
            integer_to_list(From) ++ "–" ++ integer_to_list(To)
    end,
    Info = [ show_row(?_T("Game speed") ++ ": ", site_utils:as_str(site_utils:game_speed_to_string(Table#game_table.game_speed))),
             show_row(?_T("Game mode") ++ ": ", site_utils:as_str(site_utils:game_mode_to_string(Table#game_table.game_mode))),
             show_row(?_T("Age limit") ++ ": ", AgeLimit),
             show_row(?_T("In group") ++ ": ", site_utils:as_str_list(Table#game_table.groups_only)),
             show_row(?_T("User count") ++ ": ", CurrentState)],
    wf:update(user_list, ElUsersList),
    wf:update(info, Info),
    wf:flush().

html_user_info(UserName, IsOwner, IsPlaceholder, OwnerName, GameType) ->
    IsRobot = (UserName == robot),
    Name =
	case IsRobot or IsPlaceholder of
	    true -> case GameType of
                       game_okey -> ?_T("Robot Player");
                       game_tavla -> ?_T("Robot Player");
                       _ -> ?_T("Waiting Player")
                    end;
	    false -> site_utils:linkify_name(UserName, parent)
	end,
    Buttons =
	case {IsPlaceholder, IsRobot, IsOwner and (UserName =/= OwnerName)} of
	    {false, false, true} ->
		[#br{}, #link{class="button", body=?_T("Kick from table"), postback={kick, UserName}}];
		%% TODO: add "Block" button as it on image
	    {false, true, true} ->
		#link{class="robo-button", body=?_T("Remove"), postback={kick, UserName}};
	    {true, _, true} ->
                case GameType of
                   game_okey -> #link{class="robo-button", text=?_T("Add"), postback=add_robot};
                   game_tavla -> #link{class="robo-button", text=?_T("Add"), postback=add_robot};
                   _ -> ""
                end;
	    _ -> ""
	end,

    {Class, Image} =
	case IsRobot of
	    true ->
		{"user robot", #image{class="avatar", image="/images/robot.png"}};
	    false ->
		%% TODO: get actual avatar
		{"user", #image{image=avatar:get_avatar_by_username(UserName, small), class=
            case nsm_accounts:user_paid(UserName) of
                true -> "table_paid_user_avatar";
                _ -> "avatar"
            end
        }}
	end,
    #listitem{body=
       #panel{class=Class,
	      body=[Image,#panel{class="info",body=[Name, Buttons]}]
	      }}.

% events
api_event(Name, Tag, Args)->
  webutils:api_event(Name, Tag, Args).

event(chat) ->
%    Id = wf:state_default(table_id, undefined),
    User = webutils:user_info(),
    Msg = wf:q(message_text_box),
    case string:strip(Msg) of
        "" -> ok;  % dont send empty messages
        Message ->
            case length(Message) > ?MAX_CHAT_LENGTH of
                true ->
                    chat_info(#span{class=error, text= ?_T("Message too long.")});
                false ->
                    Tables = get_tables(wf:state(table_id)),
                    [ begin ?INFO("Sent to ~p",[User#user.username]),
                            T#game_table.game_process ! {chat, User, Message}
                      end || T <- Tables],
                      wf:set(message_text_box, "")
            end
    end,
    wf:wire("obj('message_text_box').focus();");

event(add_robot) ->
    Tables = get_tables(wf:state(table_id)),
    [ T#game_table.game_process ! {add_robot} || T <- Tables];

event(start_game) ->
    Tables = get_tables(wf:state(table_id)),
    [ T#game_table.game_process ! {start_game} || T <- Tables ];

event({kick, User}) -> 
    Tables = get_tables(wf:state(table_id)),
    [ T#game_table.game_process ! {kick, User} || T <- Tables ];

event({leave_table, User, Id}) ->
    Tables = get_tables(Id),
    ?INFO("LEAVE button"),
    [ case T#game_table.owner == User of 
           true ->  T#game_table.game_process ! {leave, User, T},
                    ?INFO("leaving message sent to ~p of user ~p",[T#game_table.owner,User]),
                    T#game_table.game_process ! {leave_table, User, Id},
                    ?INFO("leave table message sent to ~p of user ~p",[T#game_table.owner,User]);
           false -> ?INFO("leaving message sent to ~p of user ~p",[T#game_table.owner,User]),
                    T#game_table.game_process ! {leave, User, T}
      end || T <- Tables ], close_window_after(1), wf:flush();
  
event(Unknown) ->
    ?INFO("Uknown Event: ~p",[Unknown]).

rss_container() -> [ #panel { class="silver rss", body=rss() } ].

rss() -> [ "Feeds" ].

game_type() -> list_to_existing_atom("game_"++wf:q('__submodule__')).
game_requirements(Table) -> 
    ?INFO("Game: ~p:~p",[Table#game_table.game_type,Table#game_table.game_mode]),
    rpc:call(?GAMESRVR_NODE,game_manager, get_requirements, [Table#game_table.game_type,Table#game_table.game_mode]).
binarize_name(robot) -> robot;
binarize_name(Name) -> list_to_binary(Name).

close_window_after(I) ->
    TimerText = ?_TS("Time before table will be closed: $sec$", [{sec, "~s"}]),
    TimerTextJS = wf:f("'"++wf:js_escape(TimerText)++"'", ["'+i+'"]), %% i is value
    wf:wire(wf:f(" function closeAfter(sec) { "
		 "   var i = sec + 1;"
		 "   var printagain = function() {"
                 "     if (--i) { objs('chat_history').append('<p>'+~s+'</p>'); setTimeout(printagain, 1000); }"
		 "     else { window.close(); }; "
                 "   }; "
		 "   printagain(); "
		 " }; closeAfter(~b); ", [TimerTextJS, wf:to_integer(I)])),
    ok.
