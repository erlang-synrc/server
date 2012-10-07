%% -*- mode: nitrogen -*-
-module (view_tournament).
-compile(export_all).

-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("nsm_db/include/user.hrl").
-include_lib("nsm_db/include/tournaments.hrl").
-include("elements/records.hrl").
-include("common.hrl").
-include("setup.hrl").

%% time to wait after heartbeat request to update userlist
-define(SLEEP, 35000). %% 35 sec

-define(MAX_CHAT_LENGTH, 1024). % 1024 bytes

-define(COMET_POOL, tournament_lobby).



main() ->
    case wf:user() /= undefined of
        true  -> main_authorized();
        false -> wf:redirect_to_login(?_U("/login"))
    end.


main_authorized() ->
    TournamentId = wf:q(id),
    UserInfo = webutils:user_info(),
    wf:state(tournament_id, TournamentId),

    %% user became ready automatically
    nsx_util_notification:notify_tournament_user_ready(TournamentId, UserInfo),

    TournamentInfo = nsm_tournaments:get(TournamentId),
    wf:state(tournament, TournamentInfo),

    start_comet(),

    #template { file=code:priv_dir(nsw_srv)++"/templates/bare.html" }.

title() ->
    webutils:title(?MODULE).

body() ->
    update_userlist(),

    #section{class="white-block", body=[
        chat(),
        #panel{class="cleafix"},
        counters(),
        #panel{id=notifications, body=[#notice{type=info, is_short=false, body=["participate!!!"]}]},
        #panel{id=users_placeholder, body= []}
    ]}.

chat() ->
    TID = wf:state(tournament_id),
    case nsm_tournaments:chat_history(TID) of
        H when is_list(H) ->
            add_chat_history(H);
        _ ->
            ok
    end,

    #panel{class="chat-section", body=[
        #panel { class="chat-wrapper", body=[
            #panel{class="chat-header", body=[
                #span{class=left},
                #span{class=right},
                #span{class=center, text=?_T("Chat")}
            ]},
            #panel{class="container", body=[
                #panel { id=chat_history, class="chat-history", style="overflow:auto; height: 170px;" },
                #textbox { id=message_text_box, class="chat-text", next=chat_send_button },
                #button { id=chat_send_button, class="chat-send", style="width: 75px;margin-right: 0;", text=?_T("Send"), postback=chat }
            ]}
        ]}
    ]}.


counters() ->
    {Y, M, D} = (wf:state(tournament))#tournament.start_date,
    Date = lists:concat([M,"/",D,"/",Y]),

    %% classes here needed for updation with comet. wfid_ prefix added to use standard
    %% nitrogen modification methods
    UsersCounter = wf:f("<strong class=\"wfid_user_count\">~p</strong>", [0]),
    StartDate    = wf:f("<strong class=\"wfid_date date\">~s</strong>", [Date]),
    TimeLeft     = wf:f("<strong class=\"wfid_countdown date\" id=\"time_left\">~s</strong>", ["89:34:22"]),

    #panel{class="info-bar info-bar-in", body=[UsersCounter, StartDate, TimeLeft]}.


users_tables(ReadyUsers, NotReadyUsers) ->
    #panel{class="table-area", body=[
        #panel{class=col, body=[
            #table{id=not_ready_table, class="table-heading", rows=[
                #tablerow{cells=[
                    #tableheader{class="col-01", body=?_T("User Avatar")},
                    #tableheader{body=?_T("User Name")},
                    #tableheader{body=?_T("Game Points")},
                    #tableheader{body=?_T("Skill Points")},
                    #tableheader{body=?_T("Ready")}
                ]},
                [user_row(false, User) || User <- NotReadyUsers]
            ]}
        ]},
        #panel{class=col, body=[
             #table{id=ready_table, class="table-heading", rows=[
                #tablerow{cells=[
                    #tableheader{class="col-01", body=?_T("User Avatar")},
                    #tableheader{body=?_T("User Name")},
                    #tableheader{body=?_T("Game Points")},
                    #tableheader{body=?_T("Skill Points")},
                    #tableheader{body=?_T("Ready")}
                ]},
                [user_row(true, User) || User <- ReadyUsers]
            ]}
        ]}
    ]}.


user_row(IsActive, User) ->
    StatusImage = case IsActive of
            true ->
                "/images/ico-004.gif";
            false ->
                "/images/ico-003.gif"
        end,

    #tablerow{id=row_id(User), cells=[
        #tablecell{class="col-01", body=#image{class="avatar", image=User#user.avatar, style="width:50px; height:50px"}},
        #tablecell{body=#link{url="#", text=User#user.username}},
        #tablecell{body="4500"},
        #tablecell{body="3421"},
        #tablecell{body=#image{image=StatusImage, style="width:16px; height:15px"}}]}.

row_id(User) ->
    lists:concat(["row_", erlang:phash2(User#user.name)]).


update_userlist() ->
    TID = wf:state(tournament_id),

    ActiveUsers       = nsm_tournaments:active_users(TID),
    NotActiveUsers    = not_active_users(TID, ActiveUsers),
    wf:update(users_placeholder, users_tables(ActiveUsers, NotActiveUsers)),
    wf:flush().


%% Events handlers
event(chat) ->
    User = wf:user(),
    TID = wf:state(tournament_id),
    Msg = wf:q(message_text_box),

    case string:strip(Msg) of
        "" ->
            ok;
        % dont send empty messages
        Message ->
            case length(Message) > ?MAX_CHAT_LENGTH of
                true ->
                    chat_info(#span{class=error, text= ?_T("Message too long.")});
                false ->
                    nsx_util_notification:notify_tournament_chat(
                        TID, "message", User, Msg),
                    wf:set(message_text_box, ""),
                    wf:wire("obj('message_text_box').focus();")
            end,
            wf:flush()
    end;

event(Other) ->
    webutils:event(Other).


add_chat_history(Messages) ->
    [process_chat(Action, User, Message) || {User, Action, Message} <- Messages].


process_chat("message", User, Message) ->
    chat_new_msg(User, Message).


chat_info(Info) ->
    Terms = #panel{class="info", body = Info},
    update_table_chat(Terms).

chat_error(Message) ->
    chat_info(#span{class=error, text= Message}).

chat_user_in(Username) ->
    Terms = #panel{class="user join",
		   body = [
			   ?_TS("User $username$ connected.", [{username, Username}])
			  ]},
    update_table_chat(Terms).

chat_user_out(Username) ->
    Terms = #panel{class="user left",
		   body = [
			   ?_TS("User $username$ has left.", [{username, Username}])
			  ]},
    update_table_chat(Terms).

chat_new_msg(User, Message) ->
    Terms = #panel{class="chat",
        body = [
            #span{class="username", text = User},
            ":&nbsp;", #span{text = Message}
    ]},
    update_table_chat(Terms).


update_table_chat(Terms) ->
    wf:insert_bottom(chat_history, Terms),
    wf:wire("obj('chat_history').scrollTop = obj('chat_history').scrollHeight;"),
    wf:flush(),
    ok.


start_comet() ->
    User = wf:user(),
    TournamentId = wf:state(tournament_id),

    ?INFO("start comet for ~p, previous comet pid: ~p", [User, wf:state(comet_pid)]),

    {ok, Pid} = wf:comet(fun()->
        ?PRINT("start comet"),
        CometProcess = self(),
        ?INFO("(in comet):start comet for User=~p, Tournament=~p pid: ~p", [wf:user(), wf:state(tournament_id), self()]),

        %% TODO: error handling when unable to subscribe
        (catch nsx_util_notification:subscribe_for_tournament(TournamentId, User, CometProcess)),
        comet_update(wf:user(), wf:state(tournament_id))
    end,  ?COMET_POOL),

    wf:state(comet_pid, Pid),

    ?INFO("start comet for User=~p, Tournament=~p pid: ~p", [wf:user(), wf:state(tournament_id), Pid]).


comet_update(User, TournamentId) ->
    receive
        {delivery, _, tournament_heartbeat} ->
            UserRecord = webutils:user_info(),

            nsx_util_notification:notify_tournament_heartbeat_reply(
                TournamentId, UserRecord),

            %% afer sleep send update userlist message to self
            timer:apply_after(?SLEEP, erlang, send, [self(), update_userlist]),
            ?PRINT({TournamentId, UserRecord, reply_to_heartbeat});

        %% start game section
        {delivery, ["tournament", TournamentId, User, "start_game"], Data}  ->
            ?INFO("(in comet): start game TId: ~p, User: ~p, Data: ~p", [TournamentId, User, Data]),
            Id = 10, % Game Id
            Url = lists:concat([?_U("/client"), "/", ?_U("okey"), "/id/", Id]),
            StartClient = webutils:new_window_js(Url),
            wf:wire(#script{script=StartClient}),
            wf:flush(),
            ?PRINT({"Start Game" ,TournamentId, User, Data});

        %% chat section
        {delivery, ["tournament", TournamentId, "chat", _Action], {UserName, Action, Message}}  ->
            process_chat(Action, UserName, Message);

        %% local request
        update_userlist ->
            ?PRINT({update_userlist, wf:user()}),
            update_userlist();

        {delivery, Route, Other}  ->
            ?PRINT({other_in_comet, Route, Other, wf:user()})
    end,

    comet_update(User, TournamentId).


not_active_users(TID, ActiveUsers) ->
    CurrentUser = wf:user(),
    %% get play records for tournament
    AllUsers = nsm_tournaments:joined_users(TID),

    AllUsersCount = length(AllUsers),
    wf:update(user_count, wf:to_list(AllUsersCount)),

    ActiveKeysSet0 = sets:from_list([U#user.username    || U <- ActiveUsers]),
    AllKeysSet     = sets:from_list([PR#play_record.who || PR <- AllUsers]),

    %% add user to active list
    ActiveKeysSet = sets:add_element(CurrentUser, ActiveKeysSet0),
    NotActiveKeys = sets:subtract(AllKeysSet, ActiveKeysSet),

    lists:foldl(
        fun(User, Acc) ->
            case nsm_users:get_user(User) of
                {ok, U} ->
                    [U | Acc];
                _ ->
                    ?WARNING("user not forund: ~p", [User]),
                    Acc
            end
        end, [], sets:to_list(NotActiveKeys)).
