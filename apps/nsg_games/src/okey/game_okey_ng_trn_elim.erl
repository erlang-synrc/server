%%% -------------------------------------------------------------------
%%% Author  : Sergii Polkovnikov <serge.polkovnikov@gmail.com>
%%% Description : The "Eliminate tournament" logic
%%%
%%% Created : Nov 02, 2012
%%% -------------------------------------------------------------------

%%% Terms explanation:
%%% GameId   - uniq identifier of the tournament. Type: integer().
%%% PlayerId - registration number of a player in the tournament. Type: integer()
%%% UserId   - cross system identifier of a physical user. Type: binary() (or string()?).
%%% TableId  - uniq identifier of a table in the tournament. Used by the
%%%          tournament logic. Type: integer().
%%% TableGlobalId - uniq identifier of a table in the system. Can be used
%%%          to refer to a table directly - without pointing to a tournament.
%%%          Type: integer()

-module(game_okey_ng_trn_elim).

-behaviour(gen_fsm).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("nsx_config/include/log.hrl").
-include_lib("nsg_srv/include/basic_types.hrl").
-include_lib("nsm_db/include/table.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([start/2, start_link/2, reg/2]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-export([table_message/3, client_message/2, client_request/2, client_request/3]).

-record(state,
        {
         game_id           :: pos_integer(),
         params            :: proplists:proplist(),
         bots_params       :: proplists:proplist(),
         turns_plan        :: list(integer()), %% Defines how many players will be passed to a next turn
         players,          %% The register of tournament players
         tables,           %% The register of tournament tables
         seats,            %% Stores relation between players and tables seats
         tournament_table  :: list(), %% XXX [{TurnNum, TurnRes}], TurnRes = [{PlayerId, Points}]
         table_id_counter  :: pos_integer(),
         turn              :: pos_integer(),
         cr_tab_requests   :: dict(),  %% {TableId, PlayersIds}
         reg_requests      :: dict(),  %% {PlayerId, From}
         tab_requests      :: dict(),  %% {RequestId, RequestContext}
         timer             :: undefined | reference(),
         timer_magic       :: undefined | reference(),
         tables_wl         :: list(), %% Tables waiting list
         tables_results    :: list()  %% [{TableId, TableResult}]
        }).

-record(player,
        {
         id              :: pos_integer(),
         user_id,
         user_info       :: #'PlayerInfo'{},
         is_bot          :: boolean()
        }).

-record(table,
        {
         id              :: pos_integer(),
         global_id       :: pos_integer(),
         pid,
         relay           :: {atom(), pid()}, %%{RelayMod, RelayPid}
         mon_ref,
         state           :: initializing | ready | in_process | finished,
         scoring_state,
         timer           :: reference()
        }).

-record(seat,
        {
         table           :: pos_integer(),
         seat_num        :: integer(),
         player_id       :: undefined | pos_integer(),
         registered_by_table :: undefined | boolean(),
         connected       :: undefined | boolean()
        }).


-define(STATE_INIT, state_init).
-define(STATE_WAITING_FOR_TABLES, state_waiting_for_tables).
-define(STATE_WAITING_FOR_PLAYERS, state_waiting_for_players).
-define(STATE_TURN_PROCESSING, state_turn_processing).
-define(STATE_SHOW_TURN_RESULT, state_show_turn_result).
-define(STATE_FINISHED, state_finished).

-define(TAB_MOD, game_okey_ng_table_trn).

-define(TABLE_STATE_INITIALIZING, initializing).
-define(TABLE_STATE_READY, ready).
-define(TABLE_STATE_IN_PROGRESS, in_progress).
-define(TABLE_STATE_FINISHED, finished).

-define(WAITING_PLAYERS_TIMEOUT, 15000). %% Time between all table was created and starting a turn
-define(REST_TIMEOUT, 20000). %% Time between game finsh and start of new round
-define(SHOW_TURN_RESULT_TIMEOUT, 15000).
-define(SHOW_TOURNAMENT_RESULT_TIMEOUT, 15000).

%% ====================================================================
%% External functions
%% ====================================================================

start(GameId, Params) ->
    gen_fsm:start(?MODULE, [GameId, Params, self()], []).

start_link(GameId, Params) ->
    gen_fsm:start_link(?MODULE, [GameId, Params, self()], []).

reg(Pid, User) ->
    client_request(Pid, {join, User}, 10000).

table_message(Pid, TableId, Message) ->
    gen_fsm:send_all_state_event(Pid, {table_message, TableId, Message}).

client_message(Pid, Message) ->
    gen_fsm:send_all_state_event(Pid, {client_message, Message}).

client_request(Pid, Message) ->
    client_request(Pid, Message, 5000).

client_request(Pid, Message, Timeout) ->
    gen_fsm:sync_send_all_state_event(Pid, {client_request, Message}, Timeout).


%% ====================================================================
%% Server functions
%% ====================================================================

init([GameId, Params, _Manager]) ->
    Registrants = get_param(registrants, Params),
    KakushPerRound = get_param(kakush_per_round, Params),

    RegistrantsNum = length(Registrants),
    {ok, {_, TurnsPlan, _}} = get_plan(KakushPerRound, RegistrantsNum),
    TableParams = table_parameters(?MODULE, self()),
    BotsParams = bots_parameters(),

    Players = setup_players(Registrants),
    PlayersIds = get_players_ids(Players),
    TTable = ttable_init(PlayersIds),
    ?INFO("OKEY_NG_TRN_ELIM <~p> started.  Pid:~p",[GameId, self()]),

    gen_fsm:send_all_state_event(self(), go),
    {ok, ?STATE_INIT, #state{game_id = GameId,
                             params = TableParams,
                             bots_params = BotsParams,
                             turns_plan = TurnsPlan,
                             players = Players,
                             tournament_table = TTable,
                             table_id_counter = 1
                            }}.

%%===================================================================
handle_event(go, ?STATE_INIT, #state{game_id = GameId} = StateData) ->
    GProcVal = #game_table{game_type = game_okey,
                           game_process = self(),
                           game_module = ?MODULE,
                           id = GameId,
                           age_limit = 100,
                           game_mode = undefined,
                           game_speed = undefined,
                           feel_lucky = false,
                           owner = undefined,
                           creator = undefined,
                           rounds = undefined,
                           pointing_rules   = [],
                           pointing_rules_ex = [],
                           users = [],
                           name = "Okey Elimination Tournament - " ++ erlang:integer_to_list(GameId) ++ " "
                          },
    gproc:reg({p,g,self()}, GProcVal),
    init_turn(1, StateData);

handle_event({client_message, Message}, StateName, StateData) ->
    handle_client_message(Message, StateName, StateData);

handle_event({table_message, TableId, Message}, StateName, StateData) ->
    handle_table_message(TableId, Message, StateName, StateData);

handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

handle_sync_event({client_request, Request}, From, StateName, StateData) ->
    handle_client_request(Request, From, StateName, StateData);

handle_sync_event(_Event, _From, StateName, StateData) ->
    Reply = ok,
    {reply, Reply, StateName, StateData}.

%%===================================================================

handle_info({'DOWN', MonRef, process, _Pid, _}, StateName,
            #state{game_id = GameId, tables = Tables} = StateData) ->
    case get_table_by_mon_ref(MonRef, Tables) of
        #table{id = TableId} ->
            ?INFO("OKEY_NG_TRN_ELIM <~p> Table <~p> is down. Stopping", [GameId, TableId]),
            %% TODO: More smart handling (failover) needed
            {stop, {one_of_tables_down, TableId}, StateName, StateData};
        not_found ->
            {next_state, StateName, StateData}
    end;


handle_info({rest_timeout, TableId}, StateName,
            #state{tables = Tables} = StateData) ->
    #table{pid = TablePid} = Table = fetch_table(TableId, Tables),
    NewTable = Table#table{state = in_process},
    NewTables = store_table(NewTable, Tables),
    send_to_table(TablePid, start_game),
    {next_state, StateName, StateData#state{tables = NewTables}};


handle_info({timeout, Magic}, ?STATE_WAITING_FOR_PLAYERS,
            #state{timer_magic = Magic} = StateData) ->
    start_turn(StateData);


handle_info({timeout, Magic}, ?STATE_SHOW_TURN_RESULT,
            #state{timer_magic = Magic, turn = Turn,
                   turns_plan = Plan} = StateData) ->
    if Turn == length(Plan) -> finalize_tournament(StateData);
       true -> init_turn(Turn + 1, StateData)
    end;


handle_info({timeout, Magic}, ?STATE_FINISHED = StateName,
            #state{timer_magic = Magic, tables = Tables} = StateData) ->
    finalize_tables_with_disconnect(Tables),
    {stop, normal, StateName, StateData#state{tables = [], seats = []}};


handle_info(_Info, StateName, StateData) ->
    {next_state, StateName, StateData}.

%%===================================================================

terminate(_Reason, _StateName, #state{game_id=GameId}=_StatData) ->
    ?INFO("OKEY_NG_TRN_ELIM <~p> Shutting down at state: <~p>. Reason: ~p",
          [GameId, _StateName, _Reason]),
    ok.

%%===================================================================

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------


handle_client_message(_Msg, StateName, StateData) ->
    {next_state, StateName, StateData}.

%%===================================================================

handle_table_message(TableId, {player_connected, PlayerId},
                     StateName,
                     #state{game_id = GameId, seats = Seats} = StateData) ->
    ?INFO("OKEY_NG_TRN_ELIM <~p> The player_connected notification received from "
          "table <~p>. PlayerId: <~p>", [GameId, TableId, PlayerId]),
    case find_seats_by_player_id(PlayerId, Seats) of
        [#seat{seat_num = SeatNum}] ->
            NewSeats = update_seat_connect_status(TableId, SeatNum, true, Seats),
            {next_state, StateName, StateData#state{seats = NewSeats}};
        [] -> %% Ignoring the message
            {next_state, StateName, StateData}
    end;


handle_table_message(TableId, {player_disconnected, PlayerId},
                     StateName, #state{game_id = GameId, seats = Seats} = StateData) ->
    ?INFO("OKEY_NG_TRN_ELIM <~p> The player_disconnected notification received from "
          "table <~p>. PlayerId: <~p>", [GameId, TableId, PlayerId]),
    case find_seats_by_player_id(PlayerId, Seats) of
        [#seat{seat_num = SeatNum}] ->
            NewSeats = update_seat_connect_status(TableId, SeatNum, false, Seats),
            {next_state, StateName, StateData#state{seats = NewSeats}};
        [] -> %% Ignoring the message
            {next_state, StateName, StateData}
    end;


handle_table_message(TableId, {table_created, Relay},
                     ?STATE_WAITING_FOR_TABLES,
                     #state{game_id = GameId, tables = Tables, seats = Seats,
                            cr_tab_requests = TCrRequests,
                            reg_requests = RegRequests} = StateData) ->
    ?INFO("OKEY_NG_TRN_ELIM <~p> The <table_created> notification received from table: ~p.",
          [GameId, TableId]),

    TabInitPlayers = dict:fetch(TableId, TCrRequests),
    NewTCrRequests = dict:erase(TableId, TCrRequests),
    %% Update status of players
    TabSeats = find_seats_by_table_id(TableId, Seats),
    F = fun(#seat{player_id = PlayerId} = S, Acc) ->
                case lists:member(PlayerId, TabInitPlayers) of
                    true -> store_seat(S#seat{registered_by_table = true}, Acc);
                    false -> Acc
                end
        end,
    NewSeats = lists:foldl(F, Seats, TabSeats),

    %% Process delayed registration requests
    TablePid = get_table_pid(TableId, Tables),
    F2 = fun(PlayerId, Acc) ->
                 case dict:find(PlayerId, Acc) of
                     {ok, From} ->
                         gen_fsm:reply(From, {ok, {PlayerId, Relay, {?TAB_MOD, TablePid}}}),
                         dict:erase(PlayerId, Acc);
                     error -> Acc
                 end
         end,
    NewRegRequests = lists:foldl(F2, RegRequests, TabInitPlayers),
    NewTables = update_created_table(TableId, Relay, Tables),
    case dict:size(NewTCrRequests) of
        0 -> 
            {TRef, Magic} = start_timer(?WAITING_PLAYERS_TIMEOUT),
            {next_state, ?STATE_WAITING_FOR_PLAYERS,
              StateData#state{tables = NewTables, seats = NewSeats, cr_tab_requests = NewTCrRequests,
                              reg_requests = NewRegRequests, timer = TRef, timer_magic = Magic}};
        _ -> {next_state, ?STATE_WAITING_FOR_TABLES,
              StateData#state{tables = NewTables, seats = NewSeats,
                              cr_tab_requests = NewTCrRequests, reg_requests = NewRegRequests}}
    end;


handle_table_message(TableId, {round_finished, NewScoringState},
                     ?STATE_TURN_PROCESSING,
                     #state{game_id = GameId, tables = Tables} = StateData) ->
    ?INFO("OKEY_NG_TRN_ELIM <~p> The <round_finished> notification received from table: ~p.",
          [GameId, TableId]),
    #table{pid = TablePid} = Table = fetch_table(TableId, Tables),
    TRef = erlang:send_after(?REST_TIMEOUT, self(), {rest_timeout, TableId}),
    NewTable = Table#table{scoring_state = NewScoringState, state = ?TABLE_STATE_FINISHED, timer = TRef},
    NewTables = store_table(NewTable, Tables),
    send_to_table(TablePid, show_round_result),
    {next_state, ?STATE_TURN_PROCESSING, StateData#state{tables = NewTables}};


handle_table_message(TableId, {game_finished, TableContext, Result},
                     ?STATE_TURN_PROCESSING = StateName,
                     #state{game_id = GameId, tables = Tables, tables_wl = WL,
                            tables_results = TablesResults} = StateData) ->
    ?INFO("OKEY_NG_TRN_ELIM <~p> The <round_finished> notification received from table: ~p.",
          [GameId, TableId]),
    NewTablesResults = [{TableId, Result} | TablesResults],
    #table{pid = TablePid} = Table = fetch_table(TableId, Tables),
    NewTable = Table#table{scoring_state = TableContext, state = ?TABLE_STATE_FINISHED},
    NewTables = store_table(NewTable, Tables),
    send_to_table(TablePid, show_round_result),
    %% TODO: Send to table "Waiting for the end of the turn"
    NewWL = lists:delete(TableId, WL),
    if NewWL == [] ->
           process_turn_result(StateData#state{tables = NewTables,
                                               tables_results = NewTablesResults,
                                               tables_wl = []});
       true ->
           {next_state, StateName, StateData#state{tables = NewTables,
                                                   tables_results = NewTablesResults,
                                                   tables_wl = NewWL}}
    end;


handle_table_message(TableId, {response, RequestId, Response},
                     StateName,
                     #state{game_id = GameId, tab_requests = TabRequests} = StateData) ->
    NewTabRequests = dict:erase(RequestId, TabRequests),
    case dict:find(RequestId, TabRequests) of
        {ok, ReqContext} ->
            ?INFO("OKEY_NG_TRN_ELIM <~p> The a response received from table <~p>. "
                  "RequestId: ~p. Request context: ~p. Response: ~p",
                  [GameId, TableId, RequestId, ReqContext, Response]),
            handle_table_response(ReqContext, Response, StateName,
                                  StateData#state{tab_requests = NewTabRequests});
        error ->
            ?ERROR("OKEY_NG_TRN_ELIM <~p> Table <~p> sent a response for unknown request. "
                   "RequestId: ~p. Response", []),
            {next_state, StateName, StateData#state{tab_requests = NewTabRequests}}
    end;


handle_table_message(_TableId, _Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

%%===================================================================
handle_table_response({register_player, PlayerId, TableId, SeatNum}, ok = _Response,
                      StateName,
                      #state{reg_requests = RegRequests, seats = Seats,
                             tables = Tables} = StateData) ->
    Seat = fetch_seat(TableId, SeatNum, Seats),
    NewSeats = store_seat(Seat#seat{registered_by_table = true}, Seats),
    %% Send response to a client for a delayed request
    NewRegRequests =
        case dict:find(PlayerId, RegRequests) of
            {ok, From} ->
                #table{relay = Relay, pid = TablePid} = fetch_table(TableId, Tables),
                gen_fsm:reply(From, {ok, {PlayerId, Relay, {?TAB_MOD, TablePid}}}),
                dict:erase(PlayerId, RegRequests);
            error -> RegRequests
        end,
    {next_state, StateName, StateData#state{seats = NewSeats,
                                            reg_requests = NewRegRequests}};

handle_table_response({replace_player, PlayerId, TableId, SeatNum}, ok = _Response,
                      StateName,
                      #state{reg_requests = RegRequests, seats = Seats,
                             tables = Tables} = StateData) ->
    Seat = fetch_seat(TableId, SeatNum, Seats),
    NewSeats = store_seat(Seat#seat{registered_by_table = true}, Seats),
    %% Send response to a client for a delayed request
    NewRegRequests =
        case dict:find(PlayerId, RegRequests) of
            {ok, From} ->
                #table{relay = Relay, pid = TablePid} = fetch_table(TableId, Tables),
                gen_fsm:reply(From, {ok, {PlayerId, Relay, {?TAB_MOD, TablePid}}}),
                dict:erase(PlayerId, RegRequests);
            error -> RegRequests
        end,
    {next_state, StateName, StateData#state{seats = NewSeats,
                                            reg_requests = NewRegRequests}}.
%%===================================================================

handle_client_request({join, User}, From, StateName,
                      #state{game_id = GameId, reg_requests = RegRequests,
                             seats = Seats, players=Players, tables = Tables} = StateData) ->
    #'PlayerInfo'{id = UserId, robot = _IsBot} = User,
    ?INFO("OKEY_NG_TRN_ELIM <~p> The Register request received from user: ~p.", [GameId, UserId]),
    case get_player_id_by_user_id(UserId, Players) of
        {ok, PlayerId} -> %% The user is a member of the tournament.
            %% TODO: Check the user is eliminated. If yes then reject join.
            [#seat{table = TableId, registered_by_table = RegByTable}] = find_seats_by_player_id(PlayerId, Seats),
            case RegByTable of
                false -> %% Store this request to the waiting pool
                    NewRegRequests = dict:store(PlayerId, From, RegRequests),
                    {next_state, StateName, StateData#state{reg_requests = NewRegRequests}};
                _ ->
                    #table{relay = Relay, pid = TPid} = fetch_table(TableId, Tables),
                    {reply, {ok, {PlayerId, Relay, {?TAB_MOD, TPid}}}, StateName, StateData}
            end;
        error -> %% Not a member
            ?INFO("OKEY_NG_TRN_ELIM <~p> User ~p is not a tournament member. "
                      "Rejecting join.", [GameId, UserId]),
            {reply, {error, not_allowed}, StateName, StateData}
    end;

handle_client_request(_Request, _From, StateName, StateData) ->
   Reply = {error, unexpected_request},
   {reply, Reply, StateName, StateData}.

%%===================================================================
init_turn(Turn, #state{game_id = GameId, turns_plan = Plan, tournament_table = TTable,
                       params = TableParams, players = Players,
                       table_id_counter = TableIdCounter, tables = OldTables} = StateData) ->
    PlayersList = prepare_players_for_new_turn(Turn, TTable, Plan, Players),
    {NewTables, Seats, NewTableIdCounter, CrRequests} =
        setup_tables(PlayersList, TableIdCounter, GameId, TableParams),
    if Turn > 1 -> finalize_tables_with_rejoin(OldTables);
       true -> do_nothing
    end,
    {ok, ?STATE_WAITING_FOR_TABLES, StateData#state{tables = NewTables,
                                                    seats = Seats,
                                                    table_id_counter = NewTableIdCounter,
                                                    turn = Turn,
                                                    cr_tab_requests = CrRequests,
                                                    reg_requests = dict:new(),
                                                    tab_requests = dict:new(),
                                                    tables_results = []
                                                   }}.


start_turn(#state{tables = Tables} = StateData) ->
    TablesList = tables_to_list(Tables),
    [send_to_table(Pid, start_round) || #table{pid = Pid} <- TablesList],
    F = fun(Table, Acc) ->
                store_table(Table#table{state = ?TABLE_STATE_IN_PROGRESS}, Acc)
        end,
    NewTables = lists:foldl(F, Tables, TablesList),
    WL = [T#table.id || T <- TablesList],
    {next_state, ?STATE_TURN_PROCESSING, StateData#state{tables = NewTables,
                                                         tables_wl = WL}}.


process_turn_result(#state{game_id = GameId, tournament_table = TTable,
                           turns_plan = Plan, turn = Turn, tables_results = TablesResults
                          } = StateData) ->
    ?INFO("OKEY_NG_TRN_ELIM <~p> turn completed. Starting results processing...", [GameId]),
    NextTurnLimit = lists:nth(Turn, Plan),
    TurnResult =  %% TODO: Redesign turns plan (add turn type)
        if  NextTurnLimit == 4 ->
                turn_result_all(TablesResults);
            true ->
                if Turn == 1 -> turn_result_per_table(NextTurnLimit, TablesResults);
                   true ->
                       case lists:nth(Turn - 1, Plan) of
                           4 -> turn_result_overall(NextTurnLimit, TablesResults);
                           _ -> turn_result_per_table(NextTurnLimit, TablesResults)
                       end
                end
        end,
    NewTTable = ttable_store_turn_result(Turn, TurnResult, TTable),
    {TRef, Magic} = start_timer(?SHOW_TURN_RESULT_TIMEOUT),
    {next_state, ?STATE_SHOW_TURN_RESULT, StateData#state{timer = TRef, timer_magic = Magic,
                                                          tournament_table = NewTTable}}.

finalize_tournament(StateData) ->
    %% TODO: Real finalization needed
    {TRef, Magic} = start_timer(?SHOW_TOURNAMENT_RESULT_TIMEOUT),
    {next_state, ?STATE_FINISHED, StateData#state{timer = TRef, timer_magic = Magic}}.

turn_result_all(TablesResults) ->
    F = fun({_, TableRes}, Acc) ->
            [{Pl, Points, active} || {Pl, Points} <- TableRes] ++ Acc
        end,
    lists:foldl(F, [], TablesResults).


turn_result_per_table(NextTurnLimit, TablesResults) ->
    F = fun({_, TableResult}, Acc) ->
                SortedRes = sort_results(TableResult),
                {Winners, _} = lists:unzip(lists:sublist(SortedRes, NextTurnLimit)),
                [case lists:member(Pl, Winners) of
                     true -> {Pl, Points, active};
                     false -> {Pl, Points, {out, eliminated}}
                 end || {Pl, Points} <- TableResult] ++ Acc
        end,
    lists:foldl(F, [], TablesResults).


turn_result_overall(NextTurnLimit1, TablesResults) ->
    NextTurnLimit = NextTurnLimit1 * length(TablesResults),
    F = fun({_, TableRes}, Acc) -> TableRes ++ Acc end,
    OverallResults = lists:foldl(F, [], TablesResults),
    SortedResults = sort_results(OverallResults),
    {Winners, _} = lists:unzip(lists:sublist(SortedResults, NextTurnLimit)),
    [case lists:member(Pl, Winners) of
         true -> {Pl, Points, active};
         false -> {Pl, Points, {out, eliminated}}
     end || {Pl, Points} <- OverallResults].


%% sort_results(Results) -> SortedResults
%% Types: Results = SortedResults = [{PlayerId, Points}]
%% Description: Sort the list from a best result to a lower one.
sort_results(Results) ->
    SF = fun({PId1, Points}, {PId2, Points}) -> PId1 =< PId2;
            ({_, Points1}, {_, Points2}) -> Points2 =< Points1
         end,
    lists:sort(SF, Results).



%% replace_player_by_bot(PlayerId, TableId, SeatNum,
%%                       #state{players = Players, seats = Seats,
%%                              game_id = GameId, bots_params = BotsParams,
%%                              player_id_counter = NewPlayerId, tables = Tables,
%%                              tab_requests = Requests} = StateData) ->
%%     NewPlayers = del_player(PlayerId, Players),
%%     [#'PlayerInfo'{id = UserId} = UserInfo] = spawn_bots(GameId, BotsParams, 1),
%%     NewPlayers2 = reg_player(#player{id = NewPlayerId, user_id = UserId, is_bot = true}, NewPlayers),
%%     NewSeats = assign_seat(TableId, SeatNum, NewPlayerId, true, false, false, Seats),
%%     TablePid = get_table_pid(TableId, Tables),
%%     NewRequests = table_req_replace_player(TablePid, NewPlayerId, UserInfo, TableId, SeatNum, Requests),
%%     {next_state, ?STATE_PROCESSING, StateData#state{players = NewPlayers2,
%%                                                     seats = NewSeats,
%%                                                     player_id_counter = NewPlayerId + 1,
%%                                                     tab_requests = NewRequests}}.
%% 

table_req_replace_player(TablePid, PlayerId, UserInfo, TableId, SeatNum, TabRequests) ->
    RequestId = make_ref(),
    NewRequests = dict:store(RequestId, {replace_player, PlayerId, TableId, SeatNum}, TabRequests),
    send_to_table(TablePid, {replace_player, RequestId, UserInfo, PlayerId, SeatNum}),
    NewRequests.



%% prepare_players_for_new_turn(Turn, TTable, TurnsPlan, Players) -> [{PlayerId, UserInfo, Points}]
prepare_players_for_new_turn(Turn, TTable, TurnsPlan, Players) ->
    PrevTurn = Turn - 1,
    TResult = ttable_get_turn_result(PrevTurn, TTable),
    if Turn == 1 ->
           [{PlayerId, get_user_info(PlayerId, Players), _Points = 0}
            || {PlayerId, _, active} <- TResult];
       true ->
           case lists:nth(PrevTurn, TurnsPlan) of
               4 -> %% No one was eliminated => using the prev turn points
                   [{PlayerId, get_user_info(PlayerId, Players), Points}
                    || {PlayerId, Points, active} <- TResult];
               _ ->
                   [{PlayerId, get_user_info(PlayerId, Players), _Points = 0}
                    || {PlayerId, _, active} <- TResult]
           end
    end.


%% setup_tables(Players, TableIdCounter, GameId, TableParams) ->
%%                              {Tables, Seats, NewTableIdCounter, CrRequests}
%% Types: Players = {PlayerId, UserInfo, Points}
setup_tables(Players, TableIdCounter, GameId, TableParams) ->
    SPlayers = shuffle(Players),
    Groups = split_by_num(4, SPlayers),
    F = fun(Group, {TAcc, SAcc, TableId, TCrRequestsAcc}) ->
                {TPlayers, _} = lists:mapfoldl(fun({PlayerId, UserInfo, Points}, SeatNum) ->
                                                       {{PlayerId, UserInfo, SeatNum, Points}, SeatNum+1}
                                               end, 1, Group),
                TableParams2 = [{players, TPlayers} | TableParams],
                {ok, TabPid} = spawn_table(GameId, TableId, TableParams2),
                MonRef = erlang:monitor(process, TabPid),
                GlobalTableId = 0, Scoring = undefined, %% FIXME: Table global id should use a persistent counter
                NewTAcc = reg_table(TableId, TabPid, MonRef, GlobalTableId, Scoring, TAcc),
                F2 = fun({PlId, SNum}, Acc) ->
                             RegByTable = false, Connected = false,
                             assign_seat(TableId, SNum, PlId, RegByTable, Connected, Acc)
                     end,
                NewSAcc = lists:foldl(F2, SAcc, TPlayers),
                PlayersIds = [PlayerId || {PlayerId, _, _} <- Group],
                NewTCrRequestsAcc = dict:store(TableId, PlayersIds, TCrRequestsAcc),
                {NewTAcc, NewSAcc, TableId + 1, NewTCrRequestsAcc}
        end,
    lists:foldl(F, {tables_init(), seats_init(), TableIdCounter, dict:new()}, Groups).


%% setup_players(Registrants) -> Players
setup_players(Registrants) ->
    F = fun(UserId, {Acc, PlayerId}) ->
                {ok, UserInfo} = auth_server:get_user_info_by_user_id(UserId),
                NewAcc = reg_player(#player{id = PlayerId, user_id = UserId,
                                            user_info = UserInfo}, Acc),
                {NewAcc, PlayerId + 1}
        end,
    {Players, _} = lists:foldl(F, {players_init(), 1}, Registrants),
    Players.


%% finalize_tables_with_rejoin(Tables) -> ok
finalize_tables_with_rejoin(Tables) ->
    F = fun(#table{mon_ref = MonRef, pid = TablePid}) ->
                erlang:demonitor(MonRef, [flush]),
                send_to_table(TablePid, rejoin_players),
                send_to_table(TablePid, stop)
        end,
    lists:foreach(F, tables_to_list(Tables)).

%% finalize_tables_with_rejoin(Tables) -> ok
finalize_tables_with_disconnect(Tables) ->
    F = fun(#table{mon_ref = MonRef, pid = TablePid}) ->
                erlang:demonitor(MonRef, [flush]),
                send_to_table(TablePid, disconnect_players),
                send_to_table(TablePid, stop)
        end,
    lists:foreach(F, tables_to_list(Tables)).


%% ttable_init(PlayersIds) -> TTable
%% Types: TTable = [{Turn, TurnResult}]
ttable_init(PlayersIds) -> [{0, [{Id, 0, active} || Id <- PlayersIds]}].

%% ttable_get_turn_result(Turn, TTable) -> undefined | TurnResult
%% Types: TurnResult = [{PlayerId, Points, PlayerState}]
%%          PlayerState = undefined | active | {out, Reason}, Reason = atom()
ttable_get_turn_result(Turn, TTable) ->
    proplists:get_value(Turn, TTable).

%% ttable_store_turn_result(Turn, TurnResult, TTable) -> NewTTable
ttable_store_turn_result(Turn, TurnResult, TTable) ->
    lists:keystore(Turn, 1, TTable, {Turn, TurnResult}).


%% players_init() -> players()
players_init() -> midict:new().

%% reg_player(#player{}, Players) -> NewPlayers
reg_player(#player{id =Id, user_id = UserId} = Player, Players) ->
    midict:store(Id, Player, [{user_id, UserId}], Players).

get_players_ids(Players) ->
    [P#player.id || P <- players_to_list(Players)].

get_player_id_by_user_id(UserId, Players) ->
    case midict:geti(UserId, user_id, Players) of
        [#player{id = PlayerId}] -> {ok, PlayerId};
        [] -> error
    end.

%% del_player(PlayerId, Players) -> NewPlayers
del_player(PlayerId, Players) -> midict:erase(PlayerId, Players).

%% del_player(PlayersIds, Players) -> NewPlayers
del_players([], Players) -> Players;
del_players([PlayerId | Rest], Players) ->
    del_players(Rest, del_player(PlayerId, Players)).

%% players_to_list(Players) -> List
players_to_list(Players) -> midict:all_values(Players).

get_user_info(PlayerId, Players) ->
    #player{user_id = UserId} = midict:fetch(PlayerId, Players),
    UserId.


tables_init() -> midict:new().

reg_table(TableId, Pid, MonRef, GlobalId, Scoring, Tables) ->
    Table = #table{id = TableId, pid = Pid, mon_ref = MonRef, global_id = GlobalId,
                   state = initializing, scoring_state = Scoring},
    store_table(Table, Tables).

update_created_table(TableId, Relay, Tables) ->
    Table = midict:fetch(TableId, Tables),
    NewTable = Table#table{relay = Relay, state = ?TABLE_STATE_READY},
    store_table(NewTable, Tables).

store_table(#table{id = TableId, pid = Pid, mon_ref = MonRef, global_id = GlobalId} = Table, Tables) ->
    midict:store(TableId, Table, [{pid, Pid}, {global_id, GlobalId}, {mon_ref, MonRef}], Tables).

fetch_table(TableId, Tables) -> midict:fetch(TableId, Tables).

get_table_pid(TabId, Tables) ->
    {ok, #table{pid = TabPid}} = midict:find(TabId, Tables),
    TabPid.

del_table(TabId, Tables) -> midict:erase(TabId, Tables).

get_table_by_mon_ref(MonRef, Tables) ->
    case midict:geti(MonRef, mon_ref, Tables) of
        [Table] -> Table;
        [] -> not_found
    end.

tables_to_list(Tables) -> midict:all_values(Tables).

set_table_state(TableId, State, Tables) ->
    Table = midict:fetch(TableId, Tables),
    store_table(Table#table{state = State}, Tables).

seats_init() -> midict:new().

find_seats_by_player_id(PlayerId, Seats) ->
    midict:geti(PlayerId, player_id, Seats).

find_seats_by_table_id(TabId, Seats) ->
    midict:geti(TabId, table_id, Seats).

is_all_players_connected(TableId, TableSeatsNum, Seats) ->
    TableSeatsNum == length(midict:geti(true, {connected, TableId}, Seats)).

fetch_seat(TableId, SeatNum, Seats) -> midict:fetch({TableId, SeatNum}, Seats).

%% assign_seat(TabId, SeatNum, PlayerId, RegByTable, Connected, Seats) -> NewSeats
%% PlayerId = integer()
%% RegByTable = Connected = undefined | boolean()
assign_seat(TabId, SeatNum, PlayerId, RegByTable, Connected, Seats) ->
    Seat = #seat{table = TabId, seat_num = SeatNum, player_id = PlayerId,
                 registered_by_table = RegByTable, connected = Connected},
    store_seat(Seat, Seats).

update_seat_connect_status(TableId, SeatNum, ConnStatus, Seats) ->
    Seat = midict:fetch({TableId, SeatNum}, Seats),
    NewSeat = Seat#seat{connected = ConnStatus},
    store_seat(NewSeat, Seats).

store_seat(#seat{table = TabId, seat_num = SeatNum, player_id = PlayerId,
                 registered_by_table = _RegByTable,
                 connected = Connected} = Seat, Seats) ->
    Indices = if PlayerId == undefined ->
                     [{table_id, TabId}, {free, true}, {free_at_tab, TabId}];
                 true ->
                     [{table_id, TabId}, {free, false}, {non_free_at_tab, TabId},
                      {player_id, PlayerId}, {{connected, TabId}, Connected}]
              end,
    midict:store({TabId, SeatNum}, Seat, Indices, Seats).


shuffle(List) ->
    deck:to_list(deck:shuffle(deck:from_list(List))).

split_by_num(Num, List) ->
    split_by_num(Num, List, []).

split_by_num(_, [], Acc) -> lists:reverse(Acc);
split_by_num(Num, List, Acc) ->
    {Group, Rest} = lists:split(Num, List),
    split_by_num(Num, Rest, [Group | Acc]).

%% start_timer(Timeout) -> {TRef, Magic}
start_timer(Timeout) ->
    Magic = make_ref(),
    TRef = erlang:send_after(Timeout, self(), {timeout, Magic}),
    {TRef, Magic}.

spawn_bots(GameId, Params, BotsNum) ->
    spawn_bots(GameId, Params, BotsNum, []).

spawn_bots(_GameId, _Params, 0, Acc) -> Acc;
spawn_bots(GameId, Params, BotsNum, Acc) ->
    UserInfo = spawn_bot(bot_module(Params), GameId),
    spawn_bots(GameId, Params, BotsNum-1, [UserInfo | Acc]).

spawn_bot(BM, GameId) ->
    {NPid, _SPid, _NUId, User} = create_robot(BM, GameId),
    BM:join_game(NPid),
    User.

create_robot(BM, GameId) ->
    User = auth_server:robot_credentials(),
    NUId = User#'PlayerInfo'.id,
    {ok, NPid} = BM:start_link(self(), User, GameId),
    SPid = BM:get_session(NPid),
    {NPid, SPid, NUId, User}.

spawn_table(GameId, TableId, Params) ->
    Pid = ?TAB_MOD:start(GameId, TableId, Params),
    Pid.

send_to_table(TabPid, Message) ->
    ?TAB_MOD:parent_message(TabPid, Message).

%% table_parameters(ParentMod, ParentPid) -> Proplist
table_parameters(ParentMod, ParentPid) ->
    [
     {parent, {ParentMod, ParentPid}},
     {seats_num, 4},
%%     {players, []},
     {table_name, ""},
     {mult_factor, 1},
     {slang_allowed, false},
     {observers_allowed, false},
     {speed, normal},
     {game_type, standard},
     {rounds, 10},
     {reveal_confirmation, true},
     {pause_mode, normal}
    ].

%% bots_parameters() -> Proplist
bots_parameters() ->
    [
     {game, game_okey},
     {game_mode, standard},
     {lucky, true},
     {speed, normal},
     {rounds, 10}
    ].

seats_num(TableParams) ->
    proplists:get_value(seats_num, TableParams).

bot_module(TableParams) ->
    case proplists:get_value(game, TableParams) of
        game_okey -> game_okey_bot
    end.
get_param(ParamId, Params) ->
    {_, Value} = lists:keyfind(ParamId, 1, Params),
    Value.

get_plan(KakushPerRound, RegistrantsNum) ->
    case lists:keyfind({KakushPerRound, RegistrantsNum}, 1, tournament_matrix()) of
        fasle -> {error, no_such_plan};
        Plan -> {ok, Plan}
    end.

tournament_matrix() ->
    [%% Kakush Pl.No   1  2  3  4  5  6  7
     { {  8,   16},   [4, 1, 1],                        0 },
     { { 10,   16},   [4, 1, 1],                        0 },
     { {  2,   64},   [4, 1, 1, 1],                     0 },
     { {  4,   64},   [4, 1, 1, 1],                     0 },
     { {  6,   64},   [4, 1, 1, 1],                     0 },
     { {  8,   64},   [4, 1, 1, 1],                     0 },
     { { 10,   64},   [4, 1, 1, 1],                     0 },
     { {  2,  128},   [2, 2, 2, 1, 1],                  0 },
     { {  4,  128},   [2, 2, 2, 1, 1],                  0 },
     { {  6,  128},   [2, 2, 2, 1, 1],                  0 },
     { {  8,  128},   [2, 2, 2, 1, 1],                  0 },
     { { 10,  128},   [2, 2, 2, 1, 1],                  0 },
     { {  2,  256},   [4, 1, 1, 1, 1],                  0 },
     { {  4,  256},   [4, 1, 1, 1, 1],                  0 },
     { {  6,  256},   [4, 1, 1, 1, 1],                  0 },
     { {  8,  256},   [4, 1, 1, 1, 1],                  0 },
     { { 10,  256},   [4, 1, 1, 1, 1],                  0 },
     { {  2,  256},   [4, 2, 4, 2, 1, 1, 1],            0 },
     { {  4,  256},   [4, 2, 4, 2, 1, 1, 1],            0 },
     { {  6,  256},   [4, 2, 4, 2, 1, 1, 1],            0 },
     { {  8,  256},   [4, 2, 4, 2, 1, 1, 1],            0 },
     { { 10,  256},   [4, 2, 4, 2, 1, 1, 1],            0 },
     { {  2,  512},   [2, 2, 2, 1, 1, 1],               0 },
     { {  4,  512},   [2, 2, 2, 1, 1, 1],               0 },
     { {  6,  512},   [2, 2, 2, 1, 1, 1],               0 },
     { {  8,  512},   [2, 2, 2, 1, 1, 1],               0 },
     { { 10,  512},   [2, 2, 2, 1, 1, 1],               0 },
     { {  2,  512},   [4, 2, 4, 2, 2, 1, 1, 1],         0 },
     { {  4,  512},   [4, 2, 4, 2, 2, 1, 1, 1],         0 },
     { {  6,  512},   [4, 2, 4, 2, 2, 1, 1, 1],         0 },
     { {  8,  512},   [4, 2, 4, 2, 2, 1, 1, 1],         0 },
     { { 10,  512},   [4, 2, 4, 2, 2, 1, 1, 1],         0 },
     { {  2, 1024},   [4, 1, 1, 1, 1, 1],               0 },
     { {  4, 1024},   [4, 1, 1, 1, 1, 1],               0 },
     { {  6, 1024},   [4, 1, 1, 1, 1, 1],               0 },
     { {  8, 1024},   [4, 1, 1, 1, 1, 1],               0 },
     { { 10, 1024},   [4, 1, 1, 1, 1, 1],               0 },
     { {  2, 1024},   [4, 2, 4, 2, 1, 1, 1, 1],         0 },
     { {  4, 1024},   [4, 2, 4, 2, 1, 1, 1, 1],         0 },
     { {  6, 1024},   [4, 2, 4, 2, 1, 1, 1, 1],         0 },
     { {  8, 1024},   [4, 2, 4, 2, 1, 1, 1, 1],         0 },
     { { 10, 1024},   [4, 2, 4, 2, 1, 1, 1, 1],         0 },
     { {  2, 2048},   [2, 1, 2, 1, 1, 1],               0 },
     { {  4, 2048},   [2, 1, 2, 1, 1, 1],               0 },
     { {  6, 2048},   [2, 1, 2, 1, 1, 1],               0 },
     { {  8, 2048},   [2, 1, 2, 1, 1, 1],               0 },
     { { 10, 2048},   [2, 1, 2, 1, 1, 1],               0 },
     { {  2, 2048},   [4, 2, 2, 2, 1, 1, 1, 1],         0 },
     { {  4, 2048},   [4, 2, 2, 2, 1, 1, 1, 1],         0 },
     { {  6, 2048},   [4, 2, 2, 2, 1, 1, 1, 1],         0 }
  ].
