%%% -------------------------------------------------------------------
%%% Author  : Sergei Polkovnikov <serge.polkovnikov@gmail.com>
%%% Description : The "Stand alone table" logic
%%%
%%% Created : Nov 19, 2012
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

-module(game_okey_ng_trn_standalone).

-behaviour(gen_fsm).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("nsx_config/include/log.hrl").
-include_lib("nsg_srv/include/basic_types.hrl").
-include_lib("nsm_db/include/table.hrl").
-include_lib("nsm_db/include/accounts.hrl").
-include_lib("eunit/include/eunit.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([start/2, start_link/2, reg/2]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-export([table_message/3, client_message/2, client_request/2, client_request/3]).

-record(state,
        {%% Static values
         game_id           :: pos_integer(),
         trn_id            :: term(),
         game              :: atom(),
         game_mode         :: atom(),
         seats_per_table   :: integer(),
         params            :: proplists:proplist(),
         table_module      :: atom(),
         bot_module        :: atom(),
         quota_per_round   :: integer(),
         kakush_for_winners :: integer(),
         kakush_for_loser  :: integer(),
         win_game_points   :: integer(),
         mul_factor        :: integer(),
         registrants       :: [robot | binary()],
         initial_points    :: integer(),
         common_params     :: proplists:proplist(),
         %% Dinamic values
         players,          %% The register of tournament players
         tables,           %% The register of tournament tables
         seats,            %% Stores relation between players and tables seats
         table_id_counter  :: pos_integer(),
         player_id_counter :: pos_integer(),
         cur_table         :: pos_integer(),
         tour              :: pos_integer(),
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
         pid             :: pid(),
         relay           :: {atom(), pid()}, %% {RelayMod, RelayPid}
         mon_ref         :: reference(),
         state           :: initializing | ready | in_process | finished,
         context         :: term(), %% Context term of a table. For failover proposes.
         timer           :: reference()
        }).

-record(seat,
        {
         table           :: pos_integer(),
         seat_num        :: integer(),
         player_id       :: undefined | pos_integer(),
         registered_by_table :: undefined | boolean(),
         connected       :: undefined | boolean(),
         free            :: boolean()
        }).


-define(STATE_INIT, state_init).
-define(STATE_WAITING_FOR_TABLES, state_waiting_for_tables).
-define(STATE_EMPTY_SEATS_FILLING, state_empty_seats_filling).
-define(STATE_WAITING_FOR_PLAYERS, state_waiting_for_players).
-define(STATE_SET_PROCESSING, state_set_processing).
-define(STATE_SET_FINISHED, state_set_finished).
-define(STATE_SHOW_SET_RESULT, state_show_set_result).
-define(STATE_FINISHED, state_finished).

-define(TOURNAMENT_TYPE, standalone).

-define(TABLE_STATE_INITIALIZING, initializing).
-define(TABLE_STATE_READY, ready).
-define(TABLE_STATE_IN_PROGRESS, in_progress).
-define(TABLE_STATE_FINISHED, finished).

-define(WAITING_PLAYERS_TIMEOUT, 3000) . %% Time between a table was created and start of first round
-define(REST_TIMEOUT, 5000).             %% Time between a round finish and start of a new one
-define(SHOW_SET_RESULT_TIMEOUT, 15000). %% Time between a set finish and start of a new one
-define(SHOW_TOURNAMENT_RESULT_TIMEOUT, 15000). %% Time between last tour result showing and the tournament finish

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
    ?INFO("OKEY_NG_TRN_STANDALONE <~p> Init started",[GameId]),
    Registrants =   get_param(registrants, Params),
    SeatsPerTable = get_param(seats, Params),
    Game =          get_param(game, Params),
    GameMode =      get_param(game_mode, Params),
    QuotaPerRound = get_param(quota_per_round, Params),
    KakushForWinners = get_param(kakush_for_winners, Params),
    KakushForLoser = get_param(kakush_for_loser, Params),
    WinGamePoints = get_param(win_game_points, Params),
    MulFactor =     get_param(mul_factor, Params),
    TableParams =   get_param(table_params, Params),
    TableModule =   get_param(table_module, Params),
    BotModule =     get_param(bot_module, Params),
    InitialPoints = get_param(initial_points, Params),
    CommonParams  = get_param(common_params, Params),


    ?INFO("OKEY_NG_TRN_STANDALONE <~p> Inital points:~p", [GameId, InitialPoints]),
    ?INFO("OKEY_NG_TRN_STANDALONE <~p> started.  Pid:~p", [GameId, self()]),
    gen_fsm:send_all_state_event(self(), go),
    {ok, ?STATE_INIT, #state{game_id = GameId,
                             game = Game,
                             game_mode = GameMode,
                             seats_per_table = SeatsPerTable,
                             params = TableParams,
                             table_module = TableModule,
                             bot_module = BotModule,
                             quota_per_round = QuotaPerRound,
                             kakush_for_winners = KakushForWinners,
                             kakush_for_loser = KakushForLoser,
                             win_game_points = WinGamePoints,
                             mul_factor = MulFactor,
                             registrants = Registrants,
                             initial_points = InitialPoints,
                             table_id_counter = 1,
                             common_params = CommonParams
                            }}.

%%===================================================================
handle_event(go, ?STATE_INIT, #state{game_id = GameId,
                                     registrants = Registrants, bot_module = BotModule,
                                     common_params = CommonParams} = StateData) ->
    ?INFO("OKEY_NG_TRN_STANDALONE <~p> Received a directive to starting the tournament.", [GameId]),
    DeclRec = create_decl_rec(CommonParams, GameId, Registrants),
    gproc:reg({p,l,self()}, DeclRec),
    {Players, PlayerIdCounter} = setup_players(Registrants, GameId, BotModule),
    NewStateData = StateData#state{players = Players,
                                   player_id_counter = PlayerIdCounter},
    init_tour(1, NewStateData);

handle_event({client_message, Message}, StateName, #state{game_id = GameId} = StateData) ->
    ?INFO("OKEY_NG_TRN_STANDALONE <~p> Received the message from a client: ~p.", [GameId, Message]),
    handle_client_message(Message, StateName, StateData);

handle_event({table_message, TableId, Message}, StateName, #state{game_id = GameId} = StateData) ->
    ?INFO("OKEY_NG_TRN_STANDALONE <~p> Received the message from table <~p>: ~p.", [GameId, TableId, Message]),
    handle_table_message(TableId, Message, StateName, StateData);

handle_event(Message, StateName, #state{game_id = GameId} = StateData) ->
    ?INFO("OKEY_NG_TRN_STANDALONE <~p> Unhandled message(event) received in state <~p>: ~p.",
          [GameId, StateName, Message]),
    {next_state, StateName, StateData}.

handle_sync_event({client_request, Request}, From, StateName, #state{game_id = GameId} = StateData) ->
    ?INFO("OKEY_NG_TRN_STANDALONE <~p> Received the request from a client: ~p.", [GameId, Request]),
    handle_client_request(Request, From, StateName, StateData);

handle_sync_event(Request, From, StateName, #state{game_id = GameId} = StateData) ->
    ?INFO("OKEY_NG_TRN_STANDALONE <~p> Unhandled request(event) received in state <~p> from ~p: ~p.",
          [GameId, StateName, From, Request]),
    {reply, {error, unknown_request}, StateName, StateData}.

%%===================================================================

handle_info({'DOWN', MonRef, process, _Pid, _}, StateName,
            #state{game_id = GameId, tables = Tables} = StateData) ->
    case get_table_by_mon_ref(MonRef, Tables) of
        #table{id = TableId} ->
            ?INFO("OKEY_NG_TRN_STANDALONE <~p> Table <~p> is down. Stopping", [GameId, TableId]),
            %% TODO: More smart handling (failover) needed
            {stop, {one_of_tables_down, TableId}, StateData};
        not_found ->
            {next_state, StateName, StateData}
    end;


handle_info({rest_timeout, TableId}, ?STATE_SET_PROCESSING = StateName,
            #state{game_id = GameId, game = Game, game_mode = GameMode,
                   quota_per_round = Amount, mul_factor = MulFactor, tables = Tables,
                   players = Players, seats = Seats, cur_table = TableId, bot_module = BotModule,
                   player_id_counter = PlayerIdCounter, tab_requests = Requests,
                   table_module = TableMod, common_params = CommonParams} = StateData) ->
    ?INFO("OKEY_NG_TRN_STANDALONE <~p> Time to start new round for table <~p>.", [GameId, TableId]),
    Disconnected = find_disconnected_players(TableId, Seats),
    ConnectedRealPlayers = [PlayerId || #player{id = PlayerId, is_bot = false} <- players_to_list(Players),
                                        not lists:member(PlayerId, Disconnected)],
    case ConnectedRealPlayers of
        [] -> %% Finish game
            ?INFO("OKEY_NG_TRN_STANDALONE <~p> No real players left in table <~p>. "
                  "Stopping the game.", [GameId, TableId]),
            finalize_tables_with_disconnect(TableMod, Tables),
            {stop, normal, StateData#state{tables = [], seats = []}};
        _ -> %% Replace disconnected players by bots
            ?INFO("OKEY_NG_TRN_STANDALONE <~p> Initiating new round at table <~p>.", [GameId, TableId]),
            {Replacements, NewPlayers, NewSeats, NewPlayerIdCounter} =
                replace_by_bots(Disconnected, GameId, BotModule, TableId, Players, Seats, PlayerIdCounter),
            #table{pid = TablePid} = Table = fetch_table(TableId, Tables),
            NewTables = store_table(Table#table{state = ?TABLE_STATE_IN_PROGRESS}, Tables),
            RealUsersIds = [UserId || #player{user_id = UserId, is_bot = false} <- players_to_list(NewPlayers)],
            deduct_quota(GameId, Game, GameMode, Amount, MulFactor, RealUsersIds),
            NewRequests = table_req_replace_players(TableMod, TablePid, TableId, Replacements, Requests),
            send_to_table(TableMod, TablePid, start_round),
            Users = [if Bot -> robot; true -> UserId end || #player{user_id = UserId, is_bot = Bot} <- players_to_list(NewPlayers)],
            DeclRec = create_decl_rec(CommonParams, GameId, Users),
            gproc:set_value({p,l,self()}, DeclRec),
            {next_state, StateName, StateData#state{tables = NewTables, players = NewPlayers, seats = NewSeats,
                                                    tab_requests = NewRequests, player_id_counter = NewPlayerIdCounter}}
    end;


handle_info({rest_timeout, TableId}, ?STATE_SET_FINISHED,
            #state{game_id = GameId, game = GameType, game_mode = GameMode,
                   tables_results = TablesResults, tables = Tables,
                   players = Players, cur_table = TableId, table_module = TableMod,
                   kakush_for_winners = KakushForWinners, kakush_for_loser = KakushForLoser,
                   win_game_points = WinGamePoints, mul_factor = MulFactor} = StateData) ->
    ?INFO("OKEY_NG_TRN_STANDALONE <~p> Time to determinate set results (table: <~p>).", [GameId, TableId]),
    #table{pid = TablePid} = fetch_table(TableId, Tables),
    {_, TableScore} = lists:keyfind(TableId, 1, TablesResults),
    SeriesResult = series_result(TableScore),
    ?INFO("OKEY_NG_TRN_STANDALONE <~p> Set result: ~p", [GameId, SeriesResult]),
    send_to_table(TableMod, TablePid, {show_series_result, SeriesResult}),
    SeriesResult1 = [{PlayerId, Status} || {PlayerId, _Pos, _Points, Status} <- SeriesResult],
    Points = calc_players_points(SeriesResult1, KakushForWinners, KakushForLoser, WinGamePoints, MulFactor, Players),
    PointsWithUserId = [{user_id_to_string(get_user_id(PlayerId, Players)), K, G} || {PlayerId, K, G} <- Points],
    ?INFO("OKEY_NG_TRN_STANDALONE <~p> Prizes: ~p", [GameId, PointsWithUserId]),
    add_points_to_accounts(PointsWithUserId, GameId, GameType, GameMode, MulFactor),
    {TRef, Magic} = start_timer(?SHOW_SET_RESULT_TIMEOUT),
    {next_state, ?STATE_SHOW_SET_RESULT, StateData#state{timer = TRef,
                                                         timer_magic = Magic}};


handle_info({timeout, Magic}, ?STATE_WAITING_FOR_PLAYERS,
            #state{timer_magic = Magic, game_id = GameId} = StateData) ->
    ?INFO("OKEY_NG_TRN_STANDALONE <~p> Time to start new set.", [GameId]),
    start_set(StateData);


handle_info({timeout, Magic}, ?STATE_SHOW_SET_RESULT,
            #state{timer_magic = Magic, game_id = GameId} = StateData) ->
    ?INFO("OKEY_NG_TRN_STANDALONE <~p> Time to finalize the tournament.", [GameId]),
    finalize_tournament(StateData);


handle_info({timeout, Magic}, ?STATE_FINISHED,
            #state{timer_magic = Magic, tables = Tables, game_id = GameId,
                   table_module = TableMod} = StateData) ->
    ?INFO("OKEY_NG_TRN_STANDALONE <~p> Time to stopping the tournament.", [GameId]),
    finalize_tables_with_disconnect(TableMod, Tables),
    {stop, normal, StateData#state{tables = [], seats = []}};


handle_info(Message, StateName, #state{game_id = GameId} = StateData) ->
    ?INFO("OKEY_NG_TRN_STANDALONE <~p> Unhandled message(info) received in state <~p>: ~p.",
          [GameId, StateName, Message]),
    {next_state, StateName, StateData}.

%%===================================================================

terminate(_Reason, _StateName, #state{game_id=GameId}=_StatData) ->
    ?INFO("OKEY_NG_TRN_STANDALONE <~p> Shutting down at state: <~p>. Reason: ~p",
          [GameId, _StateName, _Reason]),
    ok.

%%===================================================================

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------


handle_client_message(Message, StateName, #state{game_id = GameId} = StateData) ->
    ?INFO("OKEY_NG_TRN_STANDALONE <~p> Unhandled client message received in "
          "state <~p>: ~p.", [GameId, StateName, Message]),
    {next_state, StateName, StateData}.

%%===================================================================

handle_table_message(TableId, {player_connected, PlayerId},
                     StateName,
                     #state{seats = Seats} = StateData) ->
    case find_seats_by_player_id(PlayerId, Seats) of
        [#seat{seat_num = SeatNum}] ->
            NewSeats = update_seat_connect_status(TableId, SeatNum, true, Seats),
            {next_state, StateName, StateData#state{seats = NewSeats}};
        [] -> %% Ignoring the message
            {next_state, StateName, StateData}
    end;


handle_table_message(TableId, {player_disconnected, PlayerId},
                     StateName, #state{seats = Seats} = StateData) ->
    case find_seats_by_player_id(PlayerId, Seats) of
        [#seat{seat_num = SeatNum}] ->
            NewSeats = update_seat_connect_status(TableId, SeatNum, false, Seats),
            {next_state, StateName, StateData#state{seats = NewSeats}};
        [] -> %% Ignoring the message
            {next_state, StateName, StateData}
    end;


handle_table_message(TableId, {table_created, Relay},
                     ?STATE_WAITING_FOR_TABLES,
                     #state{tables = Tables, seats = Seats, table_module = TableMod,
                            cr_tab_requests = TCrRequests, reg_requests = RegRequests,
                            seats_per_table = SeatsPerTable} = StateData) ->
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

    %% Process delayed join/registration requests
    TablePid = get_table_pid(TableId, Tables),
    F2 = fun(PlayerId, Acc) ->
                 case dict:find(PlayerId, Acc) of
                     {ok, From} ->
                         gen_fsm:reply(From, {ok, {PlayerId, Relay, {TableMod, TablePid}}}),
                         dict:erase(PlayerId, Acc);
                     error -> Acc
                 end
         end,
    NewRegRequests = lists:foldl(F2, RegRequests, TabInitPlayers),
    NewTables = update_created_table(TableId, Relay, Tables),

    case is_table_full_enought(TableId, NewSeats, SeatsPerTable) of
        true ->
            {TRef, Magic} = start_timer(?WAITING_PLAYERS_TIMEOUT),
            {next_state, ?STATE_WAITING_FOR_PLAYERS,
             StateData#state{tables = NewTables, seats = NewSeats, cr_tab_requests = NewTCrRequests,
                             reg_requests = NewRegRequests, timer = TRef, timer_magic = Magic}};
        false ->
            {next_state, ?STATE_EMPTY_SEATS_FILLING,
             StateData#state{tables = NewTables, seats = NewSeats, cr_tab_requests = NewTCrRequests,
                             reg_requests = NewRegRequests}}
    end;


handle_table_message(TableId, {round_finished, TableContext, _RoundScore, _TotalScore},
                     ?STATE_SET_PROCESSING,
                     #state{game_id = GameId, tables = Tables, table_module = TableMod} = StateData) ->
    ?INFO("OKEY_NG_TRN_STANDALONE <~p> Round is finished (table: <~p>).", [GameId, TableId]),
    #table{pid = TablePid} = Table = fetch_table(TableId, Tables),
    TRef = erlang:send_after(?REST_TIMEOUT, self(), {rest_timeout, TableId}),
    NewTable = Table#table{context = TableContext, state = ?TABLE_STATE_FINISHED, timer = TRef},
    NewTables = store_table(NewTable, Tables),
    send_to_table(TableMod, TablePid, show_round_result),
    ?INFO("OKEY_NG_TRN_STANDALONE <~p> Waiting some time (~p secs) before start of next round.",
          [GameId, ?REST_TIMEOUT div 1000]),
    {next_state, ?STATE_SET_PROCESSING, StateData#state{tables = NewTables}};


handle_table_message(TableId, {game_finished, TableContext, _RoundScore, TableScore},
                     ?STATE_SET_PROCESSING,
                     #state{game_id = GameId, tables = Tables, table_module = TableMod} = StateData) ->
    ?INFO("OKEY_NG_TRN_STANDALONE <~p> Last round of the set is finished (table: <~p>).", [GameId, TableId]),
    TablesResults = [{TableId, TableScore}],
    #table{pid = TablePid} = Table = fetch_table(TableId, Tables),
    TRef = erlang:send_after(?REST_TIMEOUT, self(), {rest_timeout, TableId}),
    NewTable = Table#table{context = TableContext, state = ?TABLE_STATE_FINISHED, timer = TRef},
    NewTables = store_table(NewTable, Tables),
    send_to_table(TableMod, TablePid, show_round_result),
    ?INFO("OKEY_NG_TRN_STANDALONE <~p> Waiting some time (~p secs) before the set results calculation.",
          [GameId, ?REST_TIMEOUT div 1000]),
    {next_state, ?STATE_SET_FINISHED, StateData#state{tables = NewTables,
                                                      tables_results = TablesResults}};


handle_table_message(TableId, {response, RequestId, Response},
                     StateName,
                     #state{game_id = GameId, tab_requests = TabRequests} = StateData) ->
    NewTabRequests = dict:erase(RequestId, TabRequests),
    case dict:find(RequestId, TabRequests) of
        {ok, ReqContext} ->
            ?INFO("OKEY_NG_TRN_STANDALONE <~p> The a response received from table <~p>. "
                  "RequestId: ~p. Request context: ~p. Response: ~p",
                  [GameId, TableId, RequestId, ReqContext, Response]),
            handle_table_response(TableId, ReqContext, Response, StateName,
                                  StateData#state{tab_requests = NewTabRequests});
        error ->
            ?ERROR("OKEY_NG_TRN_STANDALONE <~p> Table <~p> sent a response for unknown request. "
                   "RequestId: ~p. Response", []),
            {next_state, StateName, StateData#state{tab_requests = NewTabRequests}}
    end;


handle_table_message(TableId, Message, StateName, #state{game_id = GameId} = StateData) ->
    ?INFO("OKEY_NG_TRN_STANDALONE <~p> Unhandled table message received from table <~p> in "
          "state <~p>: ~p.", [GameId, TableId, StateName, Message]),
    {next_state, StateName, StateData}.

%%===================================================================
%% handle_table_response(_TableId, {register_player, PlayerId, TableId, SeatNum}, ok = _Response,
%%                       StateName,
%%                       #state{reg_requests = RegRequests, seats = Seats,
%%                              tables = Tables} = StateData) ->
%%     Seat = fetch_seat(TableId, SeatNum, Seats),
%%     NewSeats = store_seat(Seat#seat{registered_by_table = true}, Seats),
%%     %% Send response to a client for a delayed request
%%     NewRegRequests =
%%         case dict:find(PlayerId, RegRequests) of
%%             {ok, From} ->
%%                 #table{relay = Relay, pid = TablePid} = fetch_table(TableId, Tables),
%%                 gen_fsm:reply(From, {ok, {PlayerId, Relay, {?TAB_MOD, TablePid}}}),
%%                 dict:erase(PlayerId, RegRequests);
%%             error -> RegRequests
%%         end,
%%     {next_state, StateName, StateData#state{seats = NewSeats,
%%                                             reg_requests = NewRegRequests}};

handle_table_response(_TableId, {replace_player, PlayerId, TableId, SeatNum}, ok = _Response,
                      StateName,
                      #state{reg_requests = RegRequests, seats = Seats,
                             tables = Tables, table_module = TableMod} = StateData) ->
    Seat = fetch_seat(TableId, SeatNum, Seats),
    NewSeats = store_seat(Seat#seat{registered_by_table = true}, Seats),
    %% Send response to a client for a delayed request
    NewRegRequests =
        case dict:find(PlayerId, RegRequests) of
            {ok, From} ->
                #table{relay = Relay, pid = TablePid} = fetch_table(TableId, Tables),
                gen_fsm:reply(From, {ok, {PlayerId, Relay, {TableMod, TablePid}}}),
                dict:erase(PlayerId, RegRequests);
            error -> RegRequests
        end,
    {next_state, StateName, StateData#state{seats = NewSeats,
                                            reg_requests = NewRegRequests}};

handle_table_response(TableId, RequestContext, Response, StateName,
                      #state{game_id = GameId} = StateData) ->
    ?INFO("OKEY_NG_TRN_STANDALONE <~p> Unhandled 'table response' received from table <~p> "
          "in state <~p>. Request context: ~p. Response: ~p.",
          [GameId, TableId, StateName, RequestContext, Response]),
    {next_state, StateName, StateData}.

%%===================================================================

handle_client_request({join, UserInfo}, From, StateName,
                      #state{game_id = GameId, reg_requests = RegRequests,
                             seats = Seats, players=Players, tables = Tables,
                             table_module = TableMod, cur_table = TableId,
                             player_id_counter = PlayerIdCounter, tab_requests = TabRequests,
                             seats_per_table = SeatsPerTable, common_params = CommonParams} = StateData) ->
    #'PlayerInfo'{id = UserId, robot = _IsBot} = UserInfo,
    ?INFO("OKEY_NG_TRN_STANDALONE <~p> The 'Join' request received from user: ~p.", [GameId, UserId]),
    if StateName == ?STATE_FINISHED ->
           ?INFO("OKEY_NG_TRN_STANDALONE <~p> The tournament is finished. "
                 "Reject to join user ~p.", [GameId, UserId]),
           {reply, {error, finished}, StateName, StateData};
       true ->
           case get_player_by_user_id(UserId, Players) of
               {ok, #player{id = PlayerId}} -> %% The user is an active member of the tournament.
                   ?INFO("OKEY_NG_TRN_STANDALONE <~p> User ~p is an active member of the tournament. "
                         "Allow to join.", [GameId, UserId]),
                   [#seat{table = TableId, registered_by_table = RegByTable}] = find_seats_by_player_id(PlayerId, Seats),
                   case RegByTable of
                       false -> %% Store this request to the waiting pool
                           ?INFO("OKEY_NG_TRN_STANDALONE <~p> User ~p not yet regirested by the table. "
                                 "Add the request to the waiting pool.", [GameId, UserId]),
                           NewRegRequests = dict:store(PlayerId, From, RegRequests),
                           {next_state, StateName, StateData#state{reg_requests = NewRegRequests}};
                       _ ->
                           ?INFO("OKEY_NG_TRN_STANDALONE <~p> Return the join response for player ~p immediately.",
                                 [GameId, UserId]),
                           #table{relay = Relay, pid = TPid} = fetch_table(TableId, Tables),
                           {reply, {ok, {PlayerId, Relay, {TableMod, TPid}}}, StateName, StateData}
                   end;
               error -> %% Not a member yet
                   ?INFO("OKEY_NG_TRN_STANDALONE <~p> User ~p is not a member of the tournament.", [GameId, UserId]),
                   case find_free_seats(TableId, Seats) of
                       [] ->
                           ?INFO("OKEY_NG_TRN_STANDALONE <~p> No free seats for user ~p. "
                                 "Reject to join.", [GameId, UserId]),
                           {reply, {error, not_allowed}, StateName, StateData};
                       [_ | _] ->
                           ?INFO("OKEY_NG_TRN_STANDALONE <~p> There is a free seat for user ~p. "
                                 "Registering.", [GameId, UserId]),
                           {SeatNum, NewPlayers, NewSeats} =
                               register_new_player(UserInfo, TableId, Players, Seats, PlayerIdCounter),
                           TablePid = get_table_pid(TableId, Tables),
                           NewTabRequests = table_req_replace_player(TableMod, TablePid, PlayerIdCounter, UserInfo,
                                                                     TableId, SeatNum, TabRequests),
                           NewRegRequests = dict:store(PlayerIdCounter, From, RegRequests),

                           Users = [if Bot -> robot; true -> UId end || #player{user_id = UId, is_bot = Bot}
                                                                                   <- players_to_list(NewPlayers)],
                           DeclRec = create_decl_rec(CommonParams, GameId, Users),
                           gproc:set_value({p,l,self()}, DeclRec),

                           TableIsFull = is_table_full_enought(TableId, NewSeats, SeatsPerTable),
                           if StateName == ?STATE_EMPTY_SEATS_FILLING andalso TableIsFull ->
                                  ?INFO("OKEY_NG_TRN_STANDALONE <~p> It's enought players registered to start the game. "
                                            "Initiating the procedure.", [GameId]),
                                  {TRef, Magic} = start_timer(?WAITING_PLAYERS_TIMEOUT),
                                  {next_state, ?STATE_WAITING_FOR_PLAYERS,
                                   StateData#state{reg_requests = NewRegRequests, tab_requests = NewTabRequests,
                                                   players = NewPlayers, seats = NewSeats, timer = TRef,
                                                   timer_magic = Magic, player_id_counter = PlayerIdCounter+1}};
                              true ->
                                  ?INFO("OKEY_NG_TRN_STANDALONE <~p> Not enought players registered to start the game. "
                                            "Waiting for more registrations.", [GameId]),
                                  {next_state, StateName,
                                   StateData#state{reg_requests = NewRegRequests, tab_requests = NewTabRequests,
                                                   players = NewPlayers, seats = NewSeats,
                                                   player_id_counter = PlayerIdCounter+1}}
                           end
                   end
           end
    end;

handle_client_request(Request, From, StateName, #state{game_id = GameId} = StateData) ->
    ?INFO("OKEY_NG_TRN_STANDALONE <~p> Unhandled client request received from ~p in "
          "state <~p>: ~p.", [GameId, From, StateName, Request]),
   {reply, {error, unexpected_request}, StateName, StateData}.

%%===================================================================
init_tour(Tour, #state{game_id = GameId, seats_per_table = SeatsPerTable,
                       params = TableParams, players = Players, table_module = TableMod,
                       table_id_counter = TableIdCounter, tables = OldTables,
                       initial_points = InitialPoints} = StateData) ->
    ?INFO("OKEY_NG_TRN_STANDALONE <~p> Initializing tour <~p>...", [GameId, Tour]),
    PlayersList = prepare_players_for_new_tour(InitialPoints, Players),
    {NewTables, Seats, NewTableIdCounter, CrRequests} =
        setup_tables(TableMod, PlayersList, SeatsPerTable, undefined, Tour,
                     undefined, TableIdCounter, GameId, TableParams),
    if Tour > 1 -> finalize_tables_with_rejoin(TableMod, OldTables);
       true -> do_nothing
    end,
    ?INFO("OKEY_NG_TRN_STANDALONE <~p> Initializing of tour <~p> is finished. "
          "Waiting creating confirmations from the tours' tables...",
          [GameId, Tour]),
    {next_state, ?STATE_WAITING_FOR_TABLES, StateData#state{tables = NewTables,
                                                            seats = Seats,
                                                            table_id_counter = NewTableIdCounter,
                                                            cur_table = TableIdCounter,
                                                            tour = Tour,
                                                            cr_tab_requests = CrRequests,
                                                            reg_requests = dict:new(),
                                                            tab_requests = dict:new(),
                                                            tables_results = []
                                                           }}.

start_set(#state{game_id = GameId, game = Game, game_mode = GameMode, mul_factor = MulFactor,
                 quota_per_round = Amount, tour = Tour, tables = Tables, players = Players,
                 table_module = TableMod} = StateData) ->
    ?INFO("OKEY_NG_TRN_STANDALONE <~p> Starting tour <~p>...", [GameId, Tour]),
    UsersIds = [UserId || #player{user_id = UserId, is_bot = false} <- players_to_list(Players)],
    deduct_quota(GameId, Game, GameMode, Amount, MulFactor, UsersIds),
    TablesList = tables_to_list(Tables),
    [send_to_table(TableMod, Pid, start_round) || #table{pid = Pid} <- TablesList],
    F = fun(Table, Acc) ->
                store_table(Table#table{state = ?TABLE_STATE_IN_PROGRESS}, Acc)
        end,
    NewTables = lists:foldl(F, Tables, TablesList),
    WL = [T#table.id || T <- TablesList],
    ?INFO("OKEY_NG_TRN_STANDALONE <~p> Tour <~p> is started. Processing...",
          [GameId, Tour]),
    {next_state, ?STATE_SET_PROCESSING, StateData#state{tables = NewTables,
                                                        tables_wl = WL}}.


finalize_tournament(#state{game_id = GameId} = StateData) ->
    ?INFO("OKEY_NG_TRN_STANDALONE <~p> Finalizing the tournament...", [GameId]),
    %% TODO: Real finalization needed
    {TRef, Magic} = start_timer(?SHOW_TOURNAMENT_RESULT_TIMEOUT),
    ?INFO("OKEY_NG_TRN_STANDALONE <~p> The tournament is finalized. "
          "Waiting some time (~p secs) before continue...",
          [GameId, ?SHOW_TOURNAMENT_RESULT_TIMEOUT div 1000]),
    {next_state, ?STATE_FINISHED, StateData#state{timer = TRef, timer_magic = Magic}}.

%% series_result(TableResult) -> WithPlaceAndStatus
%% Types: TableResult = [{PlayerId, Points}] 
%%        WithPlaceAndStatus = [{PlayerId, Place, Points, Status}]
%%          Status = winner | looser

series_result(TableResult) ->
    {_, PointsList} = lists:unzip(TableResult),
    Max = lists:max(PointsList),
    F = fun({Pl, Points}, {CurPlace, CurPos, LastPoints}) ->
                if Points == LastPoints ->
                       {{Pl, CurPlace, Points, if Points == Max -> winner; true -> looser end},
                        {CurPlace, CurPos + 1, Points}};
                   true ->
                       {{Pl, CurPos, Points, looser},
                        {CurPos, CurPos + 1, Points}}
                end
        end,
    {WithPlaceAndStatus, _} = lists:mapfoldl(F, {1, 1, Max}, lists:reverse(lists:keysort(2, TableResult))),
    WithPlaceAndStatus.


is_table_full_enought(TableId, Seats, EnoughtNumber) ->
    NonEmptySeats = find_non_free_seats(TableId, Seats),
    length(NonEmptySeats) >= EnoughtNumber.

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

%% table_req_replace_player(TablePid, PlayerId, UserInfo, TableId, SeatNum, TabRequests) ->
%%     RequestId = make_ref(),
%%     NewRequests = dict:store(RequestId, {replace_player, PlayerId, TableId, SeatNum}, TabRequests),
%%     send_to_table(TablePid, {replace_player, RequestId, UserInfo, PlayerId, SeatNum}),
%%     NewRequests.



%% prepare_players_for_new_tour(InitialPoints, Players) -> [{PlayerId, UserInfo, Points}]
prepare_players_for_new_tour(InitialPoints, Players) ->
    [{PlayerId, UserInfo, InitialPoints}
     || #player{id = PlayerId, user_info = UserInfo} <- players_to_list(Players)].



%% setup_tables(TableMod, Players, SeatsPerTable, TTable, TableIdCounter, GameId, TableParams) ->
%%                              {Tables, Seats, NewTableIdCounter, CrRequests}
%% Types: Players = {PlayerId, UserInfo, Points}
%%        TTable = [{Tour, [{UserId, CommonPos, Score, Status}]}]
setup_tables(TableMod, Players, SeatsPerTable, TTable, Tour, Tours, TableId, GameId, TableParams) ->
    EmptySeatsNum = SeatsPerTable - length(Players),
    Players2 = lists:duplicate(EmptySeatsNum, empty) ++ Players,
    SPlayers = shuffle(Players2),
    {TPlayers, _} = lists:mapfoldl(fun({PlayerId, UserInfo, Points}, SeatNum) ->
                                           {{PlayerId, UserInfo, SeatNum, Points}, SeatNum+1};
                                      (empty, SeatNum) ->
                                           {{_PlayerId = {empty, SeatNum+1}, empty_seat_userinfo(SeatNum+1), SeatNum, _Points=0}, SeatNum+1}
                                   end, 1, SPlayers),
    ?INFO("EmptySeatsNum:~p SPlayers: ~p TPlayers:~p", [EmptySeatsNum, SPlayers, TPlayers]),
    TableParams2 = [{players, TPlayers}, {ttable, TTable}, {tour, Tour},
                    {tours, Tours}, {parent, {?MODULE, self()}} | TableParams],
    {ok, TabPid} = spawn_table(TableMod, GameId, TableId, TableParams2),
    MonRef = erlang:monitor(process, TabPid),
    Tables = reg_table(TableId, TabPid, MonRef, _GlTableId = 0, _Context = undefined, tables_init()),
    F2 = fun({PlId, _UInfo, SNum, _Points}, Acc) ->
                 case PlId of
                     {empty,_} -> assign_seat(TableId, SNum, _PlId = undefined, _Reg = false, _Conn = false, _Free = true, Acc);
                     _ -> assign_seat(TableId, SNum, PlId, _Reg = false, _Conn = false, _Free = false, Acc)
                 end
         end,
    Seats = lists:foldl(F2, seats_init(), TPlayers),
    PlayersIds = [PlayerId || {PlayerId, _, _} <- SPlayers],
    TCrRequests = dict:store(TableId, PlayersIds, dict:new()),
    {Tables, Seats, TableId + 1, TCrRequests}.

empty_seat_userinfo(Num) ->
    #'PlayerInfo'{id         = list_to_binary(["empty_", integer_to_list(Num)]),
                  login      = <<"">>,
                  name       = <<"empty">>,
                  surname    = <<" ">>,
                  age        = 0,
                  skill      = 0,
                  score      = 0,
                  avatar_url = null,
                  robot      = true }.

%% setup_players(Registrants, GameId, BotModule) -> {Players, PlayerIdCounter}
setup_players(Registrants, GameId, BotModule) ->
    F = fun(robot, {Acc, PlayerId}) ->
                #'PlayerInfo'{id = UserId} = UserInfo = spawn_bot(GameId, BotModule),
                NewAcc = store_player(#player{id = PlayerId, user_id = UserId,
                                              user_info = UserInfo, is_bot = true}, Acc),
                {NewAcc, PlayerId + 1};
           (UserId, {Acc, PlayerId}) ->
                {ok, UserInfo} = auth_server:get_user_info_by_user_id(UserId),
                NewAcc = store_player(#player{id = PlayerId, user_id = UserId,
                                              user_info = UserInfo, is_bot = false}, Acc),
                {NewAcc, PlayerId + 1}
        end,
    lists:foldl(F, {players_init(), 1}, Registrants).



%% register_new_player(UserInfo, TableId, Players, Seats, PlayerId) -> {SeatNum, NewPlayers, NewSeats}

register_new_player(UserInfo, TableId, Players, Seats, PlayerId) ->
    #'PlayerInfo'{id = UserId, robot = Bot} = UserInfo,
    [#seat{seat_num = SeatNum} = Seat|_] = find_free_seats(TableId, Seats),
    NewSeats = store_seat(Seat#seat{table = TableId, player_id = PlayerId,
                                    registered_by_table = false, connected = false, free = false}, Seats),
    NewPlayers = store_player(#player{id = PlayerId, user_id = UserId,
                                      user_info = UserInfo, is_bot = Bot}, Players),
    {SeatNum, NewPlayers, NewSeats}.

%% replace_by_bots(Disconnected, GameId, BotModule, TableId, Players, Seats, PlayerIdCounter) ->
%%                                       {Replacements, NewPlayers, NewSeats, NewPlayerIdCounter}
%% Types: Disconnected = [PlayerId]
%%        Replacements = [{PlayerId, UserInfo, SeatNum}]
replace_by_bots(Disconnected, GameId, BotModule, TableId, Players, Seats, PlayerIdCounter) ->
    F = fun(PlayerId, {RAcc, PAcc, SAcc, Counter}) ->
                [#seat{seat_num = SeatNum}] = find_seats_by_player_id(PlayerId, TableId, SAcc),
                NewSAcc = store_seat(#seat{table = TableId, seat_num = SeatNum, player_id = Counter,
                                           registered_by_table = false, connected = false}, SAcc),
                #'PlayerInfo'{id = UserId} = UserInfo = spawn_bot(GameId, BotModule),
                NewPAcc = store_player(#player{id = Counter, user_id = UserId,
                                               user_info = UserInfo, is_bot = true}, PAcc),
                NewRAcc = [{Counter, UserInfo, SeatNum} | RAcc],
                {NewRAcc, NewPAcc, NewSAcc, Counter + 1}
        end,
    lists:foldl(F, {[], Players, Seats, PlayerIdCounter}, Disconnected).


%% finalize_tables_with_rejoin(TableMod, Tables) -> ok
finalize_tables_with_rejoin(TableMod, Tables) ->
    F = fun(#table{mon_ref = MonRef, pid = TablePid}) ->
                erlang:demonitor(MonRef, [flush]),
                send_to_table(TableMod, TablePid, rejoin_players),
                send_to_table(TableMod, TablePid, stop)
        end,
    lists:foreach(F, tables_to_list(Tables)).

%% finalize_tables_with_rejoin(TableMod, Tables) -> ok
finalize_tables_with_disconnect(TableMod, Tables) ->
    F = fun(#table{mon_ref = MonRef, pid = TablePid}) ->
                erlang:demonitor(MonRef, [flush]),
                send_to_table(TableMod, TablePid, disconnect_players),
                send_to_table(TableMod, TablePid, stop)
        end,
    lists:foreach(F, tables_to_list(Tables)).


%% table_req_replace_players(TableMod, TablePid, TableId, Replacements, TabRequests) -> NewRequests
table_req_replace_players(TableMod, TablePid, TableId, Replacements, TabRequests) ->
    F = fun({NewPlayerId, UserInfo, SeatNum}, Acc) ->
                table_req_replace_player(TableMod, TablePid, NewPlayerId, UserInfo, TableId, SeatNum, Acc)
        end,
    lists:foldl(F, TabRequests, Replacements).


%% table_req_replace_player(TableMod, TablePid, PlayerId, UserInfo, TableId, SeatNum, TabRequests) -> NewRequests
table_req_replace_player(TableMod, TablePid, PlayerId, UserInfo, TableId, SeatNum, TabRequests) ->
    RequestId = make_ref(),
    NewRequests = dict:store(RequestId, {replace_player, PlayerId, TableId, SeatNum}, TabRequests),
    send_to_table(TableMod, TablePid, {replace_player, RequestId, UserInfo, PlayerId, SeatNum}),
    NewRequests.


%% find_disconnected_players(TableId, Seats) -> PlayersIds
find_disconnected_players(TableId, Seats) ->
    [PlayerId || #seat{player_id = PlayerId} <- find_disconnected_seats(TableId, Seats)].

%% players_init() -> players()
players_init() -> midict:new().

%% store_player(#player{}, Players) -> NewPlayers
store_player(#player{id =Id, user_id = UserId} = Player, Players) ->
    midict:store(Id, Player, [{user_id, UserId}], Players).

get_players_ids(Players) ->
    [P#player.id || P <- players_to_list(Players)].

get_player_by_user_id(UserId, Players) ->
    case midict:geti(UserId, user_id, Players) of
        [Player] -> {ok, Player};
        [] -> error
    end.

%% players_to_list(Players) -> List
players_to_list(Players) -> midict:all_values(Players).

get_user_info(PlayerId, Players) ->
    #player{user_info = UserInfo} = midict:fetch(PlayerId, Players),
    UserInfo.

get_user_id(PlayerId, Players) ->
    #player{user_id = UserId} = midict:fetch(PlayerId, Players),
    UserId.

tables_init() -> midict:new().

reg_table(TableId, Pid, MonRef, GlobalId, TableContext, Tables) ->
    Table = #table{id = TableId, pid = Pid, mon_ref = MonRef, global_id = GlobalId,
                   state = initializing, context = TableContext},
    store_table(Table, Tables).

update_created_table(TableId, Relay, Tables) ->
    Table = midict:fetch(TableId, Tables),
    NewTable = Table#table{relay = Relay, state = ?TABLE_STATE_READY},
    store_table(NewTable, Tables).

store_table(#table{id = TableId, pid = Pid, mon_ref = MonRef, global_id = GlobalId} = Table, Tables) ->
    midict:store(TableId, Table, [{pid, Pid}, {global_id, GlobalId}, {mon_ref, MonRef}], Tables).

fetch_table(TableId, Tables) -> midict:fetch(TableId, Tables).

get_table_pid(TabId, Tables) ->
    #table{pid = TabPid} = midict:fetch(TabId, Tables),
    TabPid.

del_table(TabId, Tables) -> midict:erase(TabId, Tables).

get_table_by_mon_ref(MonRef, Tables) ->
    case midict:geti(MonRef, mon_ref, Tables) of
        [Table] -> Table;
        [] -> not_found
    end.

tables_to_list(Tables) -> midict:all_values(Tables).

seats_init() -> midict:new().

find_seats_by_player_id(PlayerId, Seats) ->
    midict:geti(PlayerId, player_id, Seats).

find_seats_by_player_id(PlayerId, TableId, Seats) ->
    midict:geti({PlayerId, TableId}, player_at_table, Seats).

find_seats_by_table_id(TabId, Seats) ->
    midict:geti(TabId, table_id, Seats).

find_disconnected_seats(TableId, Seats) ->
    midict:geti(false, {connected, TableId}, Seats).

find_non_free_seats(TableId, Seats) ->
    midict:geti(false, {free_at_tab, TableId}, Seats).

find_free_seats(TableId, Seats) ->
    midict:geti(true, {free_at_tab, TableId}, Seats).


fetch_seat(TableId, SeatNum, Seats) -> midict:fetch({TableId, SeatNum}, Seats).

%% assign_seat(TabId, SeatNum, PlayerId, RegByTable, Connected, Free, Seats) -> NewSeats
%% PlayerId = integer()
%% RegByTable = Connected = undefined | boolean()
assign_seat(TabId, SeatNum, PlayerId, RegByTable, Connected, Free, Seats) ->
    Seat = #seat{table = TabId, seat_num = SeatNum, player_id = PlayerId,
                 registered_by_table = RegByTable, connected = Connected, free = Free},
    store_seat(Seat, Seats).

update_seat_connect_status(TableId, SeatNum, ConnStatus, Seats) ->
    Seat = midict:fetch({TableId, SeatNum}, Seats),
    NewSeat = Seat#seat{connected = ConnStatus},
    store_seat(NewSeat, Seats).

store_seat(#seat{table = TabId, seat_num = SeatNum, player_id = PlayerId,
                 registered_by_table = _RegByTable,
                 connected = Connected, free = Free} = Seat, Seats) ->
    Indices = if Free == true ->
                     [{table_id, TabId}, {free, true}, {{free_at_tab, TabId}, true}];
                 true ->
                     [{table_id, TabId}, {free, false}, {{free_at_tab, TabId}, false},
                      {player_at_table, {PlayerId, TabId}}, {player_id, PlayerId},
                      {{connected, TabId}, Connected}]
              end,
    midict:store({TabId, SeatNum}, Seat, Indices, Seats).


%% calc_players_points(SeriesResult, KakushForWinner, KakushForLoser,
%%                     WinGamePoints, MulFactor, Players) -> Points
%% Types:
%%     SeriesResult = [{PlayerId, Status}]
%%          Status = winner | looser
%%     Points = [{PlayerId, KakushPoints, GamePoints}]
calc_players_points(SeriesResult, KakushForWinners, KakushForLoser, WinGamePoints, MulFactor, Players) ->
    SeriesResult1 = [begin
                         #'PlayerInfo'{id = UserId, robot = Robot} = get_user_info(PlayerId, Players),
                         Paid = is_paid(user_id_to_string(UserId)),
                         Winner = Status == winner,
                         {PlayerId, Winner, Robot, Paid}
                     end || {PlayerId, Status} <- SeriesResult],
    Paids   = [PlayerId || {PlayerId, _Winner, _Robot, _Paid = true} <- SeriesResult1],
    Winners = [PlayerId || {PlayerId, _Winner = true, _Robot, _Paid} <- SeriesResult1],
    TotalNum = length(SeriesResult),
    PaidsNum = length(Paids),
    WinnersNum = length(Winners),
    KakushPerWinner = round(((KakushForWinners * MulFactor) * PaidsNum div TotalNum) / WinnersNum),
    KakushPerLoser = (KakushForLoser * MulFactor) * PaidsNum div TotalNum,
    WinGamePoints1 = WinGamePoints * MulFactor,
    [begin
         {KakushPoints, GamePoints} = calc_points(KakushPerWinner, KakushPerLoser, WinGamePoints1, Paid, Robot, Winner),
         {PlayerId, KakushPoints, GamePoints}
     end || {PlayerId, Winner, Robot, Paid} <- SeriesResult1].


calc_points(KakushPerWinner, KakushPerLoser, WinGamePoints, Paid, Robot, Winner) ->
    if Robot -> {0, 0};
       not Paid andalso Winner -> {0, WinGamePoints};
       not Paid -> {0, 0};
       Paid andalso Winner -> {KakushPerWinner, WinGamePoints};
       Paid -> {KakushPerLoser, 0}
    end.

is_paid(UserId) -> nsm_accounts:user_paid(UserId).

deduct_quota(GameId, GameType, GameMode, Amount, MulFactor, UsersIds) ->
    RealAmount = Amount * MulFactor,
    [begin
         TI = #ti_game_event{game_name = GameType, game_mode = GameMode,
                             id = GameId, double_points = MulFactor,
                             type = start_round, tournament_type = ?TOURNAMENT_TYPE},
         nsm_accounts:transaction(binary_to_list(UserId), ?CURRENCY_QUOTA, -RealAmount, TI)
     end || UserId <- UsersIds],
    ok.


%% add_points_to_accounts(Points, GameId, GameType, GameMode, MulFactor) -> ok
%% Types: Points = [{UserId, KakushPoints, GamePoints}]
add_points_to_accounts(Points, GameId, GameType, GameMode, MulFactor) ->
    TI = #ti_game_event{game_name = GameType, game_mode = GameMode,
                        id = GameId, double_points = MulFactor,
                        type = game_end, tournament_type = ?TOURNAMENT_TYPE},
    [begin
         if KakushPoints =/= 0 ->
                ok = nsm_accounts:transaction(UserId, ?CURRENCY_KAKUSH, KakushPoints, TI);
            true -> do_nothing
         end,
         if GamePoints =/= 0 ->
                ok = nsm_accounts:transaction(UserId, ?CURRENCY_GAME_POINTS, GamePoints, TI);
            true -> do_nothing
         end
     end || {UserId, KakushPoints, GamePoints} <- Points],
    ok.



shuffle(List) -> deck:to_list(deck:shuffle(deck:from_list(List))).


create_decl_rec(CParams, GameId, Users) ->
    #game_table{id              = GameId,
                name            = proplists:get_value(table_name, CParams),
%                gameid,
%                trn_id,
                game_type       = proplists:get_value(game, CParams),
                rounds          = proplists:get_value(rounds, CParams),
                sets            = proplists:get_value(sets, CParams),
                owner           = proplists:get_value(owner, CParams),
                timestamp       = now(),
                users           = Users,
                users_options   = proplists:get_value(users_options, CParams),
                game_mode       = proplists:get_value(game_mode, CParams),
%                game_options,
                game_speed      = proplists:get_value(speed, CParams),
                friends_only    = proplists:get_value(friends_only, CParams),
%                invited_users = [],
                private         = proplists:get_value(private, CParams),
                feel_lucky = false,
%                creator,
                age_limit       = proplists:get_value(age, CParams),
%                groups_only = [],
                gender_limit    = proplists:get_value(gender_limit, CParams),
%                location_limit = "",
                paid_only       = proplists:get_value(paid_only, CParams),
                deny_robots     = proplists:get_value(deny_robots, CParams),
                slang           = proplists:get_value(slang, CParams),
                deny_observers  = proplists:get_value(deny_observers, CParams),
                gosterge_finish = proplists:get_value(gosterge_finish, CParams),
                double_points   = proplists:get_value(double_points, CParams),
                game_state      = started,
                game_process    = self(),
                game_module     = ?MODULE,
                pointing_rules  = proplists:get_value(pointing_rules, CParams),
                pointing_rules_ex = proplists:get_value(pointing_rules, CParams)
%                game_process_monitor =
%                tournament_type = 
               }.

user_id_to_string(UserId) -> binary_to_list(UserId).

%% start_timer(Timeout) -> {TRef, Magic}
start_timer(Timeout) ->
    Magic = make_ref(),
    TRef = erlang:send_after(Timeout, self(), {timeout, Magic}),
    {TRef, Magic}.

%% spawn_bot(GameId, BotModule) -> UserInfo
spawn_bot(GameId, BotModule) ->
    {NPid, UserInfo} = create_robot(BotModule, GameId),
    BotModule:join_game(NPid),
    UserInfo.

create_robot(BM, GameId) ->
    UserInfo = auth_server:robot_credentials(),
    {ok, NPid} = BM:start(self(), UserInfo, GameId),
    {NPid, UserInfo}.

spawn_table(TabMod, GameId, TableId, Params) -> TabMod:start(GameId, TableId, Params).

send_to_table(TabMod, TabPid, Message) -> TabMod:parent_message(TabPid, Message).

get_param(ParamId, Params) ->
    {_, Value} = lists:keyfind(ParamId, 1, Params),
    Value.

get_option(OptionId, Params, DefValue) ->
    proplists:get_value(OptionId, Params, DefValue).



series_result_test_() ->
    [
     ?_assertEqual(lists:sort([{2, 1, 100, winner},
                               {4, 1, 100, winner},
                               {1, 3,  50, looser},
                               {3, 4,  20, looser}]),
                   lists:sort(series_result([{1,  50},
                                             {2, 100},
                                             {3,  20},
                                             {4, 100}]))),
     ?_assertEqual(lists:sort([{2, 1, 100, winner},
                               {4, 2,  50, looser},
                               {1, 2,  50, looser},
                               {3, 2,  50, looser}]),
                   lists:sort(series_result([{1,  50},
                                             {2, 100},
                                             {3,  50},
                                             {4,  50}]))),
     ?_assertEqual(lists:sort([{2, 1, 100, winner},
                               {4, 2,  70, looser},
                               {1, 3,  50, looser},
                               {3, 3,  50, looser}]),
                   lists:sort(series_result([{1,  50},
                                             {2, 100},
                                             {3,  50},
                                             {4,  70}])))
    ].
