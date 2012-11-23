%%% -------------------------------------------------------------------
%%% Author  : Sergii Polkovnikov <serge.polkovnikov@gmail.com>
%%% Description : The "Quick play" logic
%%%
%%% Created : Oct 16, 2012
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

-module(game_okey_ng_trn_lucky).

-behaviour(gen_fsm).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("nsx_config/include/log.hrl").
-include_lib("nsg_srv/include/basic_types.hrl").
-include_lib("nsm_db/include/table.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([start/1, start/2, start_link/2, reg/2]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-export([table_message/3, client_message/2, client_request/2, client_request/3]).

-record(state,
        {
         game_id           :: pos_integer(),
         params            :: proplists:proplist(),
         bots_params       :: proplists:proplist(),
         players,          %% The register of tournament players
         tables,           %% The register of tournament tables
         seats,            %% Stores relation between players and tables seats
         player_id_counter :: pos_integer(),
         table_id_counter  :: pos_integer(),
         mode              :: normal | exclusive,
         cr_tab_requests :: dict(), %% {TableId, PlayersIds}
         reg_requests   :: dict(),  %% {PlayerId, From}
         tab_requests   :: dict()   %% {RequestId, RequestContext}
        }).

-record(player,
        {
         id              :: pos_integer(),
         user_id,
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
         is_bot          :: undefined | boolean(),
         registered_by_table :: undefined | boolean(),
         connected       :: undefined | boolean()
        }).


-define(STATE_PROCESSING, state_processing).

-define(TAB_MOD, game_okey_ng_table_trn).

-define(TABLE_STATE_INITIALIZING, initializing).
-define(TABLE_STATE_READY, ready).
-define(TABLE_STATE_IN_PROGRESS, in_progress).
-define(TABLE_STATE_FINISHED, finished).

-define(REST_TIMEOUT, 5000). %% Time between game finsh and start of new round

%% ====================================================================
%% External functions
%% ====================================================================

start([GameId, Params]) -> 
    ?INFO(" +++ START OKEY LUCKY"),
    start(GameId,Params).

start(GameId, Params) ->
    gen_fsm:start(?MODULE, [GameId, Params, self()], []).

start_link(GameId, Params) ->
    gen_fsm:start_link(?MODULE, [GameId, Params, self()], []).

reg(Pid, User) ->
    client_request(Pid, {reg, User}, 10000).

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

init([GameId, _Params]) -> init([GameId, _Params, self()]);

init([GameId, _Params, _Manager]) ->
    TableParams = table_parameters(?MODULE, self()),
    BotsParams = bots_parameters(),
    GProcVal = #game_table{game_type = game_okey,
                           game_process = self(),
                           game_module = ?MODULE,
                           id = GameId,
                           age_limit = 100,
                           game_mode = undefined,
                           game_speed = undefined,
                           feel_lucky = true,
                           owner = undefined,
                           creator = undefined,
                           rounds = undefined,
                           pointing_rules   = [],
                           pointing_rules_ex = [],
                           users = [],
                           name = "I'm filling lucky - " ++ erlang:integer_to_list(GameId) ++ " "
                          },
    ?INFO("Okey I'm filling lucky - ~p started.  Pid:~p",[GameId, self()]),
    ?INFO("GProc Registration: ~p", [GProcVal]),
    gproc:reg({p,l,self()}, GProcVal),

    {ok, ?STATE_PROCESSING,
         #state{game_id = GameId,
                                   params = TableParams,
                                   bots_params = BotsParams,
                                   players = players_init(),
                                   tables = tables_init(),
                                   seats = seats_init(),
                                   player_id_counter = 1,
                                   table_id_counter = 1,
                                   cr_tab_requests = dict:new(),
                                   reg_requests = dict:new(),
                                   tab_requests = dict:new()
                                  }}.

%%===================================================================

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
            #state{game_id = GameId, tables = Tables,
                   seats = Seats, players = Players} = StateData) ->
    case get_table_by_mon_ref(MonRef, Tables) of
        #table{id = TabId, timer = TRef} ->
            ?INFO("OKEY_NG_TRN_LUCKY <~p> Table <~p> is down. Cleaning up registeres.", [GameId, TabId]),
            case TRef == undefined of false -> erlang:cancel_timer(TRef); true -> skip end,
            PlayersIds =
                [PlayerId || #seat{player_id = PlayerId} <- find_seats_with_players_for_table_id(TabId, Seats)],
            NewTables = del_table(TabId, Tables),
            NewSeats = del_seats_by_table_id(TabId, Seats),
            NewPlayers = del_players(PlayersIds, Players),
            {next_state, StateName, StateData#state{tables = NewTables,
                                                    seats = NewSeats,
                                                    players = NewPlayers}};
        not_found ->
            {next_state, StateName, StateData}
    end;

handle_info({rest_timeout, TableId}, StateName,
            #state{tables = Tables} = StateData) ->
    case get_table(TableId, Tables) of
        {ok, #table{pid = TablePid} = Table} ->
            NewTable = Table#table{state = in_process},
            NewTables = store_table(NewTable, Tables),
            send_to_table(TablePid, start_round),
            {next_state, StateName, StateData#state{tables = NewTables}};
        error -> %% If no such table ignore the timeout
            {next_state, StateName, StateData}
    end;

handle_info(_Info, StateName, StateData) ->
    {next_state, StateName, StateData}.

%%===================================================================

terminate(_Reason, _StateName, #state{game_id=GameId}=_StatData) ->
    ?INFO("OKEY_NG_TRN_LUCKY <~p> Shutting down at state: <~p>. Reason: ~p",
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

%% handle_table_message(TableId, Message, StateName, StateData)

handle_table_message(TableId, {player_connected, PlayerId},
                     ?STATE_PROCESSING,
                     #state{game_id = GameId, seats = Seats,
                            tables = Tables, params = TableParams} = StateData)
  when is_integer(TableId), is_integer(PlayerId) ->
    ?INFO("OKEY_NG_TRN_LUCKY <~p> The player_connected notification received from "
          "table <~p>. PlayerId: <~p>", [GameId, TableId, PlayerId]),
    case find_seats_by_player_id(PlayerId, Seats) of
        [#seat{seat_num = SeatNum}] ->
            NewSeats = update_seat_connect_status(TableId, SeatNum, true, Seats),
            case fetch_table(TableId, Tables) of
                #table{state = ?TABLE_STATE_READY, pid = TabPid} ->
                    SeatsNum = seats_num(TableParams),
                    case is_all_players_connected(TableId, SeatsNum, NewSeats) of
                        true ->
                            ?INFO("OKEY_NG_TRN_LUCKY <~p> All clients connected. Starting a game.",
                                  [GameId]),
                            NewTables = set_table_state(TableId, ?TABLE_STATE_IN_PROGRESS, Tables),
                            ?INFO("OKEY_NG_TRN_LUCKY <~p> TablePid: ~p.", [GameId, TabPid]),
                            send_to_table(TabPid, start_round),
                            {next_state, ?STATE_PROCESSING, StateData#state{seats = NewSeats,
                                                                            tables = NewTables}};
                        false ->
                            {next_state, ?STATE_PROCESSING, StateData#state{seats = NewSeats}}
                    end;
                _ ->
                    {next_state, ?STATE_PROCESSING, StateData#state{seats = NewSeats}}
            end;
        [] -> %% Ignoring the message
            {next_state, ?STATE_PROCESSING, StateData}
    end;


handle_table_message(TableId, {player_disconnected, PlayerId},
                     ?STATE_PROCESSING, #state{game_id = GameId, seats = Seats} = StateData)
  when is_integer(TableId), is_integer(PlayerId) ->
    ?INFO("OKEY_NG_TRN_LUCKY <~p> The player_disconnected notification received from "
          "table <~p>. PlayerId: <~p>", [GameId, TableId, PlayerId]),
    case find_seats_by_player_id(PlayerId, Seats) of
        [#seat{seat_num = SeatNum, is_bot = IsBot}] ->
            case real_players_at_table(TableId, Seats) of
                1 when not IsBot -> %% Last real player gone
                    ?INFO("OKEY_NG_TRN_LUCKY <~p> Last real player gone from "
                          "table <~p>. Closing the table.", [GameId, TableId]),
                    unreg_player_and_eliminate_table(PlayerId, TableId, StateData);
                _ ->
                    ?INFO("OKEY_NG_TRN_LUCKY <~p> Al least one real player is at table <~p>. "
                          "Starting a bot to replace free seat.", [GameId, TableId]),
                    replace_player_by_bot(PlayerId, TableId, SeatNum, StateData)
            end;
        [] -> %% Ignoring the message
            {next_state, ?STATE_PROCESSING, StateData}
    end;

handle_table_message(TableId, {table_created, Relay}, ?STATE_PROCESSING,
                    #state{game_id = GameId, tables = Tables, seats = Seats,
                           cr_tab_requests = TCrRequests,
                           reg_requests = RegRequests} = StateData)
  when is_integer(TableId) ->
    ?INFO("OKEY_NG_TRN_LUCKY <~p> The <table_created> notification received from table: ~p.",
          [GameId, TableId]),

    TabInitPlayers = dict:fetch(TableId, TCrRequests),
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
    NewTCrRequests = dict:erase(TableId, TCrRequests),
    NewTables = update_created_table(TableId, Relay, Tables),
    {next_state, ?STATE_PROCESSING, StateData#state{tables = NewTables,
                                                    seats = NewSeats,
                                                    cr_tab_requests = NewTCrRequests,
                                                    reg_requests = NewRegRequests}};


handle_table_message(TableId, {round_finished, NewScoringState, _RoundScore, _TotalScore},
                     ?STATE_PROCESSING,
                     #state{game_id = GameId, tables = Tables} = StateData)
  when is_integer(TableId) ->
    ?INFO("OKEY_NG_TRN_LUCKY <~p> The <round_finished> notification received from table: ~p.",
          [GameId, TableId]),
    #table{pid = TablePid} = Table = fetch_table(TableId, Tables),
    TRef = erlang:send_after(?REST_TIMEOUT, self(), {rest_timeout, TableId}),
    NewTable = Table#table{scoring_state = NewScoringState, state = ?TABLE_STATE_FINISHED, timer = TRef},
    NewTables = store_table(NewTable, Tables),
    send_to_table(TablePid, show_round_result),
    {next_state, ?STATE_PROCESSING, StateData#state{tables = NewTables}};


handle_table_message(TableId, {response, RequestId, Response},
                     ?STATE_PROCESSING,
                     #state{game_id = GameId, tab_requests = TabRequests} = StateData)
  when is_integer(TableId) ->
    NewTabRequests = dict:erase(RequestId, TabRequests),
    case dict:find(RequestId, TabRequests) of
        {ok, ReqContext} ->
            ?INFO("OKEY_NG_TRN_LUCKY <~p> A response received from table <~p>. "
                  "RequestId: ~p. Request context: ~p. Response: ~p",
                  [GameId, TableId, RequestId, ReqContext, Response]),
            handle_table_response(ReqContext, Response, ?STATE_PROCESSING,
                                  StateData#state{tab_requests = NewTabRequests});
        error ->
            ?ERROR("OKEY_NG_TRN_LUCKY <~p> Table <~p> sent a response for unknown request. "
                   "RequestId: ~p. Response", []),
            {next_state, ?STATE_PROCESSING, StateData#state{tab_requests = NewTabRequests}}
    end;

handle_table_message(_TableId, _Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

%%===================================================================

%% handle_table_response(RequestContext, Response, StateName, StateData)

handle_table_response({replace_player, PlayerId, TableId, SeatNum}, ok = _Response,
                      ?STATE_PROCESSING,
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
    {next_state, ?STATE_PROCESSING, StateData#state{seats = NewSeats,
                                                    reg_requests = NewRegRequests}}.

%%===================================================================

handle_client_request({reg, User}, From, ?STATE_PROCESSING,
                      #state{game_id = GameId, reg_requests = RegRequests,
                             seats = Seats, players=Players, tables = Tables} = StateData) ->
    #'PlayerInfo'{id = UserId, robot = IsBot} = User,
    ?INFO("OKEY_NG_TRN_LUCKY <~p> The Register request received from user: ~p.", [GameId, UserId]),
    case IsBot of
        true -> %% Bots can't initiate a registration
            case get_player_id_by_user_id(UserId, Players) of
                {ok, PlayerId} -> %% Already registered. Send table requsites.
                    [#seat{table = TableId, registered_by_table = RegByTable}] = find_seats_by_player_id(PlayerId, Seats),
                    case RegByTable of
                        false -> %% Store delayed request
                            NewRegRequests = dict:store(PlayerId, From, RegRequests),
                            {next_state, ?STATE_PROCESSING, StateData#state{reg_requests = NewRegRequests}};
                        _ ->
                            #table{relay = Relay, pid = TPid} = fetch_table(TableId, Tables),
                            {reply, {ok, {PlayerId, Relay, {?TAB_MOD, TPid}}}, ?STATE_PROCESSING, StateData}
                    end;
                error -> %% Not registered
                    ?INFO("OKEY_NG_TRN_LUCKY <~p> User ~p is a bot. The user not registered. "
                              "Rejecting registration.", [GameId, UserId]),
                    {reply, {error, indy_bots_not_allowed}, ?STATE_PROCESSING, StateData}
            end;
        false -> %% Normal user
            IgnoredPlayers = [Id || #player{id = Id} <- midict:geti(UserId, user_id, Players)],
            ?INFO("OKEY_NG_TRN_LUCKY <~p> There are no table with free seats.", [GameId]),
            case find_bot_seat_without_players(Seats, IgnoredPlayers) of
                #seat{table = TabId, seat_num = SeatNum, player_id = OldPlayerId} ->
                    ?INFO("OKEY_NG_TRN_LUCKY <~p> Found a seat with a bot. Replacing by the user. "
                              "UserId:~p TabId: ~p SeatNum: ~p.", [GameId, UserId, TabId, SeatNum]),
                    reg_player_with_replace(User, TabId, SeatNum, OldPlayerId, From, StateData);
                not_found ->
                    ?INFO("OKEY_NG_TRN_LUCKY <~p> There are no seats with bots. "
                              "Creating new table for user: ~p.", [GameId, UserId]),
                    reg_player_at_new_table(User, From, StateData)
            end
    end;

handle_client_request(_Request, _From, StateName, StateData) ->
   Reply = {error, unexpected_request},
   {reply, Reply, StateName, StateData}.

%%===================================================================


reg_player_with_replace(UserInfo, TableId, SeatNum, OldPlayerId, From,
                        #state{game_id = GameId, players = Players, tables = Tables,
                               seats = Seats, player_id_counter = PlayerId,
                               tab_requests = TabRequests, reg_requests = RegRequests
                              } = StateData) ->
    #'PlayerInfo'{id = UserId, robot = IsBot} = UserInfo,
    NewPlayers = del_player(OldPlayerId, Players),
    NewPlayers2 = reg_player(#player{id = PlayerId, user_id = UserId, is_bot = IsBot}, NewPlayers),
    ?INFO("OKEY_NG_TRN_LUCKY <~p> User ~p registered as player <~p>.", [GameId, UserId, PlayerId]),
    NewSeats = assign_seat(TableId, SeatNum, PlayerId, IsBot, false, false, Seats),
    ?INFO("OKEY_NG_TRN_LUCKY <~p> User ~p assigned to seat <~p> of table <~p>.", [GameId, UserId, SeatNum, TableId]),
    NewRegRequests = dict:store(PlayerId, From, RegRequests),
    TablePid = get_table_pid(TableId, Tables),
    NewTabRequests = table_req_replace_player(TablePid, PlayerId, UserInfo, TableId, SeatNum, TabRequests),
    {next_state, ?STATE_PROCESSING, StateData#state{players = NewPlayers2,
                                                    seats = NewSeats,
                                                    player_id_counter = PlayerId + 1,
                                                    tab_requests = NewTabRequests,
                                                    reg_requests = NewRegRequests}}.


reg_player_at_new_table(User, From,
                        #state{game_id = GameId, players = Players,
                               tables = Tables, seats = Seats,
                               player_id_counter = PlayerIdCounter,
                               table_id_counter = TableId,
                               bots_params = BotsParams, params = TableParams,
                               reg_requests = RegRequests, cr_tab_requests = TCrRequests
                              } = StateData) ->
    #'PlayerInfo'{id = UserId, robot = IsBot} = User,
    SeatsNum = seats_num(TableParams),
    RobotsInfo = spawn_bots(GameId, BotsParams, SeatsNum - 1),
    ?INFO("OKEY_NG_TRN_LUCKY <~p> Bots for table <~p> are spawned.", [GameId, TableId]),
    F = fun(BotInfo, {PlId,SNum}) -> {{PlId, BotInfo, SNum, _Points = 0}, {PlId + 1, SNum + 1}} end,
    {RobotsRegData, {PlayerId, SeatNum}} = lists:mapfoldl(F, {PlayerIdCounter, 1}, RobotsInfo),

    TPlayers = [{PlayerId, User, SeatNum, 0} | RobotsRegData],
    TableParams2 = [{players, TPlayers} | TableParams],
    {ok, TabPid} = spawn_table(GameId, TableId, TableParams2),

    MonRef = erlang:monitor(process, TabPid),
    %% FIXME: Table global id should use a persistent counter
    NewTables = reg_table(TableId, TabPid, MonRef, 0, undefined, Tables),
    ?INFO("OKEY_NG_TRN_LUCKY <~p> New table created: ~p.", [GameId, TableId]),

    NewPlayers = reg_player(#player{id = PlayerId, user_id = UserId, is_bot = IsBot}, Players),
    F2 = fun({PlId, #'PlayerInfo'{id = UId}, _SNum, _Points}, Acc) ->
                 reg_player(#player{id = PlId, user_id = UId, is_bot = true}, Acc)
         end,
    NewPlayers2 = lists:foldl(F2, NewPlayers, RobotsRegData),
    ?INFO("OKEY_NG_TRN_LUCKY <~p> User ~p registered as player <~p>.", [GameId, UserId, PlayerId]),

    NewSeats = assign_seat(TableId, SeatNum, PlayerId, IsBot, false, false, Seats),
    F3 = fun({PlId, _UserInfo, SNum, _Points}, Acc) ->
                 assign_seat(TableId, SNum, PlId, true, false, false, Acc)
         end,
    NewSeats2 = lists:foldl(F3, NewSeats, RobotsRegData),
    ?INFO("OKEY_NG_TRN_LUCKY <~p> User ~p assigned to seat <~p> of table <~p>.", [GameId, UserId, SeatNum, TableId]),

    NewRegRequests = dict:store(PlayerId, From, RegRequests),
    PlayersIds = [PlayerId | [PlId || {PlId, _, _, _} <- RobotsRegData]],
    NewTCrRequests = dict:store(TableId, PlayersIds, TCrRequests),
    {next_state, ?STATE_PROCESSING, StateData#state{players = NewPlayers2,
                                                    seats = NewSeats2,
                                                    tables = NewTables,
                                                    player_id_counter = PlayerId + 1,
                                                    table_id_counter = TableId + 1,
                                                    reg_requests = NewRegRequests,
                                                    cr_tab_requests = NewTCrRequests}}.


unreg_player_and_eliminate_table(PlayerId, TabId,
                                 #state{players = Players, tables = Tables,
                                        seats = Seats} = StateData) ->
    NewPlayers = del_player(PlayerId, Players),
    TabPid = get_table_pid(TabId, Tables),
    NewSeats = del_seats_by_table_id(TabId, Seats),
    NewTables = del_table(TabId, Tables),
    send_to_table(TabPid, stop),
    {next_state, ?STATE_PROCESSING, StateData#state{players = NewPlayers,
                                                    seats = NewSeats,
                                                    tables = NewTables}}.


replace_player_by_bot(PlayerId, TableId, SeatNum,
                      #state{players = Players, seats = Seats,
                             game_id = GameId, bots_params = BotsParams,
                             player_id_counter = NewPlayerId, tables = Tables,
                             tab_requests = Requests} = StateData) ->
    NewPlayers = del_player(PlayerId, Players),
    [#'PlayerInfo'{id = UserId} = UserInfo] = spawn_bots(GameId, BotsParams, 1),
    NewPlayers2 = reg_player(#player{id = NewPlayerId, user_id = UserId, is_bot = true}, NewPlayers),
    NewSeats = assign_seat(TableId, SeatNum, NewPlayerId, true, false, false, Seats),
    TablePid = get_table_pid(TableId, Tables),
    NewRequests = table_req_replace_player(TablePid, NewPlayerId, UserInfo, TableId, SeatNum, Requests),
    {next_state, ?STATE_PROCESSING, StateData#state{players = NewPlayers2,
                                                    seats = NewSeats,
                                                    player_id_counter = NewPlayerId + 1,
                                                    tab_requests = NewRequests}}.


%% table_req_replace_player(TablePid, PlayerId, UserInfo, TableId, SeatNum, TabRequests) -> NewRequests
table_req_replace_player(TablePid, PlayerId, UserInfo, TableId, SeatNum, TabRequests) ->
    RequestId = make_ref(),
    NewRequests = dict:store(RequestId, {replace_player, PlayerId, TableId, SeatNum}, TabRequests),
    send_to_table(TablePid, {replace_player, RequestId, UserInfo, PlayerId, SeatNum}),
    NewRequests.


%% players_init() -> players()
players_init() ->
    midict:new().

%% reg_player(#player{}, Players) -> NewPlayers
reg_player(#player{id =Id, user_id = UserId} = Player, Players) ->
    midict:store(Id, Player, [{user_id, UserId}], Players).

get_player_id_by_user_id(UserId, Players) ->
    case midict:geti(UserId, user_id, Players) of
        [#player{id = PlayerId}] -> {ok, PlayerId};
        [] -> error
    end.

%% del_player(PlayerId, Players) -> NewPlayers
del_player(PlayerId, Players) ->
    midict:erase(PlayerId, Players).

%% del_player(PlayersIds, Players) -> NewPlayers
del_players([], Players) -> Players;
del_players([PlayerId | Rest], Players) ->
    del_players(Rest, del_player(PlayerId, Players)).


tables_init() ->
    midict:new().

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

fetch_table(TableId, Tables) ->
    midict:fetch(TableId, Tables).

get_table(TableId, Tables) ->
    midict:find(TableId, Tables).

get_table_pid(TabId, Tables) ->
    {ok, #table{pid = TabPid}} = midict:find(TabId, Tables),
    TabPid.

del_table(TabId, Tables) ->
    midict:erase(TabId, Tables).

get_table_by_mon_ref(MonRef, Tables) ->
    case midict:geti(MonRef, mon_ref, Tables) of
        [Table] -> Table;
        [] -> not_found
    end.

set_table_state(TableId, State, Tables) ->
    Table = midict:fetch(TableId, Tables),
    store_table(Table#table{state = State}, Tables).

seats_init() ->
    midict:new().


find_bot_seat_without_players(Seats, PlayersList) ->
    case midict:geti(true, is_bot, Seats) of
        [] -> not_found;
        List ->
            TabList = lists:usort([TabId || #seat{table = TabId} <- List]),
            lookup_bot_seat_without_players(TabList, PlayersList, Seats)
    end.

lookup_bot_seat_without_players([], _, _) -> not_found;
lookup_bot_seat_without_players([TabId | Rest], PlayersList, Seats) ->
    TabPlayers = [Id || #seat{player_id=Id} <-
                                 midict:geti(TabId, non_free_at_tab, Seats), lists:member(Id, PlayersList)],
    if TabPlayers == [] ->
           ?INFO("OKEY_NG_TRN_LUCKY Seats:~p", [midict:geti(TabId, table_id, Seats)]),
           hd(midict:geti(TabId, bot_at_tab, Seats));
       true -> lookup_bot_seat_without_players(Rest, PlayersList, Seats)
    end.

find_seats_with_players_for_table_id(TabId, Seats) ->
    midict:geti(TabId, non_free_at_tab, Seats).

find_seats_by_player_id(PlayerId, Seats) ->
    midict:geti(PlayerId, player_id, Seats).

find_seats_by_table_id(TabId, Seats) ->
    midict:geti(TabId, table_id, Seats).

%% real_players_at_table(TabId, Seats) -> Num
real_players_at_table(TabId, Seats) ->
    length(find_real_players_seats_at_tab(TabId, Seats)).

is_all_players_connected(TableId, TableSeatsNum, Seats) ->
    TableSeatsNum == length(midict:geti(true, {connected, TableId}, Seats)).

find_real_players_seats_at_tab(TabId, Seats) ->
    midict:geti(TabId, real_player_at_tab, Seats).

fetch_seat(TableId, SeatNum, Seats) ->
    midict:fetch({TableId, SeatNum}, Seats).

%% assign_seat(TabId, SeatNum, PlayerId, IsBot, RegByTable, Connected, Seats) -> NewSeats
%% PlayerId = integer()
%% IsBot = RegByTable = Connected = undefined | boolean()
assign_seat(TabId, SeatNum, PlayerId, IsBot, RegByTable, Connected, Seats) ->
    Seat = #seat{table = TabId, seat_num = SeatNum, player_id = PlayerId,
                 is_bot = IsBot, registered_by_table = RegByTable, connected = Connected},
    store_seat(Seat, Seats).

update_seat_connect_status(TableId, SeatNum, ConnStatus, Seats) ->
    Seat = midict:fetch({TableId, SeatNum}, Seats),
    NewSeat = Seat#seat{connected = ConnStatus},
    store_seat(NewSeat, Seats).

store_seat(#seat{table = TabId, seat_num = SeatNum, player_id = PlayerId,
                 is_bot = IsBot, registered_by_table = _RegByTable,
                 connected = Connected} = Seat, Seats) ->
    Indices = if PlayerId == undefined ->
                     [{table_id, TabId}, {free, true}, {free_at_tab, TabId}];
                 true ->
                     I = [{table_id, TabId}, {free, false}, {non_free_at_tab, TabId},
                          {player_id, PlayerId}, {is_bot, IsBot},
                          {{connected, TabId}, Connected}],
                     if IsBot -> [{bot_at_tab, TabId} | I];
                        true -> [{real_player_at_tab, TabId} | I]
                     end
              end,
    midict:store({TabId, SeatNum}, Seat, Indices, Seats).

create_seats(_TabId, 0, Seats) -> Seats;
create_seats(TabId, SeatNum, Seats) ->
    NewSeats = assign_seat(TabId, SeatNum, undefined, undefined, undefined, undefined, Seats),
    create_seats(TabId, SeatNum - 1, NewSeats).


del_seats_by_table_id(TabId, Seats) ->
    F = fun(#seat{seat_num = SeatNum}, Acc) ->
                midict:erase({TabId, SeatNum}, Acc)
        end,
    lists:foldl(F, Seats, find_seats_by_table_id(TabId, Seats)).

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
    {ok, NPid} = BM:start(self(), User, GameId),
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
     {tournament_type, lucky},
     {speed, normal},
     {round_timeout, infinity},
     {game_type, standard},
     {rounds, undefined},
     {reveal_confirmation, true},
     {next_series_confirmation, true},
     {pause_mode, normal},
     {social_actions_enabled, true}
    ].

%% bots_parameters() -> Proplist
bots_parameters() ->
    [
     {game, game_okey},
     {game_mode, standard},
     {lucky, true},
     {speed, normal},
     {rounds, undefined}
    ].

seats_num(TableParams) ->
    proplists:get_value(seats_num, TableParams).

bot_module(TableParams) ->
    case proplists:get_value(game, TableParams) of
        game_okey -> game_okey_bot
    end.

