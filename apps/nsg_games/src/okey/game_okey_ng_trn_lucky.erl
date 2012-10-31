%%% -------------------------------------------------------------------
%%% Author  : Sergii Polkovnikov <serge.polkovnikov@gmail.com>
%%% Description : The "Quick play" logic
%%%
%%% Created : Oct 16, 2012
%%% -------------------------------------------------------------------

%%% Terms explanation:
%%% GameId   - uniq identifier of the tournament. Type: integer().
%%% PlayerId - registration number of a player in the tournament. 
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
-export([start/2, start_link/2, reg/2]).

%% gen_fsm callbacks
-export([init/1, state_name/2, state_name/3, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-export([table_message/3, client_send/2, client_sync_send/2, client_sync_send/3]).

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
         cr_tab_requests   :: dict()
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
         timer
        }).

-record(seat,
        {
         table           :: pos_integer(),
         seat_num        :: integer(),
         player_id       :: undefined | pos_integer(),
         is_bot          :: undefined | boolean(),
         connected       :: boolean()
        }).


-define(STATE_PROCESSING, state_processing).
-define(TAB_MOD, game_okey_ng_table_trn).
-define(SCORING_MOD, game_okey_ng_scoring).

-define(TABLE_STATE_INITIALIZING, initializing).
-define(TABLE_STATE_READY, ready).
-define(TABLE_STATE_IN_PROGRESS, in_progress).

-define(SEATS_NUM, 4).

-define(REST_TIMEOUT, 20000).

%% ====================================================================
%% External functions
%% ====================================================================

start(GameId, Params) ->
    gen_fsm:start(?MODULE, [GameId, Params, self()], []).

start_link(GameId, Params) ->
    gen_fsm:start_link(?MODULE, [GameId, Params, self()], []).

reg(FLPid, User) ->
    client_sync_send(FLPid, {reg, User}, 10000).

table_message(Pid, TableId, Message) ->
    gen_fsm:send_all_state_event(Pid, {table_message, TableId, Message}).

client_send(Pid, Message) ->
    gen_fsm:send_all_state_event(Pid, {client, Message}).

client_sync_send(Pid, Message) ->
    client_sync_send(Pid, Message, 5000).

client_sync_send(Pid, Message, Timeout) ->
    gen_fsm:sync_send_all_state_event(Pid, {client, Message}, Timeout).


%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, StateName, StateData}          |
%%          {ok, StateName, StateData, Timeout} |
%%          ignore                              |
%%          {stop, StopReason}
%% --------------------------------------------------------------------
init([GameId, Params, _Manager]) ->
    Users = proplists:get_value(users, Params),
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
    gproc:reg({p,g,self()}, GProcVal),

    {ok, ?STATE_PROCESSING, #state{game_id = GameId,
                                   params = TableParams,
                                   bots_params = BotsParams,
                                   players = players_init(),
                                   tables = tables_init(),
                                   seats = seats_init(),
                                   player_id_counter = 1,
                                   table_id_counter = 1,
                                   cr_tab_requests = dict:new()
                                  }}.

%% --------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
state_name(_Event, StateData) ->
    {next_state, state_name, StateData}.

%% --------------------------------------------------------------------
%% Func: StateName/3
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%% --------------------------------------------------------------------
state_name(_Event, _From, StateData) ->
    Reply = ok,
    {reply, Reply, state_name, StateData}.

%% --------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------

handle_event({client, Event}, StateName, StateData) ->
    handle_client_event(Event, StateName, StateData);

handle_event({table_message, TableId, Message}, StateName, StateData) ->
    handle_table_message(TableId, Message, StateName, StateData);

handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

%% --------------------------------------------------------------------
%% Func: handle_sync_event/4
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%% --------------------------------------------------------------------
handle_sync_event({client, Event}, From, StateName, StateData) ->
    handle_sync_client_event(Event, From, StateName, StateData);

handle_sync_event(_Event, _From, StateName, StateData) ->
    Reply = ok,
    {reply, Reply, StateName, StateData}.

%% --------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
handle_info({'DOWN', MonRef, process, _Pid, _}, StateName,
            #state{game_id = GameId, tables = Tables,
                   seats = Seats, players = Players} = StateData) ->
    case get_table_by_mon_ref(MonRef, Tables) of
        #table{id = TabId} ->
            ?INFO("OKEY_NG_TRN_LUCKY <~p> Table <~p> is down. Cleaning up registeres.", [GameId, TabId]),
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
    #table{pid = TablePid} = Table = fetch_table(TableId, Tables),
    NewTable = Table#table{state = in_process},
    NewTables = store_table(NewTable, Tables),
    send_to_table(TablePid, start_game),
    {next_state, StateName, StateData#state{tables = NewTables}};

handle_info(_Info, StateName, StateData) ->
    {next_state, StateName, StateData}.

%% --------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%% --------------------------------------------------------------------
terminate(_Reason, _StateName, #state{game_id=GameId}=_StatData) ->
    ?INFO("OKEY_NG_TRN_LUCKY <~p> Shutting down at state: <~p>. Reason: ~p", [GameId, _StateName, _Reason]),
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/4
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState, NewStateData}
%% --------------------------------------------------------------------
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------


handle_client_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

handle_table_message(TableId, {player_connected, PlayerId},
                     ?STATE_PROCESSING,
                     #state{game_id = GameId, seats = Seats, tables = Tables} = StateData)
  when is_integer(TableId), is_integer(PlayerId) ->
    ?INFO("OKEY_NG_TRN_LUCKY <~p> The player_connected notification received from table <~p>. PlayerId: <~p>",
          [GameId, TableId, PlayerId]),
    case find_seats_by_player_id(PlayerId, Seats) of
        [#seat{seat_num = SeatNum}] ->
            NewSeats = update_seat_connect_status(TableId, SeatNum, true, Seats),
            case fetch_table(TableId, Tables) of
                #table{state = ?TABLE_STATE_READY, pid = TabPid} ->
                    case is_all_players_connected(TableId, NewSeats) of
                        true ->
                            ?INFO("OKEY_NG_TRN_LUCKY <~p> All clients connected. Starting a game.",
                                  [GameId]),
                            NewTables = set_table_state(TableId, ?TABLE_STATE_IN_PROGRESS, Tables),
                            ?INFO("OKEY_NG_TRN_LUCKY <~p> TablePid: ~p.",
                                  [GameId, TabPid]),
                            send_to_table(TabPid, start_game),
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
    ?INFO("OKEY_NG_TRN_LUCKY <~p> The player_disconnected notification received from table <~p>. PlayerId: <~p>",
          [GameId, TableId, PlayerId]),
    case find_seats_by_player_id(PlayerId, Seats) of
        [#seat{seat_num = SeatNum}] ->
            case real_players_at_table(TableId, Seats) of
                1 -> %% Last real player gone
                    ?INFO("OKEY_NG_TRN_LUCKY <~p> Last real player gone from table <~p>. Closing the table.",
                          [GameId, TableId]),
                    unreg_player_and_eliminate_table(PlayerId, TableId, StateData);
                _ ->
                    ?INFO("OKEY_NG_TRN_LUCKY <~p> Al least one real player is at table <~p>. Starting a bot to replace free seat.",
                          [GameId, TableId]),
                    unreg_player_and_call_a_bot(PlayerId, TableId, SeatNum, StateData)
            end;
        [] -> %% Ignoring the message
            {next_state, ?STATE_PROCESSING, StateData}
    end;

handle_table_message(TableId, {table_created, Relay}, ?STATE_PROCESSING,
                    #state{game_id = GameId, tables = Tables,
                           cr_tab_requests = Requests} = StateData)
  when is_integer(TableId) ->
    ?INFO("OKEY_NG_TRN_LUCKY <~p> The <table_created> notification received from table: ~p.",
          [GameId, TableId]),
    NewTables = update_created_table(TableId, Relay, Tables),
    TablePid = get_table_pid(TableId, Tables),
    WL = dict:fetch(TableId, Requests),
    ?INFO("OKEY_NG_TRN_LUCKY <~p> WL: ~p.", [GameId, WL]),
    NewRequests = dict:erase(TableId, Requests),
    [gen_fsm:reply(From, {ok, {PlayerId, Relay, {?TAB_MOD, TablePid}}}) || {From, PlayerId} <- WL],
    {next_state, ?STATE_PROCESSING, StateData#state{tables = NewTables,
                                                    cr_tab_requests = NewRequests}};

handle_table_message(TableId, {round_finished, NewScoringState},
                     ?STATE_PROCESSING,
                     #state{game_id = GameId, tables = Tables} = StateData)
  when is_integer(TableId) ->
    ?INFO("OKEY_NG_TRN_LUCKY <~p> The <round_finished> notification received from table: ~p.",
          [GameId, TableId]),
    #table{pid = TablePid} = Table = fetch_table(TableId, Tables),
    TRef = erlang:send_after(?REST_TIMEOUT, self(), {rest_timeout, TableId}),
    NewTable = Table#table{scoring_state = NewScoringState, state = finished, timer = TRef},
    NewTables = store_table(NewTable, Tables),
    send_to_table(TablePid, show_round_result),
    {next_state, ?STATE_PROCESSING, StateData#state{tables = NewTables}};

handle_table_message(_TableId, _Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

%%===================================================================

handle_sync_client_event({reg, User}, From, ?STATE_PROCESSING,
                          #state{game_id = GameId, cr_tab_requests = Requests,
                                 seats = Seats, players=Players, tables = Tables} = StateData) ->
    #'PlayerInfo'{id = UserId, robot = IsBot} = User,
    ?INFO("OKEY_NG_TRN_LUCKY <~p> Exclusive mode. Register clime received from user: ~p.", [GameId, UserId]),
    case IsBot of
        true ->
            case get_player_id_by_user_id(UserId, Players) of
                {ok, PlayerId} ->
                    [#seat{table = TableId}] = find_seats_by_player_id(PlayerId, Seats),
                    #table{state = TableState, relay = Relay, pid = TPid} = fetch_table(TableId, Tables),
                    case TableState of
                        ?TABLE_STATE_INITIALIZING ->
                            NewRequests = dict:store(TableId, [{From, PlayerId}], Requests),
                            {next_state, ?STATE_PROCESSING, StateData#state{cr_tab_requests = NewRequests}};
                        _ ->
                            {reply, {ok, {PlayerId, Relay, {?TAB_MOD, TPid}}}, ?STATE_PROCESSING, StateData}
                    end;
                error ->
                    ?INFO("OKEY_NG_TRN_LUCKY <~p> User ~p is a bot. The user not registered. "
                          "Rejecting registration.", [GameId, UserId]),
                    reject_registration(not_registered, StateData)
            end;
%%             case find_free_seat(Seats) of
%%                 #seat{table = TabId, seat_num = SeatNum} ->
%%                     ?INFO("OKEY_NG_TRN_LUCKY <~p> Found a table with free seats. TabId: ~p SeatNum: ~p.", [GameId, TabId, SeatNum]),
%%                     reg_player_normaly(UserId, IsBot, TabId, SeatNum, From, StateData);
%%                 not_found ->
%%                     ?INFO("OKEY_NG_TRN_LUCKY <~p> User ~p is a bot. No free seats. Rejecting registration.", [GameId, UserId]),
%%                     reject_registration(no_free_seats, StateData)
%%             end;
        false ->
            IgnoredPlayers = [Id || #player{id = Id} <- midict:geti(UserId, user_id, Players)],
            case find_free_seat_without_players(Seats, IgnoredPlayers) of
                #seat{table = TabId, seat_num = SeatNum} ->
                    ?INFO("OKEY_NG_TRN_LUCKY <~p> Found a table with free seats. TabId: ~p SeatNum: ~p.", [GameId, TabId, SeatNum]),
                    reg_player_normaly(User, IsBot, TabId, SeatNum, From, StateData);
                not_found ->
                    ?INFO("OKEY_NG_TRN_LUCKY <~p> There are no table with free seats.", [GameId]),
                    case find_bot_seat_without_players(Seats, IgnoredPlayers) of
                        #seat{table = TabId, seat_num = SeatNum, player_id = OldPlayerId} ->
                            ?INFO("OKEY_NG_TRN_LUCKY <~p> Found a seat with a bot. Replacing by the user. UserId:~p TabId: ~p SeatNum: ~p.",
                                  [GameId, UserId, TabId, SeatNum]),
                            reg_player_with_replace(User, IsBot, TabId, SeatNum, OldPlayerId, From, StateData);
                        not_found ->
                            ?INFO("OKEY_NG_TRN_LUCKY <~p> There are no seats with bots. Creating new table for user: ~p.",
                                  [GameId, UserId]),
                            reg_player_at_new_table(User, IsBot, From, StateData)
                    end
            end
    end;

handle_sync_client_event(_Event, _From, StateName, StateData) ->
   Reply = {error, unexpected_request},
   {reply, Reply, StateName, StateData}.

reg_player_normaly(User, IsBot, TabId, SeatNum, From,
                   #state{game_id = GameId,
                          players = Players,
                          tables = Tables,
                          seats = Seats,
                          player_id_counter = PlayerId
                         } = StateData) ->
    #'PlayerInfo'{id = UserId} = User,
    NewPlayers = reg_player(#player{id = PlayerId, user_id = UserId, is_bot = IsBot}, Players),
    ?INFO("OKEY_NG_TRN_LUCKY <~p> User ~p registered as player <~p>.", [GameId, UserId, PlayerId]),
    NewSeats = assign_seat(TabId, SeatNum, PlayerId, IsBot, Seats),
    ?INFO("OKEY_NG_TRN_LUCKY <~p> User ~p assigned to seat <~p> of table <~p>.", [GameId, UserId, SeatNum, TabId]),
    TabPid = get_table_pid(TabId, Tables),
    send_to_table(TabPid, {register, PlayerId, SeatNum, reg_confirm_fun(From, PlayerId, {?TAB_MOD, TabPid})}),
    {next_state, ?STATE_PROCESSING, StateData#state{players = NewPlayers,
                                                    seats = NewSeats,
                                                    player_id_counter = PlayerId + 1}}.


reg_player_with_replace(User, IsBot, TabId, SeatNum, OldPlayerId, From,
                        #state{game_id = GameId,
                               players = Players,
                               tables = Tables,
                               seats = Seats,
                               player_id_counter = PlayerId
                              } = StateData) ->
    #'PlayerInfo'{id = UserId} = User,
    NewPlayers = reg_player(#player{id = PlayerId, user_id = UserId, is_bot = IsBot}, Players),
    ?INFO("OKEY_NG_TRN_LUCKY <~p> User ~p registered as player <~p>.", [GameId, UserId, PlayerId]),
    NewSeats = assign_seat(TabId, SeatNum, PlayerId, IsBot, Seats),
    ?INFO("OKEY_NG_TRN_LUCKY <~p> User ~p assigned to seat <~p> of table <~p>.", [GameId, UserId, SeatNum, TabId]),
%%    OldUserId = get_player_user_id(OldPlayerId, Players),
    TabPid = get_table_pid(TabId, Tables),
    send_to_table(TabPid, {replace, OldPlayerId, PlayerId, SeatNum, reg_confirm_fun(From, PlayerId, {?TAB_MOD, TabPid})}),
    {next_state, ?STATE_PROCESSING, StateData#state{players = NewPlayers,
                                                    seats = NewSeats,
                                                    player_id_counter = PlayerId + 1}}.

reg_player_at_new_table(User, IsBot, From,
                        #state{game_id = GameId,
                               players = Players,
                               tables = Tables,
                               seats = Seats,
                               player_id_counter = PlayerIdCounter,
                               table_id_counter = TableId,
                               bots_params = BotsParams,
                               params = TableParams,
                               cr_tab_requests = Requests
                              } = StateData) ->
    #'PlayerInfo'{id = UserId} = User,
    SeatsNum = seats_num(TableParams),
    RobotsInfo = spawn_bots(GameId, BotsParams, SeatsNum - 1),
    ?INFO("OKEY_NG_TRN_LUCKY <~p> Bots for table <~p> are spawned.", [GameId, TableId]),
    F = fun(BotInfo, {PlId,SNum}) -> {{PlId, BotInfo, SNum}, {PlId + 1, SNum + 1}} end,
    {RobotsRegData, {PlayerId, SeatNum}} = lists:mapfoldl(F, {PlayerIdCounter, 1}, RobotsInfo),

    TPlayers = [{PlayerId, User, SeatNum} | RobotsRegData],
    TableParams2 = [{players, TPlayers} | TableParams],
    {ok, TabPid} = spawn_table(GameId, TableId, TableParams2),

    MonRef = erlang:monitor(process, TabPid),
    %% FIXME: Table global id should use a persistent counter
    Scoring = ?SCORING_MOD:init(proplists:get_value(game_type, TableParams2), ?SEATS_NUM),
    NewTables = reg_table(TableId, TabPid, MonRef, 0, Scoring, Tables),
    ?INFO("OKEY_NG_TRN_LUCKY <~p> New table created: ~p.", [GameId, TableId]),

    NewPlayers = reg_player(#player{id = PlayerId, user_id = UserId, is_bot = IsBot}, Players),
    F2 = fun({PlId, #'PlayerInfo'{id = UId}, _SNum}, Acc) ->
                 reg_player(#player{id = PlId, user_id = UId, is_bot = true}, Acc)
         end,
    NewPlayers2 = lists:foldl(F2, NewPlayers, RobotsRegData),

    ?INFO("OKEY_NG_TRN_LUCKY <~p> User ~p registered as player <~p>.", [GameId, UserId, PlayerId]),
    NewSeats = create_seats(TableId, ?SEATS_NUM, Seats),
    NewSeats2 = assign_seat(TableId, SeatNum, PlayerId, IsBot, NewSeats),
    F3 = fun({PlId, _UserInfo, SNum}, Acc) ->
                 assign_seat(TableId, SNum, PlId, true, Acc)
         end,
    NewSeats3 = lists:foldl(F3, NewSeats2, RobotsRegData),

    ?INFO("OKEY_NG_TRN_LUCKY <~p> User ~p assigned to seat <~p> of table <~p>.", [GameId, UserId, 1, TableId]),

    NewRequests = dict:store(TableId, [{From, PlayerId}], Requests),
    {next_state, ?STATE_PROCESSING, StateData#state{players = NewPlayers2,
                                                    seats = NewSeats3,
                                                    tables = NewTables,
                                                    player_id_counter = PlayerId + 1,
                                                    table_id_counter = TableId + 1,
                                                    cr_tab_requests = NewRequests}}.

reject_registration(Reason, StateData) ->
    {reply, {error, Reason}, ?STATE_PROCESSING, StateData}.

unreg_player_and_eliminate_table(PlayerId, TabId, #state{players = Players,
                                                         tables = Tables,
                                                         seats = Seats
                                                        } = StateData) ->
    NewPlayers = del_player(PlayerId, Players),
    TabPid = get_table_pid(TabId, Tables),
    NewSeats = del_seats_by_table_id(TabId, Seats),
    NewTables = del_table(TabId, Tables),
    send_to_table(TabPid, stop),
    {next_state, ?STATE_PROCESSING, StateData#state{players = NewPlayers,
                                                    seats = NewSeats,
                                                    tables = NewTables}}.

unreg_player_and_call_a_bot(PlayerId, TabId, SeatNum, #state{players = Players,
                                                             seats = Seats,
                                                             game_id = GameId,
                                                             params = TableParams
                                                             } = StateData) ->
    NewPlayers = del_player(PlayerId, Players),
    NewSeats = free_seat(TabId, SeatNum, Seats),
    GameParams = proplists:get_value(game_params, TableParams),
    spawn_bots(GameId, GameParams, 1),
    {next_state, ?STATE_PROCESSING, StateData#state{players = NewPlayers,
                                                    seats = NewSeats}}.

reg_confirm_fun(From, PlayerId, TabPid) ->
    fun() -> gen_fsm:reply(From, {ok, {PlayerId, TabPid}}) end.

%% players_init() -> players()
players_init() ->
    midict:new().

%% reg_player(#player{}, Players) -> NewPlayers
reg_player(#player{id =Id, user_id = UserId} = Player, Players) ->
    midict:store(Id, Player, [{user_id, UserId}], Players).

%% get_user_id_by_player_id(PlayerId, Players) -> UserId
get_user_id_by_player(PlayerId, Players) ->
    {ok, #player{user_id = UserId}} = midict:find(PlayerId, Players),
    UserId.

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

%% players_num(Players) -> Num
players_num(Players) ->
    midict:size(Players).

%% players_to_list(Players) -> List
players_to_list(Players) ->
    midict:all_values(Players).



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

find_free_seat(Seats) ->
    case midict:geti(true, free, Seats) of
        [] -> not_found;
        [FreeSeat | _] -> FreeSeat
    end.

find_free_seat_without_players(Seats, PlayersList) ->
    case midict:geti(true, free, Seats) of
        [] -> not_found;
        List ->
            TabList = lists:usort([TabId || #seat{table = TabId} <- List]),
            lookup_free_seat_without_players(TabList, PlayersList, Seats)
    end.

lookup_free_seat_without_players([], _, _) -> not_found;
lookup_free_seat_without_players([TabId | Rest], PlayersList, Seats) ->
    TabPlayers = [Id || #seat{player_id=Id} <-
                                 midict:geti(TabId, non_free_at_tab, Seats), lists:member(Id, PlayersList)],
    if TabPlayers == [] ->
           ?INFO("OKEY_NG_TRN_LUCKY Seats:~p", [midict:geti(TabId, table_id, Seats)]),
           hd(midict:geti(TabId, free_at_tab, Seats));
       true -> lookup_free_seat_without_players(Rest, PlayersList, Seats)
    end.

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

is_all_players_connected(TableId, Seats) ->
    case length(midict:geti(true, {connected, TableId}, Seats)) of
        ?SEATS_NUM -> true;
        _ -> false
    end.


find_non_free_seats_at_tab(TabId, Seats) ->
    midict:geti(TabId, non_free_at_tab, Seats).

find_real_players_seats_at_tab(TabId, Seats) ->
    midict:geti(TabId, real_player_at_tab, Seats).

%% assign_seat(TabId, SeatNum, PlayerId, IsBot, Seats) -> NewSeats
%% PlayerId = integer()
assign_seat(TabId, SeatNum, PlayerId, IsBot, Seats) ->
    Seat = #seat{table = TabId, seat_num = SeatNum, player_id = PlayerId, is_bot = IsBot},
    store_seat(Seat, Seats).

update_seat_connect_status(TableId, SeatNum, ConnStatus, Seats) ->
    Seat = midict:fetch({TableId, SeatNum}, Seats),
    NewSeat = Seat#seat{connected = ConnStatus},
    store_seat(NewSeat, Seats).

store_seat(#seat{table = TabId, seat_num = SeatNum, player_id = PlayerId,
                 is_bot = IsBot, connected = Connected} = Seat, Seats) ->
    Indices = if PlayerId == undefined ->
                     [{table_id, TabId}, {free, true}, {free_at_tab, TabId}];
                 true ->
                     I = [{table_id, TabId}, {free, false}, {non_free_at_tab, TabId},
                          {player_id, PlayerId}, {is_bot, IsBot}, {{connected, TabId}, Connected}],
                     if IsBot -> [{bot_at_tab, TabId} | I];
                        true -> [{real_player_at_tab, TabId} | I]
                     end
              end,
    midict:store({TabId, SeatNum}, Seat, Indices, Seats).

create_seats(_TabId, 0, Seats) -> Seats;
create_seats(TabId, SeatNum, Seats) ->
    NewSeats = assign_seat(TabId, SeatNum, undefined, undefined, Seats),
    create_seats(TabId, SeatNum - 1, NewSeats).

free_seat(TabId, SeatNum, Seats) ->
    assign_seat(TabId, SeatNum, undefined, undefined, Seats).

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
     {rounds, infinity}
    ].

seats_num(TableParams) ->
    proplists:get_value(seats_num, TableParams).

bot_module(TableParams) ->
    case proplists:get_value(game, TableParams) of
        game_okey -> game_okey_bot
    end.

%%     [{feel_lucky, true},
%%      {table_name, "lucky"},
%%      {game, game_tavla},
%%      {game_mode, standard},
%%      {speed, normal},
%%      {rounds, 3},
%%      {owner, UId},
%%      {users, [UId, robot]},
%%      {friends_only, false},
%%      {private, false},
%%      {deny_robots, false}].

