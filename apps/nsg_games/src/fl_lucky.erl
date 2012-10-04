%%% -------------------------------------------------------------------
%%% Author  : Sergei Polkovnikov <serge.polkovnikov@gmail.com>
%%% Description : The tournament logic for "I'm filling lucky" games
%%%
%%% Created : Sep 28, 2012
%%% -------------------------------------------------------------------

%%% Terms explanation:
%%% GameId   - uniq identifier of the tournament. Type: integer().
%%% PlayerId - registration number of a player in the tournament. One
%%%          physical user can have several registration numbers if
%%%          he starts more then one game clients. Type: initeger().
%%% UserId   - cross system identifier of a physical user. Type: binary() (or string()?).
%%% TableId  - uniq identifier of a table in the tournament. Used by the
%%%          tournament logic. Type: integer().
%%% TableGlobalId - uniq identifier of a table in the system. Can be used
%%%          to refer to a table directly - without pointing to a tournament.
%%%          Type: integer()

-module(fl_lucky).

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

-export([child_send/2, client_send/2, client_sync_send/2, client_sync_send/3]).

-record(state,
        {
         game_id           :: pos_integer(),
         params            :: proplists:proplist(),
         players,          %% The register of tournament players
         tables,           %% The register of tournament tables
         seats,            %% Stores relation between players and tables seats
         player_id_counter :: pos_integer(),
         table_id_counter  :: pos_integer(),
         mode              :: normal | exclusive
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
         mon_ref
        }).

-record(seat,
        {
         table           :: pos_integer(),
         seat_num        :: integer(),
         player_id       :: undefined | pos_integer(),
         is_bot          :: undefined | boolean()
        }).


-define(STATE_PROCESSING, state_processing).
-define(TAB_MOD, sl_simple).

-define(MODE_EXCLUSIVE, exclusive).
-define(MODE_NORMAL, normal).

%% ====================================================================
%% External functions
%% ====================================================================

start(GameId, Params) ->
    gen_fsm:start(?MODULE, [GameId, Params, self()], []).
start_link(GameId, Params) ->
    gen_fsm:start_link(?MODULE, [GameId, Params, self()], []).

reg(FLPid, User) ->
    client_sync_send(FLPid, {reg, User}, 10000).

child_send(Pid, Message) ->
    gen_fsm:send_all_state_event(Pid, {child, Message}).

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
    GameType = proplists:get_value(game_type, Params),
    Mode = proplists:get_value(mode, Params),
    TableParams = table_parameters(GameType, ?MODULE, self()),
    GProcVal = #game_table{game_type = GameType,
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
                           name = "I'm Feeling Lucky " ++ game_type_to_str(GameType) ++
                                      " " ++erlang:integer_to_list(GameId) ++ " "
                          },
    ?INFO("GProc Registration: ~p",[GProcVal]),
    gproc:reg({p,g,self()},GProcVal),

    {ok, ?STATE_PROCESSING, #state{game_id = GameId,
                                   params = TableParams,
                                   players = players_init(),
                                   tables = tables_init(),
                                   seats = seats_init(),
                                   player_id_counter = 1,
                                   table_id_counter = 1,
                                   mode = Mode
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

handle_event({child, Event}, StateName, StateData) ->
    handle_child_event(Event, StateName, StateData);

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
            ?INFO("FL_LUCKY <~p> Table <~p> is down. Cleaning up registeres.", [GameId, TabId]),
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

handle_info(_Info, StateName, StateData) ->
    {next_state, StateName, StateData}.

%% --------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%% --------------------------------------------------------------------
terminate(_Reason, _StateName, _StatData) ->
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

handle_child_event({player_disconnected, TabId, PlayerId},
                   ?STATE_PROCESSING, #state{game_id = GameId, seats = Seats} = StateData)
  when is_integer(TabId), is_integer(PlayerId) ->
    ?INFO("FL_LUCKY <~p> The player_disconnected notification received from table <~p>. PlayerId: <~p>",
          [GameId, TabId, PlayerId]),
    case find_seats_by_player_id(PlayerId, Seats) of
        [#seat{seat_num = SeatNum}] ->
            case real_players_at_table(TabId, Seats) of
                1 -> %% Last real player gone
                    ?INFO("FL_LUCKY <~p> Last real player gone from table <~p>. Closing the table.",
                          [GameId, TabId]),
                    unreg_player_and_eliminate_table(PlayerId, TabId, StateData);
                _ ->
                    ?INFO("FL_LUCKY <~p> Al least one real player is at table <~p>. Starting a bot to replace free seat.",
                          [GameId, TabId]),
                    unreg_player_and_call_a_bot(PlayerId, TabId, SeatNum, StateData)
            end;
        [] -> %% Ignoring the message
            {next_state, ?STATE_PROCESSING, StateData}
    end;

handle_child_event({table_ready, TabId}, ?STATE_PROCESSING,
                    #state{game_id = GameId, tables = Tables} = StateData)
  when is_integer(TabId) ->
    ?INFO("FL_LUCKY <~p> The waiting notification received from table: ~p. Sending the start_game command...",
          [GameId, TabId]),
    TabPid = get_table_pid(TabId, Tables),
    send_to_table(TabPid, start_game),
    {next_state, ?STATE_PROCESSING, StateData};

handle_child_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.


handle_sync_client_event({reg, User}, From, ?STATE_PROCESSING,
                          #state{game_id = GameId, mode = Mode,
                                 seats = Seats, players=Players} = StateData) ->
    #'PlayerInfo'{id = UserId, robot = IsBot} = User,
    ?INFO("FL_LUCKY <~p> Exclusive mode. Register clime received from user: ~p.", [GameId, UserId]),
    case {IsBot, Mode} of
        {true, _} ->
            case find_free_seat(Seats) of
                #seat{table = TabId, seat_num = SeatNum} ->
                    ?INFO("FL_LUCKY <~p> Found a table with free seats. TabId: ~p SeatNum: ~p.", [GameId, TabId, SeatNum]),
                    reg_player_normaly(UserId, IsBot, TabId, SeatNum, From, StateData);
                not_found ->
                    ?INFO("FL_LUCKY <~p> User ~p is a bot. No free seats. Rejecting registration.", [GameId, UserId]),
                    reject_registration(no_free_seats, StateData)
            end;
        {false, ?MODE_EXCLUSIVE} ->
            ?INFO("FL_LUCKY <~p>Creating new table for user: ~p.",
                  [GameId, UserId]),
            reg_player_at_new_table(UserId, IsBot, From, StateData);
        {false, ?MODE_NORMAL} ->
            IgnoredPlayers = [Id || #player{id = Id} <- midict:geti(UserId, user_id, Players)],
            case find_free_seat_without_players(Seats, IgnoredPlayers) of
                #seat{table = TabId, seat_num = SeatNum} ->
                    ?INFO("FL_LUCKY <~p> Found a table with free seats. TabId: ~p SeatNum: ~p.", [GameId, TabId, SeatNum]),
                    reg_player_normaly(UserId, IsBot, TabId, SeatNum, From, StateData);
                not_found ->
                    ?INFO("FL_LUCKY <~p> There are no table with free seats.", [GameId]),
                    case find_bot_seat_without_players(Seats, IgnoredPlayers) of
                        #seat{table = TabId, seat_num = SeatNum, player_id = OldPlayerId} ->
                            ?INFO("FL_LUCKY <~p> Found a seat with a bot. Replacing by the user. UserId:~p TabId: ~p SeatNum: ~p.",
                                  [GameId, UserId, TabId, SeatNum]),
                            reg_player_with_replace(UserId, IsBot, TabId, SeatNum, OldPlayerId, From, StateData);
                        not_found ->
                            ?INFO("FL_LUCKY <~p> There are no seats with bots. Creating new table for user: ~p.",
                                  [GameId, UserId]),
                            reg_player_at_new_table(UserId, IsBot, From, StateData)
                    end
            end
    end;

handle_sync_client_event(_Event, _From, StateName, StateData) ->
   Reply = {error, unexpected_request},
   {reply, Reply, StateName, StateData}.

reg_player_normaly(UserId, IsBot, TabId, SeatNum, From,
                   #state{game_id = GameId,
                          players = Players,
                          tables = Tables,
                          seats = Seats,
                          player_id_counter = PlayerId
                         } = StateData) ->
    NewPlayers = reg_player(#player{id = PlayerId, user_id = UserId, is_bot = IsBot}, Players),
    ?INFO("FL_LUCKY <~p> User ~p registered as player <~p>.", [GameId, UserId, PlayerId]),
    NewSeats = assign_seat(TabId, SeatNum, PlayerId, IsBot, Seats),
    ?INFO("FL_LUCKY <~p> User ~p assigned to seat <~p> of table <~p>.", [GameId, UserId, SeatNum, TabId]),
    TabPid = get_table_pid(TabId, Tables),
    send_to_table(TabPid, {register, PlayerId, SeatNum, reg_confirm_fun(From, PlayerId, {?TAB_MOD, TabPid})}),
    {next_state, ?STATE_PROCESSING, StateData#state{players = NewPlayers,
                                                    seats = NewSeats,
                                                    player_id_counter = PlayerId + 1}}.


reg_player_with_replace(UserId, IsBot, TabId, SeatNum, OldPlayerId, From,
                        #state{game_id = GameId,
                               players = Players,
                               tables = Tables,
                               seats = Seats,
                               player_id_counter = PlayerId
                              } = StateData) ->
    NewPlayers = reg_player(#player{id = PlayerId, user_id = UserId, is_bot = IsBot}, Players),
    ?INFO("FL_LUCKY <~p> User ~p registered as player <~p>.", [GameId, UserId, PlayerId]),
    NewSeats = assign_seat(TabId, SeatNum, PlayerId, IsBot, Seats),
    ?INFO("FL_LUCKY <~p> User ~p assigned to seat <~p> of table <~p>.", [GameId, UserId, SeatNum, TabId]),
%%    OldUserId = get_player_user_id(OldPlayerId, Players),
    TabPid = get_table_pid(TabId, Tables),
    send_to_table(TabPid, {replace, OldPlayerId, PlayerId, SeatNum, reg_confirm_fun(From, PlayerId, {?TAB_MOD, TabPid})}),
    {next_state, ?STATE_PROCESSING, StateData#state{players = NewPlayers,
                                                    seats = NewSeats,
                                                    player_id_counter = PlayerId + 1}}.

reg_player_at_new_table(UserId, IsBot, From,
                        #state{game_id = GameId,
                               players = Players,
                               tables = Tables,
                               seats = Seats,
                               player_id_counter = PlayerId,
                               table_id_counter = TableId,
                               params = TableParams
                              } = StateData) ->
    {ok, TabPid} = spawn_table(GameId, TableId, TableParams),
    MonRef = erlang:monitor(process, TabPid),
    %% FIXME: Table global id should use a persistent counter
    NewTables = reg_table(#table{id = TableId, global_id = 0, pid = TabPid, mon_ref = MonRef}, Tables),
    ?INFO("FL_LUCKY <~p> New table created: ~p.", [GameId, TableId]),
    SeatsNum = seats_num(TableParams),
    NewSeats = create_seats(TableId, SeatsNum, Seats),
    NewPlayers = reg_player(#player{id = PlayerId, user_id = UserId, is_bot = IsBot}, Players),
    ?INFO("FL_LUCKY <~p> User ~p registered as player <~p>.", [GameId, UserId, PlayerId]),
    NewSeats2 = assign_seat(TableId, 1, PlayerId, IsBot, NewSeats),
    ?INFO("FL_LUCKY <~p> User ~p assigned to seat <~p> of table <~p>.", [GameId, UserId, 1, TableId]),
    GameParams = proplists:get_value(game_params, TableParams),
    spawn_bots(GameId, GameParams, SeatsNum -1),
    ?INFO("FL_LUCKY <~p> Bots for table <~p> are spawned.", [GameId, TableId]),
    send_to_table(TabPid, {register, PlayerId, 1, reg_confirm_fun(From, PlayerId, {?TAB_MOD, TabPid})}),
    ?INFO("FL_LUCKY <~p> Registration of user ~p completed.", [GameId, UserId]),
    {next_state, ?STATE_PROCESSING, StateData#state{players = NewPlayers,
                                                    seats = NewSeats2,
                                                    tables = NewTables,
                                                    player_id_counter = PlayerId + 1,
                                                    table_id_counter = TableId + 1}}.

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

%% get_player_user_id(PlayerId, Players) -> UserId
get_player_user_id(PlayerId, Players) ->
    {ok, #player{user_id = UserId}} = midict:find(PlayerId, Players),
    UserId.

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

reg_table(#table{id = TableId, pid = Pid, mon_ref = MonRef, global_id = GlobalId} = Table, Tables) ->
    midict:store(TableId, Table, [{pid, Pid}, {global_id, GlobalId}, {mon_ref, MonRef}], Tables).

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
           ?INFO("fl_lucky Seats:~p", [midict:geti(TabId, table_id, Seats)]),
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
           ?INFO("fl_lucky Seats:~p", [midict:geti(TabId, table_id, Seats)]),
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

find_non_free_seats_at_tab(TabId, Seats) ->
    midict:geti(TabId, non_free_at_tab, Seats).

find_real_players_seats_at_tab(TabId, Seats) ->
    midict:geti(TabId, real_player_at_tab, Seats).

%% assign_seat(TabId, SeatNum, PlayerId, IsBot, Seats) -> NewSeats
%% PlayerId = integer()
assign_seat(TabId, SeatNum, PlayerId, IsBot, Seats) ->
    Seat = #seat{table = TabId, seat_num = SeatNum, player_id = PlayerId, is_bot = IsBot},
    Indices = if PlayerId == undefined ->
                     [{table_id, TabId}, {free, true}, {free_at_tab, TabId}];
                 true ->
                     I = [{table_id, TabId}, {free, false}, {non_free_at_tab, TabId},
                          {player_id, PlayerId}, {is_bot, IsBot}],
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

spawn_bots(_GameId, _Params, 0) -> ok;
spawn_bots(GameId, Params, BotsNum) ->
    spawn_bot(bot_module(Params), GameId),
    spawn_bots(GameId, Params, BotsNum-1).

spawn_bot(BM, GameId) ->
    {NPid, _SPid, _NUId, _User} = create_robot(BM, GameId),
    BM:join_game(NPid).

create_robot(BM, GameId) ->
    User = auth_server:robot_credentials(),
    NUId = User#'PlayerInfo'.id,
    {ok, NPid} = BM:start_link(self(), User, GameId),
    SPid = BM:get_session(NPid),
    {NPid, SPid, NUId, User}.

spawn_table(GameId, TableId, Params) ->
    Pid = sl_simple:start(GameId, TableId, Params),
    Pid.

send_to_table(TabPid, Message) ->
    ?TAB_MOD:parent_send(TabPid, Message).

%% table_parameters(GameType, ParentPid) -> Proplist
table_parameters(game_okey, ParentMod, ParentPid) ->
    [
     {game_module, game_okey},
     {seats_num, 4},
     {parent, {ParentMod, ParentPid}},
     {game_params, [
                    {game, game_okey},
                    {game_mode, standard},
                    {speed, normal},
                    {rounds, inifinity},
                    {pointing_rule, []}
                   ]}
    ];

table_parameters(game_tavla, ParentMod, ParentPid) ->
    [
     {game_module, game_tavla},
     {seats_num, 2},
     {parent, {ParentMod, ParentPid}},
     {game_params, [
                    {game, game_tavla},
                    {game_mode, standard},
                    {speed, normal},
                    {rounds, infinity}
                   ]}
    ].

game_type_to_str(game_okey) -> "Okey";
game_type_to_str(game_tavla) -> "Tavla".

seats_num(TableParams) ->
    proplists:get_value(seats_num, TableParams).

bot_module(TableParams) ->
    case proplists:get_value(game, TableParams) of
        game_okey -> game_okey_bot;
        game_tavla -> game_tavla_bot
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

