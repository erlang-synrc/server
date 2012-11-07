%%% -------------------------------------------------------------------
%%% Author  : Sergei Polkovnikov <serge.polkovnikov@gmail.com>
%%% Description : The table logic for simple games
%%%
%%% Created : Oct 01, 2012
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

-module(sl_simple).

-behaviour(gen_fsm).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("nsx_config/include/log.hrl").
-include_lib("nsg_srv/include/basic_types.hrl").
-include_lib("nsg_srv/include/requests.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([start/3, subscribe/4, submit/3, signal/3]).
-export([publish/2, to_session/3, update_gamestate/2]).

%% gen_fsm callbacks
-export([init/1, state_name/2, state_name/3, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-export([parent_send/2, game_send/2, client_send/2, client_sync_send/2, client_sync_send/3]).

-record(state,
        {
         game_id           :: pos_integer(),
         table_id          :: pos_integer(),
         params            :: proplists:proplist(),
         parent            :: pid(),
         players,          %% The register of table players
         seats,            %% Stores relation between players and table seats
         rule_module       :: atom(),
         rule_pid          :: undefined | pid()
        }).

-record(player,
        {
         id              :: pos_integer(), %% Player Id
         user_id,
         is_bot          :: boolean(),
         pid             :: pid(),
         info            :: #'PlayerInfo'{},
         mon_ref
        }).

-record(seat,
        {
         seat_num        :: integer(),
         player_id       :: undefined | pos_integer(),
         waiting_for     :: undefined | pos_integer()
        }).


-define(STATE_WAITING_FOR_PLAYERS, state_waiting_for_players).
-define(STATE_WAITING_FOR_START, state_waiting_for_start).
-define(STATE_PLAYING, state_playing).

%% ====================================================================
%% External functions
%% ====================================================================

start(GameId, TableId, Params) ->
    gen_fsm:start(?MODULE, [GameId, TableId, Params], []).

subscribe(Relay, SessionPid, User, RegNum) ->
    client_sync_send(Relay, {subscribe, SessionPid, User, RegNum}, 10000).

submit(Relay, _UserId, Command) ->
    client_sync_send(Relay, {submit, Command}).

signal(Relay, _UserId, Message) ->
    client_send(Relay, {signal, Message}).

publish(Relay, Message) ->
    game_send(Relay, {publish, Message}).

to_session(Relay, SessionPid, Message) -> %% Fixme PlayerId should be passed instead SessionPid
    game_send(Relay, {to_session, SessionPid, Message}).

update_gamestate(Relay, GameState) ->
    game_send(Relay, {update_gamestate, GameState}).



parent_send(Pid, Message) ->
    gen_fsm:send_all_state_event(Pid, {parent, Message}).

game_send(Pid, Message) ->
    gen_fsm:send_all_state_event(Pid, {game, Message}).

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
init([GameId, TableId, TableParams]) ->
    SeatsNum = proplists:get_value(seats_num, TableParams),
    Parent = proplists:get_value(parent, TableParams),
    GameModule = proplists:get_value(game_module, TableParams),
    ?INFO("SL_SIMPLE <~p,~p> Started.", [GameId, TableId]),
    {ok, ?STATE_WAITING_FOR_PLAYERS, #state{game_id = GameId,
                                            table_id = TableId,
                                            rule_module = GameModule,
                                            params = TableParams,
                                            parent = Parent,
                                            players = players_init(),
                                            seats = create_empty_seats(SeatsNum)
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

handle_event({parent, Event}, StateName, StateData) ->
    handle_parent_event(Event, StateName, StateData);

handle_event({game, Event}, StateName, StateData) ->
    handle_game_event(Event, StateName, StateData);

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
handle_info({'DOWN', _, process, Pid, _}, StateName,
            #state{players = Players, parent = Parent, table_id = TabId} = StateData) ->
    case get_player_by_pid(Pid, Players) of
        #player{id = PlayerId} ->
            send_to_parent(Parent, {player_disconnected, TabId, PlayerId});
        not_found -> do_nothing
    end,
    {next_state, StateName, StateData};

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

%% Clients events

handle_client_event({signal, Msg}, StateName,
                     #state{rule_module = GameFSM, rule_pid = Pid} = StateData) ->
    GameFSM:signal(Pid, Msg),
    {next_state, StateName, StateData};

handle_client_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.


%% Parent events

handle_parent_event({register, PlayerId, SeatNum, ConfirmFun}, StateName,
                    #state{game_id = GameId, table_id = TableId, seats = Seats} = StateData) ->
    ?INFO("SL_SIMPLE <~p,~p> Received command to register player <~p> to seat <~p>.", [GameId, TableId, PlayerId, SeatNum]),
    NewSeats = set_waiting_for_seat(SeatNum, PlayerId, Seats),
    ConfirmFun(),
    {next_state, StateName, StateData#state{seats = NewSeats}};

handle_parent_event({replace, OldPlayerId, PlayerId, SeatNum, ConfirmFun}, StateName,
                    #state{game_id = GameId, table_id = TableId, seats = Seats} = StateData) ->
    ?INFO("SL_SIMPLE <~p,~p> Received command to replace player <~p> by player <~p> at seat <~p>.",
          [GameId, TableId, OldPlayerId, PlayerId, SeatNum]),
    NewSeats = set_waiting_for_seat(SeatNum, PlayerId, Seats),
    ConfirmFun(),
    {next_state, StateName, StateData#state{seats = NewSeats}};

handle_parent_event(start_game, ?STATE_WAITING_FOR_START,
                    #state{game_id = GameId, table_id = TableId, 
                           rule_module = GameFSM, rule_pid = RPid} = StateData) ->
    ?INFO("SL_SIMPLE <~p,~p> Received command to start game at the table. Starting...", [GameId, TableId]),
    GameFSM:signal(RPid, state_created),
    {next_state, ?STATE_PLAYING, StateData};

handle_parent_event(stop, _StateName,
                    #state{game_id = GameId, table_id = TableId, 
                           players = Players} = StateData) ->
    ?INFO("SL_SIMPLE <~p,~p> Received command to stop. Initiate stopping procedure...", [GameId, TableId]),
    [begin
         erlang:demonitor(MonRef, [flush]),
         game_session:logout(Pid)
     end || #player{pid = Pid, mon_ref = MonRef} <- players_to_list(Players)],
    ?INFO("SL_SIMPLE <~p,~p> Stopping procedure finished. Stopping the process.", [GameId, TableId]),
    {stop, normal, StateData};

handle_parent_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.


%% Game logic evensts

handle_game_event({publish, #game_event{game = undefined}=Msg}, StateName,
                   #state{game_id = GameId} = StateData) ->
    handle_game_event({publish, Msg#game_event{game = GameId}}, StateName, StateData);

handle_game_event({publish, Msg}, StateName,
                  #state{players = Players} = StateData) ->
    [gen_server:cast(Pid, Msg) || #player{pid = Pid} <- players_to_list(Players)],
    {next_state, StateName, StateData};

handle_game_event({to_session, SessionPid, #game_event{game = undefined}=Msg}, StateName,
                  #state{game_id = GameId} = StateData) ->
     handle_game_event({to_session, SessionPid, Msg#game_event{game = GameId}}, StateName, StateData);

handle_game_event({to_session, SessionPid, Msg}, StateName,
                  #state{players = Players} = StateData) ->
    SessionPid ! Msg, %% TODO Unificate the way to send messages to a client session
    {next_state, StateName, StateData};

handle_game_event({update_gamestate, _GameState}, StateName,
                  StateData) ->
    %% TODO Ignore the event

    %% NewGameState == finished andalso
    %%    gen_server:cast(self(), start_rematch_timer),
    {next_state, StateName, StateData};

handle_game_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.



%% Client sync evensts

handle_sync_client_event({subscribe, Pid, UserInfo, PlayerId}, _From,
                         ?STATE_WAITING_FOR_PLAYERS, #state{seats = Seats,
                                                            players = Players,
                                                            parent = Parent,
                                                            table_id = TabId,
                                                            game_id = Topic,
                                                            rule_module = GameFSM,
                                                            params = TableParams
                                                           } = StateData) ->
    ?INFO("SL_SIMPLE <~p,~p> Subscribe request from user ~p, player id <~p>.",
          [Topic, TabId, UserInfo#'PlayerInfo'.id, PlayerId]),
    case find_waiting_seat(PlayerId, Seats) of
        #seat{seat_num = SeatNum, player_id = OldPlayerId} ->
            NewPlayers =
                if OldPlayerId =/= undefined -> %% Close the session of the previous player
                       #player{mon_ref = OldMonRef, pid = OldPid} = fetch_player(OldPlayerId, Players),
                       erlang:demonitor(OldMonRef, [flush]),
                       game_session:logout(OldPid),%% TODO Unificate the way to send messages to a client session
                       del_player(OldPlayerId, Players);
                   true -> Players
                end,
            #'PlayerInfo'{id = UserId, robot = IsBot} = UserInfo,
            Ref = erlang:monitor(process, Pid),
            NewPlayers2 = reg_player(PlayerId, UserId, Pid, IsBot, Ref, UserInfo, NewPlayers),
            NewSeats = take_seat(PlayerId, SeatNum, Seats),
            ?INFO("SL_SIMPLE <~p,~p> Subscribed user ~p, player id <~p>.",
                  [Topic, TabId, UserInfo#'PlayerInfo'.id, PlayerId]),
            case free_seats_num(NewSeats) of
                0 ->
                    PidsWithPInfo = all_players_pids_info(NewPlayers2), %% FIXME: Only players on seats needed here
                    GameParams = proplists:get_value(game_params, TableParams),
                    {ok, RPid} = GameFSM:start({?MODULE, self()}, PidsWithPInfo, Topic, GameParams),
                    send_to_parent(Parent, {table_ready, TabId}),
                    ?INFO("SL_SIMPLE <~p,~p> All players connected. Waiting for signal to start.", [Topic, TabId]),
                    {reply, ok, ?STATE_WAITING_FOR_START, StateData#state{seats = NewSeats,
                                                                          players = NewPlayers2,
                                                                          rule_pid = RPid}};
                N ->
                    ?INFO("SL_SIMPLE <~p,~p> Waiting for ~p players to start.",
                          [Topic, TabId, N]),
                    {reply, ok, ?STATE_WAITING_FOR_PLAYERS, StateData#state{seats = NewSeats,
                                                                            players = NewPlayers2}}
            end;
        not_found ->
            ?INFO("SL_SIMPLE <~p,~p> User ~p, player id <~p> is not allowed to participate. Subscription rejected.",
                  [Topic, TabId, UserInfo#'PlayerInfo'.id, PlayerId]),
            {reply, {error, not_allowed}, ?STATE_WAITING_FOR_PLAYERS, StateData}
    end;

handle_sync_client_event({subscribe, Pid, UserInfo, PlayerId}, _From,
                         ?STATE_PLAYING, #state{seats = Seats,
                                                players = Players,
                                                parent = Parent,
                                                table_id = TabId,
                                                game_id = Topic,
                                                rule_module = GameFSM,
                                                rule_pid = RPid,
                                                params = TableParams
                                               } = StateData) ->
    ?INFO("SL_SIMPLE <~p,~p> Subscribe request from user ~p, player id <~p>.",
          [Topic, TabId, UserInfo#'PlayerInfo'.id, PlayerId]),
    case find_waiting_seat(PlayerId, Seats) of
        #seat{seat_num = SeatNum, player_id = OldPlayerId} ->
            #player{user_id = OldUserId, mon_ref = OldMonRef, pid = OldPid} = fetch_player(OldPlayerId, Players),
            erlang:demonitor(OldMonRef, [flush]),
            game_session:logout(OldPid),%% TODO Unificate the way to send messages to a client session
            NewPlayers = del_player(OldPlayerId, Players),
            #'PlayerInfo'{id = UserId, robot = IsBot} = UserInfo,
            Ref = erlang:monitor(process, Pid),
            NewPlayers2 = reg_player(PlayerId, UserId, Pid, IsBot, Ref, UserInfo, NewPlayers),
            NewSeats = take_seat(PlayerId, SeatNum, Seats),
            ?INFO("SL_SIMPLE <~p,~p> Subscribed user ~p, player id <~p>.",
                  [Topic, TabId, UserInfo#'PlayerInfo'.id, PlayerId]),
            %% The code copypasted from relay
            {PlayerMsgs, _RobotData} =
                GameFSM:signal(RPid, {replace_player, OldUserId, UserId, UserInfo, Pid}),
            [ Pid ! #game_event{game = Topic, %% TODO Unificate the way to send messages to a client session
                                event = api_utils:name(Msg),
                                args = api_utils:members(Msg)}
                               || Msg <- PlayerMsgs ],
            Msg = #player_left{player = OldUserId, human_replaced = not IsBot, bot_replaced = IsBot, replacement = UserInfo},
            publish(self(), #game_event{event = api_utils:name(Msg), args = api_utils:members(Msg)}),
            %% End of strange code
            {reply, ok, ?STATE_PLAYING, StateData#state{seats = NewSeats,
                                                        players = NewPlayers2}};
        not_found ->
            ?INFO("SL_SIMPLE <~p,~p> User ~p, player id <~p> is not allowed to participate. Subscription rejected.",
                  [Topic, TabId, UserInfo#'PlayerInfo'.id, PlayerId]),
            {reply, {error, not_allowed}, ?STATE_PLAYING, StateData}
    end;

handle_sync_client_event({submit, Command}, {FromPid, _}=_From, StateName,
                          #state{rule_module = GameFSM, rule_pid = Pid} = StateData) ->
   Res = GameFSM:make_move(Pid, FromPid, Command),
   {reply, Res, StateName, StateData};

handle_sync_client_event(Event, From, StateName,
                         #state{game_id = GameId, table_id = TabId} = StateData) ->
    ?INFO("SL_SIMPLE <~p,~p> Unexpected request: ~p. From: ~p, StateName: ~p.",
          [GameId, TabId, Event, From]),
    Reply = {error, unexpected_request},
    {reply, Reply, StateName, StateData}.







%% players_init() -> players()
players_init() ->
    midict:new().

%% reg_player(PlayerId, UserId, Pid, IsBot, MonRef, Players) -> NewPlayers
reg_player(PlayerId, UserId, Pid, IsBot, MonRef, UserInfo, Players) ->
    reg_player(#player{id =PlayerId, user_id = UserId, is_bot = IsBot, pid = Pid, info = UserInfo, mon_ref = MonRef}, Players).

%% reg_player(#player{}, Players) -> NewPlayers
reg_player(#player{id =Id, user_id = UserId, is_bot = IsBot, pid = Pid, mon_ref = MonRef} = Player, Players) ->
    midict:store(Id, Player, [{user_id, UserId}, {is_bot, IsBot}, {pid, Pid}, {mon_ref, MonRef}], Players).

%% get_player_user_id(PlayerId, Players) -> UserId
get_player_user_id(PlayerId, Players) ->
    #player{user_id = UserId} = midict:fetch(PlayerId, Players),
    UserId.

get_player_by_pid(Pid, Players) ->
    case midict:geti(Pid, pid, Players) of
        [Player] -> Player;
        [] -> not_found
    end.

fetch_player(PlayerId, Players) ->
    midict:fetch(PlayerId, Players).

%% del_player(PlayerId, Players) -> NewPlayers
del_player(PlayerId, Players) ->
    midict:erase(PlayerId, Players).

%% players_num(Players) -> Num
players_num(Players) ->
    midict:size(Players).

%% players_to_list(Players) -> List
players_to_list(Players) ->
    midict:all_values(Players).

all_players_pids_info(Players) ->
    [{Pid, Info} || #player{pid = Pid, info = Info} <- midict:all_values(Players)].



seats_init() ->
    midict:new().

free_seats_num(Seats) ->
    length(midict:geti(true, free, Seats)).

find_waiting_seat(PlayerId, Seats) ->
    case midict:geti(PlayerId, waiting_for, Seats) of
        [Seat] -> Seat;
        [] -> not_found
    end.

find_seats_by_player_id(PlayerId, Seats) ->
    midict:geti(PlayerId, player_id, Seats).

%% store_seat(SeatNum, PlayerId,  WaitingFor, Seats) -> NewSeats
%% PlayerId = integer()
store_seat(SeatNum, PlayerId, WaitingFor, Seats) ->
    Seat = #seat{seat_num = SeatNum, player_id = PlayerId,
                 waiting_for = WaitingFor},
    store_seat(Seat, Seats).

%% store_seat(Seat, Seats) -> NewSeats
store_seat(#seat{seat_num = SeatNum, player_id = PlayerId,
                 waiting_for = WaitingFor} = Seat, Seats) ->
    FreeIndex = [{free, PlayerId == undefined}],
    WaitingForIndex = if WaitingFor == undefined -> []; true -> [{waiting_for, WaitingFor}] end,
    Indices = FreeIndex ++ WaitingForIndex,
    midict:store(SeatNum, Seat, Indices, Seats).

take_seat(PlayerId, SeatNum, Seats) ->
    store_seat(SeatNum, PlayerId, undefined, Seats).

create_empty_seats(SeatsNum) ->
    create_empty_seats(SeatsNum, seats_init()).

create_empty_seats(0, Seats) -> Seats;
create_empty_seats(SeatNum, Seats) ->
    NewSeats = store_seat(SeatNum,  undefined, undefined, Seats),
    create_empty_seats(SeatNum - 1, NewSeats).

free_seat(SeatNum, Seats) ->
    store_seat(SeatNum, undefined, undefined, Seats).

set_waiting_for_seat(SeatNum, PlayerId, Seats) ->
    Seat = midict:fetch(SeatNum, Seats),
    store_seat(Seat#seat{waiting_for = PlayerId}, Seats).

send_to_parent({Mod, Pid}, Message) ->
    Mod:child_send(Pid, Message).

