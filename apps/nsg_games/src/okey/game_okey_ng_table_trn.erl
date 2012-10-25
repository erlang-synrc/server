%%% -------------------------------------------------------------------
%%% Author  : Sergii Polkovnikov <serge.polkovnikov@gmail.com>
%%% Description :
%%%
%%% Created : Oct 15, 2012
%%% -------------------------------------------------------------------
-module(game_okey_ng_table_trn).

-behaviour(gen_fsm).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("nsx_config/include/log.hrl").
-include_lib("nsg_srv/include/basic_types.hrl").
-include_lib("nsg_srv/include/settings.hrl").
-include_lib("nsg_srv/include/game_okey.hrl").
-include_lib("nsg_srv/include/requests.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([start/3,
         player_action/3,
         parent_message/2,
         relay_message/2
        ]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4,
         handle_info/3, terminate/3, code_change/4]).


-type tash() :: false_okey | {integer(), integer()}.

-record(desk_state,
        {
         state              :: state_take | state_discard | state_finished,
         hands              :: list({integer(), list(tash())}), %% {SeatNum, Tashes}
         discarded          :: list({integer(), list(tash())}), %% {SeatNum, Tashes}
         deck               :: list(tash()),
         cur_seat           :: integer(),
         gosterge           :: tash(),
         has_gosterge       :: undefined | integer(), %% Seat num of a player who has gosterge
         finish_reason      :: tashes_out | reveal, %% Defined only when state = state_finished
         reveal_info        :: tuple() %% {SeatNum::integer(), Right::boolean(), RevealedTashes::list(null | tash()),
                                       %%  DiscardedTash::tash(), WithOkey::boolean(), HasGosterge::boolean()}
        }).

-record(state,
        {%% Fixed parameters
         game_id              :: pos_integer(),
         table_id             :: pos_integer(),
         table_name           :: string(),
         params               :: proplists:proplist(),
         parent               :: {atom(), pid()},
         relay                :: pid(),
         mult_factor          :: integer(),
         slang_flag           :: boolean(),
         observer_flag        :: boolean(),
         speed                :: slow | normal | fast,
         turn_timeout         :: integer(),
         reveal_confirmation_timeout    :: integer(),
         ready_timeout        :: integer(),
         game_type            :: standard | color | evenodd | countdown,
         rounds               :: undefined | integer(), %% Not defined for countdown game type
         reveal_confirmation  :: boolean(),
         %% Dynamic parameters
         desk_rule_pid        :: undefined | pid(),
         players,             %% The register of table players
         start_seat           :: integer(), %% The player who moves first
         cur_round            :: integer(),
         desk_state           :: #desk_state{}, %% For tracking current state of a game on the table
         reveal_confirmation_list :: list(), %% {SeatNum, Answer}
         wait_list            :: list(),
         timeout_timer        :: undefined | reference()
        }).

-record(player,
        {
         id              :: pos_integer(), %% Player Id
         seat_num        :: integer(),
         user_id         :: binary(),
         is_bot          :: boolean(),
         info            :: #'PlayerInfo'{}
        }).

-define(STATE_WAITING_FOR_START, state_waiting_for_start).
-define(STATE_PLAYING, state_playing).
-define(STATE_REVEAL_CONFIRMATION, state_reveal_confirmation).

-define(HAND_SIZE, 14).
-define(SEATS_NUM, 4).
-define(RELAY, relay_ng).
-define(DESK, game_okey_ng_desk).

%% ====================================================================
%% External functions
%% ====================================================================

start(GameId, TableId, TableParams) ->
    gen_fsm:start(?MODULE, [GameId, TableId, TableParams], []).

player_action(Srv, PlayerId, Action) ->
    gen_fsm:sync_send_all_state_event(Srv, {player_action, PlayerId, Action}).

parent_message(Srv, Message) ->
    gen_fsm:send_all_state_event(Srv, {parent_message, Message}).

relay_message(Srv, Message) ->
    gen_fsm:send_all_state_event(Srv, {relay_message, Message}).

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
init([GameId, TableId, Params]) ->
    Parent = proplists:get_value(parent, Params),
    PlayersInfo = proplists:get_value(players, Params),
    TableName = proplists:get_value(table_name, Params),
    MultFactor = proplists:get_value(mult_factor, Params),
    SlangFlag = proplists:get_value(slang_allowed, Params),
    ObserversFlag = proplists:get_value(observers_allowed, Params),
    Speed = proplists:get_value(speed, Params),
    GameType = proplists:get_value(game_type, Params),
    Rounds = proplists:get_value(rounds, Params),
    RevealConfirmation = proplists:get_value(reveal_confirmation, Params),

    Players = init_players(PlayersInfo),
    RelayParams = [{players, [{PlayerId, UserInfo#'PlayerInfo'.id} || {PlayerId, UserInfo, _} <- PlayersInfo]},
                   {observers_allowed, false},
                   {table, {?MODULE, self()}}],
    {ok, Relay} = ?RELAY:start(RelayParams),
    ?INFO("OKEY_NG_TABLE_TRN <~p,~p> Started.", [GameId, TableId]),
    parent_notify_table_created(Parent, TableId, Relay),
    {ok, ?STATE_WAITING_FOR_START, #state{game_id = GameId,
                                          table_id = TableId,
                                          table_name = TableName,
                                          params = Params,
                                          parent = Parent,
                                          relay = Relay,
                                          mult_factor = MultFactor,
                                          slang_flag = SlangFlag,
                                          observer_flag = ObserversFlag,
                                          speed = Speed,
                                          turn_timeout = get_timeout(turn, Speed),
                                          reveal_confirmation_timeout = get_timeout(challenge, Speed),
                                          ready_timeout = get_timeout(ready, Speed),
                                          game_type = GameType,
                                          rounds = Rounds,
                                          reveal_confirmation = RevealConfirmation,
                                          players = Players,
                                          start_seat = 1,
                                          cur_round = 1
                                         }}.

%% --------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
handle_event({parent_message, Message}, StateName, StateData) ->
    handle_parent_message(Message, StateName, StateData);

handle_event({relay_message, Message}, StateName, StateData) ->
    handle_relay_message(Message, StateName, StateData);

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
handle_sync_event({player_action, PlayerId, Action}, From, StateName, StateData) ->
    handle_player_action(PlayerId, Action, From, StateName, StateData);

handle_sync_event(_Event, _From, StateName, StateData) ->
    Reply = ok,
    {reply, Reply, StateName, StateData}.

%% --------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
handle_info({turn_timeout, SeatNum}, ?STATE_PLAYING,
            #state{desk_state = #desk_state{cur_seat = SeatNum}} = StateData) ->
    do_timeout_moves(StateData);

handle_info(reveal_timeout, ?STATE_REVEAL_CONFIRMATION,
            #state{wait_list = WL, reveal_confirmation_list = CList} = StateData) ->
    NewCList = lists:foldl(fun(SeatNum, Acc) -> [{SeatNum, false} | Acc] end, CList, WL), %% FIXME: false?
    finalize_round(StateData#state{reveal_confirmation_list = NewCList});

handle_info(_Info, StateName, StateData) ->
    {next_state, StateName, StateData}.

%% --------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%% --------------------------------------------------------------------
terminate(_Reason, _StateName, #state{relay = Relay}) ->
    ?RELAY:stop(Relay),
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

%% --------------------------------------------------------------------
%% Func: handle_parent_message/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
handle_parent_message({register_player, RequestId, UserInfo, PlayerId, SeatNum}, StateName,
                      #state{game_id = GameId, table_id = TableId, players = Players,
                             parent = Parent, relay = Relay} = StateData) ->
    ?INFO("OKEY_NG_TABLE_TRN <~p,~p> Received command to register player <~p> to seat <~p>. RequestId: ~p.",
          [GameId, TableId, PlayerId, SeatNum, RequestId]),
    #'PlayerInfo'{id = UserId, robot = IsBot} = UserInfo,
    NewPlayers = reg_player(PlayerId, SeatNum, UserId, IsBot, UserInfo, Players),
    ?RELAY:table_request(Relay, {register_player, UserId, PlayerId}), %% Sync registration in the relay
    %% TODO: Send notificitations to gamesessions
    parent_confirm_registration(Parent, TableId, RequestId),
    {next_state, StateName, StateData#state{players = NewPlayers}};

handle_parent_message({replace_player, RequestId, UserInfo, PlayerId, SeatNum}, StateName,
                      #state{game_id = GameId, table_id = TableId, players = Players,
                             parent = Parent, relay = Relay} = StateData) ->
    ?INFO("OKEY_NG_TABLE_TRN <~p,~p> Received command to replace player at seat <~p> by player <~p>.",
          [GameId, TableId, SeatNum, PlayerId]),
    #'PlayerInfo'{id = UserId, robot = IsBot} = UserInfo,
    NewPlayers = reg_player(PlayerId, SeatNum, UserId, IsBot, UserInfo, Players),
    ?RELAY:table_request(Relay, {register_player, UserId, PlayerId}), %% Sync registration in the relay
    ReplaceMsg = create_player_left(SeatNum, UserInfo, Players),
    publish_to_clients(Relay, ReplaceMsg),
    parent_confirm_replacement(Parent, TableId, RequestId),
    {next_state, StateName, StateData#state{players = NewPlayers}};

handle_parent_message(start_game, ?STATE_WAITING_FOR_START,
                      #state{game_id = GameId, table_id = TableId,
                             start_seat = StartSeat, players = Players,
                             relay = Relay} = StateData) ->
    ?INFO("OKEY_NG_TABLE_TRN Received command to start game at the table. Starting...", [GameId, TableId]),
    Deck = deck:shuffle(deck:init_deck(okey)),
    GostergePos = crypto:rand_uniform(0, deck:size(Deck)),
    Gosterge = deck:get(GostergePos, Deck),
    F = fun(SeatNum, AccDeck) ->
                Num = if SeatNum==StartSeat -> ?HAND_SIZE + 1; true -> ?HAND_SIZE end,
                lists:split(Num, AccDeck)
        end,
    {Hands, TableDeck} = lists:mapfoldl(F, deck:to_list(Deck), lists:seq(1, ?SEATS_NUM)),
    Params = [{hands, Hands},
              {deck, TableDeck},
              {gosterge, Gosterge},
              {cur_player, StartSeat}],
    {ok, Desk} = ?DESK:start(Params),
    DeskState = init_desk_state(Desk),
    %% Send notifications to clients
    [begin
         GameStartedMsg = create_okey_game_started(N, Desk, StateData),
         PlayerId = get_player_id_by_seat_num(N, Players),
         send_to_client_ge(Relay, PlayerId, GameStartedMsg)
     end || N <- lists:seq(1, ?SEATS_NUM)],
    CurSeat = DeskState#desk_state.cur_seat,
    publish_to_clients(Relay, create_okey_next_turn(CurSeat, Players)),
    {next_state, ?STATE_PLAYING, StateData#state{desk_rule_pid = Desk,
                                                 desk_state = DeskState}};

handle_parent_message(stop, _StateName,
                      #state{game_id = GameId, table_id = TableId} = StateData) ->
    ?INFO("OKEY_NG_TABLE_TRN <~p,~p> Received command to stop. Initiate stopping procedure...", [GameId, TableId]),
    {stop, normal, StateData};

handle_parent_message(Message, StateName,
                      #state{game_id = GameId, table_id = TableId} = StateData) ->
    ?ERROR("OKEY_NG_TABLE_TRN <~p,~p> Unknown parent message received in state <~p>: ~p. State: ~p. Stopping.",
           [GameId, TableId, StateName, Message]),
    {stop, unknown_parent_message, StateData}.


%% --------------------------------------------------------------------
%% Func: handle_relay_message/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
handle_relay_message({player_connected, PlayerId}, StateName,
                     #state{relay = Relay, parent = {ParentMod, ParentPid},
                            table_id = TableId} = StateData) ->
    GI = create_okey_game_info(StateData),
    PlState = create_okey_game_player_state(PlayerId, StateName, StateData),
    send_to_client_ge(Relay, PlayerId, GI),
    send_to_client_ge(Relay, PlayerId, PlState),
    ParentMod:table_message(ParentPid, TableId, {player_connected, PlayerId}),
    {next_state, StateName, StateData};


handle_relay_message({player_disconnected, PlayerId}, StateName,
                     #state{parent = {ParentMod, ParentPid},
                            table_id = TableId} = StateData) ->
    ParentMod:table_message(ParentPid, TableId, {player_disconnected, PlayerId}),
    {next_state, StateName, StateData};


handle_relay_message(_Message, StateName, StateData) ->
    {next_state, StateName, StateData}.

%% --------------------------------------------------------------------
%% Func: handle_player_action/4
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%% --------------------------------------------------------------------
handle_player_action(PlayerId, {submit, #game_action{action = Action, args = Args} = GA}, From,
                     StateName,
                     #state{players = Players, game_id = GameId,
                            table_id = TableId} = StateData) ->
    case get_player(PlayerId, Players) of
        {ok, #player{seat_num = SeatNum}} ->
            try api_utils:to_known_record(Action, Args) of
                ExtAction ->
                    do_action(SeatNum, ExtAction, From, StateName, StateData)
            catch
                _Type:_Reason ->
                    ?ERROR("OKEY_NG_TABLE_TRN <~p,~p> Invalid game action from player ~p: ~p. Ignoring.",
                           [GameId, TableId, PlayerId, GA]),
                    {reply, {error, invalid_action}, StateName, StateData}
            end;
        error ->
            {reply, {error, you_are_not_a_player}, StateName, StateData}
    end;

handle_player_action(_PlayerId, _Message, _From, StateName, StateData) ->
    {next_state, StateName, StateData}.



%%===================================================================

do_action(SeatNum, #okey_has_gosterge{}, From, ?STATE_PLAYING = StateName, StateData) ->
    do_game_action(SeatNum, i_have_gosterge, From, StateName, StateData);

do_action(_SeatNum, #okey_has_gosterge{}, _From, StateName, StateData) ->
    {reply, {error, message_not_valid_for_a_current_state}, StateName, StateData};


do_action(SeatNum, #okey_i_saw_okey{}, From, ?STATE_PLAYING = StateName, StateData) ->
    do_game_action(SeatNum, see_okey, From, StateName, StateData);

do_action(_SeatNum, #okey_i_saw_okey{}, _From, StateName, StateData) ->
    {reply, {error, message_not_valid_for_a_current_state}, StateName, StateData};


do_action(SeatNum, #okey_take{pile = 0}, From, ?STATE_PLAYING = StateName, StateData) ->
    do_game_action(SeatNum, take_from_table, From, StateName, StateData);

do_action(_SeatNum, #okey_take{pile = 0}, _From, StateName, StateData) ->
    {reply, {error, message_not_valid_for_a_current_state}, StateName, StateData};


do_action(SeatNum, #okey_take{pile = 1}, From, ?STATE_PLAYING = StateName, StateData) ->
    do_game_action(SeatNum, take_from_discarded, From, StateName, StateData);

do_action(_SeatNum, #okey_take{pile = 1}, _From, StateName, StateData) ->
    {reply, {error, message_not_valid_for_a_current_state}, StateName, StateData};


do_action(SeatNum, #okey_discard{tile = ExtTash}, From, ?STATE_PLAYING = StateName, StateData) ->
    Tash = ext_to_tash(ExtTash),
    do_game_action(SeatNum, {discard, Tash}, From, StateName, StateData);

do_action(_SeatNum, #okey_discard{}, _From, StateName, StateData) ->
    {reply, {error, message_not_valid_for_a_current_state}, StateName, StateData};


do_action(SeatNum, #okey_reveal{discarded = ExtDiscarded, hand = ExtHand}, From,
          ?STATE_PLAYING = StateName, StateData) ->
    Discarded = ext_to_tash(ExtDiscarded),
    Hand = [case E of
                null -> null;
                ExtTash -> ext_to_tash(ExtTash)
            end || E <- ExtHand],
    do_game_action(SeatNum, {reveal, Discarded, Hand}, From, StateName, StateData);

do_action(_SeatNum, #okey_reveal{}, _From, StateName, StateData) ->
    {reply, {error, message_not_valid_for_a_current_state}, StateName, StateData};


do_action(SeatNum, #okey_challenge{challenge = Challenge}, From,
          ?STATE_REVEAL_CONFIRMATION = StateName, #state{reveal_confirmation_list = CList,
                                                         wait_list = WL,
                                                         timeout_timer = TRef} = StateData) ->
    case lists:member(SeatNum, WL) of
        true ->
            NewCList = [{SeatNum, Challenge} | CList],
            NewWL = lists:delete(SeatNum, WL),
            if NewWL == [] ->
                   gen_fsm:reply(ok, From),
                   erlang:cancel_timer(TRef),
                   finalize_round(StateData#state{timeout_timer = undefined,
                                                  reveal_confirmation_list = NewCList});
               true ->
                   {reply, ok, StateName,
                    StateData#state{reveal_confirmation_list = NewCList,
                                    wait_list = NewWL}}
            end;
        false ->
            {reply, {error, not_your_turn}, StateName, StateData}
    end;

do_action(_SeatNum, #okey_challenge{}, _From, StateName, StateData) ->
    {reply, {error, message_not_valid_for_a_current_state}, StateName, StateData};


do_action(_SeatNum, #okey_ready{}, _From, StateName, StateData) ->
    {reply, {error, not_supported_yet}, StateName, StateData};


do_action(_SeatNum, _UnsupportedAction, _From, StateName, StateData) ->
    {reply, {error, unsupported}, StateName, StateData}.


%%===================================================================
do_timeout_moves(#state{desk_rule_pid = Desk,
                        desk_state = DeskState} = StateData) ->
    #desk_state{cur_seat = CurSeatNum,
                hands = Hands,
                state = DeskStateName} = DeskState,
    case DeskStateName of
        state_take ->
            {ok, Events1} = desk_player_action(Desk, CurSeatNum, take_from_discarded),
            [Tash] = [Tash || {taked_from_discarded, S, Tash} <- Events1, S==CurSeatNum],
            {ok, Events2} = desk_player_action(Desk, CurSeatNum, {discard, Tash}),
            Events2_1 = [case E of
                             {tash_discarded, SeatNum, Tash} ->
                                 {tash_discarded_timeout, SeatNum, Tash};
                             _ -> E
                         end || E <- Events2],
            Events = Events1 ++ [{auto_take_discard, CurSeatNum, Tash}] ++ Events2_1,
            process_game_events(Events, StateData);
        state_discard ->
            {_, [Tash | _]} = lists:keyfind(CurSeatNum, 1, Hands),
            {ok, Events1} = desk_player_action(Desk, CurSeatNum, {discard, Tash}),
            Events1_1 = [case E of
                             {tash_discarded, SeatNum, Tash} ->
                                 {tash_discarded_timeout, SeatNum, Tash};
                             _ -> E
                         end || E <- Events1],
            Events = [{auto_discard, CurSeatNum, Tash} | Events1_1],
            process_game_events(Events, StateData)
    end.

%%===================================================================

do_game_action(SeatNum, GameAction, From, StateName,
               #state{desk_rule_pid = Desk} = StateData) ->
    case desk_player_action(Desk, SeatNum, GameAction) of
        {ok, Events} ->
            Response = case GameAction of
                           i_have_gosterge ->
                               true;
                           take_from_table ->
                               [Tash] = [Tash || {taked_from_table, S, Tash} <- Events, S==SeatNum],
                               tash_to_ext(Tash);
                           take_from_discarded ->
                               [Tash] = [Tash || {taked_from_discarded, S, Tash} <- Events, S==SeatNum],
                               tash_to_ext(Tash);
                           _ -> ok
                       end,
            gen_fsm:reply(From, Response),
            process_game_events(Events, StateData);
        {error, Reason} ->
            ExtError = desk_error_to_ext(Reason),
            {reply, ExtError, StateName, StateData}
    end.


process_game_events(Events, #state{desk_state = DeskState,
                                   players = Players,
                                   relay = Relay,
                                   timeout_timer = OldTRef,
                                   turn_timeout = TurnTimeout} = StateData) ->
    NewDeskState = handle_desk_events(Events, DeskState, Players, Relay), %% Track the desk and send game events to clients
    #desk_state{state = DeskStateName,
                cur_seat = CurSeatNum} = NewDeskState,
    case DeskStateName of
        state_finish ->
            erlang:cancel_timer(OldTRef),
            on_game_finish(StateData#state{desk_state = NewDeskState});
        state_take ->
            case [E || {next_player, _} = E <- Events] of
                [] ->
                    {next_state, ?STATE_PLAYING, StateData#state{desk_state = NewDeskState}};
                [_|_] ->
                    erlang:cancel_timer(OldTRef),
                    TRef = erlang:send_after(TurnTimeout, self(), {turn_timeout, CurSeatNum}),
                    {next_state, ?STATE_PLAYING, StateData#state{desk_state = NewDeskState,
                                                                 timeout_timer = TRef}}
            end;
        state_discard ->
            {next_state, ?STATE_PLAYING, StateData#state{desk_state = NewDeskState}}
    end.


on_game_finish(#state{desk_state = DeskState,
                      reveal_confirmation = RevealConfirmation,
                      reveal_confirmation_timeout = Timeout} = StateData) ->
    #desk_state{finish_reason = FinishReason,
                reveal_info = {Revealer, _Tashes, _Discarded},
                hands = Hands} = DeskState,
    if FinishReason == reveal andalso RevealConfirmation ->
           WL = [SeatNum || {SeatNum, _} <- Hands, SeatNum =/= Revealer],
           TRef = erlang:send_after(Timeout, self(), reveal_timeout),
           {next_state, ?STATE_REVEAL_CONFIRMATION,
            StateData#state{reveal_confirmation_list = [],
                            wait_list = WL,
                            timeout_timer = TRef}};
       true ->
            finalize_round(StateData)
    end.

%%===================================================================

finalize_round(#state{desk_state = #desk_state{finish_reason = FinishReason,
                                               reveal_info = {Revealer, Tashes, Discarded},
                                               hands = Hands,
                                               gosterge = Gosterge,
                                               has_gosterge = WhoHasGosterge},
                      reveal_confirmation = RevealConfirmation,
                      reveal_confirmation_list = CList,
                      parent = Parent,
                      table_id = TableId} = StateData) ->
    FR = case FinishReason of
             tashes_out -> tashes_out;
             reveal ->
                 ConfirmationList = if RevealConfirmation -> CList; true -> [] end,
                 {reveal, Revealer, Tashes, Discarded, ConfirmationList}
         end,
    parent_send_round_res(Parent, TableId, FR, Hands, Gosterge, WhoHasGosterge),
%            Results = null,    %% FIXME: Real results needed
%            NextAction = next_round, %% FIXME: Real value needed
%            Msg = create_okey_round_ended_no_winner(Results, NextAction),
%            publish_to_clients(Relay, Msg),
    {next_state, ?STATE_WAITING_FOR_START, StateData}.



%% handle_desk_events(Events, DeskState, Players) -> NextStateData
%% Tracks the desk state and sends events to clients
handle_desk_events([], DeskState, _Players, _Relay) ->
    DeskState;

handle_desk_events([Event | Events], DeskState, Players, Relay) ->
    #desk_state{cur_seat = CurSeatNum,
                hands = Hands,
                discarded = Discarded,
                deck = Deck} = DeskState,
    NewDeskState =
        case Event of
            {has_gosterge, SeatNum} ->
                Msg = create_okey_player_has_gosterge(SeatNum, Players),
                publish_to_clients(Relay, Msg),
                DeskState#desk_state{has_gosterge = SeatNum};
            {saw_okey, SeatNum} ->
                Msg = create_okey_disable_okey(SeatNum, CurSeatNum, Players),
                publish_to_clients(Relay, Msg),
                DeskState;
            {taked_from_discarded, SeatNum, Tash} ->
                PrevSeatNum = prev_seat_num(SeatNum),
                {_, [Tash | NewPile]} = lists:keyfind(PrevSeatNum, 1, Discarded),
                Msg = create_okey_tile_taken_discarded(SeatNum, Tash, length(NewPile), Players),
                publish_to_clients(Relay, Msg),
                NewDiskarded = lists:keyreplace(PrevSeatNum, 1, Discarded, {PrevSeatNum, NewPile}),
                {_, Hand} = lists:keyfind(SeatNum, 1, Hands),
                NewHands = lists:keyreplace(SeatNum, 1, Hands, {SeatNum, [Tash | Hand]}),
                DeskState#desk_state{hands = NewHands, discarded = NewDiskarded};
            {taked_from_table, SeatNum, Tash} ->
                [Tash | NewDeck] = Deck,
                Msg = create_okey_tile_taken_table(SeatNum, length(NewDeck), Players),
                publish_to_clients(Relay, Msg),
                {_, Hand} = lists:keyfind(SeatNum, 1, Hands),
                NewHands = lists:keyreplace(SeatNum, 1, Hands, {SeatNum, [Tash | Hand]}),
                DeskState#desk_state{hands = NewHands, deck = NewDeck};
            {tash_discarded, SeatNum, Tash} ->
                Msg = create_okey_tile_discarded(SeatNum, Tash, false, Players),
                publish_to_clients(Relay, Msg),
                {_, Hand} = lists:keyfind(SeatNum, 1, Hands),
                NewHands = lists:keyreplace(SeatNum, 1, Hands, {SeatNum, lists:delete(Tash, Hand)}),
                {_, Pile} = lists:keyfind(SeatNum, 1, Discarded),
                NewDiscarded = lists:keyreplace(SeatNum, 1, Discarded, {SeatNum, [Tash | Pile]}),
                DeskState#desk_state{hands = NewHands, discarded = NewDiscarded, state = state_take};
            {tash_discarded_timeout, SeatNum, Tash} -> %% Injected event
                Msg = create_okey_tile_discarded(SeatNum, Tash, true, Players),
                publish_to_clients(Relay, Msg),
                {_, Hand} = lists:keyfind(SeatNum, 1, Hands),
                NewHands = lists:keyreplace(SeatNum, 1, Hands, {SeatNum, lists:delete(Tash, Hand)}),
                {_, Pile} = lists:keyfind(SeatNum, 1, Discarded),
                NewDiscarded = lists:keyreplace(SeatNum, 1, Discarded, {SeatNum, [Tash | Pile]}),
                DeskState#desk_state{hands = NewHands, discarded = NewDiscarded, state = state_take};
            {auto_take_discard, SeatNum, Tash} ->    %% Injected event
                #player{id = PlayerId} = get_player_by_seat_num(SeatNum, Players),
                Msg = create_okey_turn_timeout(Tash, Tash),
                send_to_client_ge(Relay, PlayerId, Msg),
                DeskState;
            {auto_discard, SeatNum, Tash} ->         %% Injected event
                #player{id = PlayerId} = get_player_by_seat_num(SeatNum, Players),
                Msg = create_okey_turn_timeout(null, Tash),
                send_to_client_ge(Relay, PlayerId, Msg),
                DeskState;
            {next_player, SeatNum} ->
                Msg = create_okey_next_turn(SeatNum, Players),
                publish_to_clients(Relay, Msg),
                DeskState#desk_state{cur_seat = SeatNum, state = state_take};
            no_winner_finish ->
                DeskState#desk_state{state = state_finished,
                                     finish_reason = tashes_out};
            {reveal, SeatNum, RevealedTashes, DiscardedTash} ->
                Msg = create_okey_revealed(SeatNum, DiscardedTash, RevealedTashes, Players),
                publish_to_clients(Relay, Msg),
                DeskState#desk_state{state = state_finished,
                                     finish_reason = reveal,
                                     reveal_info = {SeatNum, RevealedTashes, DiscardedTash}}
        end,
    handle_desk_events(Events, NewDeskState, Players, Relay).

%%===================================================================

%% players_init() -> players()
players_init() ->
    midict:new().

%% reg_player(PlayerId, SeatNum, UserId, IsBot, Players) -> NewPlayers
reg_player(PlayerId, SeatNum, UserId, IsBot, UserInfo, Players) ->
    store_player_rec(#player{id =PlayerId, seat_num = SeatNum, user_id = UserId,
                             is_bot = IsBot, info = UserInfo}, Players).

%% reg_player(#player{}, Players) -> NewPlayers
store_player_rec(#player{id =Id, seat_num = SeatNum, user_id = UserId,
                         is_bot = IsBot} = Player, Players) ->
    Indices = [{seat_num, SeatNum}, {user_id, UserId}, {is_bot, IsBot}],
    midict:store(Id, Player, Indices, Players).

%% get_player_id_by_seat_num(SeatNum, Players) -> PlayerId
get_player_id_by_seat_num(SeatNum, Players) ->
    [#player{id = PlayerId}] = midict:geti(SeatNum, seat_num, Players),
    PlayerId.

%% fetch_player(PlayerId, Players) -> Player
fetch_player(PlayerId, Players) ->
    midict:fetch(PlayerId, Players).

%% get_player(PlayerId, Players) -> {ok, Player} | error
get_player(PlayerId, Players) ->
    midict:find(PlayerId, Players).

%% get_player_by_seat_num(SeatNum, Players) -> Player
get_player_by_seat_num(SeatNum, Players) ->
    [Player] = midict:geti(SeatNum, seat_num, Players),
    Player.

%% find_players_by_seat_num(SeatNum, Players) -> Player
find_players_by_seat_num(SeatNum, Players) ->
    midict:geti(SeatNum, seat_num, Players).

%% del_player(PlayerId, Players) -> NewPlayers
del_player(PlayerId, Players) ->
    midict:erase(PlayerId, Players).

%% players_to_list(Players) -> List
players_to_list(Players) ->
    midict:all_values(Players).

%% @spec init_players(PlayersInfo) -> Players
%% @end
%% PlayersInfo = [{PlayerId, UserInfo, SeatNum}]

init_players(PlayersInfo) ->
    init_players(PlayersInfo, players_init()).

init_players([], Players) ->
    Players;

init_players([{PlayerId, UserInfo, SeatNum} | PlayersInfo], Players) ->
    #'PlayerInfo'{id = UserId, robot = IsBot} = UserInfo,
    NewPlayers = reg_player(PlayerId, SeatNum, UserId, IsBot, UserInfo, Players),
    init_players(PlayersInfo, NewPlayers).

%%===================================================================

send_to_client_ge(Relay, PlayerId, Msg) ->
    Event = #game_event{event = api_utils:name(Msg), args = api_utils:members(Msg)},
    ?RELAY:table_message(Relay, {to_client, PlayerId, Event}).

publish_to_clients(Relay, Msg) ->
    Event = #game_event{event = api_utils:name(Msg), args = api_utils:members(Msg)},
    ?RELAY:table_message(Relay, {publish, Event}).

parent_confirm_registration({ParentMod, ParentPid}, TableId, RequestId) ->
    ParentMod:table_message(ParentPid, TableId, {player_registered, RequestId}).

parent_confirm_replacement({ParentMod, ParentPid}, TableId, RequestId) ->
    ParentMod:table_message(ParentPid, TableId, {player_replaced, RequestId}).

parent_notify_table_created({ParentMod, ParentPid}, TableId, RelayPid) ->
    ParentMod:table_message(ParentPid, TableId, {table_created, RelayPid}).

parent_send_round_res({ParentMod, ParentPid}, TableId, FR, Hands, Gosterge, WhoHasGosterge) ->
    ParentMod:table_message(ParentPid, TableId, {round_finished, FR, Hands, Gosterge, WhoHasGosterge}).

desk_player_action(Desk, SeatNum, Action) ->
    ?DESK:player_action(Desk, SeatNum, Action).

%%===================================================================

create_okey_game_info(#state{table_name = TName, mult_factor = MulFactor,
                             slang_flag = SlangFlag, observer_flag = ObserverFlag,
                             speed = Speed, turn_timeout = TurnTimeout,
                             reveal_confirmation_timeout = RevealConfirmationTimeout,
                             ready_timeout = ReadyTimeout, game_type = GameType,
                             rounds = Rounds, players = Players}) ->
    PInfos = [case find_players_by_seat_num(SeatNum, Players) of
                  [#player{info = UserInfo}] -> UserInfo;
                  [] -> null
              end || SeatNum <- lists:seq(1, ?SEATS_NUM)],
    Timeouts = #'OkeyTimeouts'{speed = Speed,
                               turn_timeout = TurnTimeout,
                               challenge_timeout = RevealConfirmationTimeout,
                               ready_timeout = ReadyTimeout,
                               rematch_timeout = ?REMATCH_TIMEOUT},
    #okey_game_info{table_name = TName,
                    players = PInfos,
                    timeouts = Timeouts,
                    game_type = GameType,
                    finish_with_gosterge = false, %% XXX WTF?
                    rounds = case Rounds of
                                 infinity -> -1;
                                 RM -> RM
                             end,
                    sets = -1,     %% XXX Concept of sets is deprecated
                    set_no = -1,   %% XXX Concept of sets is deprecated
                    mul_factor = MulFactor,
                    slang_flag = SlangFlag,
                    observer_flag = ObserverFlag}.


create_okey_game_player_state(_PlayerId, ?STATE_WAITING_FOR_START,
                              #state{cur_round = CurRound}) ->
    #okey_game_player_state{whos_move = null,
                            game_state = game_initializing,
                            piles = null,
                            tiles = null,
                            gosterge = null,
                            pile_height = null,
                            current_round = CurRound,
                            game_sub_type = null,
                            next_turn_in = 0};

create_okey_game_player_state(PlayerId, ?STATE_PLAYING,
                              #state{timeout_timer = TRef, cur_round = CurRound,
                                     game_type = GameType, players = Players,
                                     desk_state = DeskState}) ->
    #player{seat_num = SeatNum} = fetch_player(PlayerId, Players),
    #desk_state{state = DeskStateName,
                hands = Hands,
                discarded = Discarded,
                gosterge = Gosterge,
                deck = DeskDeck,
                cur_seat = CurSeatNum} = DeskState,
    {_, PlayerHand} = lists:keyfind(SeatNum, 1, Hands),
    Hand = [tash_to_ext(Tash) || Tash <- PlayerHand],
    #player{user_id = CurUserId} = get_player_by_seat_num(CurSeatNum, Players),
    Timeout = calc_timeout(TRef),
    F = fun(_, N) ->
                Pile = case lists:keyfind(N, 1, Discarded) of
                           {_, []} -> null;
                           {_, [Tash|_]} -> tash_to_ext(Tash)
                       end,
                {Pile, next_seat_num(N)}
        end,
    {Piles, _} = lists:mapfoldl(F, prev_seat_num(SeatNum), lists:seq(1, ?SEATS_NUM)),
    SubType = get_scoring_mode(GameType, Gosterge),
    GameState = statename_to_api_string(DeskStateName),
    #okey_game_player_state{whos_move = CurUserId,
                            game_state = GameState,
                            piles = Piles,
                            tiles = Hand,
                            gosterge = tash_to_ext(Gosterge),
                            pile_height = length(DeskDeck),
                            current_round = CurRound,
                            game_sub_type = SubType,
                            next_turn_in = Timeout};

create_okey_game_player_state(PlayerId, ?STATE_REVEAL_CONFIRMATION,
                              #state{timeout_timer = TRef, cur_round = CurRound,
                                     game_type = GameType, players = Players,
                                     desk_state = DeskState}) ->
    #player{seat_num = SeatNum} = fetch_player(PlayerId, Players),
    #desk_state{hands = Hands,
                discarded = Discarded,
                gosterge = Gosterge,
                deck = DeskDeck,
                cur_seat = CurSeatNum} = DeskState,
    {_, PlayerHand} = lists:keyfind(SeatNum, 1, Hands),
    Hand = [tash_to_ext(Tash) || Tash <- PlayerHand],
    #player{user_id = CurUserId} = get_player_by_seat_num(CurSeatNum, Players),
    Timeout = calc_timeout(TRef),
    F = fun(_, N) ->
                Pile = case lists:keyfind(N, 1, Discarded) of
                           {_, []} -> null;
                           {_, [Tash|_]} -> tash_to_ext(Tash)
                       end,
                {Pile, next_seat_num(N)}
        end,
    {Piles, _} = lists:mapfoldl(F, prev_seat_num(SeatNum), lists:seq(1, ?SEATS_NUM)),
    SubType = get_scoring_mode(GameType, Gosterge),
    #okey_game_player_state{whos_move = CurUserId,
                            game_state = do_okey_challenge,
                            piles = Piles,
                            tiles = Hand,
                            gosterge = tash_to_ext(Gosterge),
                            pile_height = length(DeskDeck),
                            current_round = CurRound,
                            game_sub_type = SubType,
                            next_turn_in = Timeout}.


create_okey_game_started(SeatNum, DeskState, #state{cur_round = CurRound,
                                                    game_type = GameType,
                                                    speed = GameSpeed}) ->
    %% TODO: Fix chanak
    %%Chanak = game_okey_scoring:get_chanak_points(State#state.stats),
    #desk_state{hands = Hands,
                gosterge = Gosterge,
                deck = DeskDeck} = DeskState,
    {_, PlayerHand} = lists:keyfind(SeatNum, 1, Hands),
    Hand = [tash_to_ext(Tash) || Tash <- PlayerHand],
    GameSubmode = get_scoring_mode(GameType, Gosterge),
    #okey_game_started{tiles = Hand,
                       gosterge = tash_to_ext(Gosterge),
                       pile_height = length(DeskDeck),
                       current_round = CurRound,
                       current_set = -1,       %% XXX Concept of sets is deprecated
                       game_type = GameType,   %% XXX Why it isn't declared in game_info message?
                       game_speed = GameSpeed, %% XXX Why it isn't declared in game_info message?
                       game_submode = GameSubmode,
                       chanak_points = 0}.


create_okey_next_turn(CurSeat, Players) ->
    #player{user_id = UserId} = get_player_by_seat_num(CurSeat, Players),
    #okey_next_turn{player = UserId}.


create_player_left(SeatNum, UserInfo, Players) ->
    #player{user_id = OldUserId} = get_player_by_seat_num(SeatNum, Players),
    IsBot = UserInfo#'PlayerInfo'.robot,
    #player_left{player = OldUserId,
                 human_replaced = not IsBot, %% XXX WTF?
                 bot_replaced = IsBot,       %% XXX WTF?
                 replacement = UserInfo}.


create_okey_player_has_gosterge(SeatNum, Players) ->
    #player{user_id = UserId} = get_player_by_seat_num(SeatNum, Players),
    #okey_player_has_gosterge{player = UserId}.


create_okey_disable_okey(SeatNum, CurSeatNum, Players) ->
    #player{user_id = Who} = get_player_by_seat_num(SeatNum, Players),
    #player{user_id = Whom} = get_player_by_seat_num(CurSeatNum, Players),
    #okey_disable_okey{player = Whom,
                       who_disabled = Who}.


create_okey_tile_taken_discarded(SeatNum, Tash, PileHeight, Players) ->
    #player{user_id = UserId} = get_player_by_seat_num(SeatNum, Players),
    #okey_tile_taken{player = UserId,
                     pile = 1, %% From discarded tashes of the previous player
                     revealed = tash_to_ext(Tash),
                     pile_height = PileHeight}.


create_okey_tile_taken_table(SeatNum, PileHeight, Players) ->
    #player{user_id = UserId} = get_player_by_seat_num(SeatNum, Players),
    #okey_tile_taken{player = UserId,
                     pile = 0, %% From the deck on the table
                     revealed = null,
                     pile_height = PileHeight}.


create_okey_tile_discarded(SeatNum, Tash, Timeouted, Players) ->
    #player{user_id = UserId} = get_player_by_seat_num(SeatNum, Players),
    #okey_tile_discarded{player = UserId,
                         tile = Tash,
                         timeouted = Timeouted}.

create_okey_round_ended_no_winner(Results, NextAction) ->
    #okey_round_ended{good_shot = false,
                      reason = <<"draw">>,
                      results = Results,
                      next_action = NextAction}.


create_okey_revealed(SeatNum, DiscardedTash, TashPlaces, Players) ->
    #player{user_id = UserId} = get_player_by_seat_num(SeatNum, Players),
    TashPlacesExt = [case T of
                         null -> null;
                         _ -> tash_to_ext(T)
                     end || T <- TashPlaces],
    #okey_revealed{player = UserId,              %% FIXME: We need reveal message without confirmation
                   discarded = tash_to_ext(DiscardedTash),
                   hand = TashPlacesExt}.


create_okey_turn_timeout(TashTaken, TashDiscarded) ->
    #okey_turn_timeout{tile_taken = TashTaken,
                       tile_discarded = TashDiscarded}.


tash_to_ext(false_okey) -> #'OkeyPiece'{color = 0, value = 0};
tash_to_ext({Color, Value}) ->  #'OkeyPiece'{color = Color, value = Value}.

ext_to_tash(#'OkeyPiece'{color = 0, value = 0}) -> false_okey;
ext_to_tash(#'OkeyPiece'{color = Color, value = Value}) -> {Color, Value}.

%statename_to_api_string(state_wait) -> do_okey_ready;
%statename_to_api_string(state_challenge) -> do_okey_challenge;
statename_to_api_string(state_take) ->      do_okey_take;
statename_to_api_string(state_discard) ->   do_okey_discard;
statename_to_api_string(state_finished) ->  game_finished.

desk_error_to_ext(action_disabled) -> false;
desk_error_to_ext(no_gosterge) -> false;
desk_error_to_ext(no_okey_discarded) -> {error, there_is_no_okey_there};
desk_error_to_ext(not_your_order) -> {error, not_your_turn};
desk_error_to_ext(blocked) -> {error, okey_is_blocked};
desk_error_to_ext(no_tash) -> {error, no_tash};
desk_error_to_ext(no_such_tash) -> {error, no_such_tash};
desk_error_to_ext(hand_not_match) -> {error, discarded_hand_does_not_match_server_state};
desk_error_to_ext(E) -> {error, E}.

%%===================================================================

get_timeout(turn, fast) -> {ok, Val}   = nsm_db:get(config,"games/okey/turn_timeout_fast", 15000), Val;
get_timeout(turn, normal) -> {ok, Val} = nsm_db:get(config,"games/okey/turn_timeout_normal", 30000), Val;
get_timeout(turn, slow) -> {ok, Val}   = nsm_db:get(config,"games/okey/turn_timeout_slow", 60000), Val;

get_timeout(challenge, fast) ->  {ok, Val}   = nsm_db:get(config,"games/okey/challenge_timeout_fast", 5000), Val;
get_timeout(challenge, normal) ->  {ok, Val} = nsm_db:get(config,"games/okey/challenge_timeout_normal", 10000), Val;
get_timeout(challenge, slow) -> {ok, Val}    = nsm_db:get(config,"games/okey/challenge_timeout_slow", 20000), Val;

get_timeout(ready, fast) -> {ok, Val}   = nsm_db:get(config,"games/okey/ready_timeout_fast", 15000), Val;
get_timeout(ready, normal) -> {ok, Val} = nsm_db:get(config,"games/okey/ready_timeout_normal", 25000), Val;
get_timeout(ready, slow) -> {ok, Val}   = nsm_db:get(config,"games/okey/ready_timeout_slow", 45000), Val.

%%===================================================================

calc_timeout(undefined) -> 0;
calc_timeout(TRef) ->
    case erlang:read_timer(TRef) of
        false -> 0;
        Timeout -> Timeout
    end.

%%===================================================================

next_seat_num(?SEATS_NUM) -> 1;
next_seat_num(N) -> N + 1.

prev_seat_num(1) -> ?SEATS_NUM;
prev_seat_num(N) -> N - 1.

%%===================================================================
init_desk_state(Desk) ->
    SeatsNums = ?DESK:get_seats_nums(Desk),
    Hands = [{SeatNum, ?DESK:get_hand(Desk, SeatNum)} || SeatNum <- SeatsNums],
    Discarded = [{SeatNum, ?DESK:get_discarded(Desk, SeatNum)} || SeatNum <- SeatsNums],
    #desk_state{state = ?DESK:get_state_name(Desk),
                hands = Hands,
                discarded = Discarded,
                deck = ?DESK:get_deck(Desk),
                cur_seat = ?DESK:get_cur_seat(Desk),
                gosterge = ?DESK:get_gosterge(Desk)}.

%%===================================================================

get_scoring_mode(standard, _) ->  standard;
get_scoring_mode(countdown, _) -> countdown;
get_scoring_mode(color, {Color, Val}) when (Val rem 2) == 0 -> get_scoring_mode_c(even, b2c(Color));
get_scoring_mode(color, {Color, Val}) when (Val rem 2) == 1 -> get_scoring_mode_c(odd, b2c(Color));
get_scoring_mode(evenodd, {_Color, Val}) when (Val rem 2) == 0 -> even;
get_scoring_mode(evenodd, {_Color, Val}) when (Val rem 2) == 1 -> odd.

get_scoring_mode_c(odd, C) when C == yellow; C == blue -> ybodd;
get_scoring_mode_c(even, C) when C == yellow; C == blue -> ybeven;
get_scoring_mode_c(odd, C) when C == black; C == red -> rbodd;
get_scoring_mode_c(even, C) when C == black; C == red -> rbeven.

b2c(1) -> red;
b2c(2) -> blue;
b2c(3) -> yellow;
b2c(4) -> black.
