%%% -------------------------------------------------------------------
%%% Author  : Sergii Polkovnikov <serge.polkovnikov@gmail.com>
%%% Description : The desk-level logic for okey game.
%%%
%%% Created : Oct 8, 2012
%%% -------------------------------------------------------------------

%%        Players actions:        ||      Errors:
%%  i_have_gosterge               || action_disabled, no_gosterge
%%  see_okey                      || no_okey_discarded
%%  take_from_discarded           || not_your_order, blocked, no_tash
%%  take_from_table               || not_your_order, no_tash
%%  {discard, Tash}               || not_your_order, no_such_tash
%%  {reveal, Tash, TashPlaces}    || not_your_order, no_such_tash, hand_not_match

%% Outgoing events:
%%  {has_gosterge, SeatNum}
%%  {saw_okey, SeatNum}
%%  {taked_from_discarded, SeatNum, Tash}
%%  {taked_from_table, SeatNum, Tash}
%%  {tash_discarded, SeatNum, Tash}
%%  {next_player, SeatNum}
%%  no_winner_finish
%%  {reveal, SeatNum, Right, TashPlaces, DiscardedTash, WithOkey, HasGosterge}

-module(game_okey_ng_desk).

-behaviour(gen_fsm).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("nsx_config/include/log.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([
         start/1,
         stop/1,
         player_action/3
        ]).

-export([
         get_state_name/1,
         get_seats_nums/1,
         get_cur_seat/1,
         get_gosterge/1,
         get_deck/1,
         get_hand/2,
         get_discarded/2
        ]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-type tash() :: false_okey | {integer(), integer()}.

-record(player,
        {
         id                :: integer(),
         hand              :: deck:deck(),
         can_show_gosterge :: boolean(),
         finished_by_okey  :: boolean(),
         discarded         :: deck:deck()
        }).

-record(state,
        {
         players           :: list(#player{}),
         cur_player        :: integer(),
         deck              :: deck:deck(),
         gosterge          :: tash(),
         okey              :: tash(),
         okey_blocked      :: boolean(),
         has_gosterge      :: undefined | integer() %% Seat num of a player who has gosterge
        }).


-define(STATE_TAKE, state_take).
-define(STATE_DISCARD, state_discard).
-define(STATE_FINISHED, state_finished).

%% ====================================================================
%% External functions
%% ====================================================================
start(Params) -> gen_fsm:start(?MODULE, Params, []).

player_action(Desk, SeatNum, Action) ->
    gen_fsm:sync_send_all_state_event(Desk, {player_action, SeatNum, Action}).

stop(Desk) ->
    gen_fsm:send_all_state_event(Desk, stop).


get_state_name(Desk) ->
    gen_fsm:sync_send_all_state_event(Desk, get_state_name).

get_cur_seat(Desk) ->
    gen_fsm:sync_send_all_state_event(Desk, get_cur_seat).

get_seats_nums(Desk) ->
    gen_fsm:sync_send_all_state_event(Desk, get_seats_nums).

get_gosterge(Desk) ->
    gen_fsm:sync_send_all_state_event(Desk, get_gosterge).

get_deck(Desk) ->
    gen_fsm:sync_send_all_state_event(Desk, get_deck).

get_hand(Desk, SeatNum) ->
    gen_fsm:sync_send_all_state_event(Desk, {get_hand, SeatNum}).

get_discarded(Desk, SeatNum) ->
    gen_fsm:sync_send_all_state_event(Desk, {get_discarded, SeatNum}).

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
init(Params) ->
    Hands = get_param(hands, Params),
    Deck = get_param(deck, Params),
    Gosterge = get_param(gosterge, Params),
    CurPlayer = get_param(cur_player, Params),
    validate_params(Hands, Deck, Gosterge, CurPlayer),
    Players = init_players(Hands),
    Okey = init_okey(Gosterge),
    {ok, ?STATE_DISCARD, #state{players = Players,
                                deck = deck:from_list(Deck),
                                gosterge = Gosterge,
                                okey = Okey,
                                cur_player = CurPlayer,
                                okey_blocked = false,
                                has_gosterge = undefined}}.

%% --------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
handle_event(stop, _StateName, StateData) ->
    {stop, normal, StateData};

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
handle_sync_event({player_action, SeatNum, Action}, _From, StateName, StateData) ->
    case handle_player_action(SeatNum, Action, StateName, StateData) of
        {ok, Events, NewStateName, NewStateData} ->
            {reply, {ok, lists:reverse(Events)}, NewStateName, NewStateData};
        {error, Reason} ->
            {reply, {error, Reason}, StateName, StateData}
    end;

handle_sync_event(get_state_name, _From, StateName,
                  StateData) ->
    {reply, StateName, StateName, StateData};

handle_sync_event(get_cur_seat, _From, StateName,
                  #state{cur_player = CurSeat} = StateData) ->
    {reply, CurSeat, StateName, StateData};

handle_sync_event(get_seats_nums, _From, StateName,
                  #state{players = Players} = StateData) ->
    SeatsNums = [P#player.id || P <- Players],
    {reply, SeatsNums, StateName, StateData};

handle_sync_event(get_gosterge, _From, StateName,
                  #state{gosterge = Gosterge} = StateData) ->
    {reply, Gosterge, StateName, StateData};

handle_sync_event(get_deck, _From, StateName,
                  #state{deck = Deck} = StateData) ->
    {reply, deck:to_list(Deck), StateName, StateData};

handle_sync_event({get_hand, SeatNum}, _From, StateName,
                  #state{players = Players} = StateData) ->
    #player{hand = Hand} = get_player(SeatNum, Players),
    {reply, deck:to_list(Hand), StateName, StateData};

handle_sync_event({get_discarded, SeatNum}, _From, StateName,
                  #state{players = Players} = StateData) ->
    #player{discarded = Discarded} = get_player(SeatNum, Players),
    {reply, deck:to_list(Discarded), StateName, StateData};

handle_sync_event(_Event, _From, StateName, StateData) ->
    Reply = ok,
    {reply, Reply, StateName, StateData}.

%% --------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% --------------------------------------------------------------------
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

%% --------------------------------------------------------------------
%% Func: handle_player_action/4
%% Returns: {ok, Events, NextStateName, NextStateData}          |
%%          {error, Reason}
%% --------------------------------------------------------------------
handle_player_action(PlayerId, i_have_gosterge, StateName,
                     #state{players = Players,
                            gosterge = Gosterge} = StateData) when
  StateName == ?STATE_TAKE;
  StateName == ?STATE_DISCARD ->
    case get_player(PlayerId, Players) of
        #player{can_show_gosterge = true,
                hand = Hand} = Player ->
            case deck:member(Gosterge, Hand) of
                true ->
                    NewPlayer = Player#player{can_show_gosterge = false},
                    NewPlayers = update_player(NewPlayer, Players),
                    Events = [{has_gosterge, PlayerId}],
                    {ok, Events, StateName, StateData#state{players = NewPlayers,
                                                            has_gosterge = PlayerId}};
                false ->
                    {error, no_gosterge}
            end;
        #player{can_show_gosterge = false} ->
            {error, action_disabled}
    end;


handle_player_action(PlayerId, see_okey, ?STATE_TAKE,
                     #state{cur_player = CurPlayerId,
                            players = Players,
                            okey = Okey} = StateData) ->
    #player{discarded = Discarded} = get_player(prev_id(CurPlayerId), Players),
    case deck:get(1, Discarded) of
        Okey ->
            Events = [{saw_okey, PlayerId}],
            {ok, Events, ?STATE_TAKE, StateData#state{okey_blocked = true}};
        _ ->
            {error, no_okey_discarded}
    end;


handle_player_action(PlayerId, take_from_discarded, ?STATE_TAKE,
                     #state{cur_player = CurPlayerId,
                            players = Players,
                            okey_blocked = Blocked} = StateData) ->
    if PlayerId == CurPlayerId ->
           if not Blocked ->
                  case take_tash_from_discarded(PlayerId, Players) of
                      {Tash, NewPlayers} ->
                          Events = [{taked_from_discarded, PlayerId, Tash}],
                          {ok, Events, ?STATE_DISCARD,
                           StateData#state{players = NewPlayers, okey_blocked = false}};
                      error ->
                          {error, no_tash}
                  end;
              true ->
                  {error, blocked}
           end;
       true ->
           {error, not_your_order}
    end;


handle_player_action(PlayerId, take_from_table, ?STATE_TAKE,
                     #state{cur_player = CurPlayerId,
                            players = Players,
                            deck = Deck} = StateData) ->
    if PlayerId == CurPlayerId ->
           case take_tash_from_table(PlayerId, Players, Deck) of
               {Tash, NewPlayers, NewDeck} ->
                   Events = [{taked_from_table, PlayerId, Tash}],
                   {ok, Events, ?STATE_DISCARD,
                    StateData#state{players = NewPlayers, deck = NewDeck, okey_blocked = false}};
               error ->
                   {error, no_tash}
           end;
       true ->
           {error, not_your_order}
    end;


handle_player_action(PlayerId, {discard, Tash}, ?STATE_DISCARD,
                     #state{cur_player = CurPlayerId,
                            players = Players,
                            deck = Deck} = StateData) ->
    if PlayerId == CurPlayerId ->
           case discard_tash(Tash, PlayerId, Players) of
               error ->
                   ?INFO("OKEY_NG_DESK Discard error. SeatNum: ~p. Tash: ~p", [PlayerId, Tash]),
                   {error, no_such_tash};
               NewPlayers ->
                   Events1 = [{tash_discarded, PlayerId, Tash}],
                   case deck:size(Deck) of
                       0 ->
                           Events = [no_winner_finish | Events1],
                           {ok, Events, ?STATE_FINISHED,
                            StateData#state{players = NewPlayers}};
                       _ ->
                           NextPlayerId = next_id(CurPlayerId),
                           Events = [{next_player, NextPlayerId} | Events1],
                           {ok, Events, ?STATE_TAKE,
                            StateData#state{players = NewPlayers, cur_player = NextPlayerId}}
                   end
            end;
       true ->
           {error, not_your_order}
    end;

handle_player_action(PlayerId, {reveal, Tash, TashPlaces}, ?STATE_DISCARD,
                     #state{cur_player = CurPlayerId,
                            players = Players
                           } = StateData) ->
    if PlayerId == CurPlayerId ->
           case discard_tash(Tash, PlayerId, Players) of
               error ->
                   {error, no_such_tash};
               NewPlayers ->
                   RevealHand = tash_places_to_hand(TashPlaces),
                   #player{hand = PlayerHand} = get_player(PlayerId, NewPlayers),
                   case is_same_hands(RevealHand, PlayerHand) of
                       true ->
                           Events = [{reveal, PlayerId, TashPlaces, Tash}],
                           {ok, Events, ?STATE_FINISHED,
                            StateData#state{players = NewPlayers}};
                       false ->
                           {error, hand_not_match}
                   end
            end;
       true ->
           {error, not_your_order}
    end;

handle_player_action(_PlayerId, _Action, _StateName, _StateData) ->
    ?ERROR("OKEY_NG_DESK Invalid action passed. Player: ~p. Action: ~p. StateName: ~p.",
           [_PlayerId, _Action, _StateName]),
    {error, invalid_action}.

%%===================================================================

%% @spec get_param(Id, Params) -> Value
%% @end
get_param(Id, Params) ->
    {_, Value} =lists:keyfind(Id, 1, Params),
    Value.

%% TODO: Implement the validator

validate_params(_Hands, _Deck, _Gosterge, _CurPlayer) ->
    ok.

init_players(Hands) ->
    F = fun(Hand, Id) ->
                {#player{id = Id,
                         hand = Hand,
                         discarded = deck:init_deck(empty),
                         finished_by_okey = false,
                         can_show_gosterge = true},
                 Id+1}
        end,
    {Players, _} = lists:mapfoldl(F, 1, Hands),
    Players.


%% @spec init_okey(GostergyTash) -> OkeyTash

init_okey({Color, Value}) ->
    if Value == 13 -> {Color, 1};
       true -> {Color, Value + 1}
    end.


%% next_id(Id) -> NextId
next_id(4) -> 1;
next_id(Id) -> Id + 1.

%% prev_id(Id) -> PrevId
prev_id(1) -> 4;
prev_id(Id) -> Id - 1.

%% @spec take_tash_from_discarded(PlayerId, Players) -> {Tash, NewPlayers} | error
%% @end
take_tash_from_discarded(PlayerId, Players) ->
    #player{discarded = Discarded} = PrevPlayer = get_player(prev_id(PlayerId), Players),
    case deck:size(Discarded) of
        0 ->
            error;
        _ ->
            {Taken, RestDiscarded} = deck:pop(1, Discarded),
            NewPrevPlayer = PrevPlayer#player{discarded = RestDiscarded},
            #player{hand = Hand} = Player = get_player(PlayerId, Players),
            NewPlayer = Player#player{hand = deck:push(Taken, Hand),
                                      can_show_gosterge = false},
            NewPlayers1 = update_player(NewPrevPlayer, Players),
            NewPlayers2 = update_player(NewPlayer, NewPlayers1),
            [Tash] = deck:to_list(Taken),
            {Tash, NewPlayers2}
    end.

%% @spec take_tash_from_table(PlayerId, Players, Deck) -> {Tash, NewPlayers, NewDeck} | error
%% @end
take_tash_from_table(PlayerId, Players, Deck) ->
    case deck:size(Deck) of
        0 ->
            error;
        _ ->
            {Taken, NewDeck} = deck:pop(1, Deck),
            #player{hand = Hand} = Player = get_player(PlayerId, Players),
            NewPlayer = Player#player{hand = deck:push(Taken, Hand),
                                      can_show_gosterge = false},
            NewPlayers = update_player(NewPlayer, Players),
            [Tash] = deck:to_list(Taken),
            {Tash, NewPlayers, NewDeck}
    end.

%% @spec discard_tash(Tash, PlayerId, Players) -> NewPlayers
%% @end
discard_tash(Tash, PlayerId, Players) ->
    #player{hand = Hand, discarded = Discarded} = Player = get_player(PlayerId, Players),
    case deck:del_first(Tash, Hand) of
        {ok, NewHand} ->
            NewDiscarded = deck:put(Tash, 1, Discarded),
            NewPlayer = Player#player{hand = NewHand, discarded = NewDiscarded},
            update_player(NewPlayer, Players);
        error ->
            error
    end.


%% @spec tash_places_to_hand(TashPlaces) -> Hand
%% @end
tash_places_to_hand(TashPlaces) ->
    Elements = [P || P <- lists:flatten(TashPlaces), P =/= null],
    deck:from_list(Elements).

%% @spec get_player(PlayerId, Players) -> Player
%% @end
get_player(PlayerId, Players) ->
    #player{} = lists:keyfind(PlayerId, #player.id, Players).


%% @spec update_player(Player, Players) -> NewPlayers
%% @end
update_player(#player{id = Id} = Player, Players) ->
    lists:keyreplace(Id, #player.id, Players, Player).


%% @spec is_same_hands(Hand1, Hand2) -> boolean()
%% @end
is_same_hands(Hand1, Hand2) ->
    L1 = lists:sort(deck:to_list(Hand1)),
    L2 = lists:sort(deck:to_list(Hand2)),
    L1 == L2.

