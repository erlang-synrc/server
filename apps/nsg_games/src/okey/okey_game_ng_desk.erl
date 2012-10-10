%%% -------------------------------------------------------------------
%%% Author  : Serge Polkovnikov <serge.polkovnikov@gmail.com>
%%% Description : The desk-level logic for okey game.
%%%
%%% Created : Oct 8, 2012
%%% -------------------------------------------------------------------
-module(okey_game_ng_desk).

-behaviour(gen_fsm).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([
         start/1,
         stop/1,
         player_action/3
        ]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-record(player,
        {
         id               :: integer(),
         hand             :: deck:deck(),
         has_gostergi     :: boolean(),
         finished_by_okey :: boolean(),
         discarded        :: deck:deck()
        }).

-record(state,
        {
         players          :: list(#player{}),
         cur_player       :: integer(),
         deck             :: deck:deck(),
         gostergi,
         okey,
         okey_blocked     :: boolean()
        }).


-define(STATE_TAKE, state_take).
-define(STATE_DISCARD, state_discard).
-define(STATE_FINISHED, state_finished).

%% ====================================================================
%% External functions
%% ====================================================================
start(Params) -> gen_fsm:start(?MODULE, Params, []).

player_action(DeskFSM, Player, Action) ->
    gen_fsm:sync_send_all_state_event(DeskFSM, {player_action, Player, Action}).

stop(DeskFSM) ->
    gen_fsm:send_all_state_event(DeskFSM, stop).

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
    Gostergi = get_param(gostergi, Params),
    CurPlayer = get_param(cur_player, Params),
    validate_params(Hands, Deck, Gostergi, CurPlayer),
    Players = init_players(Hands),
    Okey = init_okey(Gostergi),
    {ok, ?STATE_DISCARD, #state{players = Players,
                                deck = deck:from_list(Deck),
                                gostergi = Gostergi,
                                okey = Okey,
                                cur_player = CurPlayer,
                                okey_blocked = false}}.

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
handle_sync_event({player_action, PlayerId, Action}, _From, StateName, StateData) ->
    case handle_player_action(PlayerId, Action, StateName, StateData) of
        {ok, Events, NewStateName, NewStateData} ->
            {reply, {ok, lists:reverse(Events)}, NewStateName, NewStateData};
        {error, Reason} ->
            {reply, {error, Reason}, StateName, StateData}
    end;

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
                   {error, no_tash};
               NewPlayers ->
                   Events1 = [{tash_discarded, PlayerId, Tash}],
                   case deck:size(Deck) of
                       0 ->
                           Events = [no_winner_finish | Events1],
                           {ok, Events, ?STATE_FINISHED,
                            StateData#state{players = NewPlayers}};
                       _ ->
                           {ok, Events1, ?STATE_DISCARD,
                            StateData#state{players = NewPlayers, cur_player = next_id(CurPlayerId)}}
                   end
            end;
       true ->
           {error, not_your_order}
    end;

handle_player_action(PlayerId, {reveal, Tash, TashPlaces}, ?STATE_DISCARD,
                     #state{cur_player = CurPlayerId,
                            players = Players,
                            gostergi = Gostergi
                           } = StateData) ->
    if PlayerId == CurPlayerId ->
           case discard_tash(Tash, PlayerId, Players) of
               error ->
                   {error, no_tash};
               NewPlayers ->
                   RevealHand = tash_places_to_hand(TashPlaces),
                   PlayerHand = get_player_hand(PlayerId, NewPlayers),
                   case is_same_hands(RevealHand, PlayerHand) of
                       true ->
                           Events = case is_right_finish(TashPlaces, Gostergi) of
                                        true -> [{right_finish, PlayerId, TashPlaces}];
                                        false -> [{wrong_finish, PlayerId, TashPlaces}]
                                    end,
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
    {error, invalid_action}.

%%===================================================================

%% @spec get_param(Id, Params) -> Value
%% @end
get_param(Id, Params) ->
    {_, Value} =lists:keyfind(Id, 1, Params),
    Value.

%% TODO: Implement the validator
validate_params(_Hands, _Deck, _Gostergi, _CurPlayer) ->
    ok.

init_players(Hands) ->
    F = fun(Hand, Id) ->
                {#player{id = Id,
                         hand = Hand,
                         discarded = deck:init_deck(empty),
                         finished_by_okey = false,
                         has_gostergi = false},
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
            NewPlayer = Player#player{hand = deck:push(Taken, Hand)},
            NewPlayers1 = update_player(NewPrevPlayer, Players),
            NewPlayers2 = update_player(NewPlayer, NewPlayers1),
            [Tash] = deck:to_list(Taken),
            {Tash, NewPlayers2}
    end.

get_player(PlayerId, Players) ->
    #player{} = lists:keyfind(PlayerId, #player.id, Players).

update_player(#player{id = Id} = Player, Players) ->
    lists:keyreplace(Id, #player.id, Players, Player).

take_tash_from_table(PlayerId, Players, Deck) ->
    case deck:size(Deck) of
        0 ->
            error;
        _ ->
            {Taken, NewDeck} = deck:pop(1, Deck),
            #player{hand = Hand} = Player = get_player(PlayerId, Players),
            NewPlayer = Player#player{hand = deck:push(Taken, Hand)},
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


%% @spec get_player_hand(PlayerId, Players) -> Hand
%% @end
get_player_hand(PlayerId, Players) ->
    #player{hand = Hand} = lists:keyfind(PlayerId, #player.id, Players),
    Hand.

%% @spec is_same_hands(Hand1, Hand2) -> boolean()
%% @end
is_same_hands(Hand1, Hand2) ->
    L1 = lists:sort(deck:to_list(Hand1)),
    L2 = lists:sort(deck:to_list(Hand2)),
    L1 == L2.

%% @spec is_right_finish(TashPlaces, Gostergi) -> boolean()
%% @end
is_right_finish([TopRow, BottomRow], Gostergi) ->
    FlatList = TopRow ++ [null | BottomRow],
    Okey = init_okey(Gostergi),
    Normalized = [case E of
                      Okey -> okey;
                      false_okey -> Okey
                  end || E <- FlatList],
    Sets = split_by_delimiter(null, Normalized),
    ProperHand = lists:all(fun(S) ->
                                   is_set(S) orelse is_run(S)
                           end, Sets),
    PowerHand = lists:all(fun(S) ->
                                  is_pair(S)
                          end, Sets),
    ProperHand orelse PowerHand.

%% @spec split_by_delimiter(Delimiter, List) -> ListOfList
%% @end
split_by_delimiter(Delimiter, Hand) -> split_by_delimiter(Delimiter, Hand, []).
split_by_delimiter(_, [], Acc) -> lists:reverse(Acc);
split_by_delimiter(Delimiter, [Delimiter | Hand], Acc) -> split_by_delimiter(Delimiter, Hand, Acc);
split_by_delimiter(Delimiter, Hand, Acc) ->
    {L, Rest} = lists:splitwith(fun(X) -> X == Delimiter end, Hand),
    split_by_delimiter(Delimiter, Rest, [L | Acc]).

%% @spec is_set(Set) -> boolean()
%% @end
is_set(Set) when
  length(Set) < 3;
  length(Set) > 4 -> false;
is_set(Set) ->
    Normals = [ X || X <- Set, X =/= okey ],
    {_, Value} = hd(Normals),
    SameValue = lists:all(fun({_, V}) -> V == Value end, Normals),
    UniqColors = length(Normals) == length(lists:usort([C || {C, _} <- Normals])),
    SameValue andalso UniqColors.


%% @spec is_run(Set) -> boolean()
%% @end
is_run(Set) when length(Set) < 3 -> false;
is_run(Set) ->
    {Okeys, Normals} = lists:partition(fun(X)-> X == okey end, Set),
    {Color, _} = hd(Normals),
    {Colors, Values} = lists:unzip(Normals),
    SameColor = lists:all(fun(C) -> C == Color end, Colors),
    SortedValues = lists:sort(Values),
    NormalizedValues = if hd(SortedValues)==1 -> tl(SortedValues) ++ [14]; true -> false end,
    OkeysNum = length(Okeys),
    Check1 = check_run(SortedValues, OkeysNum),
    Check2 = check_run(NormalizedValues, OkeysNum),
    SameColor andalso (Check1 orelse Check2).


check_run(false, _) -> false;
check_run([First | Rest], OkeysNum) ->
    check_run(First, Rest, OkeysNum).


check_run(Cur, [Cur | _], _OkeysNum) -> false;
check_run(Cur, [Next | Rest], OkeysNum) when Next == Cur + 1 ->
    check_run(Cur+1, Rest, OkeysNum);
check_run(_Cur, [_Next | _Rest], 0) -> false;
check_run(Cur, [Next | Rest], OkeysNum) ->
    check_run(Cur+1, [Next | Rest], OkeysNum - 1);
check_run(_Cur, [], _OkeysNum) -> true.

%% @spec is_pair(Set) -> boolean()
%% @end
is_pair([_A, okey]) -> true;
is_pair([okey, _B]) -> true;
is_pair([A, A]) -> true;
is_pair(_) -> false.


%% Tests
test_test_() ->
    [{"is_pair",
      [?_assertEqual(true,  is_pair([{1,3}, {1,3}])),
       ?_assertEqual(true,  is_pair([{4,13}, {4,13}])),
       ?_assertEqual(false, is_pair([{4,12}, {4,13}])),
       ?_assertEqual(false, is_pair([{1,1}, {4,8}])),
       ?_assertEqual(true,  is_pair([okey, {3,8}])),
       ?_assertEqual(true,  is_pair([{2,3}, okey])),
       ?_assertEqual(true,  is_pair([okey, okey])),
       ?_assertEqual(false, is_pair([{2,4}, {4,2}, {3,3}])),
       ?_assertEqual(false, is_pair([{2,4}])),
       ?_assertEqual(false, is_pair([okey])),
       ?_assertEqual(false, is_pair([okey, okey, {2,6}]))
      ]},
     {"is_set",
      [?_assertEqual(true,  is_set([{1,3}, {3,3}, {2,3}])),
       ?_assertEqual(true,  is_set([{4,8}, okey, {2,8}])),
       ?_assertEqual(true,  is_set([{4,3}, okey, okey])),
       ?_assertEqual(true,  is_set([{4,13}, {1,13}, {3,13}, {2,13}])),
       ?_assertEqual(true,  is_set([okey, {1,13}, {3,13}, {2,13}])),
       ?_assertEqual(true,  is_set([okey, okey, {3,13}, {2,13}])),
       ?_assertEqual(false, is_set([{2,6}])),
       ?_assertEqual(false, is_set([okey])),
       ?_assertEqual(false, is_set([{3,4}, {2,6}])),
       ?_assertEqual(false, is_set([{2,3}, {4,3}])),
       ?_assertEqual(false, is_set([okey, okey])),
       ?_assertEqual(false, is_set([{3,4}, {1,4}, {3,4}])),
       ?_assertEqual(false, is_set([{3,4}, {1,4}, {2,5}])),
       ?_assertEqual(false, is_set([{3,4}, okey, {2,5}])),
       ?_assertEqual(false, is_set([{2,5}, {3,5}, {4,5}, {1,6}])),
       ?_assertEqual(false, is_set([{2,5}, {3,5}, {4,5}, {2,5}])),
       ?_assertEqual(false, is_set([{2,3}, {3,3}, {4,3}, {1,3}, {3,1}])),
       ?_assertEqual(false, is_set([{2,3}, okey, {4,3}, {1,3}, {3,3}]))
      ]},
     {"is_run",
      [?_assertEqual(false, is_run([{1,3}])),
       ?_assertEqual(false, is_run([okey])),
       ?_assertEqual(false, is_run([{2,2}, {2,3}])),
       ?_assertEqual(false, is_run([okey, {2,3}])),
       ?_assertEqual(false, is_run([{4,1}, {2,3}])),
       ?_assertEqual(true,  is_run([{4,4}, {4,6}, {4,5}])),
       ?_assertEqual(true,  is_run([okey, {4,6}, {4,5}])),
       ?_assertEqual(true,  is_run([okey, {4,6}, okey])),
       ?_assertEqual(true,  is_run([{1,12}, {1,1}, {1,13}])),
       ?_assertEqual(true,  is_run([{1,12}, {1,1}, {1,11}, {1,13}])),
       ?_assertEqual(false, is_run([{1,12}, {1,1}, {1,11}, {1,11}, {1,13}])),
       ?_assertEqual(false, is_run([{1,12}, {1,1}, {1,2}, {1,11}, {1,13}])),
       ?_assertEqual(true,  is_run([{3,6}, {3,8}, okey, {3,5}, {3,9}])),
       ?_assertEqual(false, is_run([{3,6}, {3,8}, okey, {3,5}, {3,9}, {3,2}])),
       ?_assertEqual(false, is_run([{3,6}, {3,8}, okey, {3,5}, {3,9}, {1,2}]))
      ]}
    ].
