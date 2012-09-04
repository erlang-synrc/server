%%%-------------------------------------------------------------------
%%% @author Paul Peregud <pawel@saturn.lan>
%%% @copyright (C) 2011, Paul Peregud
%%% @doc
%%%
%%% Lay out any number of matches in any number of piles, as long as there is at least 2 matches in 2 piles.
%%% Each player takes a turn removing any number of matches from one pile.
%%% The player that is forced to remove the last match is out.
%%% Repeat until there is one player left.
%%%
%%% Description of the game is from http://www.boyscouttrail.com/content/game/game-406.asp
%%%
%%% Player can surrender by taking all matches from last heap
%%%
%%% @end
%%% Created : 20 Jan 2011 by Paul Peregud <pawel@saturn.lan>
%%%-------------------------------------------------------------------
-module(game_lastmatch).

-behaviour(gen_fsm).

%% API
-export([start/1]).

-export([make_move/3, get_requirements/0]).

-include_lib("nsg_srv/include/logging.hrl").

%% gen_fsm callbacks
-export([play/3, finished/3]).

-export([init/1, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-define(SERVER, ?MODULE).

-record(state, {
          initial, %% initial list of players
          players, %% list of player pids ordered in the order of moves
          matches = generate_matches(), %% proplist of heaps of matches.
                                        %% E.g. [{1, 5}, {2, 5}, {3, 3}, {4, 8}]
          winner = undefined,
          losers = [] %% list of players who has picked last match or surrendered
         }).

%% incoming messages
-record(message, {
          refid,
          type = move, %% [move, surrender, disconnected]
          who,
          heapno,
          matchno
         }).

%% outgoing messages; list of this messages can be returned
-record(player_moves, {
          refid,
          who,
          move}). %% {no_of_heap, delta}, where delta < 0

-record(game_state_new, {
          refid,
          matches,  %% new heaps
          active}). %% active player

-record(player_lost, {
          refid,
          who,
          why}). %% [disconnected, lost, surrender]

-record(player_won, {
          refid,
          who
         }).

%% messages sent directly to session
-record(ack, {
          refid,
          accepted = true,
          reason = undefined
         }).

%%%===================================================================
%%% API
%%%
%%% Notes on API.
%%% Incoming msg are #message, outgoing are listed above.
%%% If server returns "ok" atom, it should be ignored.
%%% If server receives invalid message (e.g. invalid move),
%%% it may send direct answer to sender (e.g. {error, bad_move})
%%%
%%%===================================================================

get_requirements() ->
    [{players, 2}].

make_move(Server, Who, #message{} = Msg) ->
    gen_fsm:sync_send_event(Server, {play, Who, Msg}).

%%FIX: rename to start_link
start(Relay) ->
    gen_fsm:start_link(?MODULE, [Relay], []).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

init([Pids]) ->
    {ok, play, #state{initial = Pids, players = Pids}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name StateName is called to
%% handle the event.
%%
%% @spec state_name(Event, From, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------

%% player that should make a move surrenders instead
play(#message{type = surrender, who = PPid, refid = RI}, _From, #state{players = [PPid | Rest]} = State) ->
    PPid ! #ack{refid = RI},
    case length(Rest) of
        1 ->
            Winner = hd(Rest),
            {reply, #player_won{who = Winner, refid = RI}, finished, State#state{winner = Winner}};
        _ ->
            {reply, #player_lost{who = PPid, why = surrender, refid = RI}, play, State#state{players = Rest}}
    end;

%% when player surrenders outside of his turn
play(#message{type = surrender, who = PPid, refid = RI}, _From, #state{players = Players} = State) ->
    case {lists:member(PPid, Players), length(Players)} of
        {true, 2} ->
            PPid ! #ack{refid = RI},
            [Winner] = lists:delete(PPid, Players),
            {reply, #player_won{who = Winner, refid = RI}, finished, State#state{winner = Winner}};
        {true, _} ->
            PPid ! #ack{refid = RI},
            Rest = lists:delete(PPid, Players),
            Matches = generate_matches(),
            {reply, [#player_lost{who = PPid, why = surrender, refid = RI},
                     #game_state_new{matches = Matches, active = hd(Rest), refid = RI}],
             play, State#state{matches = Matches, players = Rest}};
        {false, _} ->
            PPid ! #ack{refid = RI, accepted = false, reason = not_your_turn},
            {reply, ok, play, State}
    end;

play({#message{type = move, who = PPid, heapno = HN, matchno = MN, refid = RI}}, _From,
     #state{players = [PPid | Rest] = Players, matches = Heaps} = State) ->
    L = length(Heaps),
    case proplists:get_value(HN, Heaps, undefined) of
        _ when HN > L->
            PPid ! #ack{refid = RI, accepted = false, reason = bad_move},
            {reply, ok, play, State};
        undefined ->
            PPid ! #ack{refid = RI, accepted = false, reason = bad_move},
            {reply, ok, play, State};
        A when A > MN ->
            PPid ! #ack{refid = RI, accepted = false, reason = bad_move},
            {reply, ok, play, State};
        A ->
            PPid ! #ack{refid = RI},
            NewHeaps0 = proplists:delete(HN, Heaps),
            NewHeaps = [{HN, A-MN} | NewHeaps0],
            case {check_matches(NewHeaps), length(Players)} of
                {continue, _} ->
                    NPlayers = lists:append(Rest, [PPid]),
                    {reply, [#player_moves{who = PPid, move = {HN, MN}},
                             #game_state_new{matches = NewHeaps, active = hd(NPlayers)}], play,
                     State#state{matches = NewHeaps, players = NPlayers}};
                {_, 2} ->
                    {reply, [#player_won{who = hd(Rest)}], finished, State#state{winner = hd(Rest)}};
                {surrender, _} ->
                    NGHeaps = generate_matches(),
                    {reply, [#player_moves{who = PPid, move = {HN, MN}},
                             #player_lost{who = PPid, why = surrender},
                             #game_state_new{matches = NGHeaps, active = hd(Rest)}],
                     play, State#state{matches = NGHeaps, players = Rest}};
                {stop, _} ->
                    WhoLost = hd(Rest),
                    NPlayers = lists:append(tl(Rest), [PPid]),
                    NGHeaps = generate_matches(),
                    {reply, [#player_moves{who = PPid, move = {HN, MN}},
                             #player_lost{who = WhoLost, why = lost},
                             #game_state_new{matches = NGHeaps, active = hd(NPlayers)}],
                     play, State#state{matches = NGHeaps, players = NPlayers}}
            end
    end;

play(#message{type = move, who = WrongPlayer, refid = RI}, _From,
     #state{players = [PPid | _Rest]} = State) when PPid =/= WrongPlayer ->
    PPid ! #ack{refid = RI, accepted = false, reason = not_your_turn},
    {reply, ok, play, State}.


finished(_Msg, _From, #state{} = State) ->
    ?PP("game has ended, still receiving messages: ~p", [_Msg]),
    {reply, finished, finished, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

generate_matches() ->
    Heaps = crypto:rand_uniform(2, 6),
    lists:map(fun(Num) ->
                      {Num, crypto:rand_uniform(5, trunc(50/Heaps))}
              end, lists:seq(1, Heaps)).

check_matches(Matches) ->
    Heaps = lists:filter(fun
                             (0) -> false;
                             (_) -> true
                         end, Matches),
    case {length(Heaps), hd(Heaps)} of
        {1, 0} ->
            surrender;
        {1, 1} ->
            stop;
        {_, _} ->
            continue
    end.


