-module(game_okey_bot).
-behaviour(gen_server).

-export([start/3, start_link/3, robot_init/1]).
-export([init_state/2, join_game/1, get_session/1]).
-export([send_message/2]).
-export([call_rpc/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("nsg_srv/include/conf.hrl").
-include_lib("nsg_srv/include/requests.hrl").
-include_lib("nsg_srv/include/game_okey.hrl").
-include_lib("nsx_config/include/log.hrl").

-record(state, {
        is_robot = true :: boolean(),
        conn :: pid(),
        mode :: atom(),
        hand :: list(#'OkeyPiece'{}),
        gosterge :: #'OkeyPiece'{},
        set_state :: #'OkeySetState'{},
        delay :: integer(),
        user :: #'PlayerInfo'{},
        uid :: 'PlayerId'(),
        owner :: pid(),
        owner_mon :: 'MonitorRef'(),
        session :: pid(),
        gid :: 'GameId'(),
        bot :: pid(),
        running_requests = dict:new() :: any(),
        request_id = 0
    }).

%% session talks to bot
send_message(Pid, Message) ->
    ?INFO("OKEY message to pid ~p body ~p",[Pid,Message]),
    gen_server:call(Pid, {send_message, Message}).

%% bot talks to session
call_rpc(Pid, Message) ->
    gen_server:call(Pid, {call_rpc, Message}).

get_session(Pid) ->
    gen_server:call(Pid, get_session).

init_state(Pid, Situation) ->
    gen_server:cast(Pid, {init_state, Situation}).

join_game(Pid) ->
    gen_server:cast(Pid, join_game).

start(Owner, PlayerInfo, GameId) ->
    gen_server:start(?MODULE, [Owner, PlayerInfo, GameId], []).

start_link(Owner, PlayerInfo, GameId) ->
    gen_server:start_link(?MODULE, [Owner, PlayerInfo, GameId], []).

init([Owner, PlayerInfo, GameId]) ->
    {ok, SPid} = game_session:start_link(self()),
    game_session:bot_session_attach(SPid, PlayerInfo),
    UId = PlayerInfo#'PlayerInfo'.id,
    ?INFO("BOTMODULE ~p started with game_session pid ~p", [UId,SPid]),
    {ok, #state{user = PlayerInfo, uid = UId, owner = Owner, gid = GameId, session = SPid}}.

handle_call({send_message, Msg0}, _From, State) ->
    BPid = State#state.bot,
    Msg = flashify(Msg0),
    ?INFO("OKEY BOT: ~p",[Msg0]),
    BPid ! Msg,
    {reply, ok, State};

handle_call({call_rpc, Msg}, From, State) ->
    RR = State#state.running_requests,
    Id = State#state.request_id + 1,
    Self = self(),
    RR1 = dict:store(Id, From, RR),
    proc_lib:spawn_link(fun() ->
                                Res = try
                                          Answer = game_session:process_request(State#state.session, "OKEY BOT", Msg),
%                            		  ?INFO("Process Request from OKEY BOT:",[]),
%                            		  ?INFO("                      REQUEST: ~p",[Msg]),
%                            		  ?INFO("                        REPLY: ~p",[Answer]),
                                          {reply, Id, Answer}
                                      catch
                                          _Err:Reason ->
                                              {reply, Id, {error, Reason}}
                                      end,
                                gen_server:call(Self, Res)
                        end),
    {noreply, State#state{running_requests = RR1, request_id = Id}};

handle_call({reply, Id, Answer}, _From, State) ->
    RR = State#state.running_requests,
    From = dict:fetch(Id, RR),
    gen_server:reply(From, Answer),
    {reply, ok, State};

handle_call(get_session, _From, State) ->
    {reply, State#state.session, State};

handle_call(Request, _From, State) ->
    Reply = ok,
    ?INFO("unknown call: ~p", [Request]),
    {reply, Reply, State}.

handle_cast({init_state, Situation}, State) ->
    Mon = erlang:monitor(process, State#state.owner),
    UId = State#state.uid,
    GId = State#state.gid,
    SPid = State#state.session,
    game_session:bot_join_game(SPid, GId),
    BPid = proc_lib:spawn_link(game_okey_bot, robot_init, [#state{gid = GId, uid = UId, conn = self()}]),
    BPid ! {init_hand, Situation},
    {noreply, State#state{bot = BPid, owner_mon = Mon}};
handle_cast(join_game, State) ->
    Mon = erlang:monitor(process, State#state.owner),
    UId = State#state.uid,
    GId = State#state.gid,
    BPid = proc_lib:spawn_link(game_okey_bot, robot_init, [#state{gid = GId, uid = UId, conn = self()}]),
    BPid ! join_game,
    {noreply, State#state{bot = BPid, owner_mon = Mon}};
handle_cast(Msg, State) ->
    ?INFO("unknown cast: ~p", [Msg]),
    {noreply, State}.

handle_info({'DOWN', Ref, process, _, Reason},
            State = #state{owner_mon = OMon}) when OMon == Ref->
    ?INFO
    ("relay goes down with reason ~p so does bot", [Reason]),
    {stop, Reason, State};
handle_info(Info, State) ->
    {stop, {unrecognized_info, Info}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

flashify(R) when is_tuple(R) ->
    [RecName | Rest] = tuple_to_list(R),
    Rest1 = lists:map(fun
                          (X) -> flashify(X)
                      end, Rest),
    list_to_tuple([RecName | Rest1]);
flashify([{Key, _Value} | _] = P) when is_atom(Key) ->
    lists:map(fun
                  ({K, V}) when is_atom(K) -> {K, flashify(V)}
              end, P);
flashify(A) when A == true -> A;
flashify(A) when A == false -> A;
flashify(A) when A == null -> A;
flashify(A) when A == undefined -> A;
flashify(A) when is_atom(A) ->
    list_to_binary(atom_to_list(A));
flashify(Other) ->
    Other.

%%------------------------ BOT LOGIC

robot_init(State) ->
    robot_init_loop(State).

robot_init_loop(State) ->
    S = State#state.conn,
    Id = State#state.uid,
    GameId = State#state.gid,
    receive
        {init_hand, {[GI, GS], RobotInfo}} ->
            {Next, Delay} = RobotInfo,
            #okey_game_info{set_no = CurSet, rounds = MaxRound, sets = MaxSet, game_type = Mode} = GI,
            #okey_game_player_state{whos_move = Turn, game_state = GameState,
                                    tiles = Hand, gosterge = Gosterge, current_round = CurRound} = GS,
            SS = #'OkeySetState'{round_cur = CurRound, round_max = MaxRound,
                            set_cur = CurSet,set_max = MaxSet},
            State1 = State#state{set_state = SS, delay = Delay, mode = Mode},
            case {Turn, GameState} of
                {_, game_finished} ->
                    okey_client_rematch(State1),
                    init_set(State1);
                {_, do_okey_ready} ->
                    say_ready(State1),
                    okey_client_round(Next, State1);
                {Id, do_okey_take} ->
                    ?INFO("init bot: move both", []),
                    Hand1 = do_turn(State1, Hand),
                    okey_client_loop(State1#state{hand = Hand1, gosterge = Gosterge});
                {Id, do_okey_discard} ->
                    ?INFO("init bot: move discard only", []),
                    {TryDiscard, _} = draw_random(Hand),
                    Hand1 = do_discard(State1, Hand, TryDiscard),
                    okey_client_loop(State1#state{hand = Hand1, gosterge = Gosterge});
                {_, do_okey_challenge} ->
                    ?INFO("init bot: challenge", []),
                    do_challenge(State1),
                    okey_client_loop(State1#state{hand = Hand, gosterge = Gosterge});
                {_, _} ->
                    ?INFO("init bot: not bot's move", []),
                    okey_client_loop(State1#state{hand = Hand, gosterge = Gosterge})
            end;
        join_game ->
            case call_rpc(S, #join_game{game = GameId}) of
                #'TableInfo'{game = _Atom} ->
                    init_set(State);
                _Err ->
                    ?INFO("ID: ~p failed take with msg ~p", [Id, _Err]),
                    erlang:error(robot_cant_join_game)
            end
    end.

okey_client_loop(State) ->
    Hand0 = State#state.hand,
    Id = State#state.uid,
    receive
        #game_event{event = <<"okey_next_turn">>, args = Args} ->
            Hand1 = case {proplists:get_value(player, Args), proplists:get_value(can_challenge, Args)} of
                        {Id, false} ->
                            do_turn(State, Hand0);
                        {_OtherId, _Val} ->
                            Hand0
                    end,
            okey_client_loop(State#state{hand = Hand1});
        #'game_event'{event = <<"okey_revealed">>} ->
            do_challenge(State),
            okey_client_loop(State);
        #'game_event'{event = <<"okey_series_ended">>} ->
            S = State#state.conn,
            call_rpc(S, #logout{});
        #'game_event'{event = <<"okey_round_ended">>, args = Args} ->
            NextAction = proplists:get_value(next_action, Args),
            ?INFO("ID: ~p round ended", [Id]),
            okey_client_round(NextAction, State);
        _Other ->
            okey_client_loop(State)
    end.

do_challenge(State) ->
    GameId = State#state.gid,
    S = State#state.conn,
    ZZZ = call_rpc(S, #game_action{
                         game = GameId,
                         action = okey_challenge,
                         args = [ {challenge, random_bool(0.2)} ]}),
    ?INFO("ID: ~p challenge result: ~p", [State#state.uid, ZZZ]),
    ok.

okey_client_round(<<"done">>, State = #state{}) ->
    %% series of sets ended, do rematch, if needed.
    S = State#state.conn,
    call_rpc(S, #logout{});
    %okey_client_rematch(State),
    %init_set(State);
okey_client_round(<<"next_set">>, State = #state{}) ->
    %% set ended, wait for new set info
    init_set(State);
okey_client_round(<<"next_round">>, State) ->
    %% round ended, wait for new round info
    State2 = get_hand(State),
    okey_client_loop(State2).

okey_client_rematch(State) ->
    S = State#state.conn,
    GameId = State#state.gid,
    ?INFO("sending rematch", []),
    A = call_rpc(S, #rematch{game = GameId}),
    ?INFO("rematch result: ~p", [A]),
    ok = A,
    okey_client_rematch2(State).

okey_client_rematch2(State) ->
    S = State#state.conn,
    GameId = State#state.gid,
    ?INFO("rematch loop receive", []),
    receive
        #game_rematched{game = GI} when GameId == GI ->
            ?INFO("#game_rematched{game = GameId}", []);
        #game_event{event = <<"player_left">>, args = Args} ->
            ?INFO("#game_event{event = <<\"player_left\">>, args = Args}", []),
            Replaced = proplists:get_value(bot_replaced, Args, false) orelse
                proplists:get_value(human_replaced, Args, false),
            case Replaced of
                false ->
                    call_rpc(S, #logout{});
                true ->
                    okey_client_rematch2(State)
            end
    end.

init_set(State) ->
    State1 = receive
                 #game_event{event = <<"okey_game_info">>, args = Args} ->
                     Mode = proplists:get_value(game_type, Args),
                     SM = proplists:get_value(sets, Args),
                     SC = proplists:get_value(set_no, Args),
                     RM = proplists:get_value(rounds, Args),
                     TO = proplists:get_value(timeouts, Args),
                     Speed = TO#'OkeyTimeouts'.speed,
                     SpeedAtom = list_to_atom(binary_to_list(Speed)),
                     Delay = game_okey:get_timeout(robot, SpeedAtom),
                     ST = #'OkeySetState'{round_cur = 1, round_max = RM, set_cur = SC, set_max = SM},
                     State#state{set_state = ST, delay = Delay, mode = Mode}
             end,
    State2 = get_hand(State1),
    okey_client_loop(State2).

say_ready(State) ->
    S = State#state.conn,
    GameId = State#state.gid,
    ok = call_rpc(S, #game_action{game = GameId, action = okey_ready, args = []}).

get_hand(State) ->
    receive
        #game_event{event = <<"okey_game_started">>, args = Args} ->
            MH = proplists:get_value(tiles, Args),
            G = proplists:get_value(gosterge, Args),
            RC = proplists:get_value(current_round, Args),
            ST = State#state.set_state,
            ST1 = ST#'OkeySetState'{round_cur = RC},
            State#state{hand = MH, gosterge = G, set_state = ST1}
    end.

do_turn(State, Hand) ->
    Delay = get_delay(State),
    Hand1 = if length(Hand) == 15 ->
                   Hand;
               true ->
                   simulate_delay(take, Delay),
                   {_, H1} = do_take(State, Hand),
                   H1
            end,
    true = is_list(Hand1),
    {TryDiscard, _} = draw_random(Hand1),
    simulate_delay(discard, Delay),
    do_discard(State, Hand1, TryDiscard).

time_to_sleep(take, Delay) ->
    erlang:trunc((Delay / 3) * 1);
time_to_sleep(discard, Delay) ->
    erlang:trunc((Delay / 3) * 2).

simulate_delay(Action, Delay) ->
    TheDelay = time_to_sleep(Action, Delay),
    receive
        #game_paused{action = <<"pause">>} ->
            wait_for_resume()
    after TheDelay ->
            ok
    end.

wait_for_resume() ->
    receive
        #game_paused{action = <<"resume">>} ->
            ok
    end.

do_take(State, Hand) ->
    S = State#state.conn,
    GameId = State#state.gid,
    Id = State#state.uid,
    Pile = crypto:rand_uniform(0, 2),
    case call_rpc(S, #game_action{
                        game = GameId,
                        action = okey_take,
                        args = [ {pile, Pile} ]}) of
        #'OkeyPiece'{} = Tosh ->
            MyHand = [Tosh | Hand],
            {false, MyHand};
        {error, cant_take_do_discard} ->
            {false, Hand};
        {error, game_has_already_ended} when State#state.mode == <<"countdown">> ->
            {false, Hand};
        _Err ->
            ?INFO("ID: ~p failed take with msg ~p", [Id, _Err]),
            erlang:error(failed_take),
            {false, Hand}
    end.

do_discard(State, Hand, Item) ->
    S = State#state.conn,
    GameId = State#state.gid,
    Hand1 = lists:delete(Item, Hand),
    _Res = call_rpc(S, #game_action{game = GameId, action = okey_discard,
                                        args = [ {tile, Item} ]}),
    Hand1.

draw_random([One]) ->
    {One, []};
draw_random(List) ->
    {is_list, true} = {is_list, is_list(List)},
%    ?INFO("List: ~p",[List]),
    Pos = crypto:rand_uniform(1, length(List)),
    Item = lists:nth(Pos, List),
    ResList = lists:delete(Item, List),
    {Item, ResList}.

random_bool(Prob) ->
    Point = crypto:rand_uniform(0, 1000),
    Prob*1000 > Point.

get_delay(#state{is_robot = true, delay = Delay}) ->
    Delay;
get_delay(_) ->
    0.
