-module(game_tavla_bot).
-author('Maxim Sokhatsky <maxim@synrc.com>').
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/3, start_link/3, robot_init/1, init_state/2, join_game/1, get_session/1,
         send_message/2, call_rpc/2, do_skip/2, do_rematch/1, first_move_table/0, follow_board/3 ]).

-include_lib("nsx_config/include/log.hrl").
-include_lib("nsg_srv/include/conf.hrl").
-include_lib("nsg_srv/include/basic_types.hrl").
-include_lib("nsg_srv/include/requests.hrl").
-include_lib("nsg_srv/include/game_tavla.hrl").
-include_lib("nsg_srv/include/game_okey.hrl").
-include_lib("nsg_srv/include/settings.hrl").

-record(state, {
        moves = 0 :: integer(),
        started = false :: boolean(),
        next_initiated = false :: boolean(),
        table_id :: integer(),
        is_robot = true :: boolean(),
        board :: list(tuple('Color'(), integer) | null),
        user :: #'PlayerInfo'{},
        player_color :: integer(),
        players,
        uid :: 'PlayerId'(),
        owner :: pid(),
        owner_mon :: 'MonitorRef'(),
        session :: pid(),
        gid :: 'GameId'(),
        bot :: pid(),
        conn :: pid(),
        hand :: list(),
        set_state :: #'TavlaSetState'{},
        running_requests = dict:new() :: any(),
        delay :: integer(),
        mode :: atom(),
        request_id = 0 }).

% gen_server

send_message(Pid, Message) -> gen_server:call(Pid, {send_message, Message}).
call_rpc(Pid, Message) -> gen_server:call(Pid, {call_rpc, Message}).
get_session(Pid) -> gen_server:call(Pid, get_session).
init_state(Pid, Situation) -> gen_server:cast(Pid, {init_state, Situation}).
join_game(Pid) -> gen_server:cast(Pid, join_game).
start(Owner, PlayerInfo, GameId) -> gen_server:start(?MODULE, [Owner, PlayerInfo, GameId], []).
start_link(Owner, PlayerInfo, GameId) -> gen_server:start_link(?MODULE, [Owner, PlayerInfo, GameId], []).

init([Owner, PlayerInfo, GameId]) ->
    UId = PlayerInfo#'PlayerInfo'.id,
    ?INFO("TAVLABOT started with game_session pid: ~p user: ~p", [self(),PlayerInfo]),
    {ok, #state{user = PlayerInfo, uid = UId, owner = Owner, gid = GameId}}.

handle_call({send_message, Msg0}, _From, State) ->
    BPid = State#state.bot,
    Msg = flashify(Msg0),
    BPid ! Msg,
    {reply, ok, State};

handle_call({call_rpc, Msg}, From, State) ->
    RR = State#state.running_requests,
    Id = State#state.request_id + 1,
    Self = self(),
    RR1 = dict:store(Id, From, RR),
    proc_lib:spawn_link(fun() ->
        Res = try
                  Answer = game_session:process_request(State#state.session, "TAVLA BOT", Msg),
                  {reply, Id, Answer}
              catch
                  _Err:Reason -> {reply, Id, {error, Reason}}
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
    {ok, SPid} = game_session:start_link(self()),
    game_session:bot_session_attach(SPid, State#state.user),
    {reply, State#state.session, State#state{session = SPid}};

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
    ?INFO("Init State User ~p",[State#state.user]),
    BPid = proc_lib:spawn_link(game_tavla_bot, robot_init, [#state{gid = GId, uid = UId, conn = self(), table_id = State#state.table_id, user = State#state.user}]),
    BPid ! {init_hand, Situation},
    {noreply, State#state{bot = BPid, owner_mon = Mon}};

handle_cast(join_game, State) ->
    Mon = erlang:monitor(process, State#state.owner),
    UId = State#state.uid,
    GId = State#state.gid,
    ?INFO("Init State User ~p",[State#state.user]),
    BPid = proc_lib:spawn_link(game_tavla_bot, robot_init, [#state{gid = GId, uid = UId, conn = self(), table_id = State#state.table_id, user = State#state.user}]),
    BPid ! join_game,
    {noreply, State#state{bot = BPid, owner_mon = Mon}};

handle_cast(Msg, State) ->
    ?INFO("unknown cast: ~p", [Msg]),
    {noreply, State}.

handle_info({'DOWN', Ref, process, _, Reason}, State = #state{owner_mon = OMon}) when OMon == Ref ->
    ?INFO("relay goes down with reason ~p so does bot", [Reason]),
    {stop, Reason, State};
handle_info(Info, State) ->
    {stop, {unrecognized_info, Info}, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

% loops

robot_init(State) ->
    ?INFO("Robot Init User Info ~p",[State#state.user]),
    robot_init_loop(State).

robot_init_loop(State) -> % receiving messages from relay
    S = State#state.conn,
    Id = State#state.uid,
    GameId = State#state.gid,
    receive
        {init_hand, {[GI, GS], RobotInfo}} ->
            {_Next, Delay} = RobotInfo,
            #tavla_game_info{game_mode = Mode} = GI,
            #tavla_game_player_state{whos_move = Turn, game_state = GameState, places = Hand} = GS,
            State1 = State#state{delay = Delay, mode = Mode},
            case {Turn, GameState} of
                {_, _} ->
                    ?INFO("TAVLABOT: not bot's move", []),
                    tavla_client_loop(State1#state{hand = Hand})
            end;
        join_game ->
            case call_rpc(S, #join_game{game = GameId}) of
                #'TableInfo'{game = _Atom} -> tavla_client_loop(State);
                _Err -> ?INFO("ID: ~p failed take with msg ~p", [Id, _Err]),
                        erlang:error(robot_cant_join_game)
            end
    end.

tavla_client_loop(State) -> % incapsulate tavla protocol
    timer:sleep(300),
    S = State#state.conn,
    GameId = State#state.gid,
    Id = State#state.uid,
    Color = State#state.player_color,
    GameMode = State#state.mode,
    receive
        #game_event{event = <<"tavla_next_turn">>, args = Params} ->
            TableId = proplists:get_value(table_id, Params, 0),
            case TableId == State#state.table_id of
                true ->
                    ?INFO("TAVLABOT ~p tavla_next_turn: GameMode:~p TableId~p Params:~p", [Id, GameMode, TableId, Params]),
                    case check_can_roll(GameMode, TableId) of
                       true ->
                           ?INFO("TAVLABOT ~p tavla next turn: ~p",[Id, {State#state.started,State#state.next_initiated}]),
                           case {proplists:get_value(player, Params), State#state.started} of
                               {Id,false} ->
                                   case State#state.next_initiated of
                                       false -> roll_action(State,TableId);
                                       true -> ignore
                                   end;
                               {Id,true} ->
                                   roll_action(State,TableId);
                               {_, false} ->
                                   case State#state.next_initiated of
                                       false -> roll_action(State,TableId);
                                       true -> ignore
                                   end;
                               {_, true} -> ignore
                           end,
                           tavla_client_loop(State#state{next_initiated=true});
                       false ->
                           ?INFO("TAVLABOT ~p tavla next turn: ignoring (not main table)", [Id]),
                           tavla_client_loop(State)
                    end;
            _ -> tavla_client_loop(State) end;
        #game_event{event = <<"tavla_moves">>, args = Params} ->
            TableId = proplists:get_value(table_id, Params, 0),
            case TableId == State#state.table_id of true ->

            ?INFO("tavla moves: ~p",[Params]),
            Player = proplists:get_value(player, Params),
            PlayerColor = if Id == Player -> Color; true -> fix_color(Color) end,
            From = proplists:get_value(from, Params),
            To = proplists:get_value(to, Params),
            ?INFO("board before moves: ~p", [State#state.board]),
            Board = reverse_board(State#state.board, PlayerColor),
            FromR=rel(From,PlayerColor), ToR=rel(To,PlayerColor),
            NewBoard = reverse_board(follow_board(Board,FromR,ToR,PlayerColor), PlayerColor),
            ?INFO("board after moves: ~p", [NewBoard]),
            tavla_client_loop(State#state{started=true,board = NewBoard,moves = State#state.moves + 1});

            _ -> tavla_client_loop(State) end;
        #game_event{event = <<"tavla_vido_request">>, args = Params} ->
            TableId = proplists:get_value(table_id, Params, 0),
            case TableId == State#state.table_id of true ->

%            ?INFO("tavla moves: ~p",[Params]),
            To = proplists:get_value(from, Params),
            vido(State,To,TableId),
            tavla_client_loop(State);

            _ -> tavla_client_loop(State) end;
        #game_event{event = <<"tavla_surrender_request">>, args = Params} ->
            TableId = proplists:get_value(table_id, Params, 0),
            case TableId == State#state.table_id of true ->

%            ?INFO("tavla moves: ~p",[Params]),
            To = proplists:get_value(from, Params),
            surrender(State,To,TableId),
            tavla_client_loop(State);

            _ -> tavla_client_loop(State) end;
        #game_event{event = <<"tavla_ack">>, args = Params} ->
            TableId = proplists:get_value(table_id, Params, 0),
            case TableId == State#state.table_id of true ->
            _To = proplists:get_value(from, Params),
            _Type = proplists:get_value(type, Params),
            tavla_client_loop(State);

            _ -> tavla_client_loop(State) end;
        #game_event{event = <<"tavla_game_started">>, args = Params} ->
            TableId = proplists:get_value(table_id, Params, 0),
            case TableId == State#state.table_id of true ->
            Players = proplists:get_value(players, Params),
            User = State#state.user,
            FoundMyself = lists:keyfind(User#'PlayerInfo'.id,#tavla_color_info.name,Players),
%%            PlayerColor = case FoundMyself#tavla_color_info.color of 1 -> 2; 2 -> 1 end,
            PlayerColor = fix_color(FoundMyself#tavla_color_info.color),
            ?INFO("TAVLABOT ~p game_started, color: ~p",[User#'PlayerInfo'.id, PlayerColor]),
            Board = game_tavla:setup_board(PlayerColor),
            tavla_client_loop(State#state{board = Board,moves=0,started=false,next_initiated=false,player_color=PlayerColor});
            _ -> tavla_client_loop(State) end;
        #game_event{event = <<"tavla_rolls">>, args = Params} ->
            TableId = proplists:get_value(table_id, Params, 0),
            case TableId == State#state.table_id of true ->
            ?INFO("TAVLABOT ~p (color: ~p) tavla_rolls: ~p",[Id, Color, Params]),
            State2 = case {fix_color(proplists:get_value(color, Params)),State#state.started} of
                {Color,_} ->
                      ?INFO("TAVLABOT ~p tavla rolls: dices=~p",[Id, proplists:get_value(dices, Params)]),
                      case {Dices = proplists:get_value(dices, Params),State#state.started} of
                        {[_A,_B],false} -> do_move(State,Dices,TableId,State#state.player_color), State#state{started = true, moves = State#state.moves + 1};
                        {[_A,_B],true} -> do_move(State,Dices,TableId,State#state.player_color), State#state{moves = State#state.moves + 1};
                        {[_C],_} -> State
                      end;
                _  -> ?INFO("TAVLABOT ~p tavla rolls: Ignore rolls (another color)",[Id]), State
            end,
            tavla_client_loop(State2);
            _ -> tavla_client_loop(State) end;
        #game_rematched{game = GI} when GameId == GI ->
            ?INFO("tavla rematched: ~p", [{GameId}]);
        #game_event{event = <<"player_left">>, args = Params} ->
            TableId = proplists:get_value(table_id, Params, 0),
            case TableId == State#state.table_id of true ->
            ?INFO("tavla player left: ~p", [Params]),
            Replaced = proplists:get_value(bot_replaced, Params, false) orelse
                       proplists:get_value(human_replaced, Params, false),
            case Replaced of
                false ->
                    call_rpc(S, #logout{});
                true ->
                    tavla_client_loop(State)
            end;
            _ -> tavla_client_loop(State) end;
        #game_event{event = <<"tavla_game_info">>, args = Args} ->
             User = State#state.user,
             Mode = proplists:get_value(game_mode, Args),
             SM = proplists:get_value(sets, Args),
             TableId = proplists:get_value(table_id, Args),
             ?INFO("TAVLABOT game_info ~p ~p",[self(),User]),
             SC = proplists:get_value(set_no, Args),
             RM = proplists:get_value(rounds, Args),
             _TO = proplists:get_value(timeouts, Args),
             Players = proplists:get_value(players, Args),
             ?INFO("TAVLABOT players: ~p",[Players]),
             Delay = game_tavla:get_timeout(robot, fast),
             ST = #'TavlaSetState'{round_cur = 1, round_max = RM, set_cur = SC, set_max = SM},
             CatchTID = case User == undefined of 
                            false -> FoundMyself = lists:keyfind(User#'PlayerInfo'.id,#'PlayerInfo'.id,Players),
                                    case FoundMyself of false -> State#state.table_id; _ -> TableId end;
                            true -> ?INFO("ERROR USER in ~p is not set!",[self()]), undefined
             end,
             tavla_client_loop( State#state{set_state = ST, table_id = CatchTID, delay = Delay, mode = Mode, players = Players});
        #game_event{event = <<"tavla_series_ended">>, args = Params} ->
            TableId = proplists:get_value(table_id, Params, 0),
            case TableId == State#state.table_id of true ->
            tavla_client_loop(State);
            _ -> tavla_client_loop(State) end;
        #game_event{event = <<"tavla_game_ended">>, args = Params} ->
            TableId = proplists:get_value(table_id, Params, 0),
            case TableId == State#state.table_id of true ->
            say_ready(State,TableId),
            tavla_client_loop(State);
            _ -> tavla_client_loop(State) end;
        X ->
            ?INFO("TAVLABOT received UNKNOWN event: ~p",[X]),
            tavla_client_loop(State)
    end.

fix_color(Color) ->  case Color of 1 -> 2; 2 -> 1 end.



check_can_roll(Mode, TableId) ->
    case Mode of
        <<"paired">> -> TableId==1 orelse ?PAIRED_TAVLA_ASYNC == true;
        _ -> true
    end.



% logic

follow_board(Board,Moves,PlayerColor) ->
    lists:foldl(fun ({From,To}, Acc) -> follow_board(Acc,From,To,PlayerColor) end, Board, Moves).

follow_board(Board,From,To,PlayerColor) -> % track the moves to keep board consistent
    FromCell = lists:nth(From + 1,Board),
    {FromColor,_FromCount} = case FromCell of 
                                 null -> {0,0};
                                 _Else -> _Else
                            end,
    BoardWithKicks = [ case No of
          From -> case Cell of
                       {_Color,1} -> {{0,0},null};
                       {Color,Count} -> {{0,0},{Color,Count-1}};
                       _ -> ?INFO("Board: ~p From ~p To ~p",[Board,From,To]),
                            ?INFO("follow_board: cant move from empty slot"), exit(self(),kill), {{0,0},null}
                  end;
            To -> case Cell of
                       null -> case FromColor of 0 -> {{0,0},null}; _ -> {{0,0},{FromColor,1}} end;
                       {0,0} -> case FromColor of 0 -> {{0,0},null}; _ -> {{0,0},{FromColor,1}} end;
                       {Color,Count} -> 
                            case Color =:= FromColor of
                                 true -> {{0,0},{Color,Count+1}};
                                 false -> case Count of
                                               1 when FromColor =/= 0 -> {{Color,1},{FromColor,1}};
                                               _ -> ?INFO("Board: ~p From ~p To ~p",[Board,From,To]),
                                                    ?INFO("follow_board: cant kick tower"), exit(self(),kill), {{0,0},{Color,Count}}
                                          end
                            end
                  end;
           _ -> {{0,0},Cell}
    end || {Cell,No} <- lists:zip(Board,lists:seq(0,27)) ],
    {KickColor,KickAmount} = lists:foldl(
            fun({{KC,KA},_},{Col,Sum}) ->
                  case KC of
                       0 -> {Col,Sum};
                       _ -> {KC,Sum+KA} 
                  end
            end,{0,0}, BoardWithKicks),
    ?INFO("Kick: ~p",[{KickColor,KickAmount}]),
    NewBoard = [ case {No,KickColor} of
        {25,_} when KickColor =/= 0 -> case Cell of
                       null -> {KickColor,KickAmount};
                       {_Color,Sum} -> {KickColor,KickAmount+Sum}
                  end;
        _ -> Cell
    end || {{{_KC,_KA},Cell},No} <- lists:zip(BoardWithKicks,lists:seq(0,27))],
    NewBoard.

all_in_home(Board,Color) ->
    Lates = lists:foldr(fun (A,Acc) -> 
                              case A of
                                   {null,_No} -> Acc;
                                   {{C,_Count},No} -> NotInHome = (((No > 0)and(No < 19)) or (No == 26)),
                                       case {C, NotInHome} of
                                            {Color,true} -> Acc + 1;
                                            _ -> Acc
                                       end
                              end
                          end,0,lists:zip(Board,lists:seq(0,27))),
    Lates =:= 0.

make_decision(Board,Dices2,Color,TableId) -> % produces tavla moves
    [X,Y] = Dices2,
    Dices = case X =:= Y of true -> [X,X,X,X]; false -> Dices2 end,
    Decision = first_available_move(Board,Dices,Color,TableId),
    ?INFO("Decision: ~p",[Decision]),
    Decision.

norm([A,B]) -> case A > B of true -> {A,B}; false -> {B,A} end.
first_move_table() -> [{{6,6},[{13,7},{13,7},{24,18},{24,18}]}, % based on 
                       {{6,5},[{24,13}]},
                       {{6,4},[{24,18},{13,9}]},
                       {{6,3},[{24,18},{13,10}]},
                       {{6,2},[{24,18},{13,11}]},
                       {{6,1},[{13,7},{8,7}]},
                       {{5,5},[{13,3},{13,3}]},
                       {{5,4},[{24,20},{13,8}]},
                       {{5,3},[{8,3},{5,3}]},
                       {{5,2},[{24,22},{13,8}]},
                       {{5,1},[{24,23},{13,8}]},
                       {{4,4},[{24,20},{24,20},{13,9},{13,9}]},
                       {{4,3},[{24,21},{13,9}]},
                       {{4,2},[{8,4},{6,4}]},
                       {{4,1},[{24,23},{13,9}]},
                       {{3,3},[{24,21},{24,21},{13,10},{13,10}]},
                       {{3,2},[{24,21},{13,11}]},
                       {{3,1},[{8,5},{6,5}]},
                       {{2,2},[{13,11},{13,11},{6,4},{6,4}]},
                       {{2,1},[{13,11},{6,5}]},
                       {{1,1},[{8,7},{8,7},{6,5},{6,5}]}
                      ].

%make_first_move(Dices,TableId,PlayerColor) -> 
%    {_,Moves} = lists:keyfind(norm(Dices),1,first_move_table()),
%    case PlayerColor of
%         1 -> [ #'TavlaAtomicMove'{table_id = TableId,from=25-From,to=25-To} || {From,To} <- Moves];
%         2 -> [ #'TavlaAtomicMove'{table_id = TableId,from=From,to=To} || {From,To} <- Moves]
%    end.

tactical_criteria(Board,Color) -> 
   case all_in_home(Board,Color) of
        true  -> finish;
        false -> case lists:nth(27,Board) of
                      {Color,Count} -> kicks;
                      _ -> race
                 end
   end.

rel(X,C) -> case C of 2 -> case X of 0->27;27->0;26->25;25->26;_->25-X end; 
                      1 -> X end.

reverse_board(Board,PlayerColor) ->
    case PlayerColor of
         1 -> Board;
         2 -> [BE|Rest] = Board, [WE,WK,BK|Rest1] = lists:reverse(Rest), [WE] ++ Rest1 ++ [WK,BK,BE]
    end.

first_available_move(RealBoard,XY,Color,TableId) ->
    RelativeBoard = reverse_board(RealBoard,Color),
    OppositePlayerColor = case Color of 1 -> 2; 2 -> 1 end,

    {List,Dices,Found,Board} = lists:foldl(fun (A,Acc2) ->
         {L,D,F,B} = Acc2,
         ?INFO("board: ~p", [B]),
         Tactic = tactical_criteria(B,Color),
         ?INFO("tactical criteria: ~p", [Tactic]),
         case D of [] -> Acc2; _ ->
              [H|T] = D,
              {Cell,No}=A,
              ?INFO("checking pos: ~p",[{Cell,No,H,Tactic}]),
              case {Cell,Tactic} of

     {_   ,kicks} when No =/= H -> Acc2;
     {null,kicks} when No =:= H -> ?INFO("found kick to empty: ~p",[{26,H}]), {L++[{26,H}],T,1,follow_board(B,26,H,Color)};
     {{OppositePlayerColor,1},kicks} when H =:= No -> ?INFO("found kick kick: ~p",[{26,H}]), {L++[{26,H}],T,1,follow_board(B,26,H,Color)};
     {{Color,Count},kicks} when H =:= No -> ?INFO("found kick over own: ~p",[{26,H}]), {L++[{26,H}],T,1,follow_board(B,26,H,Color)};
     {{Color,Count},finish} when H + No >= 25 -> ?INFO("found finish move: ~p",[{No,27}]), {L++[{No,27}],T,1,follow_board(B,No,27,Color)};
     {{Color,Count},race} when H + No >= 25 -> Acc2;
     {{Color,Count},Tactic} when (Tactic==race orelse Tactic==finish) andalso (H + No < 25) ->
                   case lists:nth(No+H+1,B) of
                       null -> ?INFO("found race to empty: ~p",[{No,No+H}]), {L++[{No,No+H}],T,1,follow_board(B,No,No+H,Color)}; 
                       {Color,_} -> ?INFO("found race over own: ~p",[{No,No+H}]), {L++[{No,No+H}],T,1,follow_board(B,No,No+H,Color)};
                       {OppositePlayerColor,1} -> ?INFO("found race kick: ~p",[{No,No+H}]), {L++[{No,No+H}],T,1,follow_board(B,No,No+H,Color)};
                           _ -> Acc2 end;
     _ -> Acc2
             end end
        end, {[], XY, 0, RelativeBoard}, lists:sublist(lists:zip(RelativeBoard,lists:seq(0,27)),2,24)     ),
    ?INFO("moves found: ~p",[{List,Dices}]),

    [#'TavlaAtomicMove'{table_id = TableId,from=rel(From,Color),to=rel(To,Color)} || {From,To} <- List]  ++
    case {length(Dices) > 0,Found} of {true,1} -> first_available_move(reverse_board(Board,Color),Dices,Color,TableId); _ -> []  end.

make_end_series(Color,TableId,Board) ->
    lists:foldl(fun (A,Acc) -> ?INFO("~p / ~p~n",[A,Acc]), {Cell,No} = A, case Cell of null -> Acc; 
                                  {Color,Count} -> Acc ++
    [ #'TavlaAtomicMove'{table_id = TableId,from=No,to=27} || X <- lists:seq(1,Count)]; _ -> Acc end
                                 end, [], lists:sublist(lists:zip(Board,lists:seq(0,27)),1,27)  ).

% actions

do_rematch(State) ->
    S = State#state.conn,
    GameId = State#state.gid,
    ok = call_rpc(S, #rematch{game = GameId}).

say_ready(State,TableId) ->
    S = State#state.conn,
    GameId = State#state.gid,
    ok = call_rpc(S, #game_action{game = GameId, action = tavla_ready, args = [{table_id,TableId}]}).

vido(State,To,TableId) ->
    S = State#state.conn,
    GameId = State#state.gid,
    ok = call_rpc(S, #game_action{game = GameId, 
                                  action = tavla_vido_answer,
                                  args = [{table_id,TableId},{from,State#state.uid},{to,To},{answer,false}]}).

surrender(State,To,TableId) ->
    S = State#state.conn,
    GameId = State#state.gid,
    ok = call_rpc(S, #game_action{game = GameId, 
                                  action = tavla_surrender_answer,
                                  args = [{table_id,TableId},{from,State#state.uid},{to,To},{answer,true}]}).

roll_action(State,TableId) ->
    S = State#state.conn,
    GameId = State#state.gid,
    ok = call_rpc(S, #game_action{game = GameId, action = tavla_roll, args = [{table_id,TableId}]}).

do_skip(State,TableId) ->
    S = State#state.conn,
    GameId = State#state.gid,
    call_rpc(S, #game_action{
                        game = GameId,
                        action = tavla_skip,
                        args = [{table_id,TableId}]}).

do_move(State, Dices,TableId,PlayerColor) ->
    Delay = get_delay(State),
    simulate_delay(take, Delay),
    S = State#state.conn,
    GameId = State#state.gid,
    Id = State#state.uid,
    
%    Decision = case State#state.moves > 5 of 
%                    false -> make_decision(State#state.board, Dices, PlayerColor,TableId);
%                    true -> make_end_series(PlayerColor,TableId,State#state.board)
%               end,
    Decision = make_decision(State#state.board, Dices, PlayerColor,TableId),
    ?INFO("DO_MOVE: ~p",[Decision]),
    case A = call_rpc(S, #game_action{
                        game = GameId,
                        action = tavla_move,
                        args = [ {table_id, TableId},{player, Id},{moves, Decision } ]}) of
        _ ->
            ?INFO("do_move: A ~p ~p", [Id, A]),
            {false, Dices}
    end.

get_delay(#state{is_robot = true, delay = Delay}) -> Delay;
get_delay(_) -> 0.

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

time_to_sleep(_, Delay) ->
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
