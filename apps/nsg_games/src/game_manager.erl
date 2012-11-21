-module(game_manager).
-author('Maxim Sokhatsky <maxim@synrc.com>').

-include_lib("nsg_srv/include/requests.hrl").
-include_lib("nsg_srv/include/conf.hrl").
-include_lib("nsm_db/include/table.hrl").
-include_lib("nsm_db/include/tournaments.hrl").
-include_lib("nsx_config/include/log.hrl").
-include_lib("stdlib/include/qlc.hrl").
-compile(export_all). 

-record(state, { game_tavla = 0, game_okey = 0 }).

create_game(GameFSM, Params) ->
    GameId = id_generator:get_id(),
    {ok, Pid} = gen_server:call(?MODULE, {create_game, GameFSM, Params, GameId}),
    {ok, GameId, Pid}.

create_table(GameFSM, PlayerIds) -> create_table(GameFSM, [], PlayerIds).
create_table(GameFSM, Params, PlayerIds) ->
    GameId = id_generator:get_id(),
    {ok, Pid} = gen_server:call(?MODULE, {create_table, GameFSM, Params, GameId, PlayerIds}),
    {ok, GameId, Pid}.

get_relay_pid(GameId) -> case get_tables(GameId) of [] -> undefined;
    [#game_table{game_process = P} | _] -> ?INFO("GameRelay: ~p",[P]), P end.
get_relay_mod_pid(GameId) -> case get_tables(GameId) of [] -> undefined;
    [#game_table{game_process = P, game_module = M} | _] ->  ?INFO("GameRelay: ~p",[{M,P}]), {M,P} end.
subscribe(Pid, GameId, PlayerId) -> gen_server:call(?MODULE, {subscribe, Pid, GameId, PlayerId}).
subscribe(Pid, GameId) -> gen_server:call(?MODULE, {subscribe, Pid, GameId}).
get_relay(GameId) -> gen_server:call(?MODULE, {get_relay, GameId}).
unsubscribe(Pid, GameId) -> gen_server:cast(?MODULE, {unsubscribe, Pid, GameId}).
game_requirements(GameAtom) -> GameAtom:get_requirements().
add_game(Game) -> gen_server:cast(?MODULE, {add_game, Game}).
remove_game(Game) -> gen_server:cast(?MODULE, {remove_game, Game}).
counter(Game) -> gen_server:call(?MODULE, {game_counter, Game}).
start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop(Ref) -> gen_server:cast(Ref, stop).
init([]) -> {ok, #state{}}.

handle_call({get_relay, Topic}, _From, State) -> Res = get_relay_pid(Topic), {reply, Res, State};

handle_call({game_counter, FSM}, _From, State) ->
    {reply, case FSM of game_tavla -> State#state.game_tavla; game_okey -> State#state.game_okey; _ -> 0 end, State};

handle_call({create_table, GameFSM, Params, Topic, PlayerIds}, _From, State) ->
    {Res, State1} = create_game_monitor(Topic, {lobby, GameFSM}, Params, PlayerIds, State),
    {reply, Res, State1};

handle_call({create_game, GameFSM, Params, Topic}, _From, State) ->
    {Res, State1} = create_game_monitor2(Topic, GameFSM, Params, State),
    {reply, Res, State1};

handle_call({create_chat, Topic, Players}, _From, State) ->
    {Srv, State1} = create_game_monitor(Topic, chat, [], Players, State),
    {reply, Srv, State1};

handle_call({subscribe, Pid, Topic}, _From, State) ->
    RelayPid = get_relay_pid(Topic),
    case RelayPid of
         undefined -> ok;
         X -> relay:subscribe(RelayPid, Pid)
    end,
    {reply, RelayPid, State};

handle_call({subscribe, Pid, Topic, PlayerId}, _From, State) ->
    RelayPid = get_relay_pid(Topic),
    case RelayPid of
         undefined -> ok;
         X -> relay:subscribe(RelayPid, Pid, PlayerId)
    end,
    {reply, RelayPid, State};

handle_call({unsubscribe, Pid, Topic}, _From, State) ->
    RelayPid = get_relay_pid(Topic),
    case RelayPid of
         undefined -> ok;
         X -> relay:unsubscribe(RelayPid, Pid)
    end,
    {reply, RelayPid, State};

handle_call(Event, From, State) -> {stop, {unknown_call, Event, From}, State}.

handle_cast(stop, State) -> {stop, normal, State};
handle_cast(Event, State) -> {stop, {unknown_cast, Event}, State}.

handle_info({'DOWN', _, process, Pid, Reason}, State) ->
    ?INFO("Game Monitor ~p has died with reason: ~p", [Pid, Reason]),
    {noreply, State};

handle_info({add_game, FSM}, State) ->
    Tavlas = State#state.game_tavla + case FSM of game_tavla -> 1; _ -> 0 end,
    Okeys  = State#state.game_okey  + case FSM of game_okey -> 1; _ -> 0 end,
    {noreply, State#state{game_tavla = Tavlas, game_okey = Okeys}};

handle_info({remove_game, FSM}, State) ->
    Tavlas = State#state.game_tavla + case FSM of game_tavla -> -1; _ -> 0 end,
    Okeys  = State#state.game_okey  + case FSM of game_okey -> -1; _ -> 0 end,
    {noreply, State#state{game_tavla = Tavlas, game_okey = Okeys}};

handle_info(Info, State) -> {stop, {unknown_info, Info}, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

game_monitor_module(GameFSM, GameMode) ->
    case {GameFSM, GameMode} of
        {game_tavla, paired} -> paired_tavla;
        _ -> relay
    end.

get_requirements(GameFSM,M) -> (game_monitor_module(GameFSM, M)):get_requirements(GameFSM,M).

-spec create_game_monitor(string(), pid(), [any()], [pid()], #state{}) -> {{'ok', pid()} | {'error', any()}, #state{}}.
create_game_monitor(Topic, {lobby,GameFSM}, Params, Players, State) ->
    GameMode = proplists:get_value(game_mode, Params, standard),
    ?INFO("Create Root Game Process (Game Monitor): ~p Mode: ~p Params: ~p",[GameFSM, GameMode,Params]),
    RelayInit = (game_monitor_module(GameFSM,GameMode)):start(Topic, {lobby,GameFSM}, Params, Players, self()),
    case RelayInit of 
        {ok, Srv} ->
            Ref = erlang:monitor(process, Srv),
            {{ok, Srv}, State};
        {error, Reason} ->
            {{error, Reason}, State}
    end.

create_game_monitor2(Topic, GameFSM, Params, State) ->
    ?INFO("Create Root Game Process (Game Monitor2): ~p Params: ~p",[GameFSM, Params]),
    RelayInit = GameFSM:start(Topic, Params),
    case RelayInit of 
        {ok, Srv} ->
            Ref = erlang:monitor(process, Srv),
            {{ok, Srv}, State};
        {error, Reason} ->
            {{error, Reason}, State}
    end.

get_lucky_table(Game) ->
    Lucky = true,
    Check = fun(undefined, _Value) -> true;
               (Param, Value) ->  Param == Value
            end,
    Cursor = fun() ->
                     qlc:cursor(qlc:q([V || {{_,_,_K},_,V=#game_table{game_type=G,
                                                                      feel_lucky = L}}
                                                <- gproc:table(props),
                                            Check(Game, G),
                                            Check(Lucky, L)]))
             end,
    Tables = qlc:next_answers(Cursor(), 1),
    ?INFO("~w:get_lucky_table Table = ~p", [?MODULE, Tables]),
    Tables.

get_single_tables(Setting,UId,GameFSM,Convert) ->

    GetPropList = fun(Key,Setngs) -> 
                   case Setngs of
                        undefined -> undefined;
                        _Else -> proplists:get_value(Key, Setngs)
                   end end,

    Rounds = GetPropList(rounds, Setting),
    GameType = GetPropList(game_mode, Setting),
    Speed = GetPropList(speed, Setting),
    Game = GetPropList(game, Setting),
    Lucky = false,

    FilterAllUsers = case GetPropList(users, Setting) of
        undefined -> [];
        {multiple, ManyUsers} -> ManyUsers;
        SingleUser -> [SingleUser]
    end,

    FilterAnyUser = case GetPropList(group, Setting) of
        undefined -> [];
        GroupId -> 
            [UId || UId <- nsm_groups:list_group_members(GroupId)]
    end,

    MaxUsers = case GameFSM of "tavla" -> 2; "okey" -> 4 end,

    Check = fun(Param,Value) -> 
                   case Param of
                        undefined -> true;
                        _Else -> Param == Value
                   end end,

    Cursor = fun(Id,FilterFree,FilterUser) ->
                qlc:cursor(qlc:q([V || {{_,_,_K},_,V=#game_table{creator=C,
                                                   rounds=R, game_type=G,
                                                   users=U, game_speed=S,
                                                   game_mode=GT,
                                                   feel_lucky = L}} <- gproc:table(props),
                           FilterFree(MaxUsers - length(U)),
                           FilterUser(C,Id),
                           Check(Game,G),
                           Check(Speed,S),
                           Check(GameType,GT),
                           Check(Rounds,R),
                           Check(Lucky, L)])
                )
    end,
    OneAvailable   = fun(N) -> N == 1 end,
    TwoAvailable   = fun(N) -> N == 2 end,
    ThreeAvailable = fun(N) -> N == 3 end,
    MoreAvailable  = fun(N) -> N > 3 end,
    NotAvailable   = fun(N) -> N == 0 end,
    Others         = fun(IterUser,CurrentUser) -> IterUser =/= CurrentUser end,
    Own            = fun(IterUser,CurrentUser) -> IterUser == CurrentUser end,

    OneLeftListOther = qlc:next_answers(Cursor(UId, OneAvailable, Others), 10),
    OneLeftListOwn = qlc:next_answers(Cursor(UId, OneAvailable, Own), 10),
    TwoLeftListOther = qlc:next_answers(Cursor(UId, TwoAvailable, Others), 10),
    TwoLeftListOwn = qlc:next_answers(Cursor(UId, TwoAvailable, Own), 10),
    ThreeLeftListOther = qlc:next_answers(Cursor(UId, ThreeAvailable, Others), 10),
    ThreeLeftListOwn = qlc:next_answers(Cursor(UId, ThreeAvailable, Own), 10),
    MoreLeftListOther = qlc:next_answers(Cursor(UId, MoreAvailable, Others), 10),
    MoreLeftListOwn = qlc:next_answers(Cursor(UId, MoreAvailable, Own), 10),
    NoMoreLeftListOther = qlc:next_answers(Cursor(UId, NotAvailable, Others), 50),
    NoMoreLeftListOwn = qlc:next_answers(Cursor(UId, NotAvailable, Own), 10),

    QLC = OneLeftListOwn ++ OneLeftListOther ++
          TwoLeftListOwn ++ TwoLeftListOther ++
          ThreeLeftListOwn ++ ThreeLeftListOther ++
          MoreLeftListOwn ++ MoreLeftListOther ++
          NoMoreLeftListOwn ++ NoMoreLeftListOther.

get_tournament(TrnId) ->
    Check = fun(undefined, _Value) -> true;
               (Param, Value) ->  Param == Value
            end,
    Cursor = fun() ->
                     qlc:cursor(qlc:q([V || {{_,_,_K},_, V = #game_table{trn_id=TId}} <- gproc:table(props),
                                            Check(TrnId, TId)]))
             end,
    Table = case qlc:next_answers(Cursor(), 1) of
                   [T] -> X = T#game_table.id, integer_to_list(X);
                     _ -> []
            end,
    ?INFO("~w:get_tournament Table = ~p", [?MODULE, Table]),
    Table.

create_tables(Num) ->
    Users = ["maxim","kate","alice","sustel","ahmettez","shyronnie","kunthar"], % TODO: chose randomly

    TavlaTwoPlayers = [game_manager:create_table(game_tavla,
                         [{table_name,"maxim and alice"},
                          {speed,normal},
                          {rounds,3},
                          {default_pr,yes},
                          {game_mode,standard},
                          {owner,"kunthar"}],
                         [<<"maxim">>,<<"alice">>]) || X<-lists:seq(1,Num)],
    [{ok,T2P1,_}|_] = TavlaTwoPlayers,
    [{ok,T2P2,_}|_] = lists:reverse(TavlaTwoPlayers),
    ?INFO("Tavla two playrs rooms: ~p",[{T2P1,T2P2}]),
    TavlaRobot = [game_manager:create_table(game_tavla,[{table_name,"maxim and robot"},
                          {speed,normal},
                          {rounds,3},
                          {default_pr,yes},
                          {game_mode,standard},
                          {owner,"sustel"}],[<<"maxim">>,robot])||X<-lists:seq(1,Num)],
    [{ok,TR1,_}|_] = TavlaRobot,
    [{ok,TR2,_}|_] = lists:reverse(TavlaRobot),
    ?INFO("Tavla bot rooms: ~p",[{TR1,TR2}]),
    OkeyBots = [game_manager:create_table(game_okey,[{table_name,"okey maxim and robots"},
                          {speed,fast},
                          {rounds,3},
                          {sets,1},
                          {default_pr,yes},
                          {game_mode,color},
                          {owner,"ahmettez"}],[<<"maxim">>,robot,robot,robot])||X<-lists:seq(1,Num)],
    [{ok,OB1,_}|_] = OkeyBots,
    [{ok,OB2,_}|_] = lists:reverse(OkeyBots),
    ?INFO("Okey bot rooms: ~p~n",[{OB1,OB2}]),
    OkeyPlayers = [game_manager:create_table(game_okey,[{table_name,"okey maxim and alice + 2 robots"},
                          {speed,normal},
                          {rounds,3},
                          {default_pr,yes},
                          {sets,1},
                          {game_mode,standard},
                          {owner,"kate"}],[<<"maxim">>,<<"alice">>,robot,robot])||X<-lists:seq(1,Num)],
    [{ok,OP1,_}|_] = OkeyPlayers,
    [{ok,OP2,_}|_] = lists:reverse(OkeyPlayers),
    ?INFO("Okey two players bot rooms: ~p~n",[{OP1,OP2}]),
    TavlaPairedPlayers = [game_manager:create_table(game_tavla,[{table_name,"maxim and robots"},
                          {speed, normal},
                          {default_pr,yes},
                          {rounds,3},
                          {game_mode, paired},
                          {owner,"maxim"}],[<<"maxim">>,robot, robot, robot])|| _ <-lists:seq(1,Num)],
    [{ok,TP1,_}|_] = TavlaPairedPlayers,
    [{ok,TP2,_}|_] = lists:reverse(TavlaPairedPlayers),
    ?INFO("Paired Tavla 2 tables rooms: ~p",[{TP1,TP2}]),
    TavlaPairedPlayers5Tables = [game_manager:create_table(game_tavla,[{table_name,"maxim and robots"},
                          {speed, normal},
                          {rounds,3},
                          {default_pr,yes},
                          {game_mode, paired},
                          {owner,"maxim"}],[<<"maxim">>,robot, robot, robot, 
                                            robot, robot, robot, robot, robot, robot])|| _ <-lists:seq(1,Num)],
    [{ok,TP15,_}|_] = TavlaPairedPlayers5Tables,
    [{ok,TP25,_}|_] = lists:reverse(TavlaPairedPlayers5Tables),
    ?INFO("Paired Tavla 5 tables rooms: ~p",[{TP15,TP25}]),
    ok.

stress_test(NumberOfRooms) ->
    OkeyPlayers = [begin
          {ok,GameId,A} = game_manager:create_table(game_okey,[{table_name,"okey maxim and alice + 2 robots"},
                          {speed,normal},
                          {rounds,80},
                          {sets,1},
                          {game_mode,standard},
                          {owner,"kate"}],[<<"maxim">>,<<"alice">>,robot,robot]),

            Clients = [ proc_lib:spawn_link(fun() -> 
                                 test_okey:init_with_join_game(self(), '127.0.0.1', ?LISTEN_PORT, GameId, Id, 1, normal)
                        end) || Id <- [<<"maxim">>,<<"alice">>] ],

                    {ok,GameId,A}
                  

                   end ||X<-lists:seq(1,NumberOfRooms)],
    [{ok,OP1,_}|_] = OkeyPlayers,
    [{ok,OP2,_}|_] = lists:reverse(OkeyPlayers),
    ?INFO("Okey bot rooms runned (STRESS): ~p~n",[{OP1,OP2}]).

start_tournament(TourId,NumberOfTournaments,NumberOfPlayers,Quota,Tours,Speed) ->
    RealPlayers = [ erlang:list_to_binary(U#play_record.who) || U <- nsm_tournaments:joined_users(TourId)],
    
    Registrants = case NumberOfPlayers > length(RealPlayers) of
                       true -> RealPlayers ++ 
                                [ erlang:list_to_binary([<<"trn_player">>, integer_to_list(N)]) ||
                                      N <- lists:seq(1, NumberOfPlayers - length(RealPlayers))];
                       false -> RealPlayers
                   end,

    OkeyTournaments =
        [begin
             {ok,GameId,A} = game_manager:create_game(game_okey_ng_trn_elim, [{registrants, Registrants},
                                                               {quota_per_round, Quota},
                                                               {tours, Tours},
                                                               {speed, Speed},
                                                               {trn_id,TourId},
                                                               {demo_mode, true}]),
            % TODO: fix test_okey robot
%             [ proc_lib:spawn_link(fun() ->
%                                           test_okey:init_with_join_game(self(), '127.0.0.1', 9000, GameId, Id, 1, normal)
%                                   end) || Id <- Registrants ],
             {ok,GameId,A}
         end || _ <-lists:seq(1,NumberOfTournaments)],
    [{ok,OP1,_}|_] = OkeyTournaments,
    [{ok,OP2,_}|_] = lists:reverse(OkeyTournaments),
    ?INFO("Okey tournaments runned: ~p~n",[{OP1,OP2}]),
    OP1.

%get_tables(GameId) -> 
%    qlc:e(qlc:q([Val || {{_,_,_Key},_,Val=#game_table{id = Id}} <- gproc:table(props), GameId == Id ])).

get_tables(Id) ->
   qlc:e(qlc:q([Val || {{_,_,_Key},_,Val=#game_table{id = _Id}} <- gproc:table(props), Id == _Id ])).

qlc_id(Id) ->
    qlc:e(qlc:q([Val || {{_,_,_Key},_,Val=#game_table{gameid = _GameId, id = _Id, 
                            owner = _Owner, creator = _Creator}} <- 
             gproc:table(props), Id == _Id])).

qlc_id_creator(Id,Creator,Owner) ->
    qlc:e(qlc:q([Val || {{_,_,_Key},_,Val=#game_table{gameid = _GameId, id = _Id, 
                            owner = _Owner, creator = _Creator}} <- 
             gproc:table(props), Id == _Id, Creator == _Creator, Owner ==_Owner])).
