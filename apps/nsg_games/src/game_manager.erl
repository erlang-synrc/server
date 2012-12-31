-module(game_manager).
-behaviour(gen_server).
-author('Maxim Sokhatsky <maxim@synrc.com>').
-compile(export_all).
-export([init/1, start/0, start_link/0, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include_lib("nsg_srv/include/requests.hrl").
-include_lib("nsg_srv/include/conf.hrl").
-include_lib("nsm_db/include/table.hrl").
-include_lib("nsm_db/include/tournaments.hrl").
-include_lib("nsx_config/include/log.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include_lib("nsm_db/include/accounts.hrl").
-compile(export_all). 
-record(state, { game_tavla = 0, game_okey = 0 }).

destroy_game(Sup, Pid) -> game_sup:stop_game(Sup,Pid).

create_game(GameFSM, Params) ->
    GameId = id_generator:get_id(),
    {{ok,Pid},_} = create_game_monitor2(GameId, GameFSM, Params, self()),
    {ok, GameId, Pid}.

create_table(GameFSM, PlayerIds) -> create_table(GameFSM, [], PlayerIds).
create_table(GameFSM, Params, PlayerIds) ->
    GameId = id_generator:get_id(),
    {{ok, Pid},_} = create_game_monitor(GameId, {lobby, GameFSM}, Params, PlayerIds, self()),
    {ok, GameId, Pid}.

get_lucky_pid(Sup) ->
    [X]=game_manager:get_lucky_table(Sup),
    X#game_table.game_process.
get_relay_pid(GameId) -> case get_tables(GameId) of [] -> undefined;
    [#game_table{game_process = P} | _] -> ?INFO("GameRelay: ~p",[P]), P end.
get_relay_mod_pid(GameId) -> case get_tables(GameId) of [] -> undefined;
    [#game_table{game_process = P, game_module = M} | _] ->  ?INFO("GameRelay: ~p",[{M,P}]), {M,P} end.
get_relay(GameId) -> gen_server:call(?MODULE, {get_relay, GameId}).
game_requirements(GameAtom) -> GameAtom:get_requirements().
game_requirements(game_tavla,paired) -> paired_tavla:get_requirements();
game_requirements(GameAtom,_) -> GameAtom:get_requirements().
counter(Game) -> PL = supervisor:count_children(case Game of game_okey -> okey_sup; 
                                                            game_tavla -> tavla_sup; _ -> game_sup end),
                 Res = proplists:get_value(active, PL, 0),
                 case Game of
                      game_okey -> Res;
                      game_tavla -> Res;
                      _ -> 0 end.

%start([Module, Args]) -> gen_server:start(?MODULE,[Module,Args],[]).
%start(Module, Args) -> gen_server:start(?MODULE,[Module,Args],[]).
start() -> gen_server:start({local, ?MODULE}, ?MODULE, [], []).
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop(Ref) -> gen_server:cast(Ref, stop).

init([]) -> {ok,#state{}}.
%init([Module,Args]) -> 
%    ?INFO("STARTING MODULE ~p under SUPERVISOR with PARAMS ~p~n",[Module,Args]),
%    case Module:start(Args) of
%                         {ok,_,Pid} -> {ok, Pid};
%                         {ok,Pid} -> {ok, Pid}
%                        end.

handle_call({get_relay, Topic}, _From, State) -> Res = get_relay_pid(Topic), {reply, Res, State};
handle_call({game_counter, FSM}, _From, State) ->
    {reply, case FSM of game_tavla -> State#state.game_tavla; game_okey -> State#state.game_okey; _ -> 0 end, State};
handle_call(Event, From, State) -> {stop, {unknown_call, Event, From}, State}.
handle_cast(stop, State) -> {stop, normal, State};
handle_cast(Event, State) -> {stop, {unknown_cast, Event}, State}.
handle_info({'DOWN', _, process, Pid, Reason}, State) -> {noreply, State};
handle_info(Info, State) -> {stop, {unknown_info, Info}, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

game_monitor_module(GameFSM, GameMode) -> case {GameFSM, GameMode} of {game_tavla, paired} -> paired_tavla; _ -> relay end.
get_requirements(GameFSM,M) -> (game_monitor_module(GameFSM, M)):get_requirements(GameFSM,M).
game_sup_domain(Module, Params) ->
    case Module of
        game_okey_ng_trn_elim -> okey_sup;
        game_tavla -> tavla_sup;
        fl_lucky -> lucky_sup;
        game_okey_ng_trn_lucky -> lucky_sup;
        nsg_trn_standalone ->
            case proplists:get_value(game, Params) of
                game_okey -> okey_sup;
                game_tavla -> tavla_sup;
                _ -> game_sup
            end;
        _ -> game_sup
    end.

-spec create_game_monitor(string(), pid(), [any()], [pid()], #state{}) -> {{'ok', pid()} | {'error', any()}, #state{}}.
create_game_monitor(Topic, {lobby,GameFSM}, Params, Players, State) ->
    GameMode = proplists:get_value(game_mode, Params, standard),
    ?INFO("Create Root Game Process (Game Monitor): ~p Mode: ~p Params: ~p",[GameFSM, GameMode,Params]),
    GameModule = game_monitor_module(GameFSM,GameMode),
    Sup = game_sup_domain(GameFSM, Params),
    RelayInit = Sup:start_game(GameModule,[Topic, {lobby,GameFSM}, Params, Players, self()], Topic),
    ?INFO("RelayInit ~p",[RelayInit]),
    case RelayInit of 
        {ok, Srv} ->
%            Ref = erlang:monitor(process, Srv),
            {{ok, Srv}, State};
        {error, Reason} ->
            {{error, Reason}, State}
    end.

create_game_monitor2(Topic, GameFSM, Params, State) ->
    Sup = game_sup_domain(GameFSM, Params),
    ?INFO("Create Root Game Process (Game Monitor2): ~p Params: ~p Sup: ~p",[GameFSM, Params,Sup]),
    RelayInit = Sup:start_game(GameFSM,[Topic,Params],Topic),
    ?INFO("RelayInit ~p",[RelayInit]),
    case RelayInit of 
        {ok, Srv} ->
%            Ref = erlang:monitor(process, Srv),
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
    Tables.

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

tavla_create_tables(Num) ->

    Users = nsm_auth:imagionary_users2(),
    L = length(Users),

    TavlaTwoPlayers = [ begin

    T2U1 = nsm_auth:ima_gio2(crypto:rand_uniform(0,L),Users),
    T2U2 = nsm_auth:ima_gio2(crypto:rand_uniform(0,L),Users),

    game_manager:create_table(game_tavla,
                         [{table_name,"tavla two players"},
                          {speed,normal},
                          {rounds,3},
                          {default_pr,yes},
                          {game_mode,standard},
                          {owner,"maxim"}],
                         [list_to_binary(T2U1),list_to_binary(T2U2)]) end || X<-lists:seq(2,Num)],
    [{ok,T2P1,_}|_] = TavlaTwoPlayers,
    [{ok,T2P2,_}|_] = lists:reverse(TavlaTwoPlayers),
    ?INFO("Tavla two playrs rooms: ~p",[{T2P1,T2P2}]),

    TavlaRobot = [ begin

    T2U1 = nsm_auth:ima_gio2(crypto:rand_uniform(0,L),Users),
    T2U2 = nsm_auth:ima_gio2(crypto:rand_uniform(0,L),Users),

    game_manager:create_table(game_tavla,[{table_name,"tavla one player"},
                          {speed,normal},
                          {rounds,3},
                          {default_pr,yes},
                          {game_mode,standard},
                          {owner,"maxim"}],[list_to_binary(T2U1),robot]) end ||X<-lists:seq(2,Num div 2)],
    [{ok,TR1,_}|_] = TavlaRobot,
    [{ok,TR2,_}|_] = lists:reverse(TavlaRobot),
    ?INFO("Tavla bot rooms: ~p",[{TR1,TR2}]).


okey_create_tables(Num) ->

    Users = nsm_auth:imagionary_users2(),
    L = length(Users),

    OkeyBots = [begin

    T2U1 = nsm_auth:ima_gio2(crypto:rand_uniform(0,L),Users),
    T2U2 = nsm_auth:ima_gio2(crypto:rand_uniform(0,L),Users),

    game_manager:create_table(game_okey,[{table_name,"okey one player"},
                          {speed,fast},
                          {rounds,3},
                          {sets,1},
                          {default_pr,yes},
                          {game_mode,color},
                          {owner,"maxim"}],[list_to_binary(T2U1),robot,robot,robot]) end||X<-lists:seq(2,Num)],
    [{ok,OB1,_}|_] = OkeyBots,
    [{ok,OB2,_}|_] = lists:reverse(OkeyBots),
    ?INFO("Okey bot rooms: ~p~n",[{OB1,OB2}]),

    OkeyPlayers = [begin

    T2U1 = nsm_auth:ima_gio2(crypto:rand_uniform(0,L),Users),
    T2U2 = nsm_auth:ima_gio2(crypto:rand_uniform(0,L),Users),
    T2U3 = nsm_auth:ima_gio2(crypto:rand_uniform(0,L),Users),
    T2U4 = nsm_auth:ima_gio2(crypto:rand_uniform(0,L),Users),

    game_manager:create_table(game_okey,[{table_name,"okey four players"},
                          {speed,normal},
                          {rounds,3},
                          {default_pr,yes},
                          {sets,1},
                          {game_mode,standard},
                          {owner,"kate"}],[list_to_binary(T2U1),list_to_binary(T2U2),
                                           list_to_binary(T2U3),list_to_binary(T2U4)]) end ||X<-lists:seq(2,Num)],
    [{ok,OP1,_}|_] = OkeyPlayers,
    [{ok,OP2,_}|_] = lists:reverse(OkeyPlayers),
    ?INFO("Okey two players bot rooms: ~p~n",[{OP1,OP2}]).

create_paired(Num) ->

    Users = nsm_auth:imagionary_users2(),

    L = length(Users),

    TavlaPairedPlayers = [begin

    T2U1 = nsm_auth:ima_gio2(crypto:rand_uniform(0,L),Users),
    T2U2 = nsm_auth:ima_gio2(crypto:rand_uniform(0,L),Users),

    game_manager:create_table(game_tavla,[{table_name,"paired tavla two tables"},
                          {speed, normal},
                          {default_pr,yes},
                          {rounds,3},
                          {game_mode, paired},
                          {owner,"maxim"}],[list_to_binary(T2U1),robot, robot, robot]) end || X <-lists:seq(2,Num)],
    [{ok,TP1,_}|_] = TavlaPairedPlayers,
    [{ok,TP2,_}|_] = lists:reverse(TavlaPairedPlayers),
    ?INFO("Paired Tavla 2 tables rooms: ~p",[{TP1,TP2}]),


    TavlaPairedPlayers5Tables = [begin

    T2U1 = nsm_auth:ima_gio2(crypto:rand_uniform(0,L),Users),
    T2U2 = nsm_auth:ima_gio2(crypto:rand_uniform(0,L),Users),
    T2U3 = nsm_auth:ima_gio2(crypto:rand_uniform(0,L),Users),
    T2U4 = nsm_auth:ima_gio2(crypto:rand_uniform(0,L),Users),
    T2U5 = nsm_auth:ima_gio2(crypto:rand_uniform(0,L),Users),
    T2U6 = nsm_auth:ima_gio2(crypto:rand_uniform(0,L),Users),
    T2U7 = nsm_auth:ima_gio2(crypto:rand_uniform(0,L),Users),
    T2U8 = nsm_auth:ima_gio2(crypto:rand_uniform(0,L),Users),
    T2U9 = nsm_auth:ima_gio2(crypto:rand_uniform(0,L),Users),
    T2U10 = nsm_auth:ima_gio2(crypto:rand_uniform(0,L),Users),

    game_manager:create_table(game_tavla,[{table_name,"paired tavla 5 tables"},
                          {speed, normal},
                          {rounds,3},
                          {default_pr,yes},
                          {game_mode, paired},
                          {owner,"maxim"}],[list_to_binary(T2U1),
                                            list_to_binary(T2U2),
                                            list_to_binary(T2U3),
                                            list_to_binary(T2U4),
                                            list_to_binary(T2U5),
                                            list_to_binary(T2U6),
                                            list_to_binary(T2U7),
                                            list_to_binary(T2U8),
                                            list_to_binary(T2U9),
                                            list_to_binary(T2U10)]) end || X <-lists:seq(2,Num)],

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


create_standalone_game(Game, Params, Users) ->
    ?INFO("create_standalone_game/3 Params:~p", [Params]),
    case Game of
        game_okey ->
            #pointing_rule{quota = Quota,
                           kakush_winner = KakushForWinners,
                           kakush_other = KakushForLoser,
                           game_points = WinGamePoints
                          } = proplists:get_value(pointing_rules, Params),
            TableName = proplists:get_value(table_name, Params),
            MulFactor = proplists:get_value(double_points, Params, 1),
            SlangAllowed = proplists:get_value(slang, Params, false),
            ObserversAllowed = proplists:get_value(observers, Params, false),
            Speed = proplists:get_value(speed, Params, normal),
            GameMode = proplists:get_value(game_mode, Params),
            Rounds = case GameMode of
                         countdown -> undefined;
                         _ -> proplists:get_value(rounds, Params, undefined)
                     end,
            GostergeFinishAllowed = proplists:get_value(gosterge_finish, Params, false),
            TableParams = [
                           {table_name, TableName},
                           {mult_factor, MulFactor},
                           {slang_allowed, SlangAllowed},
                           {observers_allowed, ObserversAllowed},
                           {tournament_type, standalone},
                           {round_timeout, infinity},
%%                           {round_timeout, 30 * 1000},
                           {set_timeout, infinity},
%%                           {set_timeout, 10 * 60 *1000},
                           {speed, Speed},
                           {game_type, GameMode},
                           {rounds, Rounds},
                           {reveal_confirmation, true},
                           {next_series_confirmation, false},
                           {pause_mode, normal},
                           {social_actions_enabled, true},
                           {gosterge_finish_allowed, GostergeFinishAllowed}
                         ],

            create_game(nsg_trn_standalone,
                         [{game, Game},
                          {game_mode, GameMode},
                          {game_name, TableName},
                          {seats, 4},
                          {registrants, Users},
                          {initial_points, 0},
                          {quota_per_round, Quota},
                          {kakush_for_winners, KakushForWinners},
                          {kakush_for_loser, KakushForLoser},
                          {win_game_points, WinGamePoints},
                          {mul_factor, MulFactor},
                          {table_module, game_okey_ng_table_trn},
                          {bot_module, game_okey_bot},
                          {table_params, TableParams},
                          {common_params, Params}
                         ]);
        game_tavla ->
            create_table(Game, Params, Users)
    end.


start_tournament(TourId,NumberOfTournaments,NumberOfPlayers,Quota,Tours,Speed,GiftIds) ->

    {ok,Tournament} = nsm_db:get(tournament,TourId),
    ImagioUsers = nsm_auth:imagionary_users2(),
    RealPlayersUnsorted = nsm_tournaments:joined_users(TourId),
    RealPlayersPR = lists:sort(fun(#play_record{other=AX},#play_record{other=BX}) -> AX < BX end,RealPlayersUnsorted),
    ?INFO("Head: ~p",[hd(RealPlayersPR)]),
    RealPlayers = [list_to_binary(Who)||#play_record{who=Who}<-RealPlayersPR],

    Registrants = case NumberOfPlayers > length(RealPlayers) of
                       true -> nsm_db:put(Tournament#tournament{status=canceled}), RealPlayers;
                       false -> [lists:nth(N,RealPlayers)||N<-lists:seq(1,NumberOfPlayers)] end,

    OkeyTournaments =
        [begin

             ?INFO("Registrants: ~p",[Registrants]),

             {ok,GameId,A} = game_manager:create_game(game_okey_ng_trn_elim, [{registrants, Registrants},
                                                               {quota_per_round, Tournament#tournament.quota},
                                                               {tours, Tournament#tournament.tours},
                                                               {speed, Tournament#tournament.speed},
                                                               {game_mode, Tournament#tournament.game_mode},
                                                               {awards, GiftIds},
                                                               {trn_id,TourId},
                                                               {demo_mode, false}]),

             nsm_db:put(Tournament#tournament{status=activated}),

             {ok,GameId,A}
         end || _ <-lists:seq(1,NumberOfTournaments)],
    [{ok,OP1,_}|_] = OkeyTournaments,
    [{ok,OP2,_}|_] = lists:reverse(OkeyTournaments),
    ?INFO("Okey tournaments runned: ~p~n",[{OP1,OP2}]),
    OP1.

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
