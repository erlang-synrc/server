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
-record(state, { game_tavla = 0, game_okey = 0 }).

destroy_game(Pid,Sup) -> game_sup:stop_game(Sup,Pid).

create_game(GameFSM, Params) ->
    GameId = id_generator:get_id(),
    {ok, Pid} = create_game_monitor(GameId, GameFSM, Params),
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

start() -> gen_server:start({local, ?MODULE}, ?MODULE, [], []).
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop(Ref) -> gen_server:cast(Ref, stop).

init([]) -> {ok,#state{}}.

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

game_sup_domain(Module, Params) ->
    case Module of
        game_tavla_ng_trn_paired -> tavla_sup;
        nsg_trn_standalone ->
            case proplists:get_value(game, Params) of
                game_okey -> okey_sup;
                game_tavla -> tavla_sup;
                _ -> game_sup
            end;
        nsg_trn_elimination ->
            case proplists:get_value(game_type, Params) of
                game_okey -> okey_sup;
                game_tavla -> tavla_sup;
                _ -> game_sup
            end;
        _ -> game_sup
    end.

create_game_monitor(Topic, GameFSM, Params) ->
    Sup = game_sup_domain(GameFSM, Params),
    ?INFO("Create Root Game Process (Game Monitor2): ~p Params: ~p Sup: ~p",[GameFSM, Params,Sup]),
    RelayInit = Sup:start_game(GameFSM,[Topic,Params],Topic),
    ?INFO("RelayInit ~p",[RelayInit]),
    RelayInit.

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
%    ?INFO("~w:get_tournament Table = ~p", [?MODULE, Table]),
    Table.


%% stress_test(NumberOfRooms) ->
%%     OkeyPlayers = [begin
%%           {ok,GameId,A} = game_manager:create_table(game_okey,[{table_name,"okey maxim and alice + 2 robots"},
%%                           {speed,normal},
%%                           {rounds,80},
%%                           {sets,1},
%%                           {game_mode,standard},
%%                           {owner,"kate"}],[<<"maxim">>,<<"alice">>,robot,robot]),
%% 
%%             Clients = [ proc_lib:spawn_link(fun() -> 
%%                                  test_okey:init_with_join_game(self(), '127.0.0.1', ?LISTEN_PORT, GameId, Id, 1, normal)
%%                         end) || Id <- [<<"maxim">>,<<"alice">>] ],
%% 
%%                     {ok,GameId,A}
%%                   
%% 
%%                    end ||X<-lists:seq(1,NumberOfRooms)],
%%     [{ok,OP1,_}|_] = OkeyPlayers,
%%     [{ok,OP2,_}|_] = lists:reverse(OkeyPlayers),
%%     ?INFO("Okey bot rooms runned (STRESS): ~p~n",[{OP1,OP2}]).


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
            BotsReplacementMode = case proplists:get_value(robots_replacement_allowed, Params, true) of
                                      true -> enabled;
                                      false -> disabled
                                  end,
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
                           {next_series_confirmation, no_exit},
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
                          {bots_replacement_mode, BotsReplacementMode},
                          {table_params, TableParams},
                          {common_params, Params}
                         ]);
        game_tavla ->
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
                         _ -> proplists:get_value(rounds, Params, undefined)
                     end,
            BotsReplacementMode = case proplists:get_value(robots_replacement_allowed, Params, true) of
                                      true -> enabled;
                                      false -> disabled
                                  end,
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
                           {game_mode, GameMode},
                           {rounds, Rounds},
                           {next_series_confirmation, no_exit},
                           {pause_mode, normal},
                           {social_actions_enabled, true},
                           {tables_num, 1}
                         ],

            create_game(nsg_trn_standalone,
                         [{game, Game},
                          {game_mode, GameMode},
                          {game_name, TableName},
                          {seats, 2},
                          {registrants, Users},
                          {initial_points, 0},
                          {quota_per_round, Quota},
                          {kakush_for_winners, KakushForWinners},
                          {kakush_for_loser, KakushForLoser},
                          {win_game_points, WinGamePoints},
                          {mul_factor, MulFactor},
                          {table_module, game_tavla_ng_table},
                          {bot_module, game_tavla_bot},
                          {bots_replacement_mode, BotsReplacementMode},
                          {table_params, TableParams},
                          {common_params, Params}
                         ])
    end.


create_paired_game(Game, Params, Users) ->
    ?INFO("create_paired_game/3 Params:~p", [Params]),
    case Game of
        game_tavla ->
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
                         _ -> proplists:get_value(rounds, Params, undefined)
                     end,
            BotsReplacementMode = case proplists:get_value(robots_replacement_allowed, Params, true) of
                                      true -> enabled;
                                      false -> disabled
                                  end,
            TablesNum = length(Users) div 2 + length(Users) rem 2,
            TableParams = [
                           {table_name, TableName},
                           {mult_factor, MulFactor},
                           {slang_allowed, SlangAllowed},
                           {observers_allowed, ObserversAllowed},
                           {tournament_type, paired},
                           {round_timeout, infinity},
                           {set_timeout, infinity},
                           {speed, Speed},
                           {game_mode, GameMode},
                           {rounds, Rounds},
                           {next_series_confirmation, no_exit},
                           {pause_mode, disabled},
                           {social_actions_enabled, true},
                           {tables_num, TablesNum}
                         ],

            create_game(game_tavla_ng_trn_paired,
                         [{game, Game},
                          {game_mode, GameMode},
                          {game_name, TableName},
                          {tables_num, TablesNum},
                          {registrants, Users},
                          {quota_per_round, Quota},
                          {kakush_for_winners, KakushForWinners},
                          {kakush_for_loser, KakushForLoser},
                          {win_game_points, WinGamePoints},
                          {mul_factor, MulFactor},
                          {table_module, game_tavla_ng_table},
                          {bot_module, game_tavla_bot},
                          {bots_replacement_mode, BotsReplacementMode},
                          {table_params, TableParams},
                          {common_params, Params}
                         ])
    end.


create_elimination_trn(GameType, Params, Registrants) ->
    ?INFO("create_elimination_trn/3 Params:~p", [Params]),
    TrnId         = proplists:get_value(trn_id, Params),
    QuotaPerRound = proplists:get_value(quota_per_round, Params),
    PlayersNumber = proplists:get_value(players_number, Params),
    Tours         = proplists:get_value(tours, Params),
    GameMode      = proplists:get_value(game_mode, Params),
    Speed         = proplists:get_value(speed, Params),
    Awards        = proplists:get_value(awards, Params),
    RegistrantsNum = length(Registrants),
    if RegistrantsNum =/= PlayersNumber ->
           ?ERROR("create_elimination_trn/3 Error: Wrong number of the registrants: ~p (required: ~p). ",
                  [RegistrantsNum, PlayersNumber]),
           exit(wrong_registrants_number);
       true -> do_nothing
    end,
    {ok, Plan} = nsg_matrix_elimination:get_plan(GameType, QuotaPerRound, PlayersNumber, Tours),
    case GameType of
        game_okey ->
            Rounds = 10,
            {ok, SetTimeout} = nsm_db:get(config,"games/okey/trn/elim/tour_time_limit/"++integer_to_list(Tours), 35*60*1000),
            TableParams = [
                           {table_name, ""},
                           {tournament_type, elimination},
                           {round_timeout, infinity},
                           {set_timeout, SetTimeout},
                           {speed, Speed},
                           {game_type, GameMode},
                           {rounds, Rounds},
                           {gosterge_finish_allowed, undefined},
                           {reveal_confirmation, true},
                           {next_series_confirmation, no},
                           {pause_mode, disabled},
                           {observers_allowed, false},
                           {slang_allowed, false},
                           {social_actions_enabled, false},
                           {mult_factor, 1}
                         ],
            create_game(nsg_trn_elimination,
                        [{game_type, GameType},
                         {game_mode, GameMode},
                         {registrants, Registrants},
                         {plan, Plan},
                         {quota_per_round, QuotaPerRound},
                         {rounds_per_tour, Rounds},
                         {tours, Tours},
                         {players_per_table, 4},
                         {speed, Speed},
                         {awards, Awards},
                         {trn_id, TrnId},
                         {table_module, game_okey_ng_table_trn},
                         {demo_mode, false},
                         {table_params, TableParams}
                        ]);
        game_tavla ->
            Rounds = 3,
            {ok, SetTimeout} = nsm_db:get(config,"games/tavla/trn/elim/tour_time_limit/"++integer_to_list(Tours), 35*60*1000),
            TableParams = [
                           {table_name, ""},
                           {tournament_type, elimination},
                           {round_timeout, infinity},
                           {set_timeout, SetTimeout},
                           {speed, Speed},
                           {game_mode, GameMode},
                           {rounds, Rounds},
                           {next_series_confirmation, no},
                           {pause_mode, disabled},
                           {slang_allowed, false},
                           {observers_allowed, false},
                           {social_actions_enabled, false},
                           {mult_factor, 1},
                           {tables_num, 1}
                         ],

            create_game(nsg_trn_elimination,
                        [{game_type, GameType},
                         {game_mode, GameMode},
                         {registrants, Registrants},
                         {plan, Plan},
                         {quota_per_round, QuotaPerRound},
                         {rounds_per_tour, Rounds},
                         {tours, Tours},
                         {players_per_table, 2},
                         {speed, Speed},
                         {awards, Awards},
                         {trn_id,TrnId},
                         {table_module, game_tavla_ng_table},
                         {demo_mode, false},
                         {table_params, TableParams}
                        ])
    end.


start_tournament(TrnId,NumberOfTournaments,NumberOfPlayers,_Quota,_Tours,_Speed,GiftIds) ->

    {ok,Tournament} = nsm_db:get(tournament,TrnId),
    #tournament{quota = QuotaPerRound,
                tours = Tours,
                game_type = GameType,
                game_mode = GameMode,
                speed = Speed} = Tournament,
%%    ImagioUsers = nsm_auth:imagionary_users2(),
    RealPlayersUnsorted = nsm_tournaments:joined_users(TrnId),
    RealPlayersPR = lists:keysort(#play_record.other, RealPlayersUnsorted),
    ?INFO("Head: ~p",[hd(RealPlayersPR)]),
    RealPlayers = [list_to_binary(Who)||#play_record{who=Who}<-RealPlayersPR, Who /= undefined],

%%     Registrants = case NumberOfPlayers > length(RealPlayers) of
%%                        true -> nsm_db:put(Tournament#tournament{status=canceled}), RealPlayers;
%%                        false -> [lists:nth(N,RealPlayers)||N<-lists:seq(1,NumberOfPlayers)] end,
    RealPlayersNumber = length(RealPlayers),
    Registrants = if NumberOfPlayers == RealPlayersNumber -> RealPlayers;
                     NumberOfPlayers > RealPlayersNumber ->
                         RealPlayers ++ [list_to_binary(nsm_auth:ima_gio2(N)) ||
                                            N <- lists:seq(1, NumberOfPlayers-RealPlayersNumber)];
                     true -> lists:sublist(RealPlayers, NumberOfPlayers)
                  end,

    ?INFO("Registrants: ~p",[Registrants]),
    OkeyTournaments =
        [begin
             Params = [{trn_id, TrnId},
                       {quota_per_round, QuotaPerRound},
                       {players_number, NumberOfPlayers},
                       {tours, Tours},
                       {game_mode, GameMode},
                       {speed, Speed},
                       {awards, GiftIds}],
             {ok,GameId,A} = create_elimination_trn(GameType, Params, Registrants),
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
