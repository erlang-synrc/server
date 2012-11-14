-module(nsw_srv_sup).
-behaviour(supervisor).
-export([start_link/0, init/1, create_tables/1,stress_test/1, start_tournament/3]).
-include("setup.hrl").
-include("loger.hrl").

-define(CHILD(M, F, A, Type), {M, {M, F, A}, permanent, 5000, Type, [M]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


create_tables(Num) ->
    Users = ["maxim","kate","alice","sustel","ahmettez","shyronnie","kunthar"], % TODO: chose randomly

    TavlaTwoPlayers = [rpc:call(?GAMESRVR_NODE,game_manager,create_table,
             [game_tavla,[{table_name,"maxim and alice"},
                          {speed,normal},
                          {rounds,3},
                          {game_mode,standard},
                          {owner,"kunthar"}],[<<"maxim">>,<<"alice">>]])||X<-lists:seq(1,Num)],
    [{ok,T2P1,_}|_] = TavlaTwoPlayers,
    [{ok,T2P2,_}|_] = lists:reverse(TavlaTwoPlayers),
    ?INFO("Tavla two playrs rooms: ~p",[{T2P1,T2P2}]),
    TavlaRobot = [rpc:call(?GAMESRVR_NODE,game_manager,create_table,
            [game_tavla,[{table_name,"maxim and robot"},
                          {speed,normal},
                          {rounds,3},
                          {game_mode,standard},
                          {owner,"sustel"}],[<<"maxim">>,robot]])||X<-lists:seq(1,Num)],
    [{ok,TR1,_}|_] = TavlaRobot,
    [{ok,TR2,_}|_] = lists:reverse(TavlaRobot),
    ?INFO("Tavla bot rooms: ~p",[{TR1,TR2}]),
    OkeyBots = [rpc:call(?GAMESRVR_NODE,game_manager,create_table,
            [game_okey,[{table_name,"okey maxim and robots"},
                          {speed,fast},
                          {rounds,3},
                          {sets,1},
                          {game_mode,color},
                          {owner,"ahmettez"}],[<<"maxim">>,robot,robot,robot]])||X<-lists:seq(1,Num)],
    [{ok,OB1,_}|_] = OkeyBots,
    [{ok,OB2,_}|_] = lists:reverse(OkeyBots),
    ?INFO("Okey bot rooms: ~p~n",[{OB1,OB2}]),
    OkeyPlayers = [rpc:call(?GAMESRVR_NODE,game_manager,create_table,
            [game_okey,[{table_name,"okey maxim and alice + 2 robots"},
                          {speed,normal},
                          {rounds,3},
                          {sets,1},
                          {game_mode,standard},
                          {owner,"kate"}],[<<"maxim">>,<<"alice">>,robot,robot]])||X<-lists:seq(1,Num)],
    [{ok,OP1,_}|_] = OkeyPlayers,
    [{ok,OP2,_}|_] = lists:reverse(OkeyPlayers),
    ?INFO("Okey two players bot rooms: ~p~n",[{OP1,OP2}]),
    TavlaPairedPlayers = [rpc:call(?GAMESRVR_NODE,game_manager,create_table,
             [game_tavla,[{table_name,"maxim and robots"},
                          {speed, normal},
                          {rounds,3},
                          {game_mode, paired},
                          {owner,"maxim"}],[<<"maxim">>,robot, robot, robot]])|| _ <-lists:seq(1,Num)],
    [{ok,TP1,_}|_] = TavlaPairedPlayers,
    [{ok,TP2,_}|_] = lists:reverse(TavlaPairedPlayers),
    ?INFO("Paired Tavla 2 tables rooms: ~p",[{TP1,TP2}]),
    TavlaPairedPlayers5Tables = [rpc:call(?GAMESRVR_NODE,game_manager,create_table,
             [game_tavla,[{table_name,"maxim and robots"},
                          {speed, normal},
                          {rounds,3},
                          {game_mode, paired},
                          {owner,"maxim"}],[<<"maxim">>,robot, robot, robot, robot, robot, robot, robot, robot, robot]])|| _ <-lists:seq(1,Num)],
    [{ok,TP15,_}|_] = TavlaPairedPlayers5Tables,
    [{ok,TP25,_}|_] = lists:reverse(TavlaPairedPlayers5Tables),
    ?INFO("Paired Tavla 5 tables rooms: ~p",[{TP15,TP25}]),
    ok.

stress_test(NumberOfRooms) ->
    OkeyPlayers = [begin
          {ok,GameId,A} = rpc:call(?GAMESRVR_NODE,game_manager,create_table,
            [game_okey,[{table_name,"okey maxim and alice + 2 robots"},
                          {speed,normal},
                          {rounds,80},
                          {sets,1},
                          {game_mode,standard},
                          {owner,"kate"}],[<<"maxim">>,<<"alice">>,robot,robot]]),
            Clients = [ proc_lib:spawn_link(fun() -> 
                                 rpc:call(?GAMESRVR_NODE,test_okey,init_with_join_game,
                                    [self(), ?GAMEHOST, 9000, GameId, Id, 1, normal])
                        end) || Id <- [<<"maxim">>,<<"alice">>] ],

                    {ok,GameId,A}
                  

                   end ||X<-lists:seq(1,NumberOfRooms)],
    [{ok,OP1,_}|_] = OkeyPlayers,
    [{ok,OP2,_}|_] = lists:reverse(OkeyPlayers),
    ?INFO("Okey bot rooms runned (STRESS): ~p~n",[{OP1,OP2}]).

start_tournament(TourId,NumberOfTournaments, NumberOfPlayers) ->
    RealPlayers = [ erlang:list_to_binary(U) || U <- nsm_tournaments:joined_users(TourId)],
    
    Registrants = case NumberOfPlayers > length(RealPlayers) of
                       true -> RealPlayers ++ 
                                [ erlang:list_to_binary([<<"trn_player">>, integer_to_list(N)]) ||
                                      N <- lists:seq(1, NumberOfPlayers - length(RealPlayers))];
                       false -> RealPlayers
                   end,

    OkeyTournaments =
        [begin
             {ok,GameId,A} = rpc:call(?GAMESRVR_NODE,game_manager,create_game,
                                      [game_okey_ng_trn_elim, [{registrants, Registrants},
                                                               {kakush_per_round, 8},
                                                               {trn_id,TourId},
                                                               {demo_mode, true}]]),
             [ proc_lib:spawn_link(fun() ->
                                           rpc:call(?GAMESRVR_NODE,test_okey,init_with_join_game,
                                                    [self(), ?GAMEHOST, 9000, GameId, Id, 1, normal])
                                   end) || Id <- Registrants ],
             {ok,GameId,A}
         end || _ <-lists:seq(1,NumberOfTournaments)],
    [{ok,OP1,_}|_] = OkeyTournaments,
    [{ok,OP2,_}|_] = lists:reverse(OkeyTournaments),
    ?INFO("Okey tournaments runned: ~p~n",[{OP1,OP2}]),
    OP1.



init([]) ->
    net_kernel:connect(?APPSERVER_NODE),
    net_kernel:connect(?GAMESRVR_NODE),

    rpc:call(?GAMESRVR_NODE,nsg_srv_app,stop_gproc,[]),
    application:stop(gproc),

    rpc:call(?APPSERVER_NODE,nsm_srv_app,start_gproc,[]),
    rpc:call(?GAMESRVR_NODE,nsg_srv_app,start_gproc,[]),
    application:start(gproc),

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,
    DChild = {user_counter, {user_counter, start_link, []}, Restart, Shutdown, Type, [user_counter]},

    gettext_server:start(),
    gettext:change_gettext_dir(code:priv_dir(nsw_srv)),
    gettext:recreate_db(),

    case nsm_db:get(config, "debug/production", false) of
         {ok, true} -> ok;
         _ -> case nsx_opt:get_env(nsw_srv,create_tables,true) of 
                   false -> ok;
                   true -> create_tables(100)
              end
    end,

    LuckyChild = {nsw_srv_lucky_sup,
                  {nsw_srv_lucky_sup, start_link, []},
                  permanent, 2000, supervisor, [nsw_srv_lucky_sup]},
    {ok, { {one_for_one, 5, 10}, [DChild, LuckyChild]} }.
