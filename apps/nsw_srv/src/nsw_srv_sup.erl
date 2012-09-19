-module(nsw_srv_sup).
-behaviour(supervisor).
-export([start_link/0, init/1, create_tables/1]).

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
                          {rounds,1},
                          {game_mode,standard},
                          {owner,"kunthar"}],[<<"maxim">>,<<"alice">>]])||X<-lists:seq(1,Num)],
    [{ok,T2P1,_}|_] = TavlaTwoPlayers,
    [{ok,T2P2,_}|_] = lists:reverse(TavlaTwoPlayers),
    ?INFO("Tavla two playrs rooms: ~p",[{T2P1,T2P2}]),
    TavlaRobot = [rpc:call(?GAMESRVR_NODE,game_manager,create_table,
            [game_tavla,[{table_name,"maxim and robot"},
                          {speed,normal},
                          {rounds,1},
                          {game_mode,standard},
                          {owner,"sustel"}],[<<"maxim">>,robot]])||X<-lists:seq(1,Num)],
    [{ok,TR1,_}|_] = TavlaRobot,
    [{ok,TR2,_}|_] = lists:reverse(TavlaRobot),
    ?INFO("Tavla bot rooms: ~p",[{TR1,TR2}]),
    OkeyBots = [rpc:call(?GAMESRVR_NODE,game_manager,create_table,
            [game_okey,[{table_name,"okey maxim and robots"},
                          {speed,fast},
                          {rounds,1},
                          {sets,1},
                          {game_mode,color},
                          {owner,"ahmettez"}],[<<"maxim">>,robot,robot,robot]])||X<-lists:seq(1,Num)],
    [{ok,OB1,_}|_] = OkeyBots,
    [{ok,OB2,_}|_] = lists:reverse(OkeyBots),
    ?INFO("Okey bot rooms: ~p~n",[{OB1,OB2}]),
    OkeyPlayers = [rpc:call(?GAMESRVR_NODE,game_manager,create_table,
            [game_okey,[{table_name,"okey maxim and alice + 2 robots"},
                          {speed,normal},
                          {rounds,1},
                          {sets,1},
                          {game_mode,standard},
                          {owner,"kate"}],[<<"maxim">>,<<"alice">>,robot,robot]])||X<-lists:seq(1,Num)],
    [{ok,OP1,_}|_] = OkeyPlayers,
    [{ok,OP2,_}|_] = lists:reverse(OkeyPlayers),
    ?INFO("Okey bot rooms: ~p~n",[{OP1,OP2}]),
    TavlaPairedPlayers = [rpc:call(?GAMESRVR_NODE,game_manager,create_table,
             [game_tavla,[{table_name,"maxim and robots"},
                          {speed, normal},
                          {rounds,1},
                          {game_mode, paired},
                          {owner,"maxim"}],[<<"maxim">>,robot, robot, robot]])|| _ <-lists:seq(1,Num)],
    [{ok,TP1,_}|_] = TavlaPairedPlayers,
    [{ok,TP2,_}|_] = lists:reverse(TavlaPairedPlayers),
    ?INFO("Paired Tavla rooms: ~p",[{TP1,TP2}])
    .

stress_test(NumberOfRooms) ->
    OkeyPlayers = [begin
          {ok,GameId,A} = rpc:call(?GAMESRVR_NODE,game_manager,create_table,
            [game_okey,[{table_name,"okey maxim and alice + 2 robots"},
                          {speed,normal},
                          {rounds,1},
                          {sets,1},
                          {game_mode,standard},
                          {owner,"kate"}],[<<"maxim">>,<<"alice">>,robot,robot]]),
            Clients = [ proc_lib:spawn_link(fun() -> 
                                 rpc:call(?GAMESRVR_NODE,test_okey,init_with_join_game,
                                    [self(), localhost, 9001, GameId, Id, 1, normal])
                        end) || Id <- [<<"maxim">>,<<"alice">>] ],

                    {ok,GameId,A}
                  

                   end ||X<-lists:seq(1,NumberOfRooms)],
    [{ok,OP1,_}|_] = OkeyPlayers,
    [{ok,OP2,_}|_] = lists:reverse(OkeyPlayers),
    ?INFO("Okey bot rooms runned (STRESS): ~p~n",[{OP1,OP2}]).


init([]) ->
    application:start(nprocreg),
    application:start(cowboy),
    application:start(mimetypes),

    application:load(webmachine),
    {ok, BindAddress} = application:get_env(webmachine, bind_address),
    {ok, Port} = application:get_env(webmachine, port),

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,
    DChild = {user_counter, {user_counter, start_link, []}, Restart, Shutdown, Type, [user_counter]},

    gettext_server:start(),
    gettext:change_gettext_dir(code:priv_dir(nsw_srv)),
    gettext:recreate_db(),

    case nsm_db:get(config, "debug/production", false) of
         {ok, true} -> ok;
         _ -> create_tables(100)
    end,

    rpc:call(?APPSERVER_NODE,nsm_bg,init_workers,[]),

    Dispatch = [{'_', [ {'_',nitrogen_cowboy,[]},
                        {['...'],cowboy_http_static,[{directory,{priv_dir,nsw_srv,[]},{mimetypes,mime()}}]} ] }], 
    HttpOpts = [{max_keepalive, 50}, {dispatch, Dispatch}],

    ?INFO("Starting Cowboy Server on ~s:~p~n", [BindAddress, Port]),

    cowboy:start_listener(http, 100, cowboy_tcp_transport, [{port, Port}],
                                     cowboy_http_protocol, HttpOpts),

    {ok, { {one_for_one, 5, 10}, [DChild]} }.

mime() ->
    [
     {<<".html">>, [<<"text/html">>]},
     {<<".css">>, [<<"text/css">>]},
     {<<".png">>, [<<"image/png">>]},
     {<<".gif">>, [<<"image/gif">>]},
     {<<".jpg">>, [<<"image/jpeg">>]},
     {<<".js">>, [<<"application/javascript">>]}
    ].

