-module(nsw_srv_sup).
-behaviour(supervisor).
-export([
    start_link/0,
    init/1,
    dispatch/0
]).

-include("setup.hrl").
-include("loger.hrl").

%% Helper macro for declaring children of supervisor
-define(CHILD(M, F, A, Type), {M, {M, F, A}, permanent, 5000, Type, [M]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

create_tables() ->
    TavlaTwoPlayers = [rpc:call(?GAMESRVR_NODE,game_manager,create_table,
             [game_tavla,[{table_name,"maxim and alice"},
                          {speed,normal},
                          {rounds,1},
                          {game_mode,standard},
                          {owner,"kunthar"}],[<<"maxim">>,<<"alice">>]])||X<-lists:seq(1,100)],
    [{ok,T2P1,_}|_] = TavlaTwoPlayers,
    [{ok,T2P2,_}|_] = lists:reverse(TavlaTwoPlayers),
    ?INFO("Tavla two playrs rooms: ~p",[{T2P1,T2P2}]),
    TavlaRobot = [rpc:call(?GAMESRVR_NODE,game_manager,create_table,
            [game_tavla,[{table_name,"maxim and robot"},
                          {speed,normal},
                          {rounds,1},
                          {game_mode,standard},
                          {owner,"sustel"}],[<<"maxim">>,robot]])||X<-lists:seq(1,100)],
    [{ok,TR1,_}|_] = TavlaRobot,
    [{ok,TR2,_}|_] = lists:reverse(TavlaRobot),
    ?INFO("Tavla bot rooms: ~p",[{TR1,TR2}]),
    OkeyBots = [rpc:call(?GAMESRVR_NODE,game_manager,create_table,
            [game_okey,[{table_name,"okey maxim and robots"},
                          {speed,fast},
                          {rounds,1},
                          {sets,1},
                          {game_mode,color},
                          {owner,"ahmettez"}],[<<"maxim">>,robot,robot,robot]])||X<-lists:seq(1,100)],
    [{ok,OB1,_}|_] = OkeyBots,
    [{ok,OB2,_}|_] = lists:reverse(OkeyBots),
    ?INFO("Okey bot rooms: ~p~n",[{OB1,OB2}]),
    OkeyPlayers = [rpc:call(?GAMESRVR_NODE,game_manager,create_table,
            [game_okey,[{table_name,"okey maxim and alice + 2 robots"},
                          {speed,normal},
                          {rounds,1},
                          {sets,1},
                          {game_mode,standard},
                          {owner,"kate"}],[<<"maxim">>,<<"alice">>,robot,robot]])||X<-lists:seq(1,100)],
    [{ok,OP1,_}|_] = OkeyPlayers,
    [{ok,OP2,_}|_] = lists:reverse(OkeyPlayers),
    ?INFO("Okey bot rooms: ~p~n",[{OP1,OP2}]).

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
    %% Start the Process Registry...
    application:start(nprocreg),

    %% Start up Webmachine...
    application:load(webmachine),
    {ok, BindAddress} = application:get_env(webmachine, bind_address),
    {ok, Port} = application:get_env(webmachine, port),

    ?INFO("Starting Webmachine Server on ~s:~p~n", [BindAddress, Port]),

    Options = [
        {ip, BindAddress},
        {port, Port},
        {dispatch, dispatch()}
    ],

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,
    DChild = {user_counter, {user_counter, start_link, []}, Restart, Shutdown, Type, [user_counter]},

    gettext_server:start(),
    gettext:change_gettext_dir(code:priv_dir(nsw_srv)),
    gettext:recreate_db(),

    rpc:call(?APPSERVER_NODE,nsm_bg,init_workers,[]),
    rpc:call(?APPSERVER_NODE,zealot_db,init_db,[]),

    case rpc:call(?APPSERVER_NODE,zealot_db,get,[config, "debug/production", false]) of
         {ok, true} -> ok;
         _ -> create_tables()
    end,

    {ok, { {one_for_one, 5, 10}, [?CHILD(webmachine_mochiweb, start, [Options], worker), DChild]} }.


dispatch() ->
    [
        %% Static content handlers...
        {["js", '*'], static_resource, [{root, code:priv_dir(nsw_srv)++"/static/js"}]},
        {["css", '*'], static_resource, [{root, code:priv_dir(nsw_srv)++"/static/css"}]},
        {["images", '*'], static_resource, [{root, code:priv_dir(nsw_srv)++"/static/images"}]},
        {["fonts", '*'], static_resource, [{root, code:priv_dir(nsw_srv)++"/static/fonts"}]},
        {["files", '*'], static_resource, [{root, code:priv_dir(nsw_srv)++"/static/files"}]},
        {["nitrogen", '*'], static_resource, [{root, code:priv_dir(nsw_srv)++"/static/nitrogen"}]},
        {["test", '*'], static_resource, [{root, code:priv_dir(nsw_srv)++"/static/test"}]},
        {["testauth", '*'], static_resource, [{root, code:priv_dir(nsw_srv)++"/static/testauth"}]},
        {["facebook_login", '*'], static_resource, [{root, code:priv_dir(nsw_srv)++"/static/facebook_login"}]},
        {["robots.txt"], static_resource, [{root, code:priv_dir(nsw_srv)++"/static/robots.txt"}]},
        {["favicon.ico"], static_resource, [{root, code:priv_dir(nsw_srv)++"/static/favicon.ico"}]},


        %% Add routes to your modules here. The last entry makes the
        %% system use the dynamic_route_handler, which determines the
        %% module name based on the path. It's a good way to get
        %% started, but you'll likely want to remove it after you have
        %% added a few routes.
        %%
        %% p.s. - Remember that you will need to restart webmachine_mochiweb
        %%        (or restart the vm) for dispatch changes to take effect!!!
        %%
        %% you can restart it by erlang:exit(whereis(webmachine_mochiweb), kill).
        %%
        %% {["path","to","module1",'*'], nitrogen_webmachine, module_name_1}
        %% {["path","to","module2",'*'], nitrogen_webmachine, module_name_2}
        %% {["path","to","module3",'*'], nitrogen_webmachine, module_name_3}
        {['*'], nitrogen_webmachine, i18n_route_handler}
    ].



