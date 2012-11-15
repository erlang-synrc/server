-module(nsg_srv_sup).
-behaviour(supervisor).

-include("setup.hrl").

-export([start_link/0]).
-export([init/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    net_kernel:connect(?APPSERVER_NODE),
    RestartStrategy = one_for_one,
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    Restart = permanent,
    Shutdown = 2000,

    OkeyLucky =
        {okey_lucky, %% Id
         {nsg_proxy_lucky, start_link, [okey, [{game_type, game_okey},
                                               {mode, normal}]]},
         permanent, %% Restart
         2000,      %% Shutdown timeout
         worker,    %% Process type
         [nsw_proxy_lucky]
        },
    TavlaLucky =
        {tavla_lucky, %% Id
         {nsg_proxy_lucky, start_link, [tavla, [{game_type, game_tavla},
                                                {mode, exclusive}]]},
         permanent, %% Restart
         2000,      %% Shutdown timeout
         worker,    %% Process type
         [nsw_proxy_lucky]
        },

    case nsm_db:get(config, "debug/production", false) of
         {ok, true} -> ok;
         _ -> case nsx_opt:get_env(nsw_srv,create_tables,true) of 
                   false -> ok;
                   true -> game_manager:create_tables(100)
              end
    end,

    {ok, { SupFlags, [OkeyLucky, TavlaLucky]} }.

