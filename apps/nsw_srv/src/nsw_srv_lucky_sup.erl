-module(nsw_srv_lucky_sup).
-behaviour(supervisor).

-include("setup.hrl").

-export([start/0, start_link/0]).
-export([init/1]).

start() ->
    supervisor:start({local, ?MODULE}, ?MODULE, []).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    OkeyLucky =
        {okey_lucky, %% Id
         {nsw_proxy_lucky, start_link, [[{game_type, game_okey},
                                         {mode, normal}]]},
         permanent, %% Restart
         2000,      %% Shutdown timeout
         worker,    %% Process type
         [nsw_proxy_lucky]
        },
    TavlaLucky =
        {tavla_lucky, %% Id
         {nsw_proxy_lucky, start_link, [[{game_type, game_tavla},
                                         {mode, exclusive}]]},
         permanent, %% Restart
         2000,      %% Shutdown timeout
         worker,    %% Process type
         [nsw_proxy_lucky]
        },
    {ok, { SupFlags, [OkeyLucky, TavlaLucky]} }.

