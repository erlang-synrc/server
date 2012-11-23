-module(lucky_sup).
-behaviour(supervisor).
-include_lib("nsm_db/include/config.hrl").
-export([start_link/0]).
-export([init/1]).
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 1,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    Restart = permanent,
    Shutdown = 2000,

    OkeyGameId = id_generator:get_id(),
    OkeySpec = {okey_lucky, {game_okey_ng_trn_lucky, start_link, [OkeyGameId,[{game_type, game_okey}, {mode, normal}]]}, 
                  Restart, Shutdown, worker, [game_okey_ng_trn_lucky]},

    TavlaGameId = id_generator:get_id(),
    TavlaSpec = {tavla_lucky, {fl_lucky, start_link, [TavlaGameId,[{game_type, game_tavla}, {mode, normal}]]}, 
                  Restart, Shutdown, worker, [fl_lucky]},

    {ok, { SupFlags, [OkeySpec, TavlaSpec]} }.

