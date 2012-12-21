
-module(nsg_srv_app).
-behaviour(application).
-include_lib("nsx_config/include/log.hrl").
-include_lib("nsm_db/include/tournaments.hrl").
-export([start/2, stop/1, start_gproc/0, stop_gproc/0]).

start_gproc() -> application:start(gproc).
stop_gproc() -> application:stop(gproc).

start(_StartType, _StartArgs) ->
    nsm_srv_tournament_lobby_sup:start_link(),
    A = nsg_srv_sup:start_link(),

    Pool = nsx_opt:get_env(nsx_idgen,game_pool,1000000),

    (catch 
    [ begin
        ?INFO("Tournament Lobby Started ~p",[Tour#tournament.id]),
        nsm_srv_tournament_lobby_sup:start_lobby(erlang:integer_to_list(Tour#tournament.id))
    end || Tour <- nsm_tournaments:all(), ((Tour#tournament.id div 1000000) * 1000000) == Pool ]),

    A.

stop(_State) ->
    ok.
