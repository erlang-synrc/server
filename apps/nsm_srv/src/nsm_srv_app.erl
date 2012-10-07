
-module(nsm_srv_app).
-behaviour(application).
-include("tournaments.hrl").
-include_lib("nsx_config/include/log.hrl").
-export([start/2, stop/1, start_gproc/0, stop_gproc/0]).

start_gproc() -> application:start(gproc).
stop_gproc() -> application:stop(gproc).

start(_StartType, _StartArgs) ->
    A = nsm_srv_sup:start_link(),
%    [ nsm_srv_tournament_lobby_sup:start_lobby(Tour#tournament.id) || Tour <- nsm_tournaments:all() ],
    nsm_bg:init_workers(),
    A.

stop(_State) ->
    ok.
