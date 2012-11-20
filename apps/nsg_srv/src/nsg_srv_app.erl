
-module(nsg_srv_app).
-behaviour(application).
-include_lib("nsx_config/include/log.hrl").
-export([start/2, stop/1, start_gproc/0, stop_gproc/0]).

start_gproc() -> application:start(gproc).
stop_gproc() -> application:stop(gproc).

start(_StartType, _StartArgs) ->
    nsm_srv_tournament_lobby_sup:start_link(),
    nsg_srv_sup:start_link().

stop(_State) ->
    ok.
