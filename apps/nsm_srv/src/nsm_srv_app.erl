
-module(nsm_srv_app).
-behaviour(application).
-include_lib("nsx_config/include/log.hrl").
-export([start/2, stop/1, start_gproc/0, stop_gproc/0]).

start_gproc() -> application:start(gproc).
stop_gproc() -> application:stop(gproc).

start(_StartType, _StartArgs) ->
    A = nsm_srv_sup:start_link(),
    A.

stop(_State) ->
    ok.
