-module(nsx_config_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) -> nsx_config_sup:start_link().

stop(_State) -> ok.
