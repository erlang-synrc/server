-module(nsm_bg_app).
-behaviour(application).
-export([start/2,stop/1]).

start(_StartType, _StartArgs) -> nsm_bg_sup:start_link().
stop(_State) -> ok.
