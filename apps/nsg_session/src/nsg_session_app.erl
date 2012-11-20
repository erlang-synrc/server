-module(nsg_session_app).
-behaviour(application).

-export([ start/2, stop/0, stop/1]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("nsg_srv/include/conf.hrl").

start(_StartType, _StartArgs) -> nsg_session_sup:start_link().
stop() -> stop([]).
stop(_State) -> nsg_session_sup:stop().
