-module(nsx_idgen_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/0, stop/1]).

-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    application:start(nsx_idgen).

start(_StartType, _StartArgs) ->
    nsx_idgen_sup:start_link().

stop() ->
    stop([]).

stop(_State) ->
    nsx_idgen_sup:stop().

%% ===================================================================
%% General tests
%% ===================================================================
