-module(nsg_matchmaker_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/0, stop/1]).

-include_lib("eunit/include/eunit.hrl").

-include_lib("nsg_srv/include/conf.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    application:start(nsg_matchmaker).

start(_StartType, _StartArgs) ->
    nsg_matchmaker_sup:start_link().

stop() ->
    stop([]).

stop(_State) ->
    nsg_matchmaker_sup:stop().

%% ===================================================================
%% General tests
%% ===================================================================
