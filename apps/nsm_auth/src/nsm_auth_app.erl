-module(nsm_auth_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/0, stop/1]).

-include_lib("eunit/include/eunit.hrl").

-include_lib("nsg_srv/include/conf.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    application:start(nsm_auth).

start(_StartType, _StartArgs) ->
    nsm_auth_sup:start_link().

stop() ->
    stop([]).

stop(_State) ->
    nsm_auth_sup:stop().

%% ===================================================================
%% General tests
%% ===================================================================
