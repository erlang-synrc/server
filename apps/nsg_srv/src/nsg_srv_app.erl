
-module(nsg_srv_app).
-behaviour(application).
-include("setup.hrl").
-include_lib("nsx_config/include/log.hrl").
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
%    application:start(nsx_idgen),
%    application:start(nsx_utils),
%    application:start(nsx_config),
%    application:start(nsm_auth),
%    application:start(nsg_session),
%    application:start(nsg_games),
%    application:start(nsm_conn),
    nsg_srv_sup:start_link().

stop(_State) ->
    ok.
