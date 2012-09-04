-module(nsm_db_app).
-include_lib("alog/include/alog.hrl").
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    application:start(nsx_utils),
    application:start(nsx_config),
    timer:sleep(1000),
    ?INFO("Check Riak Connection: ~p", [riak:client_connect(node())]),
    nsm_db_sup:start_link().

stop(_State) ->
    ok.
