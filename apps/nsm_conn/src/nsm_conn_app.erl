-module(nsm_conn_app).
-behaviour(application).

-export([start/0, start/2, stop/0, stop/1]).
-export([start_client/0, get_clients/0]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("nsg_srv/include/conf.hrl").

start() -> application:start(conn).
start(_StartType, _StartArgs) -> nsm_conn_sup:start_link().
stop() -> stop([]).
stop(_State) -> nsm_conn_sup:stop().
start_client() -> supervisor:start_child(conn_worker_sup, []).
get_clients() -> supervisor:which_children(conn_worker_sup).
