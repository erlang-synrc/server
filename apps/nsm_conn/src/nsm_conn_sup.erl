%%%-------------------------------------------------------------------
%%% @author Paul Peregud <pawel@saturn.lan>
%%% @copyright (C) 2011, Paul Peregud
%%% @doc
%%%
%%% @end
%%% Created : 10 Jan 2011 by Paul Peregud <pawel@saturn.lan>
%%%-------------------------------------------------------------------
-module(nsm_conn_sup).
-behaviour(supervisor).

-export([start_link/0, stop/0]).
-export([init/1]).

-include_lib("nsg_srv/include/conf.hrl").

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

stop() ->
    exit(?SERVER, shutdown).

get_free_port() ->
    ?LISTEN_PORT.

init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,

    WorkerSup = {conn_worker_sup, {conn_worker_sup, start_link, []},
             Restart, Shutdown, worker, [conn_worker_sup]},

    Listener = {conn_listener, {conn_listener, start_link, [get_free_port()]},
                Restart, Shutdown, worker, [conn_listener]},


    {ok, {SupFlags, [WorkerSup, Listener]}}.

is_port_busy(Port) ->
    case gen_tcp:connect("localhost", Port, []) of
        {ok, Socket} ->
            gen_tcp:close(Socket),
            true;
        _ ->
            false
    end.

get_free_port(Port) ->
    case is_port_busy(Port) of
        false ->
            Port;
        true ->
            get_free_port(Port+1)
    end.
