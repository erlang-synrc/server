%%%-------------------------------------------------------------------
%%% File    : nsm_gifts_app.erl
%%% Author  : Serge Polkovnikov <serge.polkovnikov@gmail.com>
%%% Description :
%%%
%%% Created : 24 Oct 2011 by Serge Polkovnikov <serge.polkovnikov@gmail.com>
%%%-------------------------------------------------------------------
-module(nsm_gifts_app).

-behaviour(application).
-export([start/2,stop/1]).

%% @spec start(_Type, _StartArgs) -> ServerRet
start(_Type, _StartArgs) ->
    nsm_gifts_sup:start_link().

%% @spec stop(_State) -> ServerRet
stop(_State) ->
    ok.

