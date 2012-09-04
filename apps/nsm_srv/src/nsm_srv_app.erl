
-module(nsm_srv_app).
-behaviour(application).
-include("tournaments.hrl").
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
%    erlfb:start(),
    application:start(nsx_utils),
    zealot_db:start(),
    zealot_db:initialize(),
    A = nsm_srv_sup:start_link(),
%    [ nsm_srv_tournament_lobby_sup:start_lobby(Tour#tournament.id) || Tour <- tournaments:all() ],
    A.

stop(_State) ->
    ok.
