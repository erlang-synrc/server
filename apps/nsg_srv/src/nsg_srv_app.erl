
-module(nsg_srv_app).
-behaviour(application).
-include("setup.hrl").
%-include("basic_types.hrl").
%-include("game_tavla.hrl").
%-include("game_okey.hrl").
-include_lib("alog/include/alog.hrl").
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    crypto:start(),
    application:start(nsx_idgen),
    application:start(nsx_utils),
    application:start(nsx_config),
    application:start(nsm_auth),
    application:start(nsg_session),
    application:start(nsg_games),
    application:start(nsg_matchmaker),
    application:start(nsm_conn),
%    ?INFO("AMF TEST: ",[amf3:encode(amf3:record_to_object(#okey_reveal{discarded = #'OkeyPiece'{}}))]),
%    ?INFO("AMF TEST: ",[kamf:encode(kamf:record_to_object(#tavla_player{player_info = [#'PlayerInfo'{}]}))]),
%    ?MNESIA_INITIAL(),
%    ?SET_COUCH_ACC(),
%    ?SET_COUCH_GAMES(),
    nsg_srv_sup:start_link().

stop(_State) ->
    ok.
