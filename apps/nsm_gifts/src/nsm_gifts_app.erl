-module(nsm_gifts_app).
-copyright("Paynet Internet ve Bilisim Hizmetleri A.S. All Rights Reserved.").
-author('Serge Polkovnikov <serge.polkovnikov@gmail.com>').

-behaviour(application).
-export([start/2,stop/1]).

%% @spec start(_Type, _StartArgs) -> ServerRet
start(_Type, _StartArgs) ->
    nsm_gifts_sup:start_link().

%% @spec stop(_State) -> ServerRet
stop(_State) ->
    ok.

