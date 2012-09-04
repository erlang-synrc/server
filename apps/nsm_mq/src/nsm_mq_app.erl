%%----------------------------------------------------------------------
%% @author Vladimir Baranov <baranoff.vladimir@gmail.com>
%% @copyright Paynet Internet ve Bilisim Hizmetleri A.S. All Rights Reserved.
%% @doc Application callback module
%% @end
%%--------------------------------------------------------------------
-module(nsm_mq_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    application:start(nsm_mq).

start(_StartType, _StartArgs) ->
    nsm_mq_sup:start_link().

stop(_State) ->
    ok.
