-module(nsw_srv_app).
-behaviour(application).
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    case nsw_srv_sup:start_link() of
	{ok, Pid} -> {ok, Pid};
	{error, shutdown} ->
	   {ok, Port} = application:get_env(webmachine, port),
	   io:format("\nnitrogen_sup can't start. Usually it means that port ~p is used by other application\n", [Port]),
	   timer:sleep (100),
	   erlang:halt(1)
    end.

stop(_State) ->
    ok.
