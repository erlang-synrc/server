-module(nsw_srv_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    io:format("Staring Kakaranet Web Application..\n"),
    case nsw_srv_sup:start_link() of
                 {ok, Pid} -> io:format("Web Started OK\n."), {ok, Pid};
         {error, shutdown} -> {ok, Port} = application:get_env(webmachine, port),
                              io:format("Nnitrogen_sup can't start. Tried port ~p\n", [Port]),
                              erlang:halt(1);
                         X -> io:format("Error ~p",[X]), erlang:halt(1)
    end.

stop(_State) ->
    ok.
