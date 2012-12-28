-module(nsw_srv_app).
-behaviour(application).
-include_lib("nsx_config/include/log.hrl").
-include_lib("nsm_db/include/config.hrl").
-include_lib("nsm_db/include/tournaments.hrl").
-include("setup.hrl").
-export([start/2,stop/1]).
-compile(export_all).

start(_StartType, _StartArgs) ->
    io:format("Staring Kakaranet Web Application..\n"),
    case nsw_srv_sup:start_link() of
                 {ok, Pid} ->

    Pool = nsx_opt:get_env(nsx_idgen,game_pool,1000000),
    ?INFO("Game Pool: ~p", [Pool]),

                              io:format("Web Started OK\n."), {ok, Pid};
         {error, shutdown} -> {ok, Port} = application:get_env(webmachine, port),
                              io:format("Nnitrogen_sup can't start. Tried port ~p\n", [Port]),
                              erlang:halt(1);
                         X -> io:format("Error ~p",[X]), erlang:halt(1)
    end.



stop(_State) ->
    ok.
