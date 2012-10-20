-module(nsw_srv_app).
-behaviour(application).
-include_lib("nsx_config/include/log.hrl").
-export([start/2, stop/1]).

start_cowboy(HttpOpts) ->
    application:load(webmachine),
    {ok, BindAddress} = application:get_env(webmachine, bind_address),
    {ok, ParsedBindAddress} = inet_parse:address(BindAddress),
    {ok, Port} = application:get_env(webmachine, port),
    application:start(cowboy),
    cowboy:start_listener(http, 10, cowboy_tcp_transport, [{port, Port}, {ip, ParsedBindAddress}], cowboy_http_protocol, HttpOpts),
    cowboy:start_listener(https, 10, cowboy_ssl_transport, nsx_opt:get_env(nsw_srv, ssl, []) ++ [{ip, ParsedBindAddress}], cowboy_http_protocol, HttpOpts),
    ?INFO("Starting Cowboy Server on ~s:~p~n", [BindAddress, Port]).

start(_StartType, _StartArgs) ->
    io:format("Staring Kakaranet Web Application..\n"),
    application:start(nitrogen),
    application:start(cowboy),
    Dispatch = [{'_', [ {'_',nitrogen_cowboy,[]},
                  {['...'],cowboy_http_static,[{directory,{priv_dir,nsw_srv,[]},{mimetypes,mime()}}]} ] }],
    HttpOpts = [{max_keepalive, 50}, {dispatch, Dispatch}],
    start_cowboy(HttpOpts),
    case nsw_srv_sup:start_link() of
                 {ok, Pid} -> io:format("Web Started OK\n."), {ok, Pid};
         {error, shutdown} -> {ok, Port} = application:get_env(webmachine, port),
                              io:format("Nnitrogen_sup can't start. Tried port ~p\n", [Port]),
                              erlang:halt(1);
                         X -> io:format("Error ~p",[X]), erlang:halt(1)
    end.

mime() ->
    [
     {<<"html">>, [<<"text/html">>]},
     {<<"css">>, [<<"text/css">>]},
     {<<"png">>, [<<"image/png">>]},
     {<<"gif">>, [<<"image/gif">>]},
     {<<"jpg">>, [<<"image/jpeg">>]},
     {<<"js">>, [<<"application/javascript">>]}
    ].

stop(_State) ->
    ok.
