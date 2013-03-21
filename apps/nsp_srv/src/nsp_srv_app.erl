-module(nsp_srv_app).
-behaviour(application).
-include_lib("nsx_config/include/log.hrl").
-include_lib("nsm_db/include/config.hrl").
-include_lib("nsm_db/include/tournaments.hrl").
-include("setup.hrl").
-export([start/2,stop/1]).
-compile(export_all).

start_cowboy(HttpOpts) ->
    application:load(webmachine),
    {ok, BindAddress} = application:get_env(webmachine, bind_address),
    {ok, ParsedBindAddress} = inet_parse:address(BindAddress),
    {ok, Port} = application:get_env(webmachine, port),
    cowboy:start_listener(http, 1, cowboy_tcp_transport, [{port, Port}, {ip, ParsedBindAddress}], cowboy_http_protocol, HttpOpts),
%    cowboy:start_listener(https, 1, cowboy_ssl_transport, nsx_opt:get_env(nsp_srv, ssl, []) ++ [{ip, ParsedBindAddress}], cowboy_http_protocol, HttpOpts),
    ?INFO("Starting Cowboy Server on ~s:~p~n", [BindAddress, Port]).

start(_StartType, _StartArgs) ->
    io:format("Staring Kakaranet Web Application..\n"),
    application:start(nitrogen_core),
    application:start(cowboy),
    Dispatch = [{'_', [ {'_',nitrogen_cowboy,[]},
                  {['...'],cowboy_http_static,[{directory,{priv_dir,nsp_srv,[]},{mimetypes,mime()}}]} ] }],
    HttpOpts = [{dispatch, Dispatch}],
    start_cowboy(HttpOpts),
    case nsp_srv_sup:start_link() of
                 {ok, Pid} ->

    Pool = nsx_opt:get_env(nsx_idgen,game_pool,1000000),
    ?INFO("Game Pool: ~p", [Pool]),

%    (catch 
%    [ begin
%        ?INFO("Tournament Lobby Started ~p",[Tour#tournament.id]),
%        rpc:call(?GAMESRVR_NODE,nsm_srv_tournament_lobby_sup,start_lobby,[erlang:integer_to_list(Tour#tournament.id)])
%                end || Tour <- nsm_tournaments:all(), ((Tour#tournament.id div 1000000) * 1000000) == Pool ]),


%                spawn(nsp_srv_app,spawn_tables,[]),
                              io:format("Web Started OK\n."), {ok, Pid};
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
