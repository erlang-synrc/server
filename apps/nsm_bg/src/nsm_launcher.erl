-module(nsm_launcher).
-author('Vladimir Baranov <baranoff.vladimir@gmail.com>').
-behaviour(nsm_consumer).
-include_lib("nsx_config/include/log.hrl").
-include_lib("nsm_db/include/user.hrl").
-include("nsm_bg.hrl").
-export([init/1, handle_notice/3, handle_info/2, get_opts/1, start_worker/4]).
-record(state, {name}).

init([{name, Name}]) -> {ok, #state{name = Name}}.

handle_notice(["user", "init"], Message, State) ->
    start_worker(user, Message, undefined, undefined),
    {noreply, State};

handle_notice(Route, Message, State) ->
    ?INFO("Launcher notification received: ", [self(), Route, Message]),
    {noreply, State}.

start_worker(Type, Name, Feed, Direct) ->
    nsm_bg_workers_sup:start_worker(nsm_writer, [{owner,Name},{feed,Feed},{type,Type},{direct,Direct}]).

handle_info(start_all, State) ->
    ?INFO("Starting workers..."),
    Users = [User || User<-nsm_db:all(user), User#user.email /= undefined, User#user.status == ok],
    Groups = nsm_db:all(group),
    [begin start_worker(group,Name,Feed,undefined) end || #group{username=Name,feed=Feed} <- Groups],
    [begin start_worker(user,Name,Feed,Direct) end || #user{username=Name,feed=Feed,direct=Direct} <- Users],
    start_worker(system,"system",-1,undefined),
    garbage_collect(self()),
    {noreply, State};

handle_info(_Other, State) -> {noreply, State}.

get_opts(_State) ->
    [{routes, [ [user, init], [group, init], [system, init] ]},
     {grpoc_name, [bootstrap, worker, node(), utils:uuid_ex()]},
     {queue, ?BOOTSTRAP_WORKER_QUEUE},
     {queue_options, [{auto_delete, false}, durable]}].
