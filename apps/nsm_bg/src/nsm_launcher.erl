-module(nsm_launcher).
-author('Vladimir Baranov <baranoff.vladimir@gmail.com>').
-behaviour(nsm_consumer).
-include_lib("nsx_config/include/log.hrl").
-include_lib("nsm_db/include/user.hrl").
-include("nsm_bg.hrl").
-export([init/1, handle_notice/3, handle_info/2, get_opts/1, start_worker/4, start_link/2]).
-record(state, {name,type}).

start_link(Mod,Args) -> gen_server:start_link(Mod, Args, []).

init(Params) -> 
    Name = proplists:get_value(name,Params),
    Type = proplists:get_value(type,Params),
    {ok, #state{name = Name,type=Type}}.

handle_notice(["user", "init"], Message, State) ->
    start_worker(user, Message, undefined, undefined),
    {noreply, State};

handle_notice(Route, Message, State) ->
    ?INFO("Launcher notification received: ", [self(), Route, Message]),
    {noreply, State}.

start_worker(Type, Name, Feed, Direct) ->
    nsm_bg_workers_sup:start_worker(nsm_writer, [{name,Name},{feed,Feed},{type,Type},{direct,Direct}]).

handle_info(start_all, State) ->
    ?INFO("Starting workers..."),

    CheckNode = fun(X) -> lists:foldl(fun(A, Sum) -> A + Sum end, 0, X) rem 3 + 1 end,
    RunGroups = fun(Groups) -> [begin start_worker(group,Name,Feed,undefined) end 
                                || #group{username=Name,feed=Feed} <- Groups] end,
    RunSystem = fun() -> start_worker(system,"system",-1,undefined) end,
    Node = nsx_opt:get_env(nsm_bg,pool,5),
    Users = case Node of
                 4 -> [User || User<-nsm_db:all(user), User#user.email /= undefined, User#user.status == ok];
                 5 -> [User || User<-nsm_db:all(user), User#user.email /= undefined, User#user.status == ok];
                 X -> [R||R=#user{username=U,status=ok,email=E}<-nsm_db:all(user), CheckNode(U)==X,E/=undefined]
    end,
    ?INFO("Users Count on Node ~p: ~p",[Node,length(Users)]),
    Groups = [Group || Group=#group{username=G} <-nsm_db:all(group), CheckNode(G)==Node ],
    case Node of
         1 -> RunGroups(Groups);
         2 -> RunGroups(Groups);
         3 -> RunGroups(Groups);
         4 -> RunGroups(Groups);
         5 -> RunGroups(Groups);
         _ -> skip end,
    [begin start_worker(user,Name,Feed,Direct) end || #user{username=Name,feed=Feed,direct=Direct} <- Users],
    case Node of
         1 -> RunSystem();
         4 -> RunSystem();
         5 -> RunSystem();
         _ -> skip end,
    garbage_collect(self()),
    {noreply, State};

handle_info(_Other, State) -> {noreply, State}.

get_opts(_State) ->
    [{routes, [ [user, init], [group, init], [system, init] ]},
     {grpoc_name, [bootstrap, worker, node(), utils:uuid_ex()]},
     {queue, ?BOOTSTRAP_WORKER_QUEUE},
     {queue_options, [{auto_delete, false}, durable]}].
