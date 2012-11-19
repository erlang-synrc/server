-module(nsm_launcher).
-author('Vladimir Baranov <baranoff.vladimir@gmail.com>').
-behaviour(nsm_consumer).
-include_lib("nsx_config/include/log.hrl").
-include_lib("nsm_db/include/user.hrl").
-include("nsm_bg.hrl").
-export([init/1, handle_notice/3, handle_info/2, get_opts/1, start_workers/0,start_all_feed_workers/1]).
-record(state, {name}).

start_all_feed_workers(BootstrapWorkerPid) ->
    case catch erlang:send(BootstrapWorkerPid, start_all) of
        true -> ok;
        _ ->    {error, bootstrap_worker_not_running}
    end.

init([{name, Name}]) ->
    ?INFO("bootstrap start: ~p", [Name]),
    case catch register(Name, self()) of
        true -> {ok, #state{name = Name}};
        Error -> ?ERROR("unable to register bootstrap worker with name ~p. Reason: ~p", [Name, Error]), {stop, normal}
    end.

handle_notice(["user", "init"], User, State) ->
    ?INFO("internal_config(~p): user feed initialization message received: ~p", [self(), User]),
    start_local_if_not_started(user, User),
    {noreply, State};

handle_notice(["group", "init"], {Group, FeedId}, State) ->
    ?INFO("internal_config(~p): group feed ~p initialization message received: ~p", [self(), FeedId, Group]),
    start_local_if_not_started(user, Group, FeedId),
    {noreply, State};

handle_notice(["group", "init"], Group, State) ->
    ?INFO("internal_config(~p): group feed initialization message received: ~p", [self(), Group]),
    start_local_if_not_started(user, Group),
    {noreply, State};

handle_notice(["system", "init"], Name, State) ->
    ?INFO("internal_config(~p): system 'feed' initialization message received: ~p", [self(), Name]),
    start_local_if_not_started(system, Name),
    {noreply, State};

handle_notice(Route, Message, State) ->
    ?INFO("internal_config(~p): notification received: ", [self(), Route, Message]),
    {noreply, State}.

handle_info(start_all, State) ->
    ?INFO("start workers initialization"),
%    timer:sleep(1000),
    start_workers(),
    {noreply, State};

handle_info({gproc, unreg, _Ref, {n, g, ?FEED_WORKER_NAME(Type, Name)}}, State) ->
    ?INFO(" feed worker exited: ~p ~p, try restart", [Type, Name]),
    timer:sleep(100),
    start_local_if_not_started(Type, Name),
    {noreply, State};

handle_info(Other, State) ->
    ?WARNING("unexpected info received: ~p", [Other]),
    {noreply, State}.

get_opts(_State) ->
    [{routes, [
               [user, init],
               [group, init],
               [system, init]
              ]},
     {grpoc_name, [bootstrap, worker, node(), utils:uuid_ex()]},
     {queue, ?BOOTSTRAP_WORKER_QUEUE},
     {queue_options, [{auto_delete, false}, durable]}].

start_workers() ->
    %% FIXME: we have to add traversal methods to groups and users
    Users = nsm_db:all(user),
    Groups = nsm_db:all(group),
    [begin
         timer:sleep(15+random:uniform(20)),
         ?INFO(" start group: ~p",[G#group.username]),
         start_worker(G)
     end || G <- Groups],
    [begin
         timer:sleep(15+random:uniform(20)),
         ?INFO(" start user: ~p",[U#user.username]),
         start_worker(U)
     end || U <- Users],
    start_worker(system).

start_worker(#user{username = U}) ->  start_global_if_not_started(user, U);
start_worker(#group{username = G}) -> start_global_if_not_started(group, G);
start_worker(system) ->               start_global_if_not_started(system, "system").

start_local_if_not_started(Type, Name, FeedId) ->
    Action = fun() -> nsm_bg:start_feed_worker(Name, FeedId) end,
    start_worker(Type, Name, Action).

start_local_if_not_started(Type, Name) ->
    Action = fun() -> nsm_bg:start_feed_worker(Name) end,
    start_worker(Type, Name, Action).

start_global_if_not_started(Type, Name) ->
    Action = fun() -> nsx_msg:notify([Type, init], Name) end,
    start_worker(Type, Name, Action).

start_worker(Type, Name, Action) ->
    WorkerName = ?FEED_WORKER_NAME(Type, Name),
    Id = {n, g, WorkerName},
    case catch gproc:where(Id) of
        Pid when is_pid(Pid) ->
            ?INFO("start worker: ~p ~p, already started: ~p", [Type, Name, Pid]),
            %% if already exists, start to monitor worker
            gproc:monitor(Id);
        Other ->
            ?INFO("not registered, try to register: ~p ~p. Where return value: ~p",
                  [Type, Name, Other]),
            Action()
    end.
