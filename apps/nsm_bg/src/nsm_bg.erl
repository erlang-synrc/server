-module(nsm_bg).
-behaviour(gen_server).
-include_lib("nsx_config/include/log.hrl").
-export([start_link/0, init_workers/0]).
-export([start_feed_worker/1, start_feed_worker/2,
         start_all_feed_workers/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-compile(export_all).
-include("nsm_bg.hrl").
-define(BOOTSTRAP_WORKER_NAME, nsm_launcher).
-record(state, {}).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
start_all_feed_workers() -> gen_server:call(?MODULE, start_all_feed_workers).

start_feed_worker(Owner) ->
    case nsm_bg_workers_sup:start_worker(nsm_writer, [{owner, Owner}]) of
         {ok, Pid} -> %?INFO(" Consumer process started with pid: ~p for ~p", [Pid, Owner]), 
                      ok;
         Error -> %?ERROR(" Feed consumer process starting error. Owner: ~p, Reason: ~p", [Owner, Error]), 
                  Error
    end.

start_feed_worker(Owner, FeedId) ->
    case nsm_bg_workers_sup:start_worker(nsm_writer, [{owner, Owner}, {feed_id, FeedId}]) of
         {ok, Pid} -> %?INFO(" Consumer process started with pid: ~p for ~p", [Pid, Owner]), 
                      ok;
         Error -> %?ERROR(" Feed consumer process starting error. Owner: ~p, Reason: ~p", [Owner, Error]), 
                  Error
    end.

init_workers() ->
    try
       {ok, Channel} = nsm_mq:open([]),
       nsm_mq_channel:create_exchange(Channel, ?DEAD_LETTER_EXCHANGE,
                                   [{type, <<"fanout">>}, durable, {auto_delete, false}]),
        case nsx_opt:get_env(nsm_bg,start_email,false) of
             false -> skip;
             true ->  nsm_bg_workers_sup:start_worker(nsm_mailer, [])
        end,
        {ok, BPid} = nsm_bg_workers_sup:start_worker(nsm_launcher, [{name, ?BOOTSTRAP_WORKER_NAME}]),
        {ok, _} = nsm_bg_workers_sup:start_worker(nsm_reanimator, []),
        nsm_launcher:start_all_feed_workers(BPid),
        {ok, #state{}}
    catch
        _:E -> ?ERROR("nsm_bg init error: ~p", [E]), {stop, {init_error, E}}
    end.

init([]) -> init_workers(), {ok,#state{}}.

handle_call(start_all_feed_workers, _From, State) ->
     Reply = nsm_launcher:start_all_feed_workers(?BOOTSTRAP_WORKER_NAME),
    {reply, Reply, State}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
