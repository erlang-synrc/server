%%----------------------------------------------------------------------
%% @author Vladimir Baranov <baranoff.vladimir@gmail.com>
%% @copyright Paynet Internet ve Bilisim Hizmetleri A.S. All Rights Reserved.
%% @doc
%%    Backgground workers main module
%% @end
%%--------------------------------------------------------------------
-module(nsm_bg).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("alog/include/alog.hrl").
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start_link/0]).

-export([start_feed_worker/1,
         start_all_feed_workers/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("nsm_bg.hrl").
-define(BOOTSTRAP_WORKER_NAME, nsm_bg_worker_bootstrap).

-record(state, {}).

%% ====================================================================
%% External functions
%% ====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Starts db worker for user or group. This worker will write
%%      needed messages to db. Owner - user or group id.
start_feed_worker(Owner) ->
    case nsm_bg_workers_sup:start_worker(nsm_bg_worker_feed, [{owner, Owner}]) of
	{ok, _} ->
	    ok;
	Error ->
	    ?ERROR("feed worker starting error. Owner: ~p, Reason: ~p",
		   [Owner, Error]),
	    Error
    end.

start_all_feed_workers() ->
    gen_server:call(?MODULE, start_all_feed_workers).

%% ====================================================================
%% Server functions
%% ====================================================================

init([]) ->
    try
        ok = init_mq(),
        %% FIXME: move workers start to another place?
        {ok, _} = nsm_bg_workers_sup:start_worker(nsm_bg_worker_email, []),
        %% bootstrap worker, satarts another workers
        {ok, BPid} = nsm_bg_workers_sup:start_worker(nsm_bg_worker_bootstrap,
                                                     [{name, ?BOOTSTRAP_WORKER_NAME}]),
        %% reanimator for dead messages processing
        {ok, _} = nsm_bg_workers_sup:start_worker(nsm_bg_worker_reanimator, []),

        %% try to start all workers on this node
        nsm_bg_worker_bootstrap:start_all_feed_workers(BPid),

        {ok, #state{}}
    catch
        _:E ->
            ?ERROR("nsm_bg init error: ~p", [E]),
            {stop, {init_error, E}}
    end.

handle_call(start_all_feed_workers, _From, State) ->
    Reply = nsm_bg_worker_bootstrap:start_all_feed_workers(
              ?BOOTSTRAP_WORKER_NAME),
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

%% create needed message queues parts
init_mq() ->
    {ok, Channel} = nsm_mq:open([]),
    nsm_mq_channel:create_exchange(Channel, ?DEAD_LETTER_EXCHANGE,
                                   [{type, <<"fanout">>}, durable,
                                    {auto_delete, false}]),
    ok.
