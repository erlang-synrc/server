%%----------------------------------------------------------------------
%% @author Vladimir Baranov <baranoff.vladimir@gmail.com>
%% @copyright Paynet Internet ve Bilisim Hizmetleri A.S. All Rights Reserved.
%% @doc
%%   Worker for processing of dead messages
%% @end
%%-------------------------------------------------------------------

-module(nsm_bg_worker_reanimator).

-behaviour(nsm_bg_gen_worker).

%%
%% Include files
%%

-include_lib("nsx_config/include/log.hrl").
-include("nsm_bg.hrl").


%% gen_worker callbacks
-export([init/1, handle_notice/3, get_opts/1, handle_info/2]).

-record(state, {}).

init(_) ->
    ?INFO("start reanimator", []),
    {ok, #state{}}.

handle_notice(Route, Message, State) ->
    ?INFO("reanimator(~p): notification received: ~p, Message: ~p",
          [self(), Route, Message]),
    {noreply, State}.

handle_info(Info, State) ->
    ?INFO("reanimator(~p): handle info: ~p",
          [self(), Info]),
    {noreply, State}.

get_opts(_State) ->
    [{routes, [""]},
     {queue, ?REANIMATOR_QUEUE_NAME(node())},
     {exchange, ?DEAD_LETTER_EXCHANGE},
     {queue_options, [{auto_delete, false}]}].

%%
%% Local Functions
%%
