-module(nsm_reanimator).
-behaviour(nsm_consumer).
-include_lib("nsx_config/include/log.hrl").
-include("nsm_bg.hrl").
-export([init/1, handle_notice/3, get_opts/1, handle_info/2]).
-record(state, {}).

init(_) -> ?INFO("start reanimator", []), {ok, #state{}}.

handle_notice(Route, Message, State) ->
    ?INFO("reanimator(~p): notification received: ~p, Message: ~p", [self(), Route, Message]),
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

