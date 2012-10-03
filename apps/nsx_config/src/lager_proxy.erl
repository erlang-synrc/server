-module(lager_proxy).
-behaviour(gen_alog).

-include_lib("alog/include/alog.hrl").

%% gen_alogger callbacks
-export([start/1,
         stop/1,
         log/2,
         format/8]).

%% @private
-spec start(list()) -> ok.
start(_) ->
    ok.

%% @private
-spec stop(list()) -> ok.
stop(_) ->
    ok.

%% @private
-spec log(integer(), tuple()) -> ok.
log(ALoggerPrio, {FormatString, Args, _Level, Tag, Module, Line, Pid, _TimeStamp}) ->
   Level = map_prio(ALoggerPrio),
   Format = FormatString,
   Function = '',
   Time = lager_util:localtime_ms(),
   case Tag of
	'$error_logger' -> ignore;
	_ ->
	    lager:log(Level, Module, Function, Line, Pid, Time, Format, Args, 100000)
   end,
   ok.

%% @private
-spec format(string(), [term()], integer(), list(),
             atom(), integer(), pid(),
             {non_neg_integer(), non_neg_integer(), non_neg_integer()})
            -> tuple().
format(FormatString, Args, Level, Tag, Module, Line, Pid, TimeStamp) ->
    {FormatString, Args, Level, Tag, Module, Line, Pid, TimeStamp}.

-spec map_prio(integer()) -> atom().
map_prio(?emergency) -> emergency;
map_prio(?alert)     -> alert;
map_prio(?critical)  -> critical;
map_prio(?error)     -> error;
map_prio(?warning)   -> warning;
map_prio(?notice)    -> notice;
map_prio(?info)      -> info;
map_prio(?debug)     -> debug.
