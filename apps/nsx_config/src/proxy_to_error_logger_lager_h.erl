-module(proxy_to_error_logger_lager_h).
-behaviour(gen_alog).

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
-spec log(integer(), string()) -> ok.
log(_ALoggerPrio, Msg) ->
   % io:format(">>> ~p\n", [Msg]).
   error_logger_lager_h:handle_event(Msg, state).

%% @private
-spec format(string(), [term()], integer(), list(),
             atom(), integer(), pid(),
             {non_neg_integer(), non_neg_integer(), non_neg_integer()})
            -> iolist().
format(Message, _Args, _Level, _Tag, _Module, _Line, _Pid, _TimeStamp) ->
    Message.
