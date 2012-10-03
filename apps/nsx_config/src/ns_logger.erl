-module (ns_logger).

-include("config.hrl").

-export([
    start_link/0,
    reset_loggers/1,
    reset_loggers/0
]).

start_link() ->
    application:start(sasl),
    ok = lager:start(), %% console, file, error_logger handler
    ok = alog:start(),
    reset_loggers(),
    {ok, whereis(alog_sup)}.


reset_loggers() ->
    case ?PRODUCTION of
	true ->  reset_loggers(error);
	false -> reset_loggers(info)
    end.

reset_loggers(DebugLevel) ->
    alog_control:delete_all_flows(),
    alog_control:add_new_flow({tag, [flash]}, {'=<', debug}, [alog_tty]),
    % alog_control:add_new_flow({mod, ['_']}, {'=<', error}, [alog_tty]),

    use_lager_as_handler_to_error_logger(),
    alog_control:add_new_flow({mod, ['$error_logger']}, {'=<', debug}, [alog_disk_log]),
    alog_control:add_new_flow({mod, ['_']}, {'=<', warning}, [alog_disk_log]),

    alog_control:add_new_flow({mod, ['_']}, {'=<', DebugLevel}, [lager_proxy]),
    lager:set_loglevel(lager_console_backend, DebugLevel),

    %% You can add your flows to debug like
    % alog_control:add_new_flow({mod, [mymodule]}, {'=<', debug}, [alog_tty]),
    % and run > alogger:reset_loggers().

    ok.

use_lager_as_handler_to_error_logger() ->
    [begin error_logger:delete_report_handler(X), X end ||
                X <- gen_event:which_handlers(error_logger)], % including error_logger_lager_h
    alog_error_logger_handler:install(),
    alog_control:add_new_flow({tag, ['$error_logger']}, {'=<', debug}, [proxy_to_error_logger_lager_h]),
    ok.
