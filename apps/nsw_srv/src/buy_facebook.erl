-module(buy_facebook).
-author('Maxim Sokhatsky <maxim@synrc.com>').
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").

main() ->
    wf:info("Facebook Payment Callback module. signed_request: ~p~n", [wf:q(signed_request)]),
    [].