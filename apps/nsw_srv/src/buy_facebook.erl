-module(buy_facebook).
-author('Maxim Sokhatsky <maxim@synrc.com>').
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("setup.hrl").

main() ->
    wf:info("Facebook Payment Callback module."),
    case wf:q(method) of 
	payments_get_items -> 
	    {ok, Data} = fb_signed_request:parse(wf:q(signed_request), "df0ed1f649bf974189947caf832ffa01"), %?FB_APP_SECRET
	    SignedReqRec = mochijson2:decode(Data),
	    wf:info("Method:payments_get_item~n signed_request:~p~n",[SignedReqRec]),
	    [];
	payments_status_update ->
	    wf:info("Method: payment_status_update"),
	    []
    end.
