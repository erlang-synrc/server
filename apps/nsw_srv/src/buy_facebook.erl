-module(buy_facebook).
-author('Maxim Sokhatsky <maxim@synrc.com>').
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("nsm_db/include/membership_packages.hrl").
-include("setup.hrl").
-include("common.hrl").
-record(struct, {list=[]}).

main() ->
    wf:content_type("application/json"),
    {ok, Data} = fb_signed_request:parse(wf:q(signed_request), "df0ed1f649bf974189947caf832ffa01"), %?FB_APP_SECRET
    SignedReq = mochijson2:decode(Data),
    wf:info("signed_request:~p~n",[SignedReq]),
    Response = #struct{list=[
        {<<"content">>, process_order(wf:q(method), proplists:get_value(<<"credits">>, SignedReq#struct.list))},
        {<<"method">>, list_to_binary(wf:q(method))}]},
    wf:info("Response: ~p~n", [Response]),
    mochijson2:encode(Response).

process_order("payments_get_items", Credits)->
    Packages = nsm_membership_packages:list_packages([{payment_type, credit_card},{available_for_sale, true}]),
    wf:info("Check order in packs: ~p~n", [Packages]),
    [#struct{list=[
        {<<"title">>, <<"title1">>},
        {<<"price">>, 1},
        {<<"image_url">>, <<"http:\/\/www.facebook.com\/images\/gifts\/21.png">>},
        {<<"description">>, <<"Description">>}]}];
process_order("payments_status_update", Credits)->
    OrderId = proplists:get_value(<<"order_id">>, Credits#struct.list),
    OrderDetails = proplists:get_value(<<"order_details">>, Credits#struct.list),
    wf:info("Order details: ~p~n", [OrderDetails]),
    #struct{list=[{<<"status">>, <<"settled">>}, {<<"order_id">>, OrderId}]}.

event(_)-> ok.
