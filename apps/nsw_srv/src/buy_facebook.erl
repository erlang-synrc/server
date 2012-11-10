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
    {ok, Data} = fb_signed_request:parse(wf:q(signed_request), ?FB_APP_SECRET), %?FB_APP_SECRET "df0ed1f649bf974189947caf832ffa01"
    SignedReq = mochijson2:decode(Data),
    wf:info("signed_request:~p~n",[SignedReq]),
    Response = #struct{list=[
        {<<"content">>, process_order(wf:q(method), proplists:get_value(<<"credits">>, SignedReq#struct.list))},
        {<<"method">>, list_to_binary(wf:q(method))}]},
    wf:info("Response: ~p~n", [Response]),
    mochijson2:encode(Response).

process_order("payments_get_items", Credits)->
    Packages = nsm_membership_packages:list_packages([{payment_type, credit_card},{available_for_sale, true}]),
    OrderInfo = mochijson2:decode(proplists:get_value(<<"order_info">>, Credits#struct.list)),
    ItemId = binary_to_list(proplists:get_value(<<"item_id">>, OrderInfo#struct.list)),
    [#struct{list=[
	{<<"title">>, list_to_binary(["Packet No ", integer_to_list(No)])},
	{<<"price">>, Amount},
	{<<"image_url">>, <<"http:\/\/www.facebook.com\/images\/gifts\/21.png">>},
	{<<"description">>, list_to_binary(
	    ["Gift points :", integer_to_list(GiftPoints),
	    " Net membership: ", integer_to_list(NetMembership)])}]}
	|| #membership_package{id = Id, no=No,
	    amount=Amount,
	    deducted_for_gifts=GiftPoints,
	    net_membership=NetMembership} <- Packages, Id=:=ItemId];
process_order("payments_status_update", Credits)->
    OrderId = proplists:get_value(<<"order_id">>, Credits#struct.list),
    OrderDetails = proplists:get_value(<<"order_details">>, Credits#struct.list),
    wf:info("Order details: ~p~n", [OrderDetails]),
    #struct{list=[{<<"status">>, <<"settled">>}, {<<"order_id">>, OrderId}]}.

event(_)-> ok.
