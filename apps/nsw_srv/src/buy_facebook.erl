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
    {ok, Data} = fb_signed_request:parse(wf:q(signed_request), ?FB_APP_SECRET), % "df0ed1f649bf974189947caf832ffa01"
    SignedReq = mochijson2:decode(Data),
    %wf:info("signed_request:~p~n",[SignedReq]),
    Response = #struct{list=[
	{<<"content">>, process_order(wf:q(method), proplists:get_value(<<"credits">>, SignedReq#struct.list))},
	{<<"method">>, list_to_binary(wf:q(method))}]},
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
	    " Net membership: ", integer_to_list(NetMembership)])},
	{<<"item_id">>, list_to_binary(ItemId)}]}
	|| #membership_package{id = Id, no=No,
	    amount=Amount,
	    deducted_for_gifts=GiftPoints,
	    net_membership=NetMembership} <- Packages, Id=:=ItemId];
process_order("payments_status_update", Credits)->
    OrderDetails = mochijson2:decode(proplists:get_value(<<"order_details">>, Credits#struct.list)),
    Response = order_status(proplists:get_value(<<"status">>, OrderDetails#struct.list), OrderDetails#struct.list),
    Response.

order_status(<<"placed">>, OrderDetails)->
    wf:info("order placed ~p~n", [OrderDetails]),
    OrderId = proplists:get_value(<<"order_id">>, OrderDetails),
    Items = proplists:get_value(<<"items">>, OrderDetails),
    Item = lists:nth(1, Items),
    PackageId = proplists:get_value(<<"item_id">>, Item#struct.list),
    wf:info("Process package id ~p~n ", [PackageId]),

    % nsm_membership_packages:check_limit_over(UId, PackageId)
    % buy:over_limit_popup(nsm_membership_packages:get_monthly_purchase_limit());

    NextStatus = <<"settled">>, % <<"canceled">>
    #struct{list=[{<<"status">>, NextStatus}, {<<"order_id">>, OrderId}]};
order_status(<<"disputed">>, OrderDetails)->
    wf:info("order disputed: ~p~n", [OrderDetails]),
    % Track disputed item orders.
    % Investigate user's dispute and resolve by settling or refunding the order.
    % Update the order status asychronously using Graph API.
    [];
order_status(<<"refunded">>, OrderDetails)->
    wf:info("order refunded ~p~n", [OrderDetails]),
    % Track refunded item orders initiated by Facebook. No need to respond.
    [];
order_status(<<"settled">>, OrderDetails)->
    wf:info("order settled ~p~n", [OrderDetails]),
    OrderId = proplists:get_value(<<"order_id">>, OrderDetails),
    % Verify that the order ID corresponds to a purchase fullfilled
    #struct{list=[{<<"status">>, <<"settled">>}, {<<"order_id">>, OrderId}]};
order_status(OrderStatus, OrderDetails)->
    wf:info("buy_facebook: Order satus ~p, details: ~p~n", [OrderStatus, OrderDetails]),
    [].

event(_)-> ok.
