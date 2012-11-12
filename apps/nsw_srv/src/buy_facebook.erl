-module(buy_facebook).
-author('Maxim Sokhatsky <maxim@synrc.com>').
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("nsm_db/include/membership_packages.hrl").
-include_lib("nsm_db/include/user.hrl").
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
    Packages = nsm_membership_packages:list_packages([{payment_type, facebook},{available_for_sale, true}]),
    Item = mochijson2:decode(proplists:get_value(<<"order_info">>, Credits#struct.list)),
    ItemId = binary_to_list(proplists:get_value(<<"item_id">>, Item#struct.list )),
    [#struct{list=[
	{<<"title">>, list_to_binary(["Packet No ", integer_to_list(No)])},
	{<<"price">>, round(Amount*nxs_otp:get_value(nsw_srv, tl_to_usd_rate, 0.5552)/0.1)},
	{<<"image_url">>, <<"http:\/\/www.facebook.com\/images\/gifts\/21.png">>},
	{<<"description">>, list_to_binary(
	    ["Gift points :", integer_to_list(GiftPoints),
	    " Net membership: ", integer_to_list(NetMembership)])},
	{<<"item_id">>, list_to_binary(ItemId)} ]}
	|| #membership_package{
	    id = Id,
	    no=No,
	    amount=Amount,
	    deducted_for_gifts=GiftPoints,
	    net_membership=NetMembership} <- Packages, Id=:=ItemId];
process_order("payments_status_update", Credits)->
    OrderDetails = mochijson2:decode(proplists:get_value(<<"order_details">>, Credits#struct.list)),
    order_status(proplists:get_value(<<"status">>, OrderDetails#struct.list), OrderDetails#struct.list).

order_status(<<"placed">>, OrderDetails)->
    wf:info("order placed ~p~n", [OrderDetails]),
    OrderId = proplists:get_value(<<"order_id">>, OrderDetails),
    Buyer = proplists:get_value(<<"buyer">>, OrderDetails),
    {ok, #user{username = User}} = nsm_db:user_by_facebook_id(Buyer),
    [Item] = proplists:get_value(<<"items">>, OrderDetails),
    PackageId = binary_to_list(proplists:get_value(<<"item_id">>, Item#struct.list)),
    {ok, Package} = nsm_membership_packages:get_package(PackageId),
    Purchase = #membership_purchase {
	id = integer_to_list(OrderId),
	user_id= User,
	membership_package = Package,
	info = facebook
    },
    wf:info("Process purchase: ~p~n ", [Purchase]),
    nsx_util_notification:notify(["purchase", "user", User, "add_purchase"], {Purchase}),
    NextStatus = <<"settled">>, % <<"canceled">>
    #struct{list=[{<<"status">>, NextStatus}, {<<"order_id">>, OrderId}]};
order_status(<<"disputed">>, _OrderDetails)->
    % Track disputed item orders.
    % Investigate user's dispute and resolve by settling or refunding the order.
    % Update the order status asychronously using Graph API.
    [];
order_status(<<"refunded">>, _OrderDetails)->
    % Track refunded item orders initiated by Facebook. No need to respond.
    [];
order_status(<<"settled">>, _OrderDetails)->
    % deprecated
    [];
order_status(_OrderStatus, _OrderDetails)->
    %wf:info("buy_facebook: Order satus ~p, details: ~p~n", [OrderStatus, OrderDetails]),
    [].

event(_)-> ok.
