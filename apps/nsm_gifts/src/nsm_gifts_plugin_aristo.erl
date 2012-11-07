-module(nsm_gifts_plugin_aristo).
-copyright("Paynet Internet ve Bilisim Hizmetleri A.S. All Rights Reserved.").
-author('Maxim Sokhatsky <maxim@synrc.com>').

-include_lib("nsx_config/include/log.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-include("common.hrl").
-include("vendors.hrl").

-export([get_gifts/0]).

-define(PRODUCTS_FILE, "/home/kakauser/tmp/kaka/aristo").
-record(product_aristo, {no, name, brand, model, price, currency, image, tl_price, count, cargo, fix, 
                         logistic_service_rate, tl_lsr, total, desc}).

get_gifts() ->
    Res = fetch_data_files(?PRODUCTS_FILE),
    case Res of
        {error, Reason} -> {error, {fetching, Reason}};
        {ok, {ProductsData}} -> List = process_data(ProductsData), {ok, List}
%                                catch _:Exception -> {error, {parsing, Exception}} end
    end.

fetch_data_files(Products) ->
    case fetch_files([Products]) of
        {ok, Data} -> {ok, list_to_tuple(Data)};
        {error, Reason} -> {error, Reason}
    end.

fetch_files(List) -> fetch_files(List, []).
fetch_files([], Acc) -> {ok, lists:reverse(Acc)};
fetch_files([Filename | Rest], Acc) ->
    case fetch_file(Filename) of
        {ok, Body} -> fetch_files(Rest, [Body|Acc]);
        {error, Reason} -> {error, {Filename, Reason}}
    end.

fetch_file(Filename) ->
    case file:read_file(Filename) of
        {ok, Bin} -> {ok, binary_to_list(Bin)};
        {error, Reason} -> {error, Reason}
    end.

process_data(ProductsData) when is_list(ProductsData) ->
    {P_XML, _}=process_data_block(ProductsData),
    error_logger:info_msg("~w:process_data/3 Parsing finished Ok", [?MODULE]),
    process(P_XML).

process_data_block(Data) ->
    Acc = fun(#xmlText{value = " ", pos = P}, Acc, S) -> {Acc, P, S};
                                          (X, Acc, S) -> {[X|Acc], S}
          end,
    xmerl_scan:string(Data, [{space,normalize}, {acc_fun, Acc}]).

process(P_XML) ->
    Products = process_products(P_XML),
    [#ext_product_info{vendor_id = ?VENDOR_ARISTO,
                       id = ProductId,
                       active = true,
                       name = Name,
                       category = Category,
                       category_name = CategoryName,
                       short_descr = Desc,
                       long_descr = Category ++ " " ++ CategoryName,
                       small_image_url = Image,
                       big_image_url = Image,
                       in_stock = true,
                       retailer_price = RetailerPrice,
                       user_price = UserPrice
                      } || #product_aristo{no = ProductId, name = Name, brand = Category, model = CategoryName, 
                                    price = RetailerPrice, currency = Currency, tl_price = TL_Price, 
                                    count = Count, cargo = Cargo, fix = Fix, image = Image,
                                    logistic_service_rate = LSR, tl_lsr = TR_LSR, total = UserPrice, desc = Desc} <- Products].

process_products(XML = #xmlElement{name='TABLE'}) ->
    List=xmerl_xs:select("TR", XML),
    [#product_aristo{} || E <- List].

str_price_to_int(String) -> round(list_to_float_smart(String)*100).

list_to_float_smart(String) ->
    try list_to_float(String)
    catch
        _:_ ->
            list_to_integer(String)*1.0
    end.

is_production() ->
    case nsm_db:get(config, "debug/production", false) of
        {ok, true} -> true;
        _ -> false
    end.
