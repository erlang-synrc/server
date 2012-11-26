%%----------------------------------------------------------------------
%% @author Serge Polkovnikov <serge.polkovnikov@gmail.com>
%% @copyright Paynet Internet ve Bilisim Hizmetleri A.S. All Rights Reserved.
%% @doc
%% @end
%% Created: Sep 7, 2012
%%----------------------------------------------------------------------
-module(nsm_gifts_tools).
-compile(export_all).
-include("common.hrl").
-include_lib("nsx_config/include/log.hrl").
-define(FIXED_CURRENCY_AMOUNT, 5).
-define(HACKED_PRICE, 5.2).

%%
%% API Functions
%%

%%====================================================================
%% Function: show(List) -> void()
%% Types:
%%     List = [#ext_product_info{}]
%%====================================================================

show(List) ->
    [io:fwrite("----------~n~n"
               "Id            : ~w~n~n"
               "Active        : ~w~n~n"
               "Name          : ~9999999p~n~n"
               "Category      : ~w~n~n"
               "CategoryName  : ~9999999p~n~n"
               "Short descr   : ~9999999p~n~n"
               "Long descr    : ~9999999p~n~n"
               "Image URL     : ~9999999p~n~n"
               "Retailer Price: ~9999999p~n~n"
               "User Price    : ~9999999p~n~n"
               ,
               [
                Id,
                Active,
                Name,
                Category,
                CategoryName,
                ShortDescr,
                LongDescr,
                ImgURL,
                RetailerPrice,
                UserPrice
               ]
              ) ||
     #ext_product_info
     {
      id=Id,
      active=Active,
      name=Name,
      category=Category,
      category_name=CategoryName,
      short_descr=ShortDescr,
      long_descr=LongDescr,
      small_image_url=ImgURL,
      retailer_price=RetailerPrice,
      user_price=UserPrice
     } <- List],
    ok.


%% @spec dumb_store(List, CurDateTime, A, B, C, D) -> ok
%% @doc
%% Types:
%%     List = list(#ext_product_info{})
%% Stores products information to the DB. For demo only.
%% FIXME: The procedure should be processed by admin using some UI.
%% @end

dumb_store(List, CurDateTime, A, B, C, D) ->
    List2 =
        [begin
             ?INFO("User Price: ~p",[{UserPrice,{VendorId, ExtId}}]),
             {OurPrice, KakushCurrency, KakushPoints} = calc_prices(UserPrice, A, B, C, D),
             #gift{id = {VendorId, ExtId},
                   vendor_id = VendorId,
                   categories = [1],
                   ext_gift_id = ExtId,
                   gift_name = ExtName,
                   ext_gift_name = ExtName,
                   description_short =  ExtShortDescr,
                   description_long = ExtLongDescr,
                   image_small_url = ExtSmallImgURL,
                   image_big_url = ExtBigImgURL,
                   publish_start_date = CurDateTime,
                   publish_end_date = {{2999, 12, 31}, {23, 59, 59}},
                   real_price = UserPrice,
                   retailer_price = RetailerPrice,
                   our_price = OurPrice,
                   in_stock = InStock,
                   enabled_on_site = true,
                   kakush_point = KakushPoints,
                   kakush_currency = KakushCurrency,
                   creation_date = CurDateTime,
                   modify_date = CurDateTime
                  }
         end ||
         #ext_product_info{
                           vendor_id = VendorId,
                           id = ExtId,
                           %                                active = Active,
                           name = ExtName,
                           %                                category = ExtCategory,
                           %                                category_name = ExtCategoryName,
                           short_descr = ExtShortDescr,
                           long_descr = ExtLongDescr,
                           small_image_url = ExtSmallImgURL,
                           big_image_url = ExtBigImgURL,
                           retailer_price = RetailerPrice,
                           user_price = UserPrice,
                           in_stock = InStock
                          } <- List],
    [ok = nsm_gifts_db:create_gift(GiftRec) || GiftRec <- List2],
    ok.

import(VendorId) ->
    CurDateTime = calendar:now_to_datetime(now()),
    {A, B, C, D} = nsm_gifts_db:get_factors(),
    nsm_gifts_db:clear_gifts(),
    {ok, VendGifts} = nsm_gifts_vendor:get_gifts(VendorId),
    dumb_store(VendGifts, CurDateTime, A, B, C, D).

import_all() ->
    nsm_gifts_db:clear_gifts(),
    CurDateTime = calendar:now_to_datetime(now()),
    {A, B, C, D} = nsm_gifts_db:get_factors(),
    [begin
         {ok, VendGifts} = nsm_gifts_vendor:get_gifts(VendorId),
         dumb_store(VendGifts, CurDateTime, A, B, C, D)
     end || VendorId <- [1,2]].
%%
%% Local Functions
%%

%% calc_prices(UserPrice, A, B, C, D) -> {OurPrice, KakushCurrency, KakushPoints}
calc_prices(UserPrice, A, B, C, D) ->
    OurPrice = round(UserPrice * A),
    KakushCurrencyPre = round(OurPrice * D / 100),
    if KakushCurrencyPre < ?FIXED_CURRENCY_AMOUNT ->
        {OurPrice, ?FIXED_CURRENCY_AMOUNT, round(B * C * (?HACKED_PRICE - ?FIXED_CURRENCY_AMOUNT))};
        true -> {OurPrice, KakushCurrencyPre, round(B * C * (OurPrice / 100 - KakushCurrencyPre))}
    end.

