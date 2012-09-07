%% Author: serge
%% Created: Sep 7, 2012
%% Description: TODO: Add description to nsm_gifts_tools
-module(nsm_gifts_tools).

%%
%% Include files
%%

-include("common.hrl").

%%
%% Exported Functions
%%
-export([
         show/1,
         dumb_store/1
        ]).

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


%% @spec dumb_store(List) -> ok
%% @doc
%% Types:
%%     List = list(#ext_product_info{})
%% Stores products information to the DB. For demo only.
%% FIXME: The procedure should be processed by admin using some UI.
%% @end

dumb_store(List) ->
    CurDateTime = calendar:now_to_datetime(now()),
    List2 =
        [#gift{
               vendor_id = VendorId,
               category_id = 1,
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
               in_stock = Active,
               enabled_on_site = true,
               kakush_point = 1,
               kakush_currency = undefined,     % FIXME: WTF? admin
               creation_date = CurDateTime,
               modify_date = CurDateTime
              } ||
              #ext_product_info{
                                vendor_id = VendorId,
                                id = ExtId,
                                active = Active,
                                name = ExtName,
%                                category = ExtCategory,
%                                category_name = ExtCategoryName,
                                short_descr = ExtShortDescr,
                                long_descr = ExtLongDescr,
                                small_image_url = ExtSmallImgURL,
                                big_image_url = ExtBigImgURL,
                                retailer_price = RetailerPrice,
                                user_price = UserPrice
                               } <- List],
    [ok = nsm_gifts_db:create_gift(GiftRec) || GiftRec <- List2],
    ok.


%%
%% Local Functions
%%

