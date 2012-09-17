%%----------------------------------------------------------------------
%% @author Serge Polkovnikov <serge.polkovnikov@gmail.com>
%% @copyright Paynet Internet ve Bilisim Hizmetleri A.S. All Rights Reserved.
%% @doc
%% Data fetching plugin for Enilginc
%% @end
%% Created: Sep 7, 2012
%%----------------------------------------------------------------------
-module(nsm_gifts_plugin_enilginc).

%%
%% Include files
%%

-include_lib("alog/include/alog.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-include("common.hrl").
-include("vendors.hrl").

%%
%% Exported Functions
%%
-export([
         get_gifts/0
        ]).

-export([
         get_gifts_test/0
        ]).

-define(PRODUCTS_URL, "http://www.enilginc.com/xml/urunler.asp?ip=188.40.111.156").
-define(CATEGORIES_URL, "http://www.enilginc.com/xml/kategoriler.asp?ip=188.40.111.156").
-define(STOCK_URL, "http://www.enilginc.com/xml/stok.asp?ip=188.40.111.156").

-define(PRODUCTS_FILE, "/home/kakauser/tmp/kaka/urunler").
-define(CATEGORIES_FILE, "/home/kakauser/tmp/kaka/kategoriler").
-define(STOCK_FILE, "/home/kakauser/tmp/kaka/stok").

-record(product,
        {
         id,
         active,
         name,
         category,
         short_descr,
         long_descr,
         tax,
         image_url
        }).

-record(category,
        {
         id,
         active,
         name,
         parent_id
        }).


-record(stock,
        {
         id,
         in_stock,
         retailer_price,
         user_price
        }).



%%
%% API Functions
%%
%% @spec get_gifts() -> {ok, List} |
%%                      {error, {fetching, term()}} |
%%                      {error, {parsing, term()}}
%% @doc
%% Types:
%%     List = list(#ext_product_info{})
%% @end

get_gifts() ->
    case fetch_data_net(?PRODUCTS_URL, ?CATEGORIES_URL, ?STOCK_URL) of
        {ok, {ProductsData, CategoriesData, StockData}} ->
            try List =
                    process_data(ProductsData,
                                 CategoriesData,
                                 StockData),
                {ok, List}
            catch
                _:Exception ->
                    {error, {parsing, Exception}}
            end;
        {error, Reason} ->
            {error, {fetcing, Reason}}
    end.

get_gifts_test() ->
    case fetch_data_files(?PRODUCTS_FILE, ?CATEGORIES_FILE, ?STOCK_FILE) of
        {ok, {ProductsData, CategoriesData, StockData}} ->
            try List =
                    process_data(ProductsData,
                                 CategoriesData,
                                 StockData),
                {ok, List}
            catch
                _:Exception ->
                    {error, {parsing, Exception}}
            end;
        {error, Reason} ->
            {error, {fetcing, Reason}}
    end.


%%
%% Local Functions
%%

%%====================================================================
%% Function: fetch_data(ProductsURL, CategoriesURL, StockURL) ->
%%                               {ok, {Products, Categories, Stock}} |
%%                               {error, Reason}
%% Types:
%%     Products = Categories = Stock = io_list()
%%     Reason = term()
%% Description:
%%     The function fetchs data about products from the Eniginc site.
%%====================================================================

fetch_data_net(ProductsURL, CategoriesURL, StockURL) ->
    inets:start(),
    case fetch_urls([
                     ProductsURL,
                     CategoriesURL,
                     StockURL
                    ]) of
        {ok, Data} ->
            {ok, list_to_tuple(Data)};

        {error, Reason} ->
            {error, Reason}
    end.

%%====================================================================
%% Function: fetch_urls(List1) -> {ok, List2} | {error, {URL, Reason}}
%% Types:
%%     List1 = [URL]
%%       URL = string()
%%     List2 = [Body]
%%       Body = io_list()
%%     Reason = term()
%%====================================================================

fetch_urls(List) ->
    fetch_urls(List, []).

fetch_urls([], Acc) ->
    {ok, lists:reverse(Acc)};

fetch_urls([URL | Rest], Acc) ->
    case fetch_url(URL) of
        {ok, Body} ->
            fetch_urls(Rest, [Body|Acc]);

        {error, Reason} ->
            {error, {URL, Reason}}
    end.

%%====================================================================
%% Function: fetch_url(URL) -> {ok, Body} | {error, Reason}
%% Types:
%%     URL = string()
%%     Body = io_list()
%%====================================================================

fetch_url(URL) ->
    case httpc:request(URL) of
        {error, Reason} ->
            {error, Reason};

        {ok, {{_, StatusCode, _}=StatusLine, _Headers, Body}} ->
            ?INFO("Trying to fetch the URL: ~p "
                  "StatusLine=~99999p "
                  "Headers=~p "
                  "BodyLength=~w",
                  [URL, StatusLine, _Headers, length(Body)]),

            if StatusCode >= 200,
               StatusCode =< 202 ->
                   {ok, Body};

               true ->
                   {error, {http_error, StatusCode}}
            end
    end.

%%====================================================================
%% Function: fetch_data_files(ProductsFile, CategoriesFile, StockFile) ->
%%                               {ok, {Products, Categories, Stock}} |
%%                               {error, Reason}
%% Types:
%%     Products = Categories = Stock = io_list()
%%     Reason = term()
%% Description:
%%     The function fetchs data about products from the Eniginc files.
%%====================================================================

fetch_data_files(Products, Categories, Stock) ->
    case fetch_files([
                      Products,
                      Categories,
                      Stock
                     ]) of
        {ok, Data} ->
            {ok, list_to_tuple(Data)};

        {error, Reason} ->
            {error, Reason}
    end.

%%====================================================================
%% Function: fetch_files(List1) -> {ok, List2} | {error, {URL, Reason}}
%% Types:
%%     List1 = [Filename]
%%       Filename = string()
%%     List2 = [Body]
%%       Body = io_list()
%%     Reason = term()
%%====================================================================

fetch_files(List) ->
    fetch_files(List, []).

fetch_files([], Acc) ->
    {ok, lists:reverse(Acc)};

fetch_files([Filename | Rest], Acc) ->
    case fetch_file(Filename) of
        {ok, Body} ->
            fetch_files(Rest, [Body|Acc]);

        {error, Reason} ->
            {error, {Filename, Reason}}
    end.

%%====================================================================
%% Function: fetch_file(Filename) -> {ok, Body} | {error, Reason}
%% Types:
%%     Filename = string()
%%     Body = io_list()
%%====================================================================

fetch_file(Filename) ->
    case file:read_file(Filename) of
        {ok, Bin} ->
            {ok, binary_to_list(Bin)};
        {error, Reason} ->
            {error, Reason}
    end.


%%====================================================================
%% Function: process_files(ProductsData, CategoriesData, StockData) -> List
%% Types:
%%     ProductsData = CategoriesData = StockData = list()
%%     List = [#ext_product_info{}]
%% Description: Convert an xml-file to the products list.
%%====================================================================

process_data(ProductsData, CategoriesData, StockData) when
  is_list(ProductsData),
  is_list(CategoriesData),
  is_list(StockData) ->
    {P_XML, _}=process_data_block(preprocess_data(ProductsData)),
    {C_XML, _}=process_data_block(preprocess_data(CategoriesData)),
    {S_XML, _}=process_data_block(preprocess_data(StockData)),
    error_logger:info_msg("~w:process_data/3 Parsing finished Ok", [?MODULE]),
    process(P_XML, C_XML, S_XML).

process_data_block(Data) ->
    Acc = fun(#xmlText{value = " ", pos = P}, Acc, S) ->
                 {Acc, P, S};
          (X, Acc, S) ->
                 {[X|Acc], S}
       end,
    xmerl_scan:string(Data, [{space,normalize}, {acc_fun, Acc}]).


%%====================================================================
%% Function: process(ProductsXML, CategoriesXML, StockXML) -> List
%% Types:
%%     ProductsXML = CategoriesXML = StockXML = #xmlElement{}
%%     List = [#ext_product_info{}]
%% Description: Convert xml-structures to the products list.
%%====================================================================

process(P_XML, C_XML, S_XML) ->
    Products = process_products(P_XML),
    Categories = process_categories(C_XML),
    StockInfo = process_stock(S_XML),
    F = fun(#stock{id = ProductId,
                   in_stock = InStock,
                   retailer_price = RetailerPrice,
                   user_price = UserPrice},
            Acc) ->
                case lists:keyfind(ProductId, #product.id, Products) of
                    #product{active = false} ->
                        Acc;
                    #product{name=Name,
                             category=Category,
                             short_descr=ShortDescr,
                             long_descr=LongDescr,
                             image_url=ImgURL
                            } ->
                        CategoryName = 
                            case lists:keyfind(Category, #category.id, Categories) of
                                false ->
                                    "Undefined";
                                #category{name=CN} ->
                                    CN
                            end,
                        [#ext_product_info{
                                           vendor_id = ?VENDOR_ENILGINC,
                                           id = ProductId,
                                           active = true,
                                           name = Name,
                                           category = Category,
                                           category_name = CategoryName,
                                           short_descr = ShortDescr,
                                           long_descr = LongDescr,
                                           small_image_url = ImgURL,
                                           big_image_url = ImgURL,
                                           in_stock = InStock,
                                           retailer_price = RetailerPrice,
                                           user_price = UserPrice
                                          } | Acc];
                    false ->
                        Acc
                end
        end,
    lists:foldl(F, [], StockInfo).

%%====================================================================
%% Function: process_products(XML) -> List
%% Types:
%%     XML = #xmlElement{}
%%     List = [#product{}]
%% Description: Extract products data from the xml-structure.
%%====================================================================

process_products(XML = #xmlElement{name='urunler'}) ->
    List=xmerl_xs:select("urun", XML),
    %% The result of xmerl_xs:value_of/1 functions is not a string (in io_list means).
    %% It is a list of UTF-8 characters, so, an element of the list can be greater then
    %% 255.
    [#product{id = list_to_integer(hd(xmerl_xs:value_of(xmerl_xs:select("urunid", E)))),
              active = list_to_integer(hd(xmerl_xs:value_of(xmerl_xs:select("active", E)))),
              name = hd(xmerl_xs:value_of(xmerl_xs:select("urunadi", E))),
              category = list_to_integer(hd(xmerl_xs:value_of(xmerl_xs:select("kategoriid", E)))),
              short_descr = hd(xmerl_xs:value_of(xmerl_xs:select("kisabilgi", E))),
              long_descr = hd(xmerl_xs:value_of(xmerl_xs:select("bilgi", E))),
              tax = str_price_to_int(hd(xmerl_xs:value_of(xmerl_xs:select("kdv", E)))),
              image_url = hd(xmerl_xs:value_of(xmerl_xs:select("resim", E)))
             }
     || E <- List].

%%====================================================================
%% Function: process_categories(XML) -> List
%% Types:
%%     XML = #xmlElement{}
%%     List = [#category{}]
%% Description: Extract categories data from the xml-structure.
%%====================================================================

process_categories(XML = #xmlElement{name='kategoriler'}) ->
    List=xmerl_xs:select("kategori", XML),
    %% The result of xmerl_xs:value_of/1 functions is not a string (in io_list means).
    %% It is a list of UTF-8 characters, so, an element of the list can be greater then
    %% 255.
    [#category{id = list_to_integer(hd(xmerl_xs:value_of(xmerl_xs:select("kategoriid", E)))),
               active = list_to_integer(hd(xmerl_xs:value_of(xmerl_xs:select("aktif", E)))),
               name = hd(xmerl_xs:value_of(xmerl_xs:select("kategoriadi", E))),
               parent_id = list_to_integer(hd(xmerl_xs:value_of(xmerl_xs:select("parentid", E))))
              }
     || E <- List].

%%====================================================================
%% Function: process_stock(XML) -> List
%% Types:
%%     XML = #xmlElement{}
%%     List = [#stock{}]
%% Description: Extract stock data from the xml-structure.
%%====================================================================

process_stock(XML = #xmlElement{name='stokfiyat'}) ->
    List=xmerl_xs:select("urun", XML),
    %% The result of xmerl_xs:value_of/1 functions is not a string (in io_list means).
    %% It is a list of UTF-8 characters, so, an element of the list can be greater then
    %% 255.
    [#stock{id = list_to_integer(hd(xmerl_xs:value_of(xmerl_xs:select("urunid", E)))),
            in_stock = list_to_integer(hd(xmerl_xs:value_of(xmerl_xs:select("stok", E)))),
            retailer_price = str_price_to_int(hd(xmerl_xs:value_of(xmerl_xs:select("byfiyat", E)))),
            user_price = str_price_to_int(hd(xmerl_xs:value_of(xmerl_xs:select("skfiyat", E))))
           }
     || E <- List].


%% FIXME: Quick and dirty implementation. Must be reimplemented.
str_price_to_int(String) ->
    round(list_to_float_smart(String)*100).

%%====================================================================
%% Function: list_to_float_smart(string()) -> float()
%% Description: It is a workaroud for the "strange" list_to_float/1 behaviour.
%%====================================================================

list_to_float_smart(String) ->
    try list_to_float(String)
    catch
        _:_ ->
            list_to_integer(String)*1.0
    end.

preprocess_data(Data) ->
    case Data of
        "<?xml version='1.0' encoding='ISO-8859-9'?>" ++ Rest ->
            Rest;
        "<?xml version=\"1.0\" encoding=\"iso-8859-9\"?>" ++ Rest ->
            Rest;
       _ ->
            Data
    end.

