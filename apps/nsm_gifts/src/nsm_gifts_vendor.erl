%%----------------------------------------------------------------------
%% @author Serge Polkovnikov <serge.polkovnikov@gmail.com>
%% @copyright Paynet Internet ve Bilisim Hizmetleri A.S. All Rights Reserved.
%% @doc
%% Vendors API.
%% @end
%% Created: Sep 10, 2012
%%----------------------------------------------------------------------
-module(nsm_gifts_vendor).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([
         get_gifts/1
        ]).

%%
%% API Functions
%%


%% @spec get_gifts(VendorId) -> {ok, List} |
%%                              {error, unknown_vendor} |
%%                              {error, {fetching, term()}} |
%%                              {error, {parsing, term()}}
%% @doc
%% Types:
%%     VendorId = vendor_id()
%%     List = list(#ext_product_info{})
%% @end

get_gifts(VendorId) ->
    case nsm_gifts_vendor_spec:get_info(VendorId) of
        {ok, {_, _, Mod}} ->
            Mod:get_gifts();
        {error, not_found} ->
            {error, unknown_vendor}
    end.

%%
%% Local Functions
%%

