%%----------------------------------------------------------------------
%% @author Serge Polkovnikov <serge.polkovnikov@gmail.com>
%% @copyright Paynet Internet ve Bilisim Hizmetleri A.S. All Rights Reserved.
%% @doc
%% Vendors specifications API.
%% @end
%% Created: Sep 7, 2012
%%----------------------------------------------------------------------
-module(nsm_gifts_vendor_spec).

%%
%% Include files
%%

-include("vendors.hrl").

%%
%% Exported Functions
%%
-export([
         list/0,
         get_info/1
        ]).



-define(SPECS,
        [
         {?VENDOR_ENILGINC,
          <<"Enilginc">>,
          <<"Enilging">>,
          nsm_gifts_plugin_enilginc},
         {?VENDOR_ARISTO,
          <<"ARISTO">>,
          <<"ARISTO">>,
          nsm_gifts_plugin_aristo},
         {?VENDOR_KAKARA,
          <<"Kakaranet">>,
          <<"Kakaranet">>,
          nsm_gifts_plugin_kakara}
        ]).
%%
%% API Functions
%%

%% @spec list() -> List
%% @doc
%% Types:
%%     List = [{VednorId, VendorName}]
%%       VendorId = integer()
%%       VendorName = binary()
%% @end

list() ->
    [{VendorId, Name} || {VendorId, Name, _, _} <- specs()].


%% @spec get_info(VendorId) -> {ok, Info} | {error, not_found}
%% @doc
%% Types:
%%     VendorId = integer()
%%     Info = {Name, Description, Module}
%%       VendorId = integer()
%%       VendorName = Description = binary()
%%       Module = atom()
%% @end

get_info(VendorId) ->
    case lists:keyfind(VendorId, 1, specs()) of
        {_, Name, Description, Module} ->
            {ok, {Name, Description, Module}};
        false ->
            {error, not_found}
    end.
%%
%% Local Functions
%%

specs() ->
    ?SPECS.
