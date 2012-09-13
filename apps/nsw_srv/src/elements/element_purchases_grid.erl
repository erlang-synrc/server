%%----------------------------------------------------------------------
%% @author Vladimir Baranov <baranoff.vladimir@gmail.com>
%% @copyright Paynet Internet ve Bilisim Hizmetleri A.S. All Rights Reserved.
%% @doc
%% FIXME: add description of element_packages_grid
%% @end
%%--------------------------------------------------------------------
-module(element_purchases_grid).

-compile(export_all).

%%
%% Include files
%%
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-include("common.hrl").
-include("setup.hrl").

-include_lib("nsm_srv/include/membership_packages.hrl").
-include_lib("nsm_srv/include/user.hrl").



reflect() -> record_info(fields, purchases_grid).

render_element(_Record = #purchases_grid{}) ->
    Id =  wf:temp_id(),

    wf:wire(Id, #attr{target=Id, attr=id, value=Id}),
    Script = grid_script(Id),
    wf:wire(Id, Script),

    [#panel{id=Id}].


%%
%% Local Functions
%%

grid_script(Id) ->
    %% create api action, save id in tag to have access later
    APISave   = #api{anchor = Id, tag = Id,  name = saveData, delegate = ?MODULE},
	%% API to anable grid reload data
	APILoadData = #api{anchor = Id, tag = Id, name = loadData, delegate = ?MODULE},

    wf:wire(APISave),
	wf:wire(APILoadData),

    %% fields, columns and logic is in admin-lib.js file
    %% create 'grid' field in anchor object to have access later
    ScriptTemplate = "obj('~s').grid = new UI.admin.PurchasesGrid({"
        "title: '~s'"
        ",height: 400"
        ",layout: 'fit'"
        ",renderTo: '~s'"
        %% here grid will call to our api and then will be updated with new data
        %% async
		",loadDataRequest: function(){~s}"
        %% call nitrogen api to send data
        ",onSave: function(data){ ~s }"
        "});",

	wf:f(ScriptTemplate, [Id, ?_T("Purchases"), Id,
						  callback(APILoadData, ""),
						  callback(APISave, "data")
						  ]).


callback(#api{anchor = Id, name = Name}, DataVar) ->
    wf:f("obj('~s').~p(~s)", [Id, Name, DataVar]).


api_event(saveData, Anchor, [Data]) ->
    ?DBG("Got SAVE event: Anchor: ~p, Data: ~p", [Anchor, Data]),
	ok;

api_event(loadData, Anchor, []) ->
	?DBG("Got LOAD event: Anchor: ~p", [Anchor]),
	Purchases = rpc:call(?APPSERVER_NODE, nsm_membership_packages,
						 list_purchases, []),
	%%?DBG("Purchases: ~p", [Purchases]),

	JSON = purchases_to_json(Purchases),
	%?DBG("Reply: Purchases: ~p, json: ~p", [Purchases, JSON]),
	%% update grid with new data
	wf:wire(wf:f("obj('~s').grid.updateData('~s')",
				 [Anchor, JSON])).


purchases_to_json(Purchases) ->
    Items = [begin
        Package =  I#membership_purchase.membership_package,
        [
            {id, I#membership_purchase.id},
            {external_id, to_list(I#membership_purchase.external_id)},
            {username, to_list(I#membership_purchase.user_id)},
            {state, I#membership_purchase.state},
            {package, [
                {payment_type, Package#membership_package.payment_type},
                {amount, Package#membership_package.amount}]
            },
            {start_time, convert_time(I#membership_purchase.start_time)},
            {end_time,   convert_time(I#membership_purchase.end_time)},
            {info,       to_list(I#membership_purchase.info)}]
    end || I <- Purchases],

    {ok, JSON} = element_purchases_grid_dtl:render([{items, Items}]),
    FlatJson = lists:flatten(binary_to_list(iolist_to_binary(JSON))),
    site_utils:base64_encode_to_url(FlatJson).



%% convert to list
to_list(undefined)         -> "";
to_list(L) when is_list(L) -> L;
to_list(Other)             -> wf:f("~p", [Other]).

convert_time(undefined) ->
    "";
convert_time({_MSec, _Sec, _} = Now) ->
    {{Y,MM,D},{H,M,S}} = calendar:now_to_local_time(Now),
    lists:flatten(
      io_lib:format("~b-~2..0b-~2..0bT~2..0b:~2..0b:~2..0b", [Y, MM, D, H, M, S])).
