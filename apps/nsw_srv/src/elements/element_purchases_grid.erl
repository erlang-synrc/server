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

-include_lib("nsm_db/include/membership_packages.hrl").
-include_lib("nsm_db/include/user.hrl").

reflect() -> record_info(fields, purchases_grid).

render_element(_Record = #purchases_grid{}) ->
    Id =  wf:temp_id(),

    wf:wire(Id, #attr{target=Id, attr=id, value=Id}),
    Script = grid_script(Id),
    wf:wire(Id, Script),

    [#panel{id=Id}].

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
        callback(APISave, "data") ]).

callback(#api{anchor = Id, name = Name}, DataVar) ->
    wf:f("obj('~s').~p(~s)", [Id, Name, DataVar]).

api_event(saveData, Anchor, [Data]) ->
    ?DBG("Got SAVE event: Anchor: ~p, Data: ~p", [Anchor, Data]),
    ok;

api_event(loadData, Anchor, []) ->
    ?DBG("Got LOAD event: Anchor: ~p", [Anchor]),
    Purchases = nsm_membership_packages:list_purchases(),
    JSON = site_utils:base64_encode_to_url(mochijson2:encode([purchase_to_json(P) || P <- Purchases])),
    wf:wire(wf:f("obj('~s').grid.updateData('~s')", [Anchor, JSON])).

purchase_to_json(#membership_purchase{} = I)->
    P = I#membership_purchase.membership_package,
    {struct, [
        {<<"id">>, list_to_binary(I#membership_purchase.id)},
        {<<"external_id">>, list_to_binary(to_list(I#membership_purchase.external_id))},
        {<<"user_name">>, list_to_binary(to_list(I#membership_purchase.user_id))},
        {<<"user_info">>, list_to_binary("{}")},
        {<<"state">>, list_to_binary(to_list(I#membership_purchase.state))},
        {<<"m_package">>, {struct, [
            {<<"payment_type">>, list_to_binary(to_list(P#membership_package.payment_type))},
            {<<"amount">>, list_to_binary(to_list(P#membership_package.amount))}
            ]}
        },
        {<<"start_time">>, list_to_binary(convert_time(I#membership_purchase.start_time))},
        {<<"end_time">>, list_to_binary(convert_time(I#membership_purchase.end_time))},
        {<<"info">>, list_to_binary(to_list(I#membership_purchase.info))},
        {<<"state_log">>, []}
    ]};
purchase_to_json(_)-> {}.

to_list(undefined)         -> "";
to_list(L) when is_list(L) -> L;
to_list(Other)             -> wf:f("~p", [Other]).

convert_time(undefined) ->
    "";
convert_time({_MSec, _Sec, _} = Now) ->
    {{Y,MM,D},{H,M,S}} = calendar:now_to_local_time(Now),
    lists:flatten(
      io_lib:format("~b-~2..0b-~2..0bT~2..0b:~2..0b:~2..0b", [Y, MM, D, H, M, S])).
