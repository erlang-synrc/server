%%----------------------------------------------------------------------
%% @author Vladimir Baranov <baranoff.vladimir@gmail.com>
%% @copyright Paynet Internet ve Bilisim Hizmetleri A.S. All Rights Reserved.
%% @doc
%% FIXME: add description of element_packages_grid
%% @end
%%--------------------------------------------------------------------
-module(element_packages_grid).

-compile(export_all).

%%
%% Include files
%%
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").
-include("common.hrl").
-include("setup.hrl").

-include_lib("nsm_srv/include/membership_packages.hrl").


reflect() -> record_info(fields, packages_grid).

render_element(_Record = #packages_grid{}) ->
    Id =  wf:temp_id(),

    Data = rpc:call(?APPSERVER_NODE, nsm_membership_packages,
                    list_packages, []),


    wf:wire(Id, #attr{target=Id, attr=id, value=Id}),
    Script = grid_script(Id, Data),
    wf:wire(Id, Script),

    [#panel{id=Id}].


%%
%% Local Functions
%%

grid_script(Id, Data) ->
    %% create api action, save id in tag to have access later
    API = #api{anchor = Id, tag = Id,  name = savePackage, delegate = ?MODULE},
    wf:wire(API),

    %% fields, columns and logic is in admin-lib.js file
    %% create 'grid' field in anchor object to have access later
    ScriptTemplate = "obj('~s').grid = new UI.admin.PackagesGrid({"
        "data: [~s]"
        ",height: 400"
        ",title: '~s'"
        ",layout: 'fit'"
        ",renderTo: '~s'"
        %% call nitrogen api to send data
        ",onPackageSave: function(data){ ~s }"
        %% FIXME: potential race. If user creates new record before this confirmed
        ",updateId: function(id){var s = this.getStore(), r = s.getAt(0);"
                                     "  r.set('id', id); r.commit() }"
        "});",
%% r.set('id', id); r.commit();
    wf:f(ScriptTemplate, [Id, transfrom_data(Data), ?_T("Membership packages"), Id,
                          callback(API, "data")]).

transfrom_data(Data) ->
	%% sort in reversed order to skip reverse of the list in tr_d
	Sorted = lists:sort(fun(#membership_package{id = Id1},
							#membership_package{id = Id2}) -> Id1 < Id2 end,
						Data),
	io:format("Sorted: ~p~n", [Sorted]),
    tr_d(Sorted, []).

tr_d([], Acc) ->
    string:join(Acc, ",");
tr_d([MP = #membership_package{} | T], Acc) ->
    Id      = MP#membership_package.id,
    Name    = MP#membership_package.no,
    Price   = MP#membership_package.amount,
    AFS     = MP#membership_package.available_for_sale,
	Gifts   = MP#membership_package.deducted_for_gifts,
	NM      = MP#membership_package.net_membership,
	Quota   = MP#membership_package.quota,
    Payment = MP#membership_package.payment_type,

    %% Fields are:
	%% ['id','name','payment','gifts_points','net_membership','quota','available','price']

    DR = wf:f("['~s', '~p', '~p', ~p, ~p, ~p, ~p, ~p]",
              [Id, Name, Payment, Gifts, NM, Quota, AFS, Price]),

    tr_d(T, [DR | Acc]).

callback(#api{anchor = Id, name = Name}, DataVar) ->
    wf:f("obj('~s').~p(~s)", [Id, Name, DataVar]).


api_event(savePackage, Anchor, [Data]) ->
    ?DBG("Anchor: ~p, Data: ~p", [Anchor, Data]),
    case proplists:get_value(id, Data, undefined) of
        %% new record
        RawId when is_integer(RawId) andalso RawId < 0  ->
            MP = create_record(Data),
            case rpc:call(?APPSERVER_NODE, nsm_membership_packages,
                          add_package, [MP]) of
                {ok, Id} ->
                    %% update id of last inserted record
                    wf:wire(wf:f("obj('~s').grid.updateId('~p')",
                                 [Anchor, Id]));
                {error, Reason} ->
                    ?ERROR("unable to add package: ~p, Reason ~p", [MP, Reason])
            end;
        %% for already defined packages only available_for_sale property
        %% can be changed
        Id ->
            AFS = proplists:get_value(available, Data, false),
            case rpc:call(?APPSERVER_NODE, nsm_membership_packages,
                          available_for_sale, [Id, AFS]) of
                ok ->
                    ok;
                {error, Reason} ->
                    ?ERROR("~p: unable to update availability to ~p ~p",
                           [Id, AFS, Reason])
            end
    end.

create_record(Data) ->
    GV = fun(K, Def) -> proplists:get_value(K, Data, Def) end,

    #membership_package{no           = list_to_integer(GV(name, "-1")),
                        payment_type = payment_type(GV(payment, "paypal")),
						quota        = GV(quota, 0),
                        amount       = GV(price, 0),
						deducted_for_gifts = GV(gifts_points, 0),
						net_membership     = GV(net_membership, 0),
                        available_for_sale = GV(available, false)}.


payment_type("mobile")        -> mobile;
payment_type("paypal")        -> paypal;
payment_type("faceboook")     -> facebook;
payment_type("wire_transfer") -> wire_transfer;
payment_type("credit_card")   -> credit_card;
payment_type(_)               -> error.
