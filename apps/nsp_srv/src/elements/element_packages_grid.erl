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

-include_lib("nsm_db/include/membership_packages.hrl").


reflect() -> record_info(fields, packages_grid).

render_element(_Record = #packages_grid{}) ->
    Id =  wf:temp_id(),

    Data = nsm_membership_packages:list_packages(),


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
  MP = create_record(Data),
  case proplists:get_value(id, Data, undefined) of
    RawId when is_integer(RawId) andalso RawId < 0  ->
      PrevId = nsm_db:get(id_seq, "membership_package"),  % this is bad
      Id = case PrevId of
        {error,notfound} -> 0;
        {ok,{id_seq, _, DBId}} -> DBId
      end,
      nsx_msg:notify(["system", "add_package"], {MP}),
      wf:wire(wf:f("obj('~s').grid.updateId('~p')", [Anchor, Id]));

    Id ->
      ?INFO("Update package: id= ~p  base=~p~n", [Id, MP]),
      case nsm_db:get(membership_package, Id) of
        {error, notfound} -> fail;
        {ok, _} -> nsm_db:put(MP#membership_package{id=Id})
      end
  end.

create_record(Data) ->
  GV = fun(K, Def) -> proplists:get_value(K, Data, Def) end,
  No = case GV(name, -1) of
    N when is_integer(N) -> N;
    N2 when is_list(N2) -> list_to_integer(N2);
    _ -> -1
  end,
  #membership_package{
    no           = No,
    payment_type = payment_type(GV(payment, "paypal")),
    quota        = GV(quota, 0),
    amount       = GV(price, 0),
    deducted_for_gifts = GV(gifts_points, 0),
    net_membership     = GV(net_membership, 0),
    available_for_sale = GV(available, false)}.


payment_type("mobile")        -> mobile;
payment_type("paypal")        -> paypal;
payment_type("facebook")     -> facebook;
payment_type("wire_transfer") -> wire_transfer;
payment_type("credit_card")   -> credit_card;
payment_type(T)               -> ?INFO("Payment type ~p", [T]),error.
