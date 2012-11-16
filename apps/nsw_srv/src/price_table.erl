-module (price_table).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("nsm_db/include/user.hrl").
-include_lib("nsm_db/include/membership_packages.hrl").

-include("elements/records.hrl").
-include("setup.hrl").
-include("common.hrl").

-define(ENTRY_TEXT_LENGTH, 350).
-define(ACTIVATION_ACTION(ID), actions=#event{type=click, actions=activation_action(ID)}).
-define(ACTIVE_CLS, "ui-state-active").
-define(INTERNAL_URL(PaymentType), url=lists:concat(["#", ?_U(payment_type_to_url(PaymentType))])).

main() -> #template { file=code:priv_dir(nsw_srv)++"/templates/bare.html" }.

title() -> webutils:title(?MODULE).

body() -> #template{file=code:priv_dir(nsw_srv)++"/templates/price_table.html"}.

payment_type_selector()-> payment_types(wf:session(is_facebook)).

payment_types(undefined)->payment_types(false);

payment_types(false)->
    ["<ul class=\"tabset\" id=\"tabset\">",
        "<li class=\""++?ACTIVE_CLS++"\">",#link{id=credit_card,   ?INTERNAL_URL(credit_card),   postback={payment_select, credit_card},   ?ACTIVATION_ACTION("credit_card")},"<span class=\"img\"><img class=\"png\" src=\"/images/ico-06.png\" alt=\"\" width=\"48\" height=\"56\" ><img class=\"png\" src=\"/images/ico-08.png\" alt=\"\" width=\"48\" height=\"56\" ></span><strong>",?_T("Credit card"),"</strong></a></li>",
        "<li>",                            #link{id=paypal,        ?INTERNAL_URL(paypal),        postback={payment_select, paypal},        ?ACTIVATION_ACTION("paypal")}, "<span class=\"img\"><img class=\"png\" src=\"/images/ico-09.png\" alt=\"\" width=\"62\" height=\"56\" ><img class=\"png\" src=\"/images/ico-07.png\" alt=\"\" width=\"62\" height=\"56\" ></span><strong>",?_T("Paypal"),"</strong></a></li>",
        "<li>",                            #link{id=wire_transfer, ?INTERNAL_URL(wire_transfer), postback={payment_select, wire_transfer}, ?ACTIVATION_ACTION("wire_transfer")},"<span class=\"img\"><img class=\"png\" src=\"/images/ico-10.png\" alt=\"\" width=\"48\" height=\"56\" ><img class=\"png\" src=\"/images/ico-11.png\" alt=\"\" width=\"48\" height=\"56\" ></span><strong>",?_T("Wire"),"</strong></a></li>",
        "<li>",                            #link{id=mobile,        ?INTERNAL_URL(mobile),        postback={payment_select, mobile},        ?ACTIVATION_ACTION("mobile")},"<span class=\"img\"><img class=\"png\" src=\"/images/ico-12.png\" alt=\"\" width=\"31\" height=\"56\" ><img class=\"png\" src=\"/images/ico-13.png\" alt=\"\" width=\"31\" height=\"56\" ></span><strong>",?_T("Mobile"),"</strong></a></li>",
    "</ul>"];
payment_types(true)-> 
    fb_utils:pay_dialog().

-spec table()->#table{}.
table()->
    %% add table to 'data' container
    #panel{class=data, id=price_container, body=[
	case wf:session(is_facebook) of
	    true -> table(facebook);
	    _ -> table(credit_card)
	end 
    ]}.

-spec table(PaymentType::payment_type())->#table{}.
table(PaymentType)->
    Packages = nsm_membership_packages:list_packages([{payment_type, PaymentType},{available_for_sale, true}]),

    % get packages and sort them by No
    PackagesSorted =
        lists:sort(fun(#membership_package{no=No1},
                       #membership_package{no=No2})->
                           No1 < No2
                   end, Packages),
    % rows headers
    InfoColumn =
        [[?_T("Please join"), #br{}, ?_T("Select package")],
         ?_T("Gift points (kakush) charge"),
         ?_T("Net membership fee"),
         ?_T("Game quota"),
         ""], % buttons row hasn't row header

    % map membership_package fields to corresponding rows
    PacketColumns =
        [[
          [#span{class="top", text=io_lib:format("~s ~p", [?_T("Packet"), P#membership_package.no]), html_encode=false},
           #span{class="center", text=packet_price_element(P#membership_package.amount), html_encode=false}],
          to_tl(P#membership_package.deducted_for_gifts),
          to_tl(P#membership_package.net_membership),
          wf:to_list(P#membership_package.quota),
          buy_button_element(P#membership_package.id, PaymentType, nsm_membership_packages:check_limit_over(wf:user(), P#membership_package.id))
         ] || P <- PackagesSorted],

    % transform columns data to table rows
    Rows = columns_to_rows([InfoColumn|PacketColumns]),

    #table{rows=Rows}.

%% Events
event({payment_select, PaymentType}) ->
    wf:update(price_container, table(PaymentType));
event({buy, PackageId, PaymentType}) ->
    case wf:user() of
        undefined ->
            buy:unregistered_popup();
        UId ->
            case nsm_membership_packages:check_limit_over(UId, PackageId) of
                true ->
                    buy:over_limit_popup(nsm_membership_packages:get_monthly_purchase_limit());
               _ ->
                    URL = lists:concat([?_U("/buy/"), PaymentType, "/package_id/", wf:to_list(PackageId)]),
                    wf:redirect(URL)
            end
    end;
event(Any)->
    webutils:event(Any).

api_event(Name, Tag, Data) ->
    fb_utils:api_event(Name, Tag, Data).

%% Local functions
-spec columns_to_rows(Columns::list())-> Rows::list().
columns_to_rows(Rows)->
    columns_to_rows("odd", true, Rows).

-spec columns_to_rows(Class::string(), IsFirst::boolean(), Columns::list())-> [#tablerow{}].
columns_to_rows(_, _, [])->
    [];
columns_to_rows(Class, IsFirst, Columns)->
    RowCellsData = [Cell || [Cell|_] <- Columns],
    case RowCellsData of
        [_|_] ->
            {Cells, _} = lists:mapfoldl(
                fun(CellBody, ColumnCount)->
                    ColumnClass = io_lib:format("col-~2..0B", [ColumnCount]),
                    {case IsFirst of
                        true->
                            #tableheader{class=ColumnClass, body=CellBody};
                        false->
                            #tablecell{class=ColumnClass, body=CellBody}
                    end, ColumnCount+1}
                end, 1, RowCellsData),

            RowClass = case Columns of
                %% if last row add last-row class to all elements
                [[_]|_]->
                    Class++" last-row";
                _->
                    Class
            end,

            Row = #tablerow{class=RowClass, cells=Cells},

            [Row | columns_to_rows(odd_even(Class), false, [Rest||[_|Rest] <- Columns])];
        [] ->
            []
    end.

-spec packet_price_element(any())-> list().
packet_price_element(Price)->
    ["<span class=\"frame\">",
     "<span class=\"price\">",
     io_lib:format("<strong>~p</strong>", [Price]),
     "<em>TL</em>",
     "</span></span>"].

buy_button_element(PackageId, facebook, OverLimit)->
    fb_utils:buy_button(PackageId, OverLimit);
buy_button_element(PackageId, PaymentType, _)->
    #link{class="btn", text=?_T("Buy"), postback={buy, PackageId, PaymentType}}.

odd_even("odd")-> "even";
odd_even("even")-> "odd".

%% script for clicked button activation
activation_action(ID)->
    "$('#tabset li').removeClass('ui-state-active');"
    "objs('"++ID++"').parent('li').addClass('"++?ACTIVE_CLS++"')".

to_tl(Price)-> io_lib:format("~p TL", [Price]).

payment_type_to_url(mobile)        -> "mobile";
payment_type_to_url(credit_card)   -> "credit-card";
payment_type_to_url(wire_transfer) -> "wire-transfer";
payment_type_to_url(paypal)        -> "paypal".

