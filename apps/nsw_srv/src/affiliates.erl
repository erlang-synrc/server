-module (affiliates).
-author('Serge Polkovnikov <serge,polkovnikov@gmail.com>').
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("nsm_db/include/user.hrl").
-include_lib("nsm_db/include/feed.hrl").
-include_lib("alog/include/alog.hrl"). 
-include("elements/records.hrl").
-include("gettext.hrl").
-include("setup.hrl").

-define(CONTRACTSPERPAGE, 20).
-define(DETAILSPERPAGE, 20).

main() ->
    case wf:user() /= undefined of
        true  -> main_authorized();
        false -> wf:redirect_to_login(?_U("/login"))
    end.

main_authorized() ->
    UserOrNot = wf:q('of'),
    case UserOrNot of
        undefined ->
            UserName = wf:user();
        MrX ->
            UserName = MrX
    end,
    case catch rpc:call(?APPSERVER_NODE,nsm_users,get_user,[UserName]) of
        {ok, UserInfo} ->
            wf:state(user, UserInfo),
            wf:state(feed_owner, {user, UserName}),
            dashboard:main_authorized();
        Reason ->
            ?ERROR("unable to get user info: User=~p, Reason=~p", [UserName, Reason]),
            wf:redirect("404")
    end,
    webutils:js_for_main_authorized_game_stats_menu(),
    #template { file=code:priv_dir(nsw_srv)++"/templates/bare.html" }.

body() ->
    UserName = wf:user(),
    {ok, User} = rpc:call(?APPSERVER_NODE,nsm_users,get_user,[UserName]),
    case rpc:call(?APPSERVER_NODE,nsm_acl,check_access,[User, {feature, admin}]) of
	    allow -> body_authorized();
	    _ -> ?_T("You don't have access to do that.")
    end.

body_authorized() ->
    #template{file=code:priv_dir(nsw_srv)++"/templates/view-user.html"}.

% data 
get_contracts_data_from_bd(UserID) ->
    rpc:call(?APPSERVER_NODE,nsm_affiliates, get_contracts, [UserID]).

get_contract_details_from_bd(ContractId) ->
    rpc:call(?APPSERVER_NODE,nsm_affiliates, get_purchases_details, [ContractId]).

total_ammount(ContractId) ->
    Data = get_contract_details_from_bd(ContractId),    
    lists:sum([CommissionSum || {_, _, _, CommissionSum, _} <- Data]).


% utils (maybe migrate them to site_utils?)
part([], _, _) ->
    [];
part(List, From, To) ->
    N = fun(X) ->
        case X<0 of
            true ->
                XX = length(List) + 1 + X,
                case XX<1 of
                    true -> 1;
                    false -> XX
                end;
            false ->
                case X > length(List) of
                    true -> length(List);
                    false -> X
                end
        end
    end,
    RealFrom = N(From),
    RealTo = N(To),
    case RealTo<RealFrom of
        true -> "";
        false -> string:sub_string(List, N(From), N(To))
    end.

trim_dot_zero(T) ->
    case part(T, -1,-1) of
       "." -> trim_dot_zero(part(T, 1, -2));
       "0" -> trim_dot_zero(part(T, 1, -2));
        _ -> T
    end.

commission_to_list(C) when is_integer(C) ->
    integer_to_list(C);
commission_to_list(C) ->
    trim_dot_zero(hd(io_lib:format("~.2f",[C]))).

kurus_to_string(Kurus) ->
    SKurus = integer_to_list(Kurus),
    case Kurus >= 100 of 
        true ->
            part(SKurus, 1, -3) ++ "." ++ part(SKurus, -2, -1);
        false ->
            case Kurus >= 10 of
                true ->
                    "0."++SKurus;
                false ->
                    "0.0"++SKurus
            end
    end. 

% main side
content() -> 
    [
        #panel{class="top-space", body=[
            #h1{text=?_T("Affiliate contracts")}
        ]},
        #panel{id=page_content, body=[
            paged_content((wf:state(user))#user.username)
        ]}
    ].


paged_content(UserId) ->
    paged_content(1, UserId).

paged_content(Page, UserId) ->
    Contracts = lists:reverse( lists:sort( get_contracts_data_from_bd(UserId) ) ),
    ThisPageContracts = part(Contracts, 1 + (Page-1)*?CONTRACTSPERPAGE, Page*?CONTRACTSPERPAGE),
    Buttons = case length(Contracts) > ?CONTRACTSPERPAGE of
        true ->
            #panel{class="paging-2", body=[
                #panel{class="center", body=[
                    #list{body=[
                        case Page of
                            1 -> #listitem{body=#link{text="<", postback={nothing}, class="inactive"}};
                            _ -> #listitem{body=#link{text="<", postback={contracts_page, Page - 1, UserId}}}
                        end,
                        #listitem{body=#link{text=integer_to_list(Page), postback={nothing}, class="inactive"}},
                        case Page * ?CONTRACTSPERPAGE >= length(Contracts) of                 
                            false -> #listitem{body=#link{text=">", postback={contracts_page, Page + 1, UserId}}};
                            true ->  #listitem{body=#link{text=">", postback={nothing}, class="inactive"}}
                        end
                    ]}
                ]}
            ]};
        false ->
            []
    end,
            
    [begin
        PanelId = wf:temp_id(),
        #panel{ class="affiliates-box", body=[
            #link{text=ContractName, postback={show_contract_details, ContractId, PanelId}, style="font-weight:bold;"},
            #span{text=" ", style="padding-left:16px;"},
            #span{text=site_utils:date_to_text(StartDate) ++ "–" ++ site_utils:date_to_text(FinishDate), style="font-size:9.5pt;"},
            #span{text=" ", style="padding-left:16px;"},
            #span{text=?_T("Limit") ++ ": " ++ integer_to_list(Limit)},
            #span{text=" ", style="padding-left:16px;"},
            #span{text=?_T("Comission") ++ ": " ++ commission_to_list(Commission) ++ "%"},
            #span{text=" ", style="padding-left:16px;"},
            #span{text=?_T("Total") ++ ": "},
            #span{text=kurus_to_string(total_ammount(ContractId)), style="font-size:14pt;"},
            #span{text=" TL"},
            #panel{id=PanelId, body=[]}
        ]}
    end
    || {ContractId, ContractName, StartDate, FinishDate, Limit, Commission} <- ThisPageContracts] ++ [Buttons] ++ 
    case ThisPageContracts of
        [] -> [#span{text=?_T("No contracts yet."), style="font-size:10pt;"}];
        _ -> []
    end.    


contract_details_page(ContractId, Page, SelfPanelId) ->
    Details = lists:sort(get_contract_details_from_bd(ContractId)),
    ThisPageDetails = part(Details, 1 + (Page-1)*?DETAILSPERPAGE, Page*?DETAILSPERPAGE),
    Buttons = case length(Details) > ?DETAILSPERPAGE of
        true ->
            #panel{class="paging-2", style="padding: 10px 0px 0px 0px;", body=[
                #panel{class="center", body=[
                    #list{body=[
                       case Page of
                            1 -> #listitem{body=#link{text="<", postback={nothing}, class="inactive"}};
                           _ -> #listitem{body=#link{text="<", postback={details_page, ContractId, Page - 1, SelfPanelId}}}
                        end,
                        #listitem{body=#link{text=integer_to_list(Page), postback={nothing}, class="inactive"}},
                        case Page * ?DETAILSPERPAGE >= length(Details) of                 
                            true -> #listitem{body=#link{text=">", postback={nothing}, class="inactive"}};
                            false -> #listitem{body=#link{text=">", postback={details_page, ContractId, Page + 1, SelfPanelId}}}
                        end
                   ]}
               ]}
            ]};
       false ->
            []
    end,


    TL = #span{text=" TL", style="font-size:8pt;"},
    case Details of
        [] ->
            #p{body=?_T("No one bought anything yet under this contract.")};
        _ ->
            [
                #table {rows=[
                    #tablerow { cells=[
                        #tableheader { text=?_T("Username"), style="padding:4px 10px;" },
                        #tableheader { text=?_T("Purchases"), style="padding:4px 10px;" },
                        #tableheader { text=?_T("Purchases total"), style="padding:4px 10px;" },
                        #tableheader { text=?_T("Comission total"), style="padding:4px 10px;" }
                    ]}
                ] ++ [
                    begin
                        PanelId = wf:temp_id(),
                        [
                            #tablerow { cells=[
                                #tablecell { body = #link{text=UserId, postback={show_user_details, PurchasesList, PanelId}}, 
                                    style="padding:4px 10px; font-weight:bold;"},
                                #tablecell { body = integer_to_list(PurchasesNum), 
                                    style="padding:4px 10px; text-align:center;" },
                                #tablecell { body = [integer_to_list(PurchasesSum), TL], 
                                    style="padding:4px 10px; text-align:center;" },
                                #tablecell { body = [kurus_to_string(CommissionSum), TL], 
                                    style="padding:4px 10px; text-align:center;" }
                            ]},
                            #tablerow { cells=[
                                #tablecell{id=PanelId, body=[], colspan=4, style="padding-left:10px;"}
                            ]}
                        ]
                    end
                    || {UserId, PurchasesNum, PurchasesSum, CommissionSum, PurchasesList} <- ThisPageDetails
                ]},
                Buttons
            ]
    end.

purchases_list(PurchasesList) ->
    [
        #table {rows=[
            #tablerow { cells=[
                #tableheader { text=?_T("Time and date"), style="padding:4px 10px;" },
                #tableheader { text=?_T("Package"), style="padding:4px 10px;" },
                #tableheader { text=?_T("Payment type"), style="padding:4px 10px;" },
                #tableheader { text=?_T("Price"), style="padding:4px 10px;" },
                #tableheader { text=?_T("Comission"), style="padding:4px 10px;" }
            ]}
        ] ++ [
            begin
                STime = site_utils:feed_time_tuple(Date),
                SName = ?_T("Packet") ++ " " ++ integer_to_list(PackageNumber),
                SPaymentType = atom_to_list(PaymentType),
                SPrice = integer_to_list(Price),
                SCommission = kurus_to_string(Commission),
                TL = #span{text=" TL", style="font-size:8pt;"},
                [
                    #tablerow { cells=[
                        #tablecell { body = STime, 
                            style="padding:4px 10px; text-align:center;" },
                        #tablecell { body = SName, 
                            style="padding:4px 10px; text-align:center;" },
                        #tablecell { body = SPaymentType, 
                            style="padding:4px 10px; text-align:center;" },
                        #tablecell { body = [SPrice, TL], 
                            style="padding:4px 10px; text-align:center;" },
                        #tablecell { body = [SCommission, TL], 
                            style="padding:4px 10px; text-align:center;" }
                    ]}
                ]
            end
            || {_PurchaseId, Date, _PackageId, PackageNumber, PaymentType, Price, Commission} <- PurchasesList
        ]}
    ].

% left side
get_friends() ->
    User = wf:state(user),
    webutils:get_friends(User).

get_groups() ->
    User = wf:state(user),
    webutils:get_groups(User).

user_info() ->
    UserOrNot = wf:q('of'),
    case UserOrNot of
        undefined ->
            webutils:get_ribbon_menu();
        _ ->
            view_user:user_info()
    end.

% events
event({change_language,SL}) ->
    webutils:event({change_language, SL});

event({nothing}) ->
    ok;

event(Event) ->
    case wf:user() of
    	undefined -> wf:redirect_to_login(?_U("/login"));
        User      -> inner_event(Event, User)
    end.

inner_event({show_contract_details, ContractId, PanelId}, _) ->
    PanelInner = [
        #p{body=contract_details_page(ContractId, 1, PanelId)},
        #link{text=?_T("Hide"), postback={hide_contract_details, PanelId}}
    ],
    wf:update(PanelId, PanelInner);

inner_event({details_page, ContractId, Page, PanelId}, _) ->
    PanelInner = [
        #p{body=contract_details_page(ContractId, Page, PanelId)},
        #link{text=?_T("Hide"), postback={hide_contract_details, PanelId}}
    ],
    wf:update(PanelId, PanelInner);

inner_event({hide_contract_details, PanelId}, _) ->
    wf:update(PanelId, []);


inner_event({show_user_details, PurchasesList, PanelId}, _) ->
    PanelInner = [
        #p{body=purchases_list(PurchasesList)},
        #link{text=?_T("Hide details"), postback={hide_user_details, PanelId}},
        #br{},
        #br{},
        #br{}
    ],
    wf:update(PanelId, PanelInner);

inner_event({hide_user_details, PanelId}, _) ->
    wf:update(PanelId, []);

inner_event({contracts_page, Page, UserID}, _) ->
    wf:update(page_content, paged_content(Page, UserID));

inner_event(logout, _) ->
    wf:logout(),
    wf:redirect_to_login(?_U("/login")).


