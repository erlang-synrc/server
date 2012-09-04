%%----------------------------------------------------------------------
%% @author Vladimir Baranov <vladimir.b.n.b@gmail.com>
%% @copyright Paynet Internet ve Bilisim Hizmetleri A.S. All Rights Reserved.
%% @doc
%% Credit card purchases page
%% @end
%%---------------------------------------------------------------------

-module(buy_credit_card).

-compile(export_all).

-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("nsm_srv/include/membership_packages.hrl").
-include_lib("nsm_srv/include/accounts.hrl").
-include_lib("nsm_srv/include/user.hrl").

-include("elements/records.hrl").
-include("setup.hrl").
-include("common.hrl").


%% FIXME: move ip to configs
-define(CC_IP, "188.40.111.156").
-define(CC_GATE, "https://sanalposprov.garanti.com.tr/servlet/gt3dengine").
-define(CC_CONFIRM_GATE, "https://sanalposprov.garanti.com.tr/VPServlet").
-define(CC_REQ_TIMEOUT, 15000).

-define(CC_MERCHANT_ID, "9328279").
-define(CC_PROVAUT_USER_PASS, "Dr5MANt378SAqbudur").
-define(CC_TERMINAL_ID, "10015100").
-define(CC_SECURE_KEY, "SIBOPeEU2WsC34kPpR").
-define(CC_EMAIL, "info@kakaranet.com").
-define(CC_SUCCESS_URL, "https://kakaranet.com/buy/credit_card/result/success").
-define(CC_FAILURE_URL, "https://kakaranet.com/buy/credit_card/result/failure").

-define(gv, proplists:get_value).

%% record to temporary store card info in session
-record(card_info, {no,
    expiration_month,
    expiration_year,
    holder_name %% name and surname
}).

main() ->
    IsLoggedIn = wf:user() /= undefined,
    %% bank request to our site, result = "failure"|"success"
    IsResultRequset = wf:q(result) /= undefined,

    if
        IsResultRequset ->
            process_result(wf:q(result));
        not IsLoggedIn ->
            wf:redirect_to_login(?_U("/login"));
        true ->
            main_authorized()
    end.

main_authorized() ->
    #template { file=code:priv_dir(nsw_srv)++"/templates/bare.html" }.

title() -> ?_T("Buy with Credit Card").


body() ->
    buy:shared_body().


%% from template
package_name() ->
    buy:package_name().

%% from template
package_info()->
    buy:package_info().

%% from template
form()->
    PurchaseId = rpc:call(?APPSERVER_NODE, nsm_srv_membership_packages, purchase_id, []),
    Package = buy:package(),
    TerminalId = ?CC_TERMINAL_ID,
    OrderId  = PurchaseId,

    Amount = Package#membership_package.amount * 100,
    InstallmentCount = "",
    ProvisionPassword = ?CC_PROVAUT_USER_PASS,
    MerchantId = ?CC_MERCHANT_ID,
    IP = ?CC_IP,
    Email = ?CC_EMAIL,
    Type = "sales",
    StoreKey = ?CC_SECURE_KEY,
    SecurityData = lists:concat([ProvisionPassword, add_zeros(9, TerminalId)]),
    SecurityDataHash = utils:sha_upper(SecurityData),

    Hash = utils:sha_upper(
             lists:concat([TerminalId, OrderId, Amount, ?CC_SUCCESS_URL,
                           ?CC_FAILURE_URL, Type, InstallmentCount,
                           StoreKey, SecurityDataHash])),
    %% restore card info from session. Case when payment failed and we have to
    %% show message to user on the right hand side (warning area)
    {PrevCardNo, PrevExpMonth, PrevExpYear, PrevHolderName} =
    case wf:session(card_info) of
        #card_info{} = C ->
            wf:session(card_info, undefined),
            {
                C#card_info.no, C#card_info.expiration_month,
                C#card_info.expiration_year,
                C#card_info.holder_name
            };
        _ ->
            {"", "", "", ""}
    end,

    #panel{class="tab-content", body=[
        #panel{class="payment-form", body=[
            #form{action=?CC_GATE, id="credit_card_form", body=[
                "<fieldset>",

                %% predefined fields
                #form_hidden{name=mode, text="PROD"},
                #form_hidden{name=secure3dsecuritylevel, text="3D"},
                #form_hidden{name=apiversion, text="v.0.1"},
                #form_hidden{name=terminalprovuserid, text="PROVAUT"},
                #form_hidden{name=terminaluserid, text="PROVAUT"},
                #form_hidden{name=terminalmerchantid, text=MerchantId},
                #form_hidden{name=txntype, text=Type},
                #form_hidden{name=txnamount, text=wf:to_list(Amount)},
                #form_hidden{name=txncurrencycode, text="949"},
                #form_hidden{name=txninstallmentcount, text=InstallmentCount},
                #form_hidden{name=orderid, text=OrderId},
                #form_hidden{name=terminalid, text=TerminalId},
                #form_hidden{name=successurl, text=?CC_SUCCESS_URL},
                #form_hidden{name=errorurl, text=?CC_FAILURE_URL},
                #form_hidden{name=customeripaddress, text=IP},
                #form_hidden{name=customeremailaddress, text=Email},
                #form_hidden{name=secure3dhash, text=Hash},

                #panel{class="col-l",body=[
                    #panel{class="row", body = [
                        #label{text= ?_T("Card No")},
                        #panel{class="text", body = #form_textbox{id=cardnumber, name=cardnumber, size=16, maxlength=16,  text = PrevCardNo}}
                    ]},
                    expiration_date_row(PrevExpMonth, PrevExpYear),
                    #panel{class="row", body = [
                        #label{text= ?_T("Cardholder name")},
                        #panel{class="text", body = #form_textbox{id=cardholdername, text = PrevHolderName}}
                    ]},
                    #panel{class="btn-holder", body = [
                        #cool_button{class="btn-submit", text=?_T("Buy"), postback={credit_card_clicked, PurchaseId}}
                    ]}
            ]},
                #panel{class="col-r", body = [
                    warning_area()
                ]},
                "</fieldset>"
            ]}
        ]}
    ]}.

warning_area() ->
    #panel{class="msg-box",
        body = [
            "<h3>",
            case wf:q(failure) of
                %% this is a general flow, user will input data.
                %% Just show message to control values
                undefined ->
                    [#image{class="png", image="/images/ico-14.png", style="width: 21px; height: 30px"},
                    "Girdiğiniz bilgileri lütfen kontrol edin.",
                    "</h3>",
                    #list{body=[
                        #listitem{body="Kredi kartı numaranızı girin. (Kart numaranız 16 hanelidir.)"},
                        #listitem{body="Kredi kartınızın üzerindeki son kullanma ay ve yılını giriniz."},
			#listitem{body="Kart sahibinin adını giriniz."}
                               ]}];
                EncodedReason ->
                    Reason = site_utils:base64_decode_from_url(EncodedReason),
                    [#image{class="png", image="/images/ico-18.png", style="width: 32px; height: 30px"},
                        ?_T("Payment failed"),
                        "</h3>",
                        #p{body=Reason}
                    ]
            end
    ]}.

expiration_date_row(SelectedMonth0, SelectedYear) ->
    ?PRINT({SelectedMonth0, SelectedYear}),
    SelectedMonth = case SelectedMonth0 of
        "0"++Digit ->
            Digit;
        Other ->
            Other
    end,

    {Y0, _, _} = erlang:date(),
    Y = Y0 rem 2000,
	Month0 = webutils:create_option_with_number({1,12}, SelectedMonth, ?_T("Month")),
    %% add 0 to month less than 10
    Month = [
        O#option{value = case V of
            undefined ->
                V;
            [_] ->
                [$0|V];
            _ ->
                V
                end}
    || O = #option{value = V} <- Month0],

    Year  = webutils:create_option_with_number({Y, Y+7}, SelectedYear, ?_T("Year")),
    #panel{class="row", body = [
        #label{text= ?_T("Expiration Date")},
        #panel{class="sel sel-2", body = [
            #form_dropdown{id=cardexpiredatemonth, name=cardexpiredatemonth, class="cs-3", options=Month}
        ]},
        #panel{class="sel sel-2", body = [
            #form_dropdown{id=cardexpiredateyear, name=cardexpiredateyear, class="cs-3", options=Year}
        ]}
    ]}.


process_result("failure")->
    RequestBridge = wf_context:request_bridge(),
    PostParams = RequestBridge:post_params(),
    OrderId = wf:q(orderid),
    MDStatus =  wf:q(mdstatus),
    ?PRINT({"FailureURL", PostParams}),
    Reason = case MDStatus of
        "5" ->
            ?_T("Cannot be Verified, cardholder wants to register later");
        "6" ->
            ?_T("3D Secure Error");
        "7" ->
            ?_T("System Error");
        "8" ->
            ?_T("Unknown Card No");
        "0" ->
            ?_T("Verification failure");
        Other ->
            EMessage = wf:q(mderrormessage),
            ?ERROR("Order=~p, unexpected mdstatus=~9999p. MDErrorMessage=~9999p",
                [OrderId, Other, EMessage]),
            ?_T("Sorry, unknown error happened. Please try again")
    end,

    ?ERROR("Order=~p, mdstatus=~9999p, ~s", [OrderId, MDStatus, Reason]),
    error_handler(OrderId, -2, Reason);

process_result("success")->
    HashDataIn = wf:q(hash),
    HashParamsIn = wf:q(hashparams),
    SCH = cc_security_check(HashDataIn, HashParamsIn, ?CC_SECURE_KEY),

    OrderId = wf:q(orderid),
    Amount = wf:q(txnamount),


    ?PRINT(SCH),
    MDStatus = wf:q(mdstatus),

    case MDStatus of
        "1" ->
            %% we use only Full 3D!
            ?INFO("Order=~p, mdstatus=~p. Full 3D", [OrderId, MDStatus]),
            make_provision_request(OrderId, Amount);

        %% other md statuses, errors
        _ ->
            Reason = case MDStatus of
                "2" -> ?_T("Card or Bank is not 3D");
                "3" -> ?_T("Bank is not registered to 3D (Cache)");
                "4" -> ?_T("Verification attempt");
                "5" -> ?_T("Cannot be Verified, cardholder wants to register later");
                "6" -> ?_T("3D Secure Error");
                "7" -> ?_T("System Error");
                "8" -> ?_T("Unknown Card No");
                "0" -> ?_T("Verification failure");
                _ ->   ?_T("Sorry, unknown error happened. Please try again")
            end,

            ?ERROR("Order=~p, mdstatus=~p, Reason: ~p", [OrderId, MDStatus, Reason]),
            error_handler(OrderId, -2, Reason)
    end.


make_provision_request(OrderId, Amount) ->
    HostAddress = ?CC_CONFIRM_GATE,

    {ok, XML} = construct_provision_xml(),

    case buy:http_request(post, "application/x-www-form-urlencoded", [],
                          HostAddress, "data="++buy:iolist_to_string(XML), 10000) of
        {ok, Res} ->
            Parsed = parse_xml(Res),
            Response = extract_response(Parsed),
            IntCode = (catch list_to_integer(?gv(code, Response))),
            case IntCode of
                %% all ok, store purchase and show success message
                0 ->
                    process_success(OrderId, [{amount, Amount}|Response]),
                    %% delete package from session. Purchase completed
                    buy:package(undefined),
                    %% redirect to dashboard to show success message
                    wf:redirect(?_U("/dashboard/buy/success"));

                OtherCode ->
                    Reason = error_descr(OtherCode),
                    error_handler(OrderId, OtherCode, Reason)
            end;

        {error, Reason} ->
            Reason = ?_T("Unable to connect to bank server.<br/> Please try later."),
            error_handler(OrderId, -1, Reason)
    end.

%% process case when all ok and purchase completed
process_success(OrderId, Response) ->
    case rpc:call(?APPSERVER_NODE, nsm_srv_membership_packages,
        get_purchase, [OrderId]) of
        {ok, P} ->
            OldInfo = P#membership_purchase.info,
            %% fill info with values from bank
            Info = OldInfo#pi_credit_card{
                cardnumber_masked = ?gv(card_number_masked, Response),
                retref_num = ?gv(retref_num, Response),
                prov_date = ?gv(prov_date, Response),
                auth_code = ?gv(auth_code, Response)
            },
            %% store full info in purchase
            ok = rpc:call(?APPSERVER_NODE, nsm_srv_membership_packages,
                set_purchase_info, [OrderId, Info]),

            %% FIXME: move info from info field to state info?
            ok = rpc:call(?APPSERVER_NODE, nsm_srv_membership_packages,
                set_purchase_state, [OrderId, ?MP_STATE_DONE, []]);

        {error, Reason} ->
            ?ERROR("purchase not found: OrderId=~p, Amount=~p. Reason: ~9999p ",
                [OrderId, ?gv(amount, Response), Reason]),
            ok
    end.

process_failure(OrderId, IntCode, Reason) when
    IntCode == 4;    %% seize the card
    IntCode == 7;    %% seize the card
    IntCode == 18;   %% card is blocked don't try again
    IntCode == 34;   %% possibly stolen card
    IntCode == 37;   %% call bank security department
    IntCode == 41;   %% lost card, seize the card
    IntCode == 43;   %% stolen card, seize the card
    IntCode == 58;   %% you don't let to do this operation
    IntCode == 62;   %% limited card, only valid in customer country
    IntCode == 63;   %% you are not authorized to do this
    IntCode == 75    %% password entry limit exceed
    ->
    User = case rpc:call(?APPSERVER_NODE, nsm_srv_membership_packages, get_purchase, [OrderId]) of
        {ok, Purchase} ->
            %% FIXME: add user blocking
            wf:logout(),
            UserId = Purchase#membership_purchase.user_id,
            ?ERROR("payment ~p critical error: ~p, User ~p will be blocked.", [OrderId, IntCode, UserId]),
            UserId;
        _ ->
            ?ERROR("unexpected payment ~p critical error: ~p. Purchase not found.
                Logged in user ~p will be blocked ", [OrderId, IntCode, wf:user()]),
            wf:user()
    end,
    ok = rpc:call(?APPSERVER_NODE, nsm_srv_membership_packages,
        set_purchase_state, [OrderId, ?MP_STATE_FAILED,
            [[{code, IntCode}, {reason, Reason}]]
    ]),
    BlockedUser = User#user{status = banned},
    rpc:call(?APPSERVER_NODE, zealot_db, put, [BlockedUser]),
    wf:logout(),

    Message = ?_TS("Your account is blocked.<br/> Reason: $reason$ <br/> Please, contact with administration to unblock account.",
        [{reason, Reason}]),
    EncodedMessage = encode_reason(Message),
    wf:redirect(?_U("/index/message/")++EncodedMessage);
process_failure(OrderId, IntCode, Reason) ->
    ok = rpc:call(?APPSERVER_NODE, nsm_srv_membership_packages,
        set_purchase_state, [OrderId, ?MP_STATE_FAILED,
            [[{code, IntCode}, {reason, Reason}]]
    ]).

error_handler(OrderId, Code, Reason) ->
    process_failure(OrderId, Code, Reason),
    EncodedReason = encode_reason(Reason),
    %% redirect to credit card page, to show error reason
    wf:redirect(?_U("/buy/credit_card/failure/")++EncodedReason).

construct_provision_xml() ->
    Mode = wf:q(mode),
    Version = wf:q(apiversion),
    TerminalId = wf:q(clientid),
    ProvisionPassword = ?CC_PROVAUT_USER_PASS,
    ProvUserId = wf:q(terminalprovuserid),
    UserId = wf:q(terminaluserid),
    MerchantId = wf:q(terminalmerchantid),
    IP = wf:q(customeripaddress),
    Email = wf:q(customeremailaddress),
    OrderId = wf:q(orderid),
    Amount = wf:q(txnamount),
    CurrencyCode = wf:q(txncurrencycode),
    InstallmentCount = wf:q(txninstallmentcount),
    TxnType = wf:q(txntype),
    AuthCode = wf:q(cavv),
    SecurityLevel = wf:url_encode(wf:q(eci)),
    TxnId = wf:url_encode(wf:q(xid)),
    MD= wf:url_encode(wf:q(md)),
    SecurityData = utils:sha_upper(ProvisionPassword++add_zeros(9, TerminalId)),
    HashData = utils:sha_upper(OrderId++TerminalId++Amount++SecurityData),

    TplParams = [
        {mode, Mode},
        {version, Version},
        {prov_user_id, ProvUserId},
        {hash_data, HashData},
        {user_id, UserId},
        {terminal_id, TerminalId},
        {merchant_id, MerchantId},
        {ip, IP},
        {email, Email},
        {order_id, OrderId},
        {txn_type, TxnType},
        {installment, InstallmentCount},
        {amount, Amount},
        {currency, CurrencyCode},
        {auth_code, AuthCode},
        {security_level, SecurityLevel},
        {md, MD},
        {txn_id, TxnId}
    ],

    {ok, XML} = '3D_inquiry_request_dtl':render(TplParams),
    {ok, XML}.


%%
%% Events
%%

event({credit_card_clicked, PurchaseId}) ->
    Package = buy:package(),
    HolderName = wf:q(cardholdername),
    HolderSurname = wf:q(cardholdersurname),
    %% if credit card button is clicked - user redirected to bank site.
    %% Start purchase processing.
    MP = #membership_purchase{id = PurchaseId,
        user_id = wf:user(),
        membership_package = Package,
        %% fill known fields, after server reply will fill rest
        info = #pi_credit_card{
          cardholder_name = HolderName,
          cardholder_surname = HolderSurname
        }
    },

    ?INFO("credit card button clicked. Purchase id: ~p, Package id: ~p",
          [PurchaseId, Package#membership_package.id]),

    CI = #card_info{no = wf:q(cardnumber),
        expiration_month = wf:q(cardexpiredatemonth),
        expiration_year = wf:q(cardexpiredateyear),
        holder_name = wf:q(cardholdername)},

    %% put info to session to fill needed fields when error occures
    wf:session(card_info, CI),

    %% purchase will have state 'started'
    {ok, PurchaseId} = rpc:call(?APPSERVER_NODE, nsm_srv_membership_packages,
                                add_purchase, [MP]),

    buy:submit_form(credit_card_form);

event(Event) ->
    buy:event(Event).



cc_security_check(Hash, HashParams, _StoreKey) when Hash == undefined;
                                             HashParams == undefined ->
    false;
cc_security_check(Hash, HashParams, StoreKey) ->
    RequestBridge = wf_context:request_bridge(),
    PostParams = RequestBridge:post_params(),
    io:format("PARAMS: ~p~n~n", [PostParams]),
    Params = string:tokens(HashParams, ":"),
    io:format("params: ~p~n", [Params]),
    Values = [case wf:q(P) of undefined -> ""; V -> V end || P <- Params],
    io:format("Vals: ~p~n", [Values++[StoreKey]]),
    DigestData = lists:concat(Values),
    HashCalculated = utils:sha_upper(DigestData),
    Encoded = base64:encode_to_string(HashCalculated),
    io:format("hashparamval: ~p~n", [wf:q(hashparamsval)]),
    io:format("DigestData: ~p~n Hash: ~p~n Hash: ~p~n Hash: ~p~n", [DigestData, HashCalculated, Encoded, Hash]),
    HashCalculated == Hash.

%% add zeros to the head of string if string is shorter than 1-st arg
add_zeros(N, String) ->
    Len = length(String),
    if Len < N ->
            Zeros = [$0 || _ <- lists:seq(1, N - Len)],
            Zeros ++ String;
       true ->
            String
    end.

%% return nested list of parsed elements with args. For example:
%% <el>
%%    <nested id="myid">1</nested>
%% </el>
%% Will be parsed to {el, [], [" ",{nested,[{id,"myid"}], ["2"]}," "]}
%%
parse_xml(XMLString) ->
    Options = [{space,normalize},
               {encoding,"utf-8"}],
    {XML,_Rest} = xmerl_scan:string(XMLString,Options),
    xmerl_lib:simplify_element(XML).

extract_response({'GVPSResponse',[],Data}) ->
    lists:flatten(extract_response('GVPSResponse', Data)).

extract_response(_, []) -> [];

extract_response(H = 'GVPSResponse', [{'Order', [], OrderData}|Rest]) ->
    [ extract_response('Order', OrderData) | extract_response(H, Rest)];
extract_response(H = 'GVPSResponse', [{'Transaction', [], Data}|Rest]) ->
    [ extract_response('Transaction', Data) | extract_response(H, Rest)];

extract_response(H = 'Order', [{'OrderID',[],[Id]}| Rest]) ->
    [{order_id, Id} | extract_response(H, Rest)];

extract_response(H = 'Transaction', [{'Response', [], Data}|Rest]) ->
    [ extract_response('Response', Data) | extract_response(H, Rest)];
extract_response(H = 'Transaction', [{'AuthCode',[], ACode} | Rest]) ->
    [{auth_code, lists:flatten(ACode)} | extract_response(H, Rest)];
extract_response(H = 'Transaction', [{'ProvDate',[], Date} | Rest]) ->
    [{prov_date, lists:flatten(Date)} | extract_response(H, Rest)];
extract_response(H = 'Transaction', [{'CardNumberMasked',[], CNum} | Rest]) ->
    [{card_number_masked, lists:flatten(CNum)} | extract_response(H, Rest)];
extract_response(H = 'Transaction', [{'RetrefNum',[], RNum} | Rest]) ->
    [{retref_num, lists:flatten(RNum)} | extract_response(H, Rest)];


extract_response(H = 'Response', [{'ReasonCode',[],[Code]} | Rest]) ->
    [{reason_code, Code} | extract_response(H, Rest)];
extract_response(H = 'Response', [{'Code',[],[Code]} | Rest]) ->
    [{code, Code} | extract_response(H, Rest)];
extract_response(H = 'Response', [{'Message',[], Msg} | Rest]) ->
    [{message, lists:flatten(Msg)} | extract_response(H, Rest)];
extract_response(H = 'Response', [{'ErrorMsg',[], Msg} | Rest]) ->
    [{error_message, lists:flatten(Msg)} | extract_response(H, Rest)];
extract_response(H = 'Response', [{'SysErrMsg',[], Msg} | Rest]) ->
    [{sys_error_message, lists:flatten(Msg)} | extract_response(H, Rest)];

extract_response(H, [_ | Rest]) ->
    extract_response(H, Rest).


encode_reason(Reason) ->
    site_utils:base64_encode_to_url(Reason).

%% Provision error codes mapping

error_descr(1) -> ?_T("Getaprovisionfrombank");
error_descr(2) -> ?_T("(VISA) Getaprovisionfrombank(VISA)");
error_descr(3) -> ?_T("MemberCOmpanyCategoryCodeFalse");
error_descr(4) -> ?_T("Seize the card!!!");
error_descr(5) -> ?_T("Process not confirmed");
error_descr(6) -> ?_T("Your request is not accepted");
error_descr(7) -> ?_T("Seize the card!!!");
error_descr(8) -> ?_T("Check the identity and perform a transaction");
error_descr(9) -> ?_T("Ask from customer.");
error_descr(11) -> ?_T("Transaction done(VIP)");
error_descr(12) -> ?_T("Invalid operation");
error_descr(13) -> ?_T("Invalid amount");
error_descr(14) -> ?_T("Card number is false");
error_descr(15) -> ?_T("Bank name couldn't be found");
error_descr(16) -> ?_T("Try again tomorrow.");
error_descr(17) -> ?_T("Operation cancelled");
error_descr(18) -> ?_T("Do not try again.");
error_descr(19) -> ?_T("Request provision one more time.");
error_descr(21) -> ?_T("Operation couldn't be cancelled");
error_descr(25) -> ?_T("There is no such kind of information");
error_descr(28) -> ?_T("Original rejected/File is out of service");
error_descr(29) -> ?_T("(original couldn't found)");
error_descr(30) -> ?_T("Message format is wrong");
error_descr(31) -> ?_T("Issuersign is not on");
error_descr(32) -> ?_T("Operation partially done");
error_descr(33) -> ?_T("Card has expired! Seize the card.");
error_descr(34) -> ?_T("Possibly stolen card!!! Seize the card.");
error_descr(36) -> ?_T("Limited card!!! Seize the card.");
error_descr(37) -> ?_T("Please call bank security department");
error_descr(38) -> ?_T("Password entry limit exceeded!!! Seize the card.");
error_descr(39) -> ?_T("Credit account undefined");
error_descr(41) -> ?_T("Lost card!!! Seize the card.");
error_descr(43) -> ?_T("Stolen Card!!! Seize the card.");
error_descr(51) -> ?_T("Account is not suficient");
error_descr(52) -> ?_T("Cheque account undefined");
error_descr(53) -> ?_T("Account undefined");
error_descr(54) -> ?_T("Card settlement date expired");
error_descr(55) -> ?_T("Password is wrong");
error_descr(56) -> ?_T("This card is not exist");
error_descr(57) -> ?_T("Card owner can't do this operation");
error_descr(58) -> ?_T("You don't let to do this operation");
error_descr(61) -> ?_T("Withdrawal limit exceeded");
error_descr(62) -> ?_T("Limited card/Only valid in customer country");
error_descr(63) -> ?_T("You are not authorized to do this");
error_descr(65) -> ?_T("Daily operation limit exceeded");
error_descr(68) -> ?_T("Cancel the operation");
error_descr(75) -> ?_T("Password entry limit exceeded");
error_descr(76) -> ?_T("Entry limit exceeded.");
error_descr(77) -> ?_T("Incompatible data has been taken with original");
error_descr(78) -> ?_T("AccountBalanceNotAvailable.");
error_descr(80) -> ?_T("False date / Network error");
error_descr(81) -> ?_T("Encryption / Foreign network error");
error_descr(82) -> ?_T("Issuer couldn't replied.");
error_descr(83) -> ?_T("Password can't be verified");
error_descr(85) -> ?_T("Account verified");
error_descr(86) -> ?_T("Password can't be verified");
error_descr(88) -> ?_T("Encryption error");
error_descr(89) -> ?_T("Authentication error");
error_descr(90) -> ?_T("End of day operations in progress");
error_descr(91) -> ?_T("Can't be reached to card owner's bank");
error_descr(92) -> ?_T("Operation can't be directed to proper section");
error_descr(93) -> ?_T("Your transaction cancelled due to judicial reasons");
error_descr(94) -> ?_T("DuplicateTransmission");
error_descr(95) -> ?_T("Daily sum is false / Cancel rejected");
error_descr(96) -> ?_T("System error");
error_descr(98) -> ?_T("DuplicateReversal.");
error_descr(_)  -> ?_T("Sorry, unknown error happened. Please try again").
