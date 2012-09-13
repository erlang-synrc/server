%%----------------------------------------------------------------------
%% @author Vladimir Baranov <vladimir.b.n.b@gmail.com>
%% @copyright Paynet Internet ve Bilisim Hizmetleri A.S. All Rights Reserved.
%% @doc
%% Paypal purchases page
%% @end
%%---------------------------------------------------------------------

-module(buy_paypal).


-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("nsm_srv/include/membership_packages.hrl").
-include_lib("nsm_srv/include/user.hrl").

-include("elements/records.hrl").
-include("setup.hrl").
-include("common.hrl").

%% test credentials
%-define(PP_GATE, "https://www.sandbox.paypal.com/cgi-bin/webscr").
%-define(PP_COMPANY, "kakaranet@ukr.net").
%-define(PP_PDT_TOKEN, "ZmcdgHQJFz6YUxtHaIFCv9A_lmJjuQxF8rgjkDu7ZPyNG6xo5P0cTfL5Y70").
% password:  "kakara20"

-define(PP_GATE,           "https://www.paypal.com/cgi-bin/webscr").
-define(PP_COMPANY,        "paypal@kakaranet.com").
-define(PP_RECEIVER_EMAIL, "ahmettez@kakaranet.com").
-define(PP_PDT_TOKEN,      "6WZATbwyNXvCY3HpL6nLq8Rkml83dkC1G011cpT6okHnQRV-q7ud8wi5TVq").

%% Payment statuses
-define(PP_PAYMENT_COMPLETED, "Completed").
-define(PP_PAYMENT_PENDING,   "Pending").

-define(gv, proplists:get_value).

%% record to be decoded and send to payapal. Later, if we will receive
%% unexpected confirmation from paypal we can easely restore infromation
%% about order
-record(order_info, {package,
                     time,
                     user}).
%%
%% Exported Functions
%%
-compile(export_all).


main() ->
    User = wf:user(),
    %% we've added 'ipn' param to notify url or payment options.
    %% So when paypal will send notifications we can process them here.
    IsIPN = wf:q('__submodule__') == "notification",

    if
       IsIPN ->
            process_ipn_notification();

        User /= undefined ->
            main_authorized();

        true->
            wf:redirect_to_login(?_U("/login"))
    end.

main_authorized() ->
    case wf:q('__submodule__') of
        "completed" ->
            process_completed();
        _ ->
            #template { file=code:priv_dir(nsw_srv)++"/templates/bare.html" }
    end.

title() -> ?_T("Buy with Paypal").


body() ->
    buy:shared_body().


%% from template
package_name() ->
    buy:package_name().

%% from template
package_info()->
    buy:package_info().

%% from template
form() ->

    Package = buy:package(),
    No = Package#membership_package.no,
    Price = Package#membership_package.amount,

    UserInfo = wf:session(user_info),
    Username = UserInfo#user.name,
    Surname  = UserInfo#user.surname,
    Email = case UserInfo#user.email of
        undefined ->
            "";
        EM ->
            EM
    end,

    PurchaseId = rpc:call(?APPSERVER_NODE, nsm_membership_packages,
                          purchase_id, []),

    CancelUrl = lists:concat([?HTTP_ADDRESS, ?_U("/buy/paypal/cancelled")]),
    ReturnUrl = lists:concat([?HTTP_ADDRESS, ?_U("/buy/paypal/completed")]),
    %% set url to 80 port with redirection to us (if needed)
    %% NB: do not provide url translation for notification address
    NotifyUrl = lists:concat([add_redirection(?HTTP_ADDRESS) , "/buy/paypal/notification"]),

    %% encode information about order and add it to request. This will help
    %% to understand what heppens if we will get unexpected confirmation request
    %% from PP
    EncodedInfo = buy:encode_info(#order_info{package = Package,
                                              time = now(),
                                              user = UserInfo#user.username}),


    %% rm - return method, 2 - post with all variables
    [
        warning_area(),
        #panel{class = "tab-content", body =
            #panel{class = "payment-form", body =
                #form{id = paypal_form,  method = "post", action = ?PP_GATE, body = [
                    #form_hidden{name = cmd,             text = "_xclick"},
                    #form_hidden{name = business,        text = ?PP_COMPANY},
                    #form_hidden{name = item_name,       text = buy:package_description(No)},
                    #form_hidden{name = item_number,     text = wf:to_list(No)}
                    #form_hidden{name = custom,          text = EncodedInfo},
                    #form_hidden{name = invoice,         text = PurchaseId},
                    #form_hidden{name = currency_code,   text = "TRY"},
                    #form_hidden{name = amount,          text = wf:to_list(Price)},
                    #form_hidden{name = return,          text = ReturnUrl},
                    #form_hidden{name = cancel_return,   text = CancelUrl},
                    #form_hidden{name = notify_url,      text = NotifyUrl},
                    #form_hidden{name = no_shipping,     text = "1"},
                    #form_hidden{name = no_route,        text = "1"},
                    #form_hidden{name = rm,              text = "2"},

                    #form_hidden{name = first_name,      text = Username},
                    #form_hidden{name = second_name,     text = Surname},
                    #form_hidden{name = email,           text = Email},

                    #link{postback = {paypal_clicked,PurchaseId}, body = [
                        #image{image = "/images/btn_buynowCC_LG.gif"}
                    ]}

                ]}
            }
        }
    ].


warning_area() ->
    %% reason can be check_failed or verification_failed,
    %% Just show general error message to user, with transaction info
    Warning = case wf:q(failed) of
        undefined ->
            [];
        EncodedTxnId ->
            TxnId = site_utils:base64_decode_from_url(EncodedTxnId),
            TxnDetails = #list{body=[
                #listitem{text=[?_T("Paypal transaction Id"),":", TxnId]}
            ]},
            #notice{type=message, position=left, title=?_T("Paypal error"),
                body=[#panel{body=[
                    #p{body=?_T("We've got error from paypal. To get information, please, contact with administration.")},
                    #p{body=?_T("Below is Your paypal transaction id.")},
                    TxnDetails
                ]}
            ]}
    end,

    #panel{id = warning_area, body = Warning}.



process_ipn_notification() ->
    {GETParams, POSTParams} = buy:req_params(),
    VerificationResult = verify_ipn(POSTParams),

    ?INFO("PAYPAL:IPN:: GETParams=~p, POSTParams=~p, VerifRes=~p",
          [GETParams, POSTParams, VerificationResult]),

    case VerificationResult of
        %% verification successfull, now we can process payment
        true ->
            process_payment(POSTParams);
        %% if verification failed - skip payment processing
        false ->
            ?ERROR("PAYPAL:IPN: verification failed"),
            ok
    end,
    "".


%% notification type - ipn or pdt
process_payment(Params) ->
    %% our internal txn id
    PurchaseId = ?gv("invoice", Params),
    %% external txn id
    TxnId = ?gv("txn_id", Params),
    {PurchaseState, _StateInfo} = get_internal_status(Params),

    case rpc:call(?APPSERVER_NODE, nsm_membership_packages,
                  get_purchase, [PurchaseId]) of
        %% if record in our base, continue processing
        {ok, Purchase} ->
            case Purchase of
                %% this transaction is already processed. Skip
                #membership_purchase{external_id = TxnId, state = PurchaseState} ->
                    ?DBG("PAYPAL: purchase already processed. ~p", [Purchase]),
                    ok;
                _ ->
                    process_registered_payment( Purchase, Params)
            end;

        _ ->
            ?ERROR("PAYPAL: untracked purchase: Id: ~p. Received params: ~p", [Params]),
            {error, untracked}
    end.


process_registered_payment(Purchase, Params) ->
    %% check received and expected fields values
    IsPurchaseOK = check_payment_info(Purchase, Params),
    %% id of the purchase given by the paypal. Used to prevent duplicate process
    %% of the notification
    TxnId = ?gv("txn_id", Params),
    PurchaseId = Purchase#membership_purchase.id,

    if
        IsPurchaseOK ->
            ?INFO("PAYPAL: payment info checked: Params: ~p", [Params]),
            {NewState, Info} = get_internal_status(Params),
            %% store external id
            ok = rpc:call(?APPSERVER_NODE, nsm_membership_packages,
                set_purchase_external_id, [PurchaseId, TxnId]),
            %% change purchase state
            ok = rpc:call(?APPSERVER_NODE, nsm_membership_packages,
                          set_purchase_state, [PurchaseId, NewState, Info]);

        true ->
            ?ERROR("PAYPAL: check payment failed. Purchase: ~p, Params: ~p", [Purchase, Params]),
            {error, check_failed}
    end.


%% send back inforamtion received from paypal, to verify it.
verify_ipn(POSTParams) ->
    Url = lists:concat([?PP_GATE, "?", "cmd=_notify-validate"]),
    Encoded = mochiweb_util:urlencode(POSTParams),
    case buy:post(Url, Encoded) of
        {ok, "VERIFIED"} ->
            true;
        {ok, "INVALID"} ->
            ?WARNING("PAYPAL:IPN: invalid IPN received from payapal"),
            false;
        %% some error occured, so ipn is not validated
        Other ->
            ?WARNING("PAYPAL:IPN: error during validation: ~p", [Other]),
            false
    end.

verify_pdt() ->
    Tx = wf:q(tx),
    VerificationDataRaw = [
        {cmd, "_notify-synch"},
        {tx, Tx},
        {at, ?PP_PDT_TOKEN}
    ],

    VerificationData = mochiweb_util:urlencode(VerificationDataRaw),
    ?PRINT(VerificationData),

    case buy:http_request(post, "application/x-www-form-urlencoded", [],
        ?PP_GATE, VerificationData, 10000) of
        {ok, "SUCCESS"++Body} ->
            {true, parse_pdt_response(Body)};

        {ok, "FAIL"++Fail} ->
            ?ERROR("PAYPAL:PDT: verification failed. Reply: ~p", [Fail]),
            {false, Tx};
        {error, Reason} ->
            ?ERROR("PAYPAL:PDT: verification post failed. Reason: ~p", [Reason]),
            {false, Tx}
    end.

check_payment_info(Purchase, Params) ->
    %% setup expected and received pairs
    RawPurchaseInfo = ?gv("custom", Params),
    PurchaseInfo = buy:decode_info(RawPurchaseInfo),

    %% package information from encoded field
    RcvdPkg  = PurchaseInfo#order_info.package,
    ExpctdPkg = Purchase#membership_purchase.membership_package,

    %% money amount
    RcvdAmnt  = round(buy:list_to_number(?gv("mc_gross", Params))),
    ExpctdAmnt = ExpctdPkg#membership_package.amount,

    %% mail of the payment receiver
    RcvdEmail  = ?gv("receiver_email", Params),
    ExpctdEmail = ?PP_RECEIVER_EMAIL,

    if RcvdEmail /= ExpctdEmail ->
            ?WARNING("validation: email ~p is not valid. Expect ~p",
                     [RcvdEmail, ExpctdEmail]),
            false;

       RcvdAmnt /= ExpctdAmnt ->
            ?WARNING("validation: amount ~p is not valid. Expect ~p",
                     [RcvdAmnt, ExpctdAmnt]),
            false;

       RcvdPkg /= ExpctdPkg ->
            ?WARNING("validation: package ~p is not valid. Expect ~p",
                     [RcvdPkg, ExpctdPkg]),
            false;

       %% all ok - purchase valid
       true ->
            true
    end.

%% purchase completed. Process results.
process_completed() ->
    RequestBridge = wf_context:request_bridge(),
    QueryParams = RequestBridge:query_params(),
    PostParams = RequestBridge:post_params(),
    ?PRINT({QueryParams, PostParams}),

    case verify_pdt() of
        {true, PDTParams} ->
            case process_payment(PDTParams) of
                ok ->
                    %% delete package from session. Purchase completed
                    buy:package(undefined),
                    wf:redirect(?_U("/dashboard/buy/success"));
                {error, untracked} ->
                    wf:redirect(?_U("/dashboard"));
                {error, check_failed} ->
                    error_handler(?gv("txn_id", PDTParams))
            end;
        {false, TxnId} ->
            error_handler(TxnId)
    end.


%% when user press paypal button - he will be redirected to paypal page,
%% but also we will know about this. And can start transaction tracking
add_click_handler(Id)->
    wf:wire(paypal_btn, #event{type=click, postback = {paypal_clicked, Id}}).


error_handler(TxnId) ->
    Encoded = site_utils:base64_encode_to_url(TxnId),
    wf:redirect(?_U("/buy/paypal/failed/")++Encoded).

%% add redirection to have abimity to test paypal in ports
%% other than 80. To this work, nginx have to be properly configured.
%% change http to https.
add_redirection(Address) ->
    case  string:tokens(Address, ":") of
        %% our server url has port, so have to add rewrite directive
        [_Proto, Url, Port] ->
            lists:concat(["https", ":", Url, "/port-rewrite/", Port]);
        [_Proto, Url]->
            lists:concat(["https", ":", Url]);
        _ ->
            Address
    end.

%%
%% Events
%%

event({paypal_clicked, PurchaseId}) ->
    Package = buy:package(),
    %% if paypal button is clicked - user redirected to paypal.
    %% Start purchase processing.
    MP = #membership_purchase{id = PurchaseId,
                              user_id = wf:user(),
                              membership_package = Package},

    ?INFO("paypal button clicked. Purchase id: ~p, Package id: ~p",
          [PurchaseId, Package#membership_package.id]),

    %% purchase will have state 'added'
    {ok, PurchaseId} = rpc:call(?APPSERVER_NODE, nsm_membership_packages,
                                add_purchase, [MP]),
    buy:submit_form(paypal_form);

event(Event) ->
    buy:event(Event).

parse_pdt_response(Response) ->
    KeyValues = string:tokens(Response, "\n"),
    URLString = string:join(KeyValues, "&"),
    mochiweb_util:parse_qs(URLString).


get_internal_status(Params) ->
    get_internal_status(?gv("payment_status", Params), Params).

get_internal_status(?PP_PAYMENT_COMPLETED, _Params) ->
    {?MP_STATE_DONE, system_change};
get_internal_status(?PP_PAYMENT_PENDING, Params)->
    PendingReason = {system_change,
        ?gv("pending_reason", Params)},
    {?MP_STATE_PENDING, PendingReason};
get_internal_status(Other, _Params) ->
    ?WARNING("PAYPAL: Unknown payment state: ~p", [Other]),
    {?MP_STATE_UNKNOWN, Other}.

