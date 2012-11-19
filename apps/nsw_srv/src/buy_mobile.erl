-module(buy_mobile).
-author('Maxim Sokhatsky <maxim@synrc.com>').
-compile(export_all).

-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("nsm_db/include/membership_packages.hrl").
-include_lib("nsm_db/include/user.hrl").

-include("elements/records.hrl").
-include("setup.hrl").
-include("common.hrl").

main() ->
    User = wf:user(),
    case User of
        undefined ->
            wf:redirect_to_login(?_U("/login"));
         _->
            ?INFO("main"),
    PurchaseId = wf:q(mpy),
    PId = wf:q(pid),
    OrderGUID = wf:q(order),
            ?INFO("PID/Order: ~p",[{PId,OrderGUID}]),
            main_authorized()
    end.

main_authorized() ->
    #template { file=code:priv_dir(nsw_srv)++"/templates/bare.html" }.

title() -> ?_T("Buy with mobile").

body() ->
    ?INFO("body"),
    case wf:q("__submodule__") of
         "basarili" -> process_result(success);
         "basarisiz" -> process_result(failure);
         _ -> buy:shared_body()
    end.

package_name() ->
    buy:package_name().

package_info()->
    buy:package_info().

process_result(success) -> 
    PurchaseId = wf:q(mpy),
    PId = wf:q(pid),
    OrderGUID = wf:q(order),
    Referer = wf:header(referer),

    ?INFO("Mobile Operator Income URL: ~p",[Referer]),

    Site = case Referer of
         undefined -> "local";
         _ -> {Proto,No,S,Port,Page,_} = http_uri:parse(Referer), S
    end,

    case ((Site =:= "www.mikro-odeme.com") orelse true) of
         true -> case nsm_membership_packages:get_purchase(PurchaseId) of
                {ok, Purchase} ->
                    nsx_msg:notify(["purchase", "user", wf:user(), "set_purchase_state"], {element(2,Purchase), done, mobile}),                           
                    wf:redirect("/profile/account");

                _ -> "Purchase Not Found"
            end;
         false -> "Non Authorized Access"
    end;

process_result(failure) ->
    "Error".

form()->
    PurchaseId = nsm_membership_packages:purchase_id(),
    Package = buy:package(),

    ?INFO("Package/Purchase: ~p ~p",[PurchaseId,Package]),

    #panel{class="tab-content", body=
        #panel{class="payment-form", body=
            #panel{
                id = form_holder,
                body =
                #panel{class="col-l",body=[
                    #panel{class="btn-holder", body = [
                        #submit{class="btn-submit", text=?_T("Buy"), postback={buy_clicked, PurchaseId}}
                ]}]
                }
            }
        }
	  }.

event({buy_clicked, PurchaseId}) ->
    ?INFO("Buy Clicked: ~p", [PurchaseId]),
    Package = buy:package(),
    Product = case Package#membership_package.id of
          64 -> 5086;
          65 -> 5087;
          66 -> 5088;
          67 -> 5089;
          68 -> 5090;
          69 -> 5091;
          70 -> 5092;
          71 -> 5093;
          72 -> 5128;
          _ -> 5128

    end,

    MP = #membership_purchase{id = PurchaseId,
        user_id = wf:user(),
        membership_package = Package,
        info = mobile
    },

    nsx_msg:notify(["purchase", "user", wf:user(), "add_purchase"], {MP}),

    wf:redirect("http://www.mikro-odeme.com/sale-api/tr/step1.aspx?partner=17121&product=" ++ 
        wf:to_list(Product)++ "&mpay=" ++ PurchaseId);

event(Event)->
    buy:event(Event).

