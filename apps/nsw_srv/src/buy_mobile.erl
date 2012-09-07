%%----------------------------------------------------------------------
%% @author Vladimir Baranov <vladimir.b.n.b@gmail.com>
%% @copyright Paynet Internet ve Bilisim Hizmetleri A.S. All Rights Reserved.
%% @doc
%% Mobile payment page
%% @end
%%---------------------------------------------------------------------

-module(buy_mobile).

-compile(export_all).


-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("nsm_srv/include/membership_packages.hrl").
-include_lib("nsm_srv/include/user.hrl").

-include("elements/records.hrl").
-include("setup.hrl").
-include("common.hrl").

main() ->
    User = wf:user(),
    case User of
        undefined ->
            wf:redirect_to_login(?_U("/login"));
         _->
            main_authorized()
    end.

main_authorized() ->
    #template { file=code:priv_dir(nsw_srv)++"/templates/bare.html" }.

title() -> ?_T("Buy with mobile").


body() ->
    buy:shared_body().


%% from template
package_name() ->
    buy:package_name().

%% from template
package_info()->
    buy:package_info().

form()->
    PurchaseId = rpc:call(?APPSERVER_NODE, nsm_srv_membership_packages, purchase_id, []),
    Package = buy:package(),

    ?INFO("Package/Purchase: ~p ~p",[PurchaseId,Package]),

    #panel{class="tab-content", body=
        #panel{class="payment-form", body=
            #panel{
                id = form_holder,
                body =
                #panel{class="col-l",body=[
%                    bank_info(),
                    #panel{class="btn-holder", body = [
                        #submit{class="btn-submit", text=?_T("Buy"), postback={buy_clicked, PurchaseId}}
                ]}]
                }
            }
        }
	  }.
%
    
%    ?_T("Mobile payments. Coming soon.").

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
    wf:redirect("http://www.mikro-odeme.com/sale-api/tr/step1.aspx?partner=17121&product=" ++ wf:to_list(Product)++ "&mpay=" ++ PurchaseId);

event(Event)->
    buy:event(Event).

