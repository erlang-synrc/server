-module(buy_wire_transfer).
-author('Vladimir Baranov <vladimir.b.n.b@gmail.com>').
-copyright('Paynet Internet ve Bilisim Hizmetleri A.S. All Rights Reserved.').
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
            main_authorized()
    end.

main_authorized() ->
    #template { file=code:priv_dir(nsp_srv)++"/templates/bare.html" }.

title() -> ?_T("Buy with wire transfer").


body() ->
    buy:shared_body().


%% from template
package_name() ->
    buy:package_name().

%% from template
package_info()->
    buy:package_info().

form()->
    PurchaseId = nsm_membership_packages:purchase_id(),
    #panel{class="tab-content", body=
        #panel{class="payment-form", body=
            #panel{
                id = form_holder,
                body =
                #panel{class="col-l",body=[
                    bank_info(),
                    #panel{class="btn-holder", body = [
                        #submit{class="btn-submit", text=?_T("Buy"), postback={buy_clicked, PurchaseId}}
                ]}]
                }
            }
        }
    }.


event({buy_clicked, PurchaseId}) ->
    Package = buy:package(),
    %% if credit card button is clicked - user redirected to bank site.
    %% Start purchase processing.
    MP = #membership_purchase{id = PurchaseId,
        user_id = wf:user(),
        membership_package = Package
    },

    ?INFO("Wire purchase. Purchase id: ~p, Package id: ~p",
          [PurchaseId, Package#membership_package.id]),

    %% purchase will have state 'started'
    nsx_msg:notify(["purchase", "user", wf:user(), "add_purchase"], {MP}), 
    OrderForm = order_form(PurchaseId),

    wf:update(form_holder, OrderForm);

event({print_clicked, PurchaseId}) ->
    ?INFO("~w printing invoice ~p", [wf:user(), PurchaseId]),
    wf:wire(#script{script="window.print()"});

event(Event)->
    buy:event(Event).


order_form(PurchaseId) ->
    UserInfo = webutils:user_info(),
    UserName = case {UserInfo#user.name, UserInfo#user.surname} of
        {"", ""} ->
            UserInfo#user.username;
        %% if one of the name or surname given - use them?
        {Name, Surname} ->
            Name++" "++Surname
    end,


    LeftPanelContent =
    ?_TS("Dear, $name$, </br>
        Your order number is <b>$order_id$</b><br/>
        You have to do wire transfer in 3 days to our account.<br/>
        You can login to our system and play games but you can't benefit <br/>
        from our kakush gifts service.<b/>
        Also, we want to remind you that you can't participate to tournaments too.</br>
        Please indicate your name,surname and order number when you do wire transfer thank you.",
        [{name, UserName}, {order_id, PurchaseId}]),
    [
        #panel{class="col-l",body=[
            #p{body=LeftPanelContent},
            #panel{class="btn-holder", body = [
                #submit{class="btn-submit", text=?_T("Print"), postback={print_clicked, PurchaseId}}]}]
        },
        #panel{class="col-r",body=bank_info()}
    ].

bank_info() ->
    [
        #h1{text=?_T("Bank account details:")},
        #p{body=?_T("PAYNET İNTERNET HİZMETLERİ A.Ş")},
        #p{body=?_T("GARANTİ BANKASI A.Ş")},
        #p{body=?_T("ORTAKLAR ŞUBESİ")},
        #p{body=?_T("TL Account")},
        #p{body=?_T("IBAN Number")},
        #p{body=?_T("TR82 0006 2000 3570 0006 2950 88")}
    ].
