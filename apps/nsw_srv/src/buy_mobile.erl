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
    _PurchaseId = rpc:call(?APPSERVER_NODE, nsm_srv_membership_packages, purchase_id, []),
    _Package = buy:package(),

    ?_T("Mobile payments. Coming soon.").


event(Event)->
    buy:event(Event).

