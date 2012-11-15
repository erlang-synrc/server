-module(fb_utils).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("nsm_db/include/user.hrl").
-include("common.hrl").
-include("setup.hrl").
-include("elements/records.hrl").
-compile(export_all).

% demo_id 176025532423202, kakaranet_id 154227314626053
init()->
    wf:wire(#api{name=setFbIframe, tag=fb}),
    wf:wire(#api{name=fbLogin, tag=fb}),
    wf:wire(#api{name=fbLogout, tag=fb}),
    wf:wire(#api{name=fbSignedRequest, tag=fb}),
    wf:wire(#api{name=fbCheckPermissions, tag=fb}),
    wf:wire(#api{name=fbRemovePermissions, tag=fb}),
    ["<div id=fb-root></div>",
    "<script>window.fbAsyncInit = function() {",
    "FB.init({ appId: '"++ ?FB_APP_ID ++"',",
	"channelUrl: '" ++ ?HTTP_ADDRESS ++"/channel.html',",
	"status: true,",
	"cookie: true,",
	"xfbml: true,",
	"oauth: true",
    "});",
    "if(page.fbLogout) FB.Event.subscribe('auth.logout', function(response){",
	"page.fbLogout(response);",
    "});",
    "FB.getLoginStatus(function(response) {",
	"if(page.setFbIframe){",
	    "console.log(\"Set FB application flag: \"+ (top!=self));",
	    "page.setFbIframe(top!=self);",
	"}",
	"if (response.status === 'connected') {",
	    "var uid = response.authResponse.userID;",
	    "console.log(\"User is connected: \" + uid);",

	    "if(page.fbCheckPermissions){
		FB.api(\"/me/permissions\", function(response){
		    var perms = response.data[0];
		    console.log(\"Permissions: \"+perms);
		    page.fbCheckPermissions(perms);
		});
	    }else{console.log(\"No fbCheckPermissions\")}",
	"} else if (response.status === 'not_authorized') {",
	    "console.log(\"Not authenticated the app\");",
	"} else {",
	    "console.log(\"User isn't logged in\");",
	"}",
    "});",
    "};",
    "function fb_login(){",
	"FB.getLoginStatus(function(response){",
	    "if(response.status == 'connected'){",
		"console.log(\"User connected to FB, check for registered account\");",
		"if(page.fbLogin){",
		    "FB.api(\"/me?fields=id,username,first_name,last_name,email,birthday\",",
			"function(response){",
			    "page.fbLogin(response);",
			"});",
		"}",
	    "}else{",
		"FB.login(function(r){",
		    "if(r.authResponse){",
			"if(page.fbLogin){",
			    "FB.api(\"/me?fields=id,username,first_name,last_name,email,birthday\",",
			    "function(response){page.fbLogin(response);});",
			"}",
		    "}",
		"},{scope: 'email,user_birthday'});",
	    "}",
	"});",
    "}",
    "function add_fb_service(){",
	"FB.ui({
	method: 'permissions.request',
	perms: 'publish_stream',
	display: 'popup'},
	function(response){
	    if(response && response.perms){
		console.log(\"Permissions granted: \"+response.perms);",
		"if(page.fbCheckPermissions){
		    page.fbCheckPermissions(response.perms);
		}",
	    "}else if(!response.perms){
		console.log(\"User did't grant permission.\");
	    }
	});",
    "};
    function del_fb_service(){
	console.log(\"Todo: revoke fb permission.\");
	FB.api({
	    method: 'auth.revokeExtendedPermission',
	    perm: 'publish_stream'
	},
	function(response) {
	    if(page.fbRemovePermissions){
		page.fbRemovePermissions(response);
	    }
	    console.log(\"Response revoke:\" + response);
	}); 
    };",
    "(function(d){",
    "var js, id = 'facebook-jssdk', ref = d.getElementsByTagName('script')[0];",
    "if (d.getElementById(id)) {return;}",
    "js = d.createElement('script');",
    "js.id = id;",
    "js.async = true;",
    "js.src = \"//connect.facebook.net/en_US/all.js\";",
    "ref.parentNode.insertBefore(js, ref);",
    "}(document));",
    "</script>"].

login_btn()-> login_btn("Login").
login_btn(Label)->
    [#link{class="fb_login_btn", text=?_T(Label), actions=#event{type=click, actions=#script{script="fb_login()"} }}].

logout_btn()->
    [#link{text=?_T("Logout"), actions=#event{type=click, actions=#script{script="FB.logout()"}}, postback=logout }].

service_item()->
    #listitem{id=fbServiceButton, class=png, body=add_service_btn()}.

add_service_btn()->
    [#image{image="/images/img-51.png"}, #span{text="Facebook"},
    #link{class="btn", text=["<span>+</span>",?_T("Add")],
	actions=#event{type=click, actions=#script{script="add_fb_service()"}},
	html_encode = false}].

buy_button(PackageId, OverLimit) when is_atom(OverLimit)->
    #link{class="pay_fb_btn", text=?_T("Buy"),
    actions=#event{type=click, actions=#script{script="pay_with_fb(\""++ PackageId ++ "\"," ++ atom_to_list(OverLimit) ++");"}}}.

pay_dialog()->
    wf:wire(#api{name=processOrder, tag=fb}),
    wf:wire(#api{name=fbNotifyOverLimit, tag=fb}),
    ["<script type=\"text/javascript\">",
    "var callback = function(data){",
	"if(data['error_code']){",
	    "console.log(\"Code: \"+data['error_code'] + \" Message: \"+ data['error_message']);",
	    "return false;",
	"}",
	"if(data['order_id']){",
	    "console.log(\"Order:\" + data);",
	    "if(page.processOrder){",
		"page.processOrder(data);",
	    "}",
	    "return true;",
	"}",
    "};",
    "function pay_with_fb(package_id, overlimit){",
	"if(overlimit && page.fbNotifyOverLimit){",
	    "page.fbNotifyOverLimit();",
	"} else {",
	    "FB.ui({",
		"method:'pay',",
		"action:'buy_item',",
		"order_info: {'item_id': package_id},",
		"dev_purchase_params: {'oscif':true} },",
		"callback);",
	"}",
    "}",
    "</script>"].

api_event(fbLogin, _, [Args])->
    case Args of
	[{error, E}] ->
	    ErrorMsg = io_lib:format("Facebook error:~p", [E]),
	    wf:redirect( ?_U("/index/message/") ++ site_utils:base64_encode_to_url(ErrorMsg));
	_ ->
	    case nsm_users:get_user({facebook, proplists:get_value(id, Args)}) of
	    {ok, User} ->
		case same_or_undefined(wf:user(), User#user.username) of
		    true ->
			login:login_user(User#user.username),
			wf:session(logged_with_fb, true),
			wf:redirect_from_login(?_U("/dashboard"));
		    _ -> fb_user_not_match
		end;
	    _ ->
	        wf:session(fb_registration, Args),
		wf:redirect(?_U("/login/register"))
	    end
    end;
api_event(fbLogout, _, _Data)-> wf:session(fb_registration, undefined);
api_event(fbNotifyOverLimit, _, _)->
    buy:over_limit_popup(nsm_membership_packages:get_monthly_purchase_limit());
api_event(processOrder, _, [[{order_id, OrderId}, {status, Status}]])-> 
    ?INFO("Payment complete. Order:~p~n", [OrderId]),
    case nsm_membership_packages:get_purchase(integer_to_list(OrderId)) of
	{ok, Purchase} when Status =:= "settled" ->
	    nsx_util_notification:notify(["purchase", "user", wf:user(), "set_purchase_state"], {element(2,Purchase), done, facebook}),
	    wf:redirect("/profile/account");
	_ -> wf:info("Purchase Not Found")
    end;
api_event(setFbIframe, _, [IsIframe]) -> wf:session(is_facebook, IsIframe);
api_event(fbCheckPermissions, _, [Perms])-> check_permissions(Perms);
api_event(fbSignedRequest, _, _Data) -> ok;
api_event(fbRemovePermissions, _, [Data]) ->
    case Data of
	true -> 
	    wf:update(fbServiceButton, add_service_btn());
	_ -> 
	    wf:info("Remove FB permissions error: ~p~n", [Data])
    end.

check_permissions([{publish_stream, 1}|_Rest])->
    wf:info("Attach service! ~p~n", [wf:user()]),

    wf:update(fbServiceButton, [
	#image{image="/images/img-51.png"},
	#span{text="Facebook"},
	#link{class="btn", text=["<span>-</span>",?_T("Del")],
	actions=#event{type=click, actions=#script{script="del_fb_service()"}},
	html_encode = false}
    ]);
check_permissions("publish_stream") -> check_permissions([{publish_stream, 1}]);
check_permissions([])-> ok;
check_permissions([{_P,_V}|Perms]) -> check_permissions(Perms).

same_or_undefined(undefined, _) -> true;
same_or_undefined(User, FbUser) when User =:= FbUser -> true;
same_or_undefined(_,_) -> false.
