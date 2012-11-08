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
    ["<div id=fb-root></div>",
    "<script>window.fbAsyncInit = function() {",
    "FB.init({ appId: '"++ ?FB_APP_ID ++"',",
	"channelUrl: '" ++ ?HTTP_ADDRESS ++"/channel.html',",
	"status: true,",
	"cookie: true,",
	"xfbml: true,",
    "});",
    "if(page.fbLogin) FB.Event.subscribe('auth.login', function(response){",
	"if(response.authResponse){",
	    "if(page.fbLogin){",
		"FB.api(\"/me?fields=id,username,first_name,last_name,email,birthday\",",
		"function(response){page.fbLogin(response);});",
	    "}",
	"}",
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
			"function(response){page.fbLogin(response);});",
		"}",
	    "}else{",
		"FB.login(function(r){},{scope: 'email,user_birthday'});",
	    "}",
	"});",
    "}",
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

pay_dialog()->
    wf:wire(#api{name=processOrder, tag=fb}),
    ["<script type=\"text/javascript\">",
    "var callback = function(data){",
	"if(data['error_code']){",
	    "console.log(\"Code: \"+data['error_code'] + \" Message: \"+ data['error_message']);",
	    "return false;}",
	"if(data['order_id']){",
	    "console.log(\"Order:\" + data);",
	    "if(page.processOrder){",
		"page.processOrder(data);",
	    "}",
	    "return true;",
	"}",
    "};",
    "function pay_with_fb(package_id){",
	"console.log(\"Call pay dialog for\" + package_id);"
	"FB.ui({",
	    "method:'pay',",
	    "action:'buy_item',",
	    "order_info: {'item_id': package_id},",
	    "dev_purchase_params: {'oscif':true} },",
	    "callback);",
    "}",
    "</script>"].

api_event(fbLogin, _, [Args])->
    case nsm_users:get_user({facebook, proplists:get_value(id, Args)}) of
	{ok, User} -> 
	    login:login_user(User#user.username),
	    wf:session(logged_with_fb, true),
	    wf:redirect_from_login(?_U("/dashboard"));
	_ ->
	    wf:session(fb_registration, Args),
	    wf:redirect(?_U("/login/register"))
    end;
api_event(fbLogout, _, _Data)-> wf:session(fb_registration, undefined);
api_event(processOrder, _, Data)-> ?INFO("Payment complete. Order:~p~n", [Data]);
api_event(setFbIframe, _, [IsIframe]) -> wf:session(is_facebook, IsIframe).
