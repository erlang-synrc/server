-module(fb_utils).
-include_lib("nitrogen_core/include/wf.hrl").
-include("common.hrl").
-include("setup.hrl").
-include("elements/records.hrl").

%-export([init/0, login/0, api_event/3]).
-compile(export_all).

% demo_id 176025532423202, kakaranet_id 154227314626053
% TODO:
% - Change en_US locale of the JS SDK script
% - Add channel file for x-domain communication and handle its caching.
%   channelUrl : '//WWW.YOUR_DOMAIN.COM/channel.html',
init()->
    wf:wire(#api{name=checkSignedRequest, tag=fb}),
    ["<div id=fb-root></div>",
    "<script>window.fbAsyncInit = function() {
    FB.init({ appId: '"++?FB_APP_ID++"', status: true, cookie: true, xfbml: false});

    FB.getLoginStatus(function(response) {
	if (response.status === 'connected') {
	    console.log(\"Signed request: \" + response.authResponse.signedRequest);
	    var uid = response.authResponse.userID;
	    var accessToken = response.authResponse.accessToken;
	    console.log(\"User is connected: \" + uid);
	    if(page.checkSignedRequest){
		page.checkSignedRequest(response.authResponse.signedRequest);
	    }
	} else if (response.status === 'not_authorized') {
	    console.log(\"Not authenticated the app\");
	} else {
	    console.log(\"User isn't logged in\");
	} }); };

    (function(d){
    var js, id = 'facebook-jssdk', ref = d.getElementsByTagName('script')[0];
    if (d.getElementById(id)) {return;}
    js = d.createElement('script');
    js.id = id;
    js.async = true;
    js.src = \"//connect.facebook.net/en_US/all.js\";
    ref.parentNode.insertBefore(js, ref);
    }(document));
    </script>"].

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

api_event(checkSignedRequest, _, SignedRequest)->
    {ok, Data} = fb_signed_request:parse(SignedRequest, ?FB_APP_SECRET),
    ?INFO("Signed request data: ~p~n", [Data]);
api_event(processOrder, _, Data)->
    ?INFO("Payment complete. Order:~p~n", [Data]),
    .

login() ->
    case oauth(?FB_APP_ID, ?FB_APP_SECRET, ?FB_REDIRECT_URI, wf:q(code)) of
    {struct, Err} ->
	?INFO("Oauth error: ~p~n", [Err]),
	wf:update(login_hintbox, io_lib:fwrite("~p", [Err])),
	login:main();
    Token ->
	OnlyToken = proplists:get_value("access_token", Token),
	wf:session(fb_access_token, OnlyToken),
	?INFO("Save token... ~p~n~n",[OnlyToken]),

	case erlfb:get_user_info(OnlyToken) of
	{ok, {struct, UserInfo}} ->
	    check_user(UserInfo);
	Error ->
	    ?ERROR("unexpected error while erlfb:get_user_info/1: ~p, Token:~s", [Error, OnlyToken]),
	    wf:update(login_hintbox, ?_T("Application error. Please, try later")),
	    login:main()
	end
    end.

oauth(AppId, AppSecret, RedirectURI, Code)->
    Uri ="https://graph.facebook.com/oauth/access_token?client_id="++AppId++
    "&redirect_uri="++RedirectURI++"/?facebook=true&client_secret="++AppSecret++"&code="++Code,
    case httpc:request(Uri) of
	{ok, {_, _Header, Resp}} -> ok;
	{error, Resp} -> error
    end,
    erlfb:path_to_proplists(Resp).

check_user(UserInfo) ->
    UId = wf:to_list(proplists:get_value(<<"id">>, UserInfo, <<"">>)),
    UId == "" andalso ?WARNING("User id undefined. UserInfo: ~p", [UserInfo]),
    ?INFO("Userinfo: ~p, UId:~p", [UserInfo, UId]),
    case zealot_auth:login_fb([{username, UId}]) of
    {ok, UserName} -> 
	login:login_user(UserName);
    {error, banned} ->
	Message = ?_T("Your account is blocked. Please, contact with administration."),
	EncodedMessage = site_utils:base64_encode_to_url(Message),
	wf:redirect(?_U("/index/message/")++EncodedMessage);
    {error, notfound} ->
	wf:session(facebook, UserInfo),
	wf:redirect(?_U("/login/register/facebook/true"))
    end.