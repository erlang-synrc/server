-module(fb_utils).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("nsm_db/include/user.hrl").
-include("common.hrl").
-include("setup.hrl").
-include("elements/records.hrl").
-compile(export_all).

init()->
  wf:wire(#api{name=setFbIframe, tag=fb}),
  wf:wire(#api{name=fbLogin, tag=fb}),
  wf:wire(#api{name=fbLogout, tag=fb}),
  wf:wire(#api{name=fbSignedRequest, tag=fb}),
  wf:wire(#api{name=fbAddAsService, tag=fb}),
  ["<div id=fb-root></div>",
    "<script>window.fbAsyncInit = function() {",
    "FB.init({ appId: '"++ ?FB_APP_ID ++"',",
    "channelUrl: '" ++ ?HTTP_ADDRESS ++ "/channel.html',",
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
    "FB.ui({",
      "method: 'permissions.request',",
      "perms: 'publish_stream',",
      "display: 'popup'",
      "},",
      "function(response){",
        "if(response){",
          "if(response.perms){",
            "FB.api(\"/me?fields=id\",",
            "function(r){",
              "if(page.fbAddAsService){",
                "page.fbAddAsService(r.id);",
              "}",
            "});",
          "}",
        "}",
      "});",
  "};",

  "function fb_feed(Id, Msg, Token){",
    "FB.api('/'+Id+'/feed?access_token='+Token, 'post', {message: Msg },",
    "function(response) {",
        "console.log(response);",
      "if (!response || response.error) {",
          "console.log(\"Error occured\");",
      "} else {",
          "console.log(\"Post ID: \" + response.id);",
      "}",
    "});",
    "};",
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
  case nsm_users:get_user({username, wf:user()}) of
    {error, notfound}-> [];
    {ok, User} ->
      Li = case User#user.facebook_id of
        undefined -> add_service_btn();
        Id ->
          case nsm_db:get(facebook_oauth, Id) of
            {error, notfound} -> add_service_btn();
            {ok, #facebook_oauth{} = FO} when FO#facebook_oauth.access_token =/= undefined -> del_service_btn();
            {ok, #facebook_oauth{}} -> add_service_btn()
          end
      end,
      #listitem{id=fbServiceButton, class=png, body=Li}
  end.

add_service_btn()->
  [#image{image="/images/img-51.png"}, #span{text="Facebook"},
   #link{class="btn", text=["<span>+</span>",?_T("Add")],
   actions=#event{type=click, actions=#script{script="add_fb_service()"}},
   html_encode = false}].

del_service_btn()->
  [#image{image="/images/img-51.png"},
   #span{text="Facebook"},
   #link{class="btn", text=["<span>-</span>",?_T("Del")],
   postback=fb_remove_service,
   html_encode = false}].

test_btn(FbId, Token)->
  [#link{class="btn", text=["<span>-</span>",?_T("Test")],
   actions=#event{type=click, actions=#script{script="fb_feed(" ++ FbId ++",\"Test feed\",\"" ++ Token ++"\")"}},
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

event(fb_remove_service)->
  case nsm_users:get_user({username, wf:user()}) of
    {error, notfound} -> wf:redirect(?_U("/login"));
    {ok, #user{facebook_id=FbId}} ->
      case nsm_db:get(facebook_oauth, FbId) of
        {error, notfound} -> error;
        {ok, #facebook_oauth{access_token=Token}} ->
          URL = "https://graph.facebook.com/" ++ FbId ++ "/permissions/publish_stream?access_token="++ Token,
          httpc:request(delete, {URL, []}, [], []),
          nsx_msg:notify(["db", "user", wf:user(), "put"],
            #facebook_oauth{user_id=FbId, access_token=undefined}),
          wf:update(fbServiceButton, add_service_btn())
      end
  end;
event(Event)->
  ?INFO("Fbutils: ~p", [Event]).

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
                        %webutils:redirect_to_tcp(?_U("dashboard"));
		    _ -> fb_user_not_match
		end;
	    _ ->
	        wf:session(fb_registration, Args),
		wf:redirect([?HTTP_ADDRESS,?_U("/login/register")])
	    end
    end;
api_event(fbLogout, _, _Data)-> wf:session(fb_registration, undefined);
api_event(fbNotifyOverLimit, _, _)->
    buy:over_limit_popup(nsm_membership_packages:get_monthly_purchase_limit());
api_event(processOrder, _, [[{order_id, OrderId}, {status, Status}]])-> 
    ?INFO("Payment complete. Order:~p~n", [OrderId]),
    case nsm_membership_packages:get_purchase(integer_to_list(OrderId)) of
	{ok, Purchase} when Status =:= "settled" ->
	    nsx_msg:notify(["purchase", "user", wf:user(), "set_purchase_state"], {element(2,Purchase), done, facebook}),
	    wf:redirect("/profile/account");
	_ -> wf:info("Purchase Not Found")
    end;
api_event(setFbIframe, _, [IsIframe]) -> wf:session(is_facebook, IsIframe);
api_event(fbSignedRequest, _, _Data) -> ok;
api_event(fbAddAsService, _, [Id])->
  case nsm_users:get_user({username, wf:user()}) of
    {error, notfound} -> wf:redirect(?_U("/login"));
    {ok, User} when User#user.facebook_id =/= Id->
      case nsm_users:get_user({facebook, Id}) of
        {error, notfound}-> ok;
        {ok, ExUser} when ExUser#user.username =/= User#user.username ->
          nsx_msg:notify(["system", "put"], ExUser#user{facebook_id=undefined});
        _-> ok
      end,
      nsx_msg:notify(["system", "put"], User#user{facebook_id=Id}),
      update_access_token(Id);
    {ok, User} -> update_access_token(User#user.facebook_id)
  end.

update_access_token(Id)->
  ?INFO("UPDATE ACCESS TOKEN: ~p~n", [Id]),
  nsx_msg:notify(["db", "user", wf:user(), "put"], #facebook_oauth{user_id=Id, access_token=get_access_token()}),
  wf:update(fbServiceButton, del_service_btn()).

same_or_undefined(undefined, _) -> true;
same_or_undefined(User, FbUser) when User =:= FbUser -> true;
same_or_undefined(_,_) -> false.

get_user_info(FbToken) when is_list(FbToken) ->
    FbUrl = "https://graph.facebook.com",
    Uri = FbUrl++"/me?access_token="++FbToken,
    catch send_request(Uri);
get_user_info(_) ->
    {error, {fb_user_info, "string expected"}}.

get_access_token()->
  Url = "https://graph.facebook.com/oauth/access_token?client_id=" ++ ?FB_APP_ID
    ++ "&client_secret=" ++?FB_APP_SECRET
    ++ "&grant_type=client_credentials",
  case send_request(Url) of
    {ok, Resp} ->
      [_|Token] = string:tokens(Resp, "="),
      wf:url_encode(lists:flatten(Token));
    {error, _} -> undefined
  end.

send_request(Uri) ->
  ?INFO("send_request(~p)~n~n", [Uri]),
  case httpc:request(Uri) of
    {ok, {_, Header, Data}} ->
      case string:tokens(proplists:get_value("content-type", Header), ";") of
        ["text/javascript" | _Rest] -> {ok, mochijson2:decode(Data)};
        [Type | _Rest] -> wf:info("Type:~p~n", [Type]), {ok, Data}
      end;
    {error, _} = E -> E
  end.