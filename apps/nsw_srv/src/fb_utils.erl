-module(fb_utils).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("nsm_db/include/user.hrl").
-include_lib("nsm_db/include/tournaments.hrl").
-include("common.hrl").
-include("setup.hrl").
-include("elements/records.hrl").
-compile(export_all).

init()->
  wf:wire(#api{name=setFbIframe, tag=fb}),
  wf:wire(#api{name=fbAutoLogin, tag=fb}),
  wf:wire(#api{name=fbLogin, tag=fb}),
  wf:wire(#api{name=fbPreLogin, tag=fb}),
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
      "FB.getLoginStatus(function(response) {",
        "if(page.setFbIframe){",
          "var inIframe= top!=self;",
          "page.setFbIframe(inIframe);",
          "if(inIframe && response.status == 'connected' && page.fbLogin){",
            "FB.api(\"/me?fields=id,username,first_name,last_name,email,birthday\",",
            "function(response){",
              "page.fbAutoLogin(response);",
            "});",
          "}",
        "}",
      "});",
    "};",
    "function fb_login(){",
      "page.fbPreLogin();",
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
      "FB.login(function(resp){",
        "if(resp.authResponse && resp.authResponse.userID){",
          "var uid = resp.authResponse.userID;",
          "FB.ui({",
              "method: 'permissions.request',",
              "perms: 'publish_stream',",
              "display: 'popup'",
            "},",
            "function(response){",
              "if(response && response.perms){",
                "page.fbAddAsService(uid);",
            "}});",
        "}",
      "}, {scope: 'email,user_birthday'});",
    "};",
    "function fb_feed(Id, Msg, Token){",
      "FB.api('/'+Id+'/feed?access_token='+Token, 'post', {message: Msg }, function(response) {});",
    "};",
    "(function(d){",
    "var js, id = 'facebook-jssdk', ref = d.getElementsByTagName('script')[0];",
    "if (d.getElementById(id)) {return;}",
    "js = d.createElement('script');",
    "js.id = id;",
    "js.async = true;",
    "js.src = \"//connect.facebook.net/", fb_sdk_locale(site_utils:detect_language()), "/all.js\";",
    "ref.parentNode.insertBefore(js, ref);",
    "}(document));",
    "</script>"].

fb_sdk_locale("tr")-> "tr_TR";
fb_sdk_locale(_)-> "en_US".

login_btn()-> login_btn("Login").
login_btn(Label)->
    [#link{class="fb_login_btn", text=?_T(Label), actions=#event{type=click, actions=#script{script="fb_login()"} }}].

logout_btn()->
[#link{text=?_T("Logout"), postback=logout}].

service_item()->
  case nsm_users:get_user({username, wf:user()}) of
    {error, notfound}-> [];
    {ok, User} ->
      Li = case User#user.facebook_id of
        undefined -> add_service_btn();
        _ -> del_service_btn()
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
    actions=#event{type=click, actions=#script{script="pay_with_fb(\""
      ++ wf:user() ++ "\" ,\""
      ++ PackageId ++ "\","
      ++ atom_to_list(OverLimit) ++");"}}}.

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
      "if(page.processOrder){",
        "page.processOrder(data);",
      "}",
      "return true;",
    "}",
    "};",
    "function pay_with_fb(user, package_id, overlimit){",
      "if(overlimit && page.fbNotifyOverLimit){",
        "page.fbNotifyOverLimit();",
      "} else {",
        "FB.ui({",
        "method:'pay',",
        "action:'buy_item',",
        "order_info: {'item_id': package_id, 'user': user},",
        "dev_purchase_params: {'oscif':true} },",
        "callback);",
      "}",
    "}",
    "</script>"].

event(fb_remove_service)->
  case nsm_users:get_user({username, wf:user()}) of
    {error, notfound} -> wf:redirect(?_U("/login"));
    {ok, #user{facebook_id=FbId} = User} when FbId =/= undefined->
      case nsm_db:get(facebook_oauth, FbId) of
        {ok, #facebook_oauth{access_token=Token}} ->
          case Token of
            undefined -> ok;
            T ->
              URL = "https://graph.facebook.com/" ++ FbId ++ "/permissions/publish_stream?access_token="++ T,
              httpc:request(delete, {URL, []}, [], [])
          end,
          nsx_msg:notify(["system", "delete"], #facebook_oauth{user_id=FbId});
        {error, notfound} -> ok
      end,
      nsx_msg:notify(["system", "delete"], {user_by_facebook_id, FbId}),
      nsx_msg:notify(["system", "put"], User#user{facebook_id=undefined}),
      wf:update(fbServiceButton, add_service_btn());
    _ -> no_service
  end;
event(Event)->
  ?INFO("Fbutils: ~p", [Event]).
api_event(fbPreLogin, _, _)->
  wf:session(fb_registration, undefined);
api_event(fbAutoLogin, Tag, [Args])->
  api_event(fbLogin, Tag, [Args]);
api_event(fbLogin, _, [Args])->
  case Args of
    [{error, E}] ->
      ErrorMsg = io_lib:format("Facebook error:~p", [E]),
      wf:redirect( ?_U("/index/message/") ++ site_utils:base64_encode_to_url(ErrorMsg));
    _ ->
      CurrentUser = wf:user(),
      FbId = proplists:get_value(id, Args),
      UserName = proplists:get_value(username, Args),
      BirthDay = list_to_tuple([list_to_integer(X) || X <- string:tokens(proplists:get_value(birthday, Args), "/")]),

      case nsm_db:get(user_by_facebook_id, FbId) of
        {error, notfound} ->
          RegData = #user{
            username = ling:replace(UserName,".","_"),
            password = undefined,
            email = proplists:get_value(email, Args),
            name = proplists:get_value(first_name, Args),
            surname = proplists:get_value(last_name, Args),
            facebook_id = FbId,
            team = nsm_db:create_team("team"),
            verification_code = undefined,
            age = {element(3, BirthDay), element(1, BirthDay), element(2, BirthDay)},
            register_date = erlang:now(),
            sex = undefined,
            status = ok
          },
          case nsm_users:register(RegData) of
            {ok, Name} ->
              login:login_user(Name);
            {error, _Error} ->
              Msg = ?_T("This email it taken by other user. If You already have the kakaranet.com account, please login and connect the to facebook."),
              wf:session(fb_registration, Args),
              wf:redirect([?HTTPS_ADDRESS,?_U("/login/register/msg/")++site_utils:base64_encode_to_url(Msg)])
          end;
        {ok, User} when User#user.username == CurrentUser -> ok;
        {ok, User} ->
          login:login_user(element(2, User));
      end
  end;
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
api_event(setFbIframe, _, [IsIframe]) ->
  wf:session(is_facebook, IsIframe),
  LogoutBtn = case IsIframe of
    true -> [];
    _ -> #listitem{body=[#link{text=?_T("Logout"), postback=logout}]}
  end,
  wf:update(logout_btn, LogoutBtn);
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
  nsx_msg:notify(["db", "user", wf:user(), "put"], #facebook_oauth{user_id=Id, access_token=get_access_token()}),
  wf:update(fbServiceButton, del_service_btn()).

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

feed(UserName, Msg)->
  case nsm_db:get(user, UserName) of
    {error, notfound}-> fail;
    {ok, #user{facebook_id=FacebookId}} when FacebookId =/= undefined->
      case nsm_db:get(facebook_oauth, FacebookId) of
        {error, notfound}-> fail;
        {ok, #facebook_oauth{} = FO}->
          AccessToken = case FO#facebook_oauth.access_token of
            undefined ->
              AT = get_access_token(),
              nsx_msg:notify(["db", "user", UserName , "put"], #facebook_oauth{user_id=FacebookId, access_token=AT});
            AT -> AT
          end,
          Url ="https://graph.facebook.com/"++ FacebookId ++"/feed",
          Body = "access_token="++AccessToken++"&message="++ wf:url_encode(Msg),
          httpc:request(post, {Url, [], "application/x-www-form-urlencoded", Body}, [], [])
      end;
    _ -> fail
  end.

announce_tournament(UserName, Id)->
  case nsm_db:get(user, UserName) of
    {error, notfound}-> fail;
    {ok, #user{facebook_id=FacebookId}} when FacebookId =/= undefined->
      case nsm_db:get(facebook_oauth, FacebookId) of
        {error, notfound}-> fail;
        {ok, #facebook_oauth{} = FO}->
          AccessToken = case FO#facebook_oauth.access_token of
            undefined ->
              AT = get_access_token(),
              nsx_msg:notify(["db", "user", UserName , "put"], #facebook_oauth{user_id=FacebookId, access_token=AT});
            AT -> AT
          end,
          Url ="https://graph.facebook.com/"++ FacebookId ++"/kakaranet:create",

          case nsm_db:get(tournament, Id) of
            {error, not_found} -> fail;
            {ok, #tournament{} = T} ->
              {{Y, M, D}, {H, Min, _}} = Now  = calendar:now_to_local_time(now()),
              TournamentTime = calendar:datetime_to_gregorian_seconds({T#tournament.start_date, T#tournament.start_time}),
              MessageTime = calendar:datetime_to_gregorian_seconds(Now),
              CreatedTime = lists:flatten(io_lib:format("~p-~p-~pT~p:~p", [Y,M,D,H,Min])),
              Body = "access_token="++AccessToken++
              "&tournament="++ ?HTTP_ADDRESS ++ "/tournament/lobby/public/id/" ++ integer_to_list(Id)
              ++"&created_time="++CreatedTime
              ++"&expires_in="++integer_to_list(TournamentTime-MessageTime),
              httpc:request(post, {Url, [], "application/x-www-form-urlencoded", Body}, [], [])
            end
      end;
    _ -> fail
  end.
