-module(tw_utils).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("nsm_db/include/user.hrl").
-include("common.hrl").
-include("setup.hrl").
-include("elements/records.hrl").
-compile(export_all).

-define(CONSUMER_KEY, nsx_opt:get_env(nsw_srv, tw_consumer_key, "")).
-define(CONSUMER_SECRET, nsx_opt:get_env(nsw_srv, tw_consumer_secret, "")).
-define(CONSUMER, {?CONSUMER_KEY, ?CONSUMER_SECRET, hmac_sha1}).

app_callback()->
  case nsm_db:get(user, wf:user()) of
    {error, notfound} -> wf:redirect(?_U("login"));
    {ok, User} ->
      case get_access_token(wf:q(oauth_token), wf:q(oauth_verifier)) of
        {Id, Token, Secret} ->
          UserUpd = User#user{twitter_id=Id},
%          nsx_msg:notify(["system", "put"], UserUpd),
          nsm_db:put(UserUpd),
          nsx_msg:notify(["db", "user", User#user.username, "put"], #twitter_oauth{user_id=Id, token=Token, secret=Secret});
        _ -> ok
      end
  end.

get_request_token()->
  URL = "https://twitter.com/oauth/request_token",
  case oauth:get(URL, [], ?CONSUMER) of
    {ok, Response} ->
      Params = oauth:params_decode(Response),
      RequestToken = oauth:token(Params),
      RequestTokenSecret = oauth:token_secret(Params),
      CallbackConfirmed = proplists:get_value("oauth_callback_confirmed", Params),
      {RequestToken, RequestTokenSecret, CallbackConfirmed};
    {error, E}-> {error, E}
  end.

get_access_token(undefined, undefined)-> not_authorized;
get_access_token(undefined, _)-> not_authorized;
get_access_token(_, undefined)-> not_authorized;
get_access_token(Token, Verifier)->
  URL = "https://twitter.com/oauth/access_token",
  Signed = oauth:sign("GET", URL, [{"oauth_verifier", Verifier}], ?CONSUMER, Token, ""),
  {OauthParams, QueryParams} = lists:partition(fun({K, _}) -> lists:prefix("oauth_", K) end, Signed),
  Request = {oauth:uri(URL, QueryParams), [oauth:header(OauthParams)]},
  {ok, Response} = httpc:request(get, Request, [{autoredirect, false}], []),
  case Response of
    {HttpResponse, _, _}->
      case HttpResponse of
        {"HTTP/1.1",200,"OK"}->
          Params = oauth:params_decode(Response),
          {proplists:get_value("user_id", Params), oauth:token(Params), oauth:token_secret(Params)};
        _ -> not_authorized
      end;
    _ -> not_authorized
  end.

authorize_url(RequestToken)->
    oauth:uri("https://twitter.com/oauth/authorize", [{"oauth_token", RequestToken}]).

service_item()->
  case nsm_db:get(user, wf:user()) of 
    {error, notfound} -> wf:redirect(?_U("login"));
    {ok, #user{twitter_id=TwitterId}} ->
      try service_btn(TwitterId) of
        Btn ->  #listitem{id=twServiceBtn, class=png, body=Btn}
      catch
        _:_ -> []
      end
  end.

service_btn(undefined) ->
  case get_request_token() of
    {RequestToken, _, _} ->
      [#image{image="/images/img-52.png"}, #span{text="Twitter"},
      #link{class="btn", text=["<span>+</span>",?_T("Add")], url=authorize_url(RequestToken),html_encode = false}];
    {error, R} -> ?INFO("Twitter request failed:", [R]), []
  end;
service_btn(TwitterId)->
  case nsm_db:get(twitter_oauth, TwitterId) of
    {error, notfound}->
      service_btn(undefined);
    {ok, #twitter_oauth{token=Token, secret=TokenSecret}} when Token == undefined orelse TokenSecret == undefined ->
      service_btn(undefined);
    {ok, #twitter_oauth{}} ->
      [#image{image="/images/img-52.png"}, #span{text="Twitter"},
      #link{class="btn", text=["<span>-</span>",?_T("Del")], html_encode = false, postback={delete, twitter}}]
  end.

delete()->
  case nsm_db:get(user, wf:user()) of
    {error, notfound} -> wf:redirect(?_U("login"));
    {ok, #user{twitter_id=TwitterId} = User} when TwitterId =/= undefined ->
      case nsm_db:get(twitter_oauth, TwitterId) of
        {error, notfound} -> ok;
        {ok, #twitter_oauth{}} ->
          nsm_db:put(User#user{twitter_id = undefined}),
          %nsx_msg:notify(["system", "put"], User#user{twitter_id = undefined}),
          nsm_sb:delete(twitter_oauth, TwitterId),
          %nsx_msg:notify(["system", "delete"], {twitter_oauth, TwitterId}),
          wf:update(twServiceBtn, service_btn(undefined))
      end;
    _ -> ok
  end.

tweet(UserName, Msg)->
  case nsm_db:get(user, UserName) of
    {error, notfound} -> fail;
    {ok, #user{twitter_id=TwitterId}}->
      case nsm_db:get(twitter_oauth, TwitterId) of
        {error, notfound} -> fail;
        {ok, #twitter_oauth{token = AccessToken, secret=AccessTokenSecret}}->
          URL = "http://api.twitter.com/1.1/statuses/update.json",
          oauth:post(URL, [{"status", Msg}], ?CONSUMER, AccessToken, AccessTokenSecret)
    end
  end.
