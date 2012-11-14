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
-record(twuser, {id, screen_name, access_token, access_token_secret}).

app_callback()->
    case wf:session(twuser) of
	undefined ->
	    OauthToken = wf:q(oauth_token),
	    OauthVerifier = wf:q(oauth_verifier),
	    case OauthToken of
		undefined-> not_authorized;
		_ when OauthVerifier =/= undefined ->
		    wf:info("Token:~p Verifier:~p~n", [OauthToken, OauthVerifier]),
		    TwUser = get_access_token(OauthToken, OauthVerifier),
		    wf:info("Tw user~p~n", [TwUser]),
		    wf:session(twuser, TwUser);
		_ -> not_authorized
	    end;
        _ -> ok
    end.

get_request_token()->
    URL = "https://twitter.com/oauth/request_token",
    {ok, Response} = oauth:get(URL, [], ?CONSUMER),
    Params = oauth:params_decode(Response),
    RequestToken = oauth:token(Params),
    RequestTokenSecret = oauth:token_secret(Params),
    CallbackConfirmed = proplists:get_value("oauth_callback_confirmed", Params),
    {RequestToken, RequestTokenSecret, CallbackConfirmed}.

get_access_token(Token, Verifier)->
    URL = "https://twitter.com/oauth/access_token",
    Signed = oauth:sign("GET", URL, [{"oauth_verifier", Verifier}], ?CONSUMER, Token, ""),
    {OauthParams, QueryParams} = lists:partition(fun({K, _}) -> lists:prefix("oauth_", K) end, Signed),
    Request = {oauth:uri(URL, QueryParams), [oauth:header(OauthParams)]},
    {ok, Response} = httpc:request(get, Request, [{autoredirect, false}], []),
    Params = oauth:params_decode(Response),
    #twuser{
	id = proplists:get_value("user_id", Params),
	screen_name = proplists:get_value("screen_name", Params),
	access_token=oauth:token(Params),
	access_token_secret=oauth:token_secret(Params)
    }.

authorize_url(RequestToken)->
    oauth:uri("https://twitter.com/oauth/authorize", [{"oauth_token", RequestToken}]).

service_item()->
    #listitem{id=twServiceBtn, class=png, body=[
	#image{image="/images/img-52.png"},
	#span{text="Twitter"},
	service_btn(wf:session(twuser))]}.

service_btn(undefined)->
    {RequestToken, _, _} = tw_utils:get_request_token(),
    [#link{class="btn", text=["<span>+</span>",?_T("Add")], url=authorize_url(RequestToken),html_encode = false}];
service_btn(#twuser{})->
    [#link{class="btn btn-2", text=["<span>-</span>",?_T("Test")], html_encode = false, postback={service, twitter}}].

tweet(Msg)->
    #twuser{access_token=AccessToken, access_token_secret=AccessTokenSecret} = wf:session(twuser),
    URL = "http://api.twitter.com/1.1/statuses/update.json",
    oauth:post(URL, [{"status", Msg}], ?CONSUMER, AccessToken, AccessTokenSecret).
