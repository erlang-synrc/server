%%%-------------------------------------------------------------------
%%% File    : erlfb.erl
%%% Author  : pawelflis <pawel_flis@silversoft.pl>
%%% Description: This module implements very basics of new FB Graph API
%%%
%%% Currently it supports fetching access token with a code and
%%% sending generic requests to Graph API
%%%
%%% Created :  9 Mar 2011 by pawelflis <pawel_flis@silversoft.pl>
%%%-------------------------------------------------------------------

-module(erlfb).

-include("setup.hrl").
-include("common.hrl").

-define(FB_GRAPH_URI, "https://graph.facebook.com").
-define(FB_OLD_API, "https://api.facebook.com/method").

-export([start/0,
         stop/0,
         oauth/4,
         create_uri/3,
         create_uri/1,
         send_request/1,
         get/3,
         get/2,
         get_user_info/1,
         path_to_proplists/1,
         revoke_access/1]).


start() ->
    %%FIX: should be moved to application initialization code
    ssl:start(),
    inets:start().

stop() ->
    ok.

oauth(AppId, AppSecret, RedirectUri, Code) ->
    Uri = create_uri(oauth, access_token,
                     [{client_id, AppId},
                      {redirect_uri, mochiweb_util:quote_plus(RedirectUri)},
                      {client_secret, AppSecret},
                      {code, Code}]),
    wf:info("Request uri:~p~n", [Uri]),
    {ok, Data} = send_request(Uri),
    wf:info("Received: ~p~n", [Data]),
    path_to_proplists(Data).


get(Id, ConnectionType, Opts) ->
    Uri = create_uri(Id, ConnectionType, Opts),
    {ok, Data} = send_request(Uri),
    Data.

get(Id, Opts) ->
    Uri = create_uri(Id, Opts),
    {ok, Data} = send_request(Uri),
    Data.



send_request(Uri) ->
    %%FIX: don't use io:fwrite, use application-wide logging modules
    ?INFO("send_request(~p)~n~n", [Uri]),
    case httpc:request(Uri) of
        {ok, {_, Header, Data}} ->
            case string:tokens(proplists:get_value("content-type", Header), ";") of
                ["text/javascript" | _Rest] ->
                    {ok, mochijson2:decode(Data)};
                [Type | _Rest] ->
                    io:fwrite("Type:~p~n~n", [Type]),
                    {ok, Data}

            end;
        {error, _} = E ->
            E
    end.

create_uri(Id) ->
    Id0 = convert_type(Id),
    create_uri(Id0, []).

create_uri(Id, Opts) ->
    Id0 = convert_type(Id),
    ParseOpts = parse_opts(Opts),
    Uri = io_lib:fwrite("~s/~s?~s", [?FB_GRAPH_URI, Id0, ParseOpts]),
    lists:flatten(Uri).

create_uri(Id, ConnectionType, Opts) ->
    Id0 = convert_type(Id),
    ConnectionType0 = convert_type(ConnectionType),
    ParseOpts = parse_opts(Opts),
    Uri = io_lib:fwrite("~s/~s/~s?~s", [?FB_GRAPH_URI, Id0, ConnectionType0, ParseOpts]),
    lists:flatten(Uri).

parse_opts(Opts) ->
    Parse = fun({K, V}) ->
                    K1 = convert_type(K),
                    V1 = convert_type(V),
                    %%FIX: V1 should be quoted (e.g. mochiweb_util:quote_plus
                    io_lib:fwrite("~s=~s", [K1, V1])
            end,
    string:join(lists:map(Parse, Opts), "&").

convert_type(V) when is_atom(V) ->
    atom_to_list(V);
convert_type(V) when is_integer(V) ->
    integer_to_list(V);
convert_type(V) when is_list(V) ->
    V.

path_to_proplists(Path) ->
    try
        Splite = string:tokens(Path, "&"),
        [ begin
              [K,V] = string:tokens(P, "="),
              {K,V}
          end || P <- Splite ]
    catch _:_ ->
            Path
    end.

revoke_access(AccessToken) ->
    Uri = lists:flatten(io_lib:fwrite("~s/~s?format=JSON&access_token=~s", [?FB_OLD_API,
                                                                "auth.revokeAuthorization",
                                                                AccessToken])),
    case httpc:request(Uri) of
        {ok, {_, _Header, Data}} ->
            {ok, Data};
        {error, _} = E ->
            E
    end.

-spec get_user_info(string()) -> {ok, term()} | {error, term()}.

get_user_info(FbToken) when is_list(FbToken) ->
    FbUrl = "https://graph.facebook.com",
    Uri = FbUrl++"/me?access_token="++FbToken,
    catch erlfb:send_request(Uri);
get_user_info(_) ->
    {error, {fb_user_info, "string expected"}}.
