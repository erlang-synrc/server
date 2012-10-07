%%%-------------------------------------------------------------------
%%% @author Paul Peregud <pawel@saturn.lan>
%%% @copyright (C) 2011, Paul Peregud
%%% @doc
%%%
%%% @end
%%% Created :  8 Jun 2011 by Paul Peregud <pawel@saturn.lan>
%%%-------------------------------------------------------------------
-module(oembed_test).

%% API
-export([]).

-include_lib("nsm_db/include/feed.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DP(Format, Args),
	io:format(user, "~p:~p:~p::: " ++ Format ++ "~n", [self(), ?MODULE, ?LINE | Args])).
-record(oembed_media, {
          type, %% [video, audio, file]
          provider_url,
          title,
          width,
          height,
          html,
          author_name,
          thumbnail_url,
          thumbnail_width,
          thumbnail_height
         }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================

setup() ->
    List = [{"http://www.youtube.com/watch?v=Rn90iyutb7I",
             {struct,[{<<"provider_url">>,<<"http://www.youtube.com/">>},
                      {<<"title">>,<<"Dubstep Dance France">>},
                      {<<"html">>,
                       <<"<object width=\"480\" height=\"295\"><param name=\"movie\" value=\"http://www.youtube.com/v/Rn90iyutb7I?version=3\"></param><param name=\"allowFullScreen\" value=\"true\"></param><param name=\"allowscriptaccess\" value=\"always\"></param><embed src=\"http://www.youtube.com/v/Rn90iyutb7I?version=3\" type=\"application/x-shockwave-flash\" width=\"480\" height=\"295\" allowscriptaccess=\"always\" allowfullscreen=\"true\"></embed></object>">>},
                      {<<"author_name">>,<<"SAMkillerUA">>},
                      {<<"height">>,295},
                      {<<"thumbnail_width">>,480},
                      {<<"width">>,480},
                      {<<"version">>,<<"1.0">>},
                      {<<"author_url">>,
                       <<"http://www.youtube.com/user/SAMkillerUA">>},
                      {<<"provider_name">>,<<"YouTube">>},
                      {<<"thumbnail_url">>,
                       <<"http://i3.ytimg.com/vi/Rn90iyutb7I/hqdefault.jpg">>},
                      {<<"type">>,<<"video">>},
                      {<<"thumbnail_height">>,360}]}}],
    crypto:start(),
    meck:new(db),
    meck:expect(db, get_next, fun(_, _) -> crypto:rand_uniform(1, 1000000000000000) end),
    meck:expect(db, add_record, fun(Rec) -> {ok, Rec} end),
    mech:new(oembed),
    meck:expect(oembed, request, fun(Url, _, [{format, json}]) ->
                                         {_, _, Res} = lists:keyfind(Url, List),
                                         Res
                                 end),
    ok.

cleanup(_State) ->
    ok.

stub_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     ?_test(
        begin
            simple_t(),
            add_oembed_t(),
            ok
        end)
    }.


%%% NOTE: some oauth providers (e.g. blip.tv)
%%% ignore {format, json} parameter and return body as
%%% text/javascript. We need to take that into account.
add_oembed_t() ->
    ThatModule = feed_format,
    E = #entry{id = {1, 2},
               entry_id = 2,
               feed_id = 1,
               from = "myfeed",
               description = "check this out: http://www.youtube.com/watch?v=Rn90iyutb7I great moves!",
               created_time = erlang:now()},
    EMod = ThatModule:process(E),
    {ok, List} = ThatModule:get_details(EMod),
    [A | _] = List,
    #oembed_media{
                provider_url = <<"http://www.youtube.com">>,
                title = <<"Dubstep Dance France">>} = A,
    ok.

simple_t() ->
    ok.

