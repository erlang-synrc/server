-module(format_oembed).
-include_lib("nsm_db/include/feed.hrl").
-compile(export_all).

-spec search(iolist()) -> list().
search(T) ->
    Req = re:run(T, link_regex(), [{capture, all, index}, extended, global]),
    case Req of 
        nomatch ->
            [];
        {match, Data} ->
            [ begin
                  {SUrl0, LUrl0} = lists:nth(1, LinkInfo),
                  SUrl = SUrl0+1,
                  LUrl = LUrl0+SUrl0,

                  {SUrl, LUrl}
              end || LinkInfo <- Data ]
    end.


-spec format(string(), #media{}) -> {string(), #media{}}.
format(Url, #entry{media=Media} = Entry) ->
    NewElement = short_link(Url),
    MyMedia0 = oembed(Url),
    MyMedia = lists:flatten([MyMedia0 | Media]),
    NewEntry = Entry#entry{media = lists:reverse(MyMedia)},
    {lists:flatten(NewElement), NewEntry}.

-spec oembed(string()) -> string().
oembed(Url) ->
    Domain = get_domain(Url),
    case request_oembed(Url, Domain) of
        {error, _} = E ->
            io:fwrite("~p - probably oEmbed not suported for: ~p~n", [E, Url]),
            [];
        R ->
            R
    end.

-spec request_oembed(string(), string()) -> #media{} | tuple().
request_oembed(Url, Domain) ->
    Host = oembed_url(Domain),
    case oembed:request(Url, Host, [{format, json}]) of
        {struct, R} ->
            #media{title = proplists:get_value(<<"title">>, R, Url),
                   width = proplists:get_value(<<"width">>, R),
                   height = proplists:get_value(<<"height">>, R),
                   html = proplists:get_value(<<"html">>, R),
                   url = proplists:get_value(<<"url">>, R),
                   version = proplists:get_value(<<"version">>, R),
                   thumbnail_url = proplists:get_value(<<"thumbnail_url">>, R),
                   thumbnail_height = proplists:get_value(<<"thumbnail_height">>, R),
                   type = validate_type(proplists:get_value(<<"type">>, R))};
        {error, _} = E ->
            E
    end.


-spec validate_type(binary()) -> atom().
validate_type(<<"video">>) ->
    {link, video};
validate_type(<<"photo">>) ->
    {link, photo}.


-spec oembed_url(string()) -> string().
oembed_url(Domain) ->
    L = [{"youtube", "http://www.youtube.com/oembed"},
         {"vimeo", "http://vimeo.com/api/oembed.json"},
         {"flickr", "http://www.flickr.com/services/oembed/"}],
    Default = "http://oohembed.com/oohembed/",
    lists:flatten(proplists:get_value(Domain, L, Default)).    


-spec get_domain(string()) -> string().
get_domain(Url) ->
    io:fwrite("Url: ~p~n", [Url]),
    {match, [Req]} = re:run(Url, link_regex(), [{capture, all, binary}, extended, global]),
    lists:flatten( binary_to_list(lists:nth(4, Req)) ).

-spec short_link(string()) -> string().
short_link(Url) ->
    Text = case length(Url) of
               X when X >= 30 ->
                   N = string:sub_string(Url, 1, 28),
                   lists:concat([N, "..."]);
               _ ->
                   Url
           end,
    lists:flatten( io_lib:fwrite("<a href=\"~s\" target=\"_blank\">~s</a>", [Url, Text]) ).

-spec link_regex() -> string().
link_regex() ->
    "(?P<protocol>(?:(?:f|ht)tp|https|irc|steam):\\/\\/)?"
        "(?P<domain>(?:(?!-)"
        "(?P<sld>[a-zA-Z\\d\\-]+)(?<!-)"
        "[\\.]){1,2}"
        "(?P<tld>(?:[a-zA-Z]{2,}\\.?){1,}){1,}"
        "|"
        "(?P<ip>(?:(?(?<!\\/)\\.)(?:25[0-5]|2[0-4]\\d|[01]?\\d?\\d)){4})"
        ")"
        "(?::(?P<port>\\d{2,5}))?"
        "(?:\\/"
        "(?P<script>[~a-zA-Z\\/.0-9-_]*)?"
        "(?:\\?(?P<parameters>[=a-zA-Z+%&0-9,.\\/_-]*))?"
        ")?"
        "(?:\\#(?P<anchor>[!=a-zA-Z+%&0-9._\\/]*))?".


regex_test() ->
    [<<"http://a.com">>]            = search("http://a.com"),
    [<<"http://example.com">>]      = search("http://example.com"),
    [<<"http://example.com:1243">>] = search("http://example.com:1243"),
    [<<"http://example.com/">>]     = search("http://example.com/"),
    [<<"http://www.example.com/">>] = search("http://www.example.com/"),
    [<<"http://www.example-website.com/">>] = search("http://www.example-website.com/"),
    [<<"http://www.example.net/">>]    = search("http://www.example.net/"),
    [<<"http://www.example.co.uk/">>]  = search("http://www.example.co.uk/"),
    [<<"http://www.example.us/">>]     = search("http://www.example.us/"),
    [<<"https://www.example.com/">>]   = search("https://www.example.com/"),
    [<<"ftp://www.example.com/">>]     = search("ftp://www.example.com/"),
    [<<"irc://www.example.com/">>]     = search("irc://www.example.com/"),
    [<<"steam://www.example.com/">>]   = search("steam://www.example.com/"),
    [<<"http://example.museum/">>]     = search("http://example.museum/" ),
    [<<"http://10.23.55.21">>]    = search("http://10.23.55.21"),
    [<<"http://10.23.55.21/">>]   = search("http://10.23.55.21/"), 
    [<<"https://10.23.55.21/">>]  = search("https://10.23.55.21/"),
    [<<"ftp://10.23.55.21/">>]    = search("ftp://10.23.55.21/"),
    [<<"irc://10.23.55.21/">>]    = search("irc://10.23.55.21/"),
    %% [<<"http://user@example.com">>]          = search("http://user@example.com"),
    %% [<<"http://user:pass@example.com">>]     = search("http://user:pass@example.com"),
    %% [<<"http://user:pass@www.example.com">>] = search("http://user:pass@www.example.com"),
    %% [<<"http://user:pass@www.example.com/index.html">>] = search("http://user:pass@www.example.com/index.html"),
    %% [<<"http://user@10.23.55.21">>]       = search("http://user@10.23.55.21"),
    %% [<<"http://user:pass@10.23.55.21">>]  = search("http://user:pass@10.23.55.21"),
    %% [<<"http://user:pass@10.23.55.21/">>] = search("http://user:pass@10.23.55.21/"),
    %% [<<"http://user:pass@10.23.55.21/index.html">>] = search("http://user:pass@10.23.55.21/index.html"),
    [<<"http://www.example.com/index.html">>]       = search("http://www.example.com/index.html"),
    [<<"http://www.example.com/index.html">>]       = search("http://www.example.com/index.html"),
    [<<"http://www.example.com/index.html.php">>]   = search("http://www.example.com/index.html.php"),
    [<<"http://www.example.com/index.html#1234">>]  = search("http://www.example.com/index.html#1234"),
    [<<"http://www.example.com/#1234">>]      = search("http://www.example.com/#1234"),
    [<<"http://www.example.com/index.php">>]  = search("http://www.example.com/index.php"),
    [<<"http://www.example.com/index.php?">>] = search("http://www.example.com/index.php?"),
    %% [<<"http://www.example.com?foo=bar">>]    = search("http://www.example.com?foo=bar"),
    [<<"http://www.example.com/index.php?foo=bar">>]           = search("http://www.example.com/index.php?foo=bar"),
    [<<"http://www.example.com/index.php?foo=bar&crap=junk">>] = search("http://www.example.com/index.php?foo=bar&crap=junk"),
    [<<"http://www.example.com/index.php?foo=bar&crap=junk#1234">>] = search("http://www.example.com/index.php?foo=bar&crap=junk#1234"),
    [<<"http://www.example.com/index.php?foo=&crap=junk">>]         = search("http://www.example.com/index.php?foo=&crap=junk"),
    [<<"http://www.example.com/?foo=bar&crap=junk#1234">>]          = search("http://www.example.com/?foo=bar&crap=junk#1234"),
    [<<"http://www.example.com/foo/bar/index.html">>]               = search("http://www.example.com/foo/bar/index.html"),
    [<<"http://www.example.com/foo/bar/index.html#1234">>]   = search("http://www.example.com/foo/bar/index.html#1234"),
    [<<"http://www.example.com/foo/bar/index.php">>]         = search("http://www.example.com/foo/bar/index.php"),
    [<<"http://www.example.com/foo/bar/index.php?">>]        = search("http://www.example.com/foo/bar/index.php?"),
    [<<"http://www.example.com/foo/bar/index.php?foo=bar">>] = search("http://www.example.com/foo/bar/index.php?foo=bar"),
    [<<"http://www.example.com#">>]              = search("http://www.example.com#"),
    [<<"http://www.example.com/#">>]             = search("http://www.example.com/#"),
    [<<"http://www.example.com/index.html#">>]   = search("http://www.example.com/index.html#"),
    [<<"http://www.example.com/index.html#foo/bar">>]  = search("http://www.example.com/index.html#foo/bar"),
    [<<"http://www.example.com/index.html#/">>]  = search("http://www.example.com/index.html#/"),
    [<<"http://www.example.com/index.html#!/">>] = search("http://www.example.com/index.html#!/"),
    [<<"http://www.example.com/index.html#!/foo/bar">>] = search("http://www.example.com/index.html#!/foo/bar"),
    [<<"http://www.example.com/#!/foo/bar">>]           = search("http://www.example.com/#!/foo/bar"),
    
    Text = io_lib:fwrite("Simple text with links: ~n~s,~n~s and~n~s . Enjoy!", 
                          ["www.google.pl", "http://www.youtube.com/watch?v=WrrvkPo7TZ4",
                           "http://4.bp.blogspot.com/_oLbtTQY0cnE/TIgcnggvQyI/AAAAAAAAAF8/jlDFC95ZWE8/s1600/darth-vader-face.jpg"]),
    [<<"www.google.pl">>,
     <<"http://www.youtube.com/watch?v=WrrvkPo7TZ4">>,
     <<"http://4.bp.blogspot.com/_oLbtTQY0cnE/TIgcnggvQyI/AAAAAAAAAF8/jlDFC95ZWE8/s1600/darth-vader-face.jpg">>]
        =
        search(Text),
    "Regex test OK".
