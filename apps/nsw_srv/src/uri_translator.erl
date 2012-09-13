-module(uri_translator).

-include_lib("nsm_srv/include/uri_translator.hrl").
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("setup.hrl").
-include_lib("alog/include/alog.hrl").

%% API
-export([languages/0, language/1, translate/1, translate/2, translate/3]).

%%====================================================================
%% API
%%====================================================================
-spec languages() -> [string()].
languages() ->
    case get(cache__languages) of
        undefined ->
            L = languages0(),
            put(cache__languages, L),
            L;
        L -> L
    end.

% Get all supported languages
languages0() ->
    sets:to_list(
      sets:from_list( %% TODO: may be it's too bad for speed? Returning ["tr", "en"] can be just fine
        [ L || #ut_word{lang = L} <- rpc:call(?APPSERVER_NODE,nsm_db,all,[ut_word])]
        )).

-spec language(string()) -> undefined | string().
language("/") ->
    undefined;
language(Uri) ->
    UriTokens = tokenize(Uri),
    {Path, _Query} = lists:splitwith(fun(E) -> E =/= "?" end, UriTokens),
    language2(Path).

language2("") -> undefined;
language2([Word|Tail]) ->
    case Word of
	"." -> language2(Tail);
	".."-> language2(Tail);
	"/" -> language2(Tail);
	""  -> language2(Tail);
	_   -> case rpc:call(?APPSERVER_NODE,nsm_db,get_word,[Word]) of
		   {ok, #ut_word{lang = Lang}} -> Lang;
		   {error, duplicated} -> "en";
		   {error, _} ->
		       ?PRINT({unknown_language, Word}),
		       undefined
	       end
    end.

-spec translate(string()) -> string().
translate(Uri) ->
    case language(Uri) of
		undefined -> translate(Uri, site_utils:detect_language());
		SrcLang -> translate(Uri, SrcLang)
	end.

-spec translate(string(), string()) -> string().
translate(Uri, SrcLang) ->
	translate(Uri, SrcLang, "en").

-spec translate(string(), string(), string()) -> string().
translate("/", _Lang, _DstLang) -> "/";
translate(Uri, Lang, Lang) -> Uri;
translate(Uri, "en", DstLang) ->
    translate2(Uri, DstLang, from_en);
translate(Uri, SrcLang, "en") ->
    translate2(Uri, SrcLang, to_en);
translate(Uri, SrcLang, DstLang) ->
    translate(translate(Uri, SrcLang, "en"), "en", DstLang).

translate2(Uri, Lang, Direction) ->
    is_supported(Lang),
    UriTokens = tokenize(Uri),
    {Path, Query} = lists:splitwith(fun(E) -> E =/= "?" end, UriTokens),
    TranslatedPath =
	[case Word of
	     "." -> ".";
	     ".."-> "..";
	     "/" -> "/";
	     ""  -> "";
	     _   ->
                 %% don't try to translate numeric words, leave as is
                 case re:run(Word, "^[0-9]+$") of
                     nomatch ->
                         translate_word(Direction, Word, Lang);
                     {match, _} ->
                         Word
                 end
	 end || Word <- Path ],
    lists:flatten([TranslatedPath,Query]).

translate_word(to_en, ForeignWord, SrcLang) ->
    case rpc:call(?APPSERVER_NODE,nsm_db,get_translation,[{SrcLang,ForeignWord}]) of
	{ok, #ut_translation{word = EnglishWord}} -> EnglishWord;
	{error, _} ->
	    ?ERROR("unknown translation: ~p", [{ForeignWord, SrcLang}]),
	    ForeignWord
    end;
translate_word(from_en, EnglishWord, DstLang) ->
    case rpc:call(?APPSERVER_NODE,nsm_db,get_translation,[{DstLang, EnglishWord}]) of
	{ok, #ut_translation{word = ForeignWord}} -> ForeignWord;
	{error, _} ->
	    ?ERROR("unknown translation: ~p", [{EnglishWord, DstLang}]),
	    EnglishWord
    end.

is_supported(Lang) ->
    case lists:member(Lang, languages()) of
	false ->
            ?ERROR("Unsupported language: ~p",[Lang]);
%	    throw(unsupported_language);
	true ->
	    ok
    end.

translate_test() ->
    ?assertEqual("/", translate("/")),
    ?assertEqual("/matchmaker/okey?csid=218765", translate("/matchmaker/okey?csid=218765")),
    ?assertEqual("/matchmaker/okey/?csid=218765", translate("/matchmaker/okey/?csid=218765")),
    ?assertEqual("/matchmaker/?csid=218765", translate("/matchmaker/?csid=218765")),
    ?assertEqual("/matchmaker?csid=218765", translate("/matchmaker?csid=218765")),
    ?assertEqual("/matchmake/oke?csid=218765", translate("/matchmake/oke?csid=218765")),
    ?assertEqual("/matchmake/oke/?csid=218765", translate("/matchmake/oke/?csid=218765")),
    ?assertEqual("/matchmake/?csid=218765", translate("/matchmake/?csid=218765")),
    ?assertEqual("/matchmake?csid=218765", translate("/matchmake?csid=218765")),

    ?assertEqual("./matchmaker/okey?csid=218765", translate("./matchmaker/okey?csid=218765")),
    ?assertEqual("./matchmaker/okey/?csid=218765", translate("./matchmaker/okey/?csid=218765")),
    ?assertEqual("./matchmaker/?csid=218765", translate("./matchmaker/?csid=218765")),
    ?assertEqual("./matchmaker?csid=218765", translate("./matchmaker?csid=218765")),
    ?assertEqual("./matchmake/oke?csid=218765", translate("./matchmake/oke?csid=218765")),
    ?assertEqual("./matchmake/oke/?csid=218765", translate("./matchmake/oke/?csid=218765")),
    ?assertEqual("./matchmake/?csid=218765", translate("./matchmake/?csid=218765")),
    ?assertEqual("./matchmake?csid=218765", translate("./matchmake?csid=218765")),

    ?assertEqual("/matchmaker/okey", translate("/matchmaker/okey")),
    ?assertEqual("/matchmaker/", translate("/matchmaker/")),
    ?assertEqual("/matchmaker/?test", translate("/matchmaker/?test")),
    ?assertEqual("/matchmake/oke", translate("/matchmake/oke")),
    ?assertEqual("/matchmake/", translate("/matchmake/")),
    ?assertEqual("/matchmake/?test", translate("/matchmake/?test")),
    ?assertEqual("/?test", translate("/?test")),

    ?assertEqual("../?test", translate("../?test")),

    ?assertEqual("/oyun-kuran/okey/?csid=218765", translate("/matchmaker/okey/?csid=218765", "en", "tr")),
    ?assertEqual("/oyun-kuran/okey/csid/218765", translate("/matchmaker/okey/csid/218765", "en", "tr")),
    ok.


-spec tokenize(string()) -> [string()].
% @doc splits string into deep list containing substrings
% that can be "", "/", "?", and string containin any other text.
% example tokenize("a/bc/d?efg") = ["a", "/", "bc", "/", "d", "?", "efg"]
% example tokenize("/a/?") = ["", "/", "a", "/", "", "?", ""]
% example tokenize("/") = ["", "/", ""]
tokenize(Uri) ->
    tokenize(Uri, [""]).

tokenize("/"++Tail, [AccHead|AccTail]) ->
    NewAcc = [lists:reverse(AccHead)|AccTail],
    NewHead = [], NewTail = ["/"|NewAcc],
    tokenize(Tail, [NewHead|NewTail]);
tokenize("?"++Tail, [AccHead|AccTail]) ->
    NewAcc = [lists:reverse(AccHead)|AccTail],
    NewHead = [], NewTail = ["?"|NewAcc],
    tokenize(Tail, [NewHead|NewTail]);
tokenize([H|Tail], [AccHead|AccTail]) ->
    NewHead = [H] ++ AccHead, NewTail = AccTail,
    tokenize(Tail, [NewHead|NewTail]);
tokenize("", [AccHead|AccTail]) ->
    NewAcc = [lists:reverse(AccHead)|AccTail],
    lists:reverse(NewAcc).
