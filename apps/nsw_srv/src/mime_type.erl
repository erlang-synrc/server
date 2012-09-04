%%----------------------------------------------------------------------
%% @author Yura Zhloba <yzh44yzh@gmail.com>
%% @copyright Paynet Internet ve Bilisim Hizmetleri A.S. All Rights Reserved.
%% @doc
%% This module implements mime type detection.
%% It uses unix file utility and works properly on unix only.
%% @end
%%----------------------------------------------------------------------


-module(mime_type).

-export([identify/1]).

-spec identify(string()) -> {ok, string()} | {error, string()}.
identify(Filename) ->
    Info = os:cmd("file -b --mime-type " ++ Filename),
    Result = case re:run(Info, "^[a-zA-Z0-9_\\-\\.]+/[a-zA-Z0-9\\.\\-_]+", [{capture, first, list}]) of
		 nomatch -> {error, Info};
		 {match, [Type|_]} -> {ok, Type}
	     end,
    Result.
