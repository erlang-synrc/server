%%----------------------------------------------------------------------
%% @author Yura Zhloba <yzh44yzh@gmail.com>
%% @copyright Paynet Internet ve Bilisim Hizmetleri A.S. All Rights Reserved.
%% @doc
%% This module resizes images. It uses graphics magick http://www.graphicsmagick.org/index.html
%% @end
%%----------------------------------------------------------------------


-module(image_processing).

-export([make_thumb/4, make_thumb/5, resize/4, resize/5, rescale/4, rescale/5]).
-export([is_image/1, get_size/1, get_min_size/1]).

%% @doc
%% Input should be path to the image (e.g. /tmp/image232.jpg)
%% Width, Height should be pixels (e.g. 400)
%% Output should be path to the output image (e.g. images/user1.jpg)
%% @end
-spec make_thumb(string(), pos_integer(), pos_integer(), string()) -> ok | {error, string()}.
make_thumb(Input, Width, Height, Output) ->
    make_thumb(Input, Width, Height, Output, 75).


-spec make_thumb(string(), pos_integer(), pos_integer(), string(), pos_integer()) -> ok | {error, string()}.
make_thumb(Input, Width, Height, Output, Quality) ->
    MinSize = get_min_size(Input),
    ISize = size_to_str(MinSize, MinSize),
    TSize = size_to_str(Width, Height),
    Cmd = lists:concat(["gm convert ", Input, "[0]",
			" -gravity center",
			" -extent ", ISize,
			" -quality ", integer_to_list(Quality),
			" -resize ", TSize,
    			" +profile \"*\" ", Output]),
    case os:cmd(Cmd) of
	[] -> ok;
	Error -> {error, Error}
    end.


-spec resize(string(), pos_integer(), pos_integer(), string()) -> ok | {error, string()}.
resize(Input, Width, Height, Output) ->
    resize(Input, Width, Height, Output, 75).


-spec resize(string(), pos_integer(), pos_integer(), string(), pos_integer()) -> ok | {error, string()}.
resize(Input, Width, Height, Output, Quality) ->
    Size = size_to_str(Width, Height),
    Cmd = lists:concat(["gm convert ", Input,
    			" -quality ", integer_to_list(Quality),
    			" -resize ", Size,
    			" +profile \"*\" ", Output]),
    case os:cmd(Cmd) of
	[] -> ok;
	Error -> {error, Error}
    end.


-spec rescale(string(), pos_integer(), pos_integer(), string()) -> ok | {error, string()}.
rescale(Input, Width, Height, Output) ->
    rescale(Input, Width, Height, Output, 75).


-spec rescale(string(), pos_integer(), pos_integer(), string(), pos_integer()) -> ok | {error, string()}.
rescale(Input, Width, Height, Output, Quality) ->
    Size = size_to_str(Width, Height),
    Cmd = lists:concat(["convert ", Input,
			" -quality ", integer_to_list(Quality),
			" -thumbnail ", Size,
			" -gravity center",
			" -extent ", Size,
			Output]),
    case os:cmd(Cmd) of
	[] -> ok;
	Error -> {error, Error}
    end.


-spec is_image(string()) -> boolean().
is_image(Filename) ->
    case mime_type:identify(Filename) of
	{ok, "image/jpeg"} -> true;
	{ok, "image/png"} -> true;
	{ok, "image/gif"} -> true;
	_ -> false
    end.


-spec get_size(string()) -> {integer(), integer()}.
get_size(Input) ->
    Cmd = lists:concat(["gm identify ", Input]),
    Output = os:cmd(Cmd),
    {match, [Size]} = re:run(Output, "[0-9]+x[0-9]+", [{capture, first, list}]),
    [W, H] = re:split(Size, "x"),
    {list_to_integer(binary_to_list(W)), list_to_integer(binary_to_list(H))}.


-spec get_min_size(string()) -> integer().
get_min_size(Input) ->
    case get_size(Input) of
	{W, H} when W > H -> H;
	{W, _H} -> W
    end.

size_to_str(Width, Height) ->
    lists:concat([integer_to_list(Width), "x", integer_to_list(Height), " "]).

