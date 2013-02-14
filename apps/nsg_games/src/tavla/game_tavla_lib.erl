%% Author: serge
%% Created: Feb 14, 2013
%% Description: TODO: Add description to game_tavla_lib
-module(game_tavla_lib).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([board_to_text1/1,
         board_to_text2/1]).

-define(BLACK, black).
-define(WHITE, white).
-define(WHITE_OUT, wo).
-define(WHITE_BAR, wb).
-define(BLACK_OUT, bo).
-define(BLACK_BAR, bb).

%%
%% API Functions
%%
board_to_text1(Board) ->
    Str1 = "13 14 15 16 17 18 BB 19 20 21 22 23 24 BO",
    Str4 = "                  ||                     ",
    Str7 = "12 11 10 09 08 07 WB 06 05 04 03 02 01 WO",
    List1 = [13, 14, 15, 16, 17, 18, ?BLACK_BAR, 19, 20 ,21, 22, 23, 24, ?BLACK_OUT],
    List2 = [12, 11, 10, 09, 08, 07, ?WHITE_BAR, 06, 05 ,04, 03, 02, 01, ?WHITE_OUT],
    Str2 = list_to_colors(List1, Board),
    Str3 = list_to_checkers(List1, Board),
    Str5 = list_to_checkers(List2, Board),
    Str6 = list_to_colors(List2, Board),
    [Str1, Str2, Str3, Str4, Str5, Str6, Str7].


board_to_text2(Board) ->
    Str1 = "|xoxoxo-xoxoxo| ",
    Str4 = "|------+------| ",
    Str7 = "|xoxoxo-xoxoxo| ",
    List1 = [13, 14, 15, 16, 17, 18, ?BLACK_BAR, 19, 20 ,21, 22, 23, 24],
    List2 = [12, 11, 10, 09, 08, 07, ?WHITE_BAR, 06, 05 ,04, 03, 02, 01],
    Str2 = "|" ++ list_to_colors2(List1, Board) ++ "|" ++ list_to_colors2([?BLACK_OUT], Board),
    Str3 = "|" ++ list_to_checkers2(List1, Board) ++ "|" ++ list_to_checkers2([?BLACK_OUT], Board),
    Str5 = "|" ++ list_to_checkers2(List2, Board) ++ "|" ++ list_to_checkers2([?WHITE_OUT], Board),
    Str6 = "|" ++ list_to_colors2(List2, Board) ++ "|" ++ list_to_colors2([?WHITE_OUT], Board),
    [Str1, Str2, Str3, Str4, Str5, Str6, Str7].


get_checkers(Pos, Board) ->
    {_, Value} = lists:keyfind(Pos, 1, Board),
    Value.

%%
%% Local Functions
%%

list_to_checkers(List, Board) ->
    F = fun(Pos) ->
                case get_checkers(Pos, Board) of
                    empty -> "   ";
                    {_, Num} -> io_lib:format("~2b ", [Num])
                end
        end,
    lists:flatmap(F, List).

list_to_colors(List, Board) ->
    F = fun(Pos) ->
                case get_checkers(Pos, Board) of
                    empty -> "   ";
                    {?WHITE, _} -> " W ";
                    {?BLACK, _} -> " B "
                end
        end,
    lists:flatmap(F, List).

list_to_checkers2(List, Board) ->
    F = fun(Pos) ->
                case get_checkers(Pos, Board) of
                    empty -> " ";
                    {_, Num} -> io_lib:format("~.16b", [Num])
                end
        end,
    lists:flatmap(F, List).

list_to_colors2(List, Board) ->
    [case get_checkers(Pos, Board) of
         empty -> " ";
         {?WHITE, _} -> "W";
         {?BLACK, _} -> "B"
     end || Pos <- List].

