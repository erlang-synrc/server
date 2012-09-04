-module(nsx_util_slugs).

-export([slugify/1]).

-define(tolower(C), (C+32)).
-define(islower(C), (C >= $a andalso C =< $z)).
-define(isupper(C), (C >= $A andalso C =< $Z)).
-define(isdigit(C), (C >= $1 andalso C =< $9)).
-define(isspace(C), (
    C =:= $\s orelse C =:= $\n orelse C =:= $\t orelse C =:= $\r
)).

-define(isdiacrit(C), (
    (C >= 224 andalso C =< 255 andalso C =/= 247) orelse
    (C >= 192 andalso C =< 223 andalso C =/= 215) orelse
    C =:= 131 orelse C =:= 138 orelse C =:= 140 orelse C =:= 142 orelse
    C =:= 154 orelse C =:= 156 orelse C =:= 158 orelse C =:= 159 orelse
    C =:= 304 orelse C =:= 305 orelse C =:= 287 orelse C =:= 286 orelse
    C =:= 350 orelse C =:= 351

)).


slugify([]) ->
    [];
slugify(<<>>) -> <<>>;
slugify(Str) when is_list(Str) ->
    lists:flatten(slugify(f(Str), []));
slugify(Str) when is_binary(Str) ->
    list_to_binary(slugify(f(Str), [])).

slugify([C | Rest], Acc) when ?islower(C) orelse ?isdigit(C) orelse C =:= $_ ->
    slugify(Rest, [C | Acc]);
slugify([C | Rest], Acc) when ?isupper(C) ->
    slugify(Rest, [?tolower(C) | Acc]);
slugify([C | Rest], Acc) when ?isspace(C) orelse C =:= $/ ->
    Acc1 = case Acc of
        [$- | _] -> Acc;
        _ -> [$- | Acc]
    end,
    slugify(Rest, Acc1);
slugify([C | Rest], Acc) when ?isdiacrit(C) ->
    slugify(Rest, [translit(C) | Acc]);
slugify([_ | Rest], Acc) ->
  slugify(Rest, Acc);
slugify([], Acc) ->
    Acc1 = case Acc of
        [$- | T] -> T;
        _ -> Acc
    end,
    case lists:reverse(Acc1) of
        [$- | T2] -> T2;
        Out -> Out
    end.

translit(131) -> $f;
translit(138) -> $s;
translit(140) -> "oe";
translit(142) -> $z;
translit(154) -> $s;
translit(156) -> "oe";
translit(158) -> $z;
translit(159) -> $y;
translit(192) -> $a;
translit(193) -> $a;
translit(194) -> $a;
translit(195) -> $a;
translit(196) -> $a;
translit(197) -> $a;
translit(198) -> "ae";
translit(199) -> $c;
translit(200) -> $e;
translit(201) -> $e;
translit(202) -> $e;
translit(203) -> $e;
translit(204) -> $i;
translit(205) -> $i;
translit(206) -> $i;
translit(207) -> $i;
translit(208) -> "dh";
translit(209) -> $n;
translit(210) -> $o;
translit(211) -> $o;
translit(212) -> $o;
translit(213) -> $o;
translit(214) -> $o;
translit(216) -> $o;
translit(217) -> $u;
translit(218) -> $u;
translit(219) -> $u;
translit(220) -> $u;
translit(221) -> $y;
translit(222) -> "th";
translit(223) -> "ss";
translit(224) -> $a;
translit(225) -> $a;
translit(226) -> $a;
translit(227) -> $a;
translit(228) -> $a;
translit(229) -> $a;
translit(230) -> "ae";
translit(231) -> $c;
translit(232) -> $e;
translit(233) -> $e;
translit(234) -> $e;
translit(235) -> $e;
translit(236) -> $i;
translit(237) -> $i;
translit(238) -> $i;
translit(239) -> $i;
translit(240) -> "dh";
translit(241) -> $n;
translit(242) -> $o;
translit(243) -> $o;
translit(244) -> $o;
translit(245) -> $o;
translit(246) -> $o;
translit(248) -> $o;
translit(249) -> $u;
translit(250) -> $u;
translit(251) -> $u;
translit(252) -> $u;
translit(253) -> $y;
translit(254) -> "th";
translit(255) -> $y;
%% turkish specific
translit(286) -> $g;
translit(287) -> $g;
translit(304) -> $i;
translit(305) -> $i;
translit(350) -> $s;
translit(351) -> $s;
translit(C)  ->  C.


f(S) ->
    %% make this format to get UTF-8 codes
    [UTF] = io_lib:format("~ts", [S]),
    UTF.


