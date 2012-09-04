%%%-------------------------------------------------------------------
%%% @author Paul Peregud <pawel@saturn.lan>
%%% @copyright (C) 2011, Paul Peregud
%%% @doc
%%%
%%% @end
%%% Created :  4 May 2011 by Paul Peregud <pawel@saturn.lan>
%%%-------------------------------------------------------------------
-module(kakamath).

%% API
-export([variate/1, combination/2, powerset/1]).
-export([draw_random/1]).
-export([all_substitutions/3]).

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

-spec combination(number(),[any()]) -> [[any()]].
combination(1, L) -> [ [X] || X <- L ];
combination(K, L) when K == length(L) -> [L];
combination(K, [H|T]) ->
    [[H | Sub] || Sub <- combination(K-1, T)]
        ++(combination(K, T)).

-spec powerset([any()]) -> [[any()]].
powerset(List) ->
    lists:foldl(
      fun(K, Acc) -> Acc++(combination(K, List)) end,
      [[]],
      lists:seq(1, length(List))
     ).

-spec variate([any()]) -> [any()].
variate(List) ->
    L = lists:map(fun(X) -> {crypto:rand_uniform(1, 10000), X} end, List),
    {_, Res} = lists:unzip(lists:sort(L)),
    Res.

-spec draw_random([any()]) -> {any(), [any()]}.
draw_random(List) ->
    Pos = crypto:rand_uniform(1, length(List)),
    Element = lists:nth(Pos, List),
    ResList = lists:delete(Element, List),
    {Element, ResList}.

%% this function returns all possible substitutions as if they are optional
-spec all_substitutions([any()], [any()], [any()]) -> [[any()]].
all_substitutions([], _What, _With) ->
    [[]];
all_substitutions([H | T], What, With) when H == What->
    LofL = all_substitutions(T, What, With),
    [ [NewHead | SomeTail] || NewHead <- With, SomeTail <- LofL ];
all_substitutions([H | T], What, With) ->
    LofL = all_substitutions(T, What, With),
    [ [H | SomeTail] || SomeTail <- LofL ].

