%%----------------------------------------------------------------------
%% @author Paul Peregud <paulperegud@gmail.com>
%% @copyright Paynet Internet ve Bilisim Hizmetleri A.S. All Rights Reserved.
%% @doc
%% Implements grouping of users in groups with minimal variance of
%% skill. Takes into account time, spent by user in queue.
%% @end
%%-------------------------------------------------------------------
-module(skill).

%% API
-export([prepare_request/2, match/3]).
-export([get_replacement/1]).

-record(skill, {
          sp = 500   :: integer(),   %% skill points
          wr = 0.0   :: float(),     %% win/lose ratio, ranged from [-1.0, 1.0]
          since = local_now()        %%
         }).

-include_lib("nsg_srv/include/logging.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%% DFI stands for "data field index"
%% List :: list(Tuple())
-spec match([any()],_,[any()]) -> 'false' | {'ok',[tuple()]}.
match(List, DFI, Requirements) ->
    ReqPlayers = proplists:get_value(players, Requirements),
    case length(List) of
        L when L >= ReqPlayers ->
            ?PP("queue of sufficient length, doing skill based matching. ~nList: ~p", [List]),
            {Score, Party} = pick_by_skill(List, DFI, ReqPlayers),
            case Score of
                Score when Score < 1 ->
                    {ok, Party};
                _ ->
                    false
            end;
        _ ->
            ?PP("queue too short. ~nList: ~p", [List]),
            false
    end.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
-spec get_replacement([any()]) -> 'false' | {'ok', tuple()}.
get_replacement(List) ->
    {A, _B} = lists:partition(fun(_) -> true end, List),
    case A of
        [] -> false;
        _ -> {ok, hd(A)}
    end.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
-spec prepare_request(pid(),_) -> #skill{sp::500,wr::float(),since::float()}.
prepare_request(Session, _GameAtom) when is_pid(Session) ->
    #skill{}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TODO:
% create a testing simulation with candidates added is in pareto
% distribution and candidate's skill being that of logistic
% distribution. Do simulation for large (over 300 candidates per minute)
% and small (below 10 per minute) traffic
% http://stackoverflow.com/questions/3955877/generating-samples-from-the-logistic-distribution

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% idea : get all combinations of length 4, rate each of them
% using standard deviation of skill, lowering it somehow to take
% into account long waiting time of some players
% return group with best score
-spec pick_by_skill([any()],_,_) -> {float(),[tuple()]}.
pick_by_skill(List0, DFI, N) ->
    Now = local_now(),
    List = lists:zip(lists:seq(1, length(List0)), List0),
    Candidates = [ {Key, element(DFI, X)} || {Key, X} <- List ],

    Combs = kakamath:combination(N, Candidates),

    Rated = [ {rate(Now, X, N), X} || X <- Combs ],
    Sorted = lists:keysort(1, Rated),
    {LScore, WinList0} = hd(Sorted),
    WinList = [ element(2, lists:keyfind(LKey, 1, List)) || {LKey, _} <- WinList0 ],
    {LScore, WinList}.

-spec rate(float(),[any()],number()) -> float().
rate(Now, Group, N) ->
    Sps = [ SP + (WR * 200) || {_, #skill{sp = SP, wr = WR}} <- Group ],
    Mean = lists:sum(Sps) / N,
    SD = lists:sum([ (X-Mean)*(X-Mean) || X <- Sps ]),
    Wt = lists:sum([ T-Now || {_, #skill{since = T}} <- Group ]),
    SD / Wt.

-spec local_now() -> float().
local_now() ->
    {_A, B, C} = now(),
    B+C/1000000.
