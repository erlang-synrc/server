-module(score_db).

-define(COUCH_ACC, kakaconfig:fetch([couchdb, couch_acc], {"test.kakaranet.com", 5984})).
-define(COUCH_GAMES, kakaconfig:fetch([couchdb, couch_games], "okey_games")).

-export([get_skill/1, get_game_points/2, get_player_stats/1]).
-export([save_game/1]).
-export([reset_okey_stats/0]).

-include_lib("nsx_config/include/log.hrl").

%% TODO: move score db to riak

get_skill(UserId) ->
    {ok,0}.
    
%    A = erlang_couchdb:invoke_view(?COUCH_ACC, ?COUCH_GAMES,
%                                   "skill_rating", "okey_per_user",
%                                   [{"key", lists:flatten(io_lib:format("\"~s\"", [UserId]))}]),
%    {json, {struct, Rows}} = A,
%    case proplists:get_value(<<"rows">>, Rows) of
%        [{struct, Entry}] ->
%            {struct, ValueList} = proplists:get_value(<<"value">>, Entry),
%            {struct, Value} = proplists:get_value(list_to_binary(UserId), ValueList),
%            Delta = proplists:get_value(<<"d">>, Value),
%            {ok, Delta};
%        [] ->
%            {error, not_found};
%        undefined ->
%            {error, not_found}
%        undefined ->
%            {ok, 0};
%        _ -> {ok, 0}
%    end.

get_game_points(GameType, UserId) ->
%    ViewName = lists:flatten(io_lib:format("~s_per_user", [GameType])),
%    Key = lists:flatten(io_lib:format("\"~s\"", [UserId])),
%    A = erlang_couchdb:invoke_view(?COUCH_ACC, ?COUCH_GAMES,
%                                   "game_points", ViewName,
%                                   [{"key", Key}]),
%    {json, {struct, Rows}} = A,
%    case proplists:get_value(<<"rows">>, Rows) of
%        [{struct, Entry}] ->
%            {struct, ValueList} = proplists:get_value(<<"value">>, Entry),
%            {struct, Value} = proplists:get_value(list_to_binary(UserId), ValueList),
%            GPD = proplists:get_value(<<"gpd">>, Value),
%            WOD = proplists:get_value(<<"wod">>, Value),
%            W8TD = proplists:get_value(<<"w8td">>, Value),
%            {ok, [{game_points, GPD},
%                  {finished_with_okey, WOD},
%                  {finished_with_8_tashes, W8TD}]};
%        [] ->
%            {error, not_found};
%        undefined ->
%            {error, not_found}
%        undefined -> {ok, [{game_points,0}]};
%        _ -> 
        {ok, [{game_points,0}]}.
%    end.

get_player_stats(UserId) when is_binary(UserId) ->
    get_player_stats(binary_to_list(UserId));
get_player_stats(UserId) ->
%    ViewName = "per_user",
%    Key = lists:flatten(io_lib:format("\"~s\"", [UserId])),
%    A = erlang_couchdb:invoke_view(?COUCH_ACC, ?COUCH_GAMES,
%                                   "game_stats", ViewName,
%                                   [{"key", Key}]),
%    {json, {struct, Rows}} = A,
%    case proplists:get_value(<<"rows">>, Rows) of
%        [{struct, Entry}] ->
%            {struct, ValueList} = proplists:get_value(<<"value">>, Entry),
%            {struct, Value} = proplists:get_value(list_to_binary(UserId), ValueList),
%            TG = proplists:get_value(<<"gc">>, Value),
%            TW = proplists:get_value(<<"wd">>, Value),
%            TL = TG - TW,
%            OSR = float(TW) / float(TG),
%            {ok, [{total_games, TG},
%                  {total_wins, TW},
%                  {total_loses, TL},
%                  {overall_success_ratio, OSR},
%                  {total_disconnects, proplists:get_value(<<"dd">>, Value)},
%                  {average_play_time, proplists:get_value(<<"gt">>, Value)}
%                 ]};
%        [] ->
%            {error, not_found};
%        undefined ->
%            {error, not_found}
%        undefined -> {ok, [{total_games,0},{total_wins,0},{total_loses,0}]};
%        _ -> 
        {ok, [{total_games,-1},{total_wins,-1},{total_loses,0}]}.
%    end.

save_game(Game) ->
    ?INFO("SCORE SAVE GAME: ~p",[Game]),
%    save(?COUCH_GAMES, Game),
    {0,0}.

save(Db, Doc) ->
    DocConv = create_doc(Doc),
    A = erlang_couchdb:create_document(?COUCH_ACC, Db, DocConv),
    {json, {struct, Props}} = A,
    Id = binary_to_list(proplists:get_value(<<"id">>, Props, undefined)),
    Rev = binary_to_list(proplists:get_value(<<"rev">>, Props, undefined)),
    {Id, Rev}.

create_doc([{Key, _Val} | _] = Proplist) when is_atom(Key) ->
    L = [ {list_to_binary(atom_to_list(K)), create_doc(V)} || {K, V} <- Proplist ],
    erlang_couchdb:fold([ erlang_couchdb:set_value(K, V) || {K, V} <- L ],
                        erlang_couchdb:empty());
create_doc([T | _] = TupleList) when is_tuple(T) ->
    [ create_doc(X) || X <- TupleList ];
create_doc(Rec) when is_tuple(Rec) ->
    L0 = api_utils:members(Rec),
    RecType = api_utils:name(Rec),
    L = [ {list_to_binary(atom_to_list(K)), create_doc(V)} || {K, V} <- [{rectype, RecType} | L0] ],
    erlang_couchdb:fold([ erlang_couchdb:set_value(K, V) || {K, V} <- L ],
                        erlang_couchdb:empty());
create_doc(Else) ->
    Else.

%%%===================================================================
%%% Debug
%%%===================================================================

reset_okey_stats() ->
    A = erlang_couchdb:invoke_view(?COUCH_ACC, ?COUCH_GAMES, "games", "all_okey_game_results", []),
    {Total, _Offset, [_H | _] = Docs} = erlang_couchdb:parse_view(A),
    delete_iter(Docs, Total).

delete_iter([], _Counter) ->
    ok;
delete_iter([H | T], Counter) ->
    {_, _, P} = H,
    Id = binary_to_list(proplists:get_value(<<"_id">>, P)),
    Rev = binary_to_list(proplists:get_value(<<"_rev">>, P)),
    _Answer = erlang_couchdb:delete_document(?COUCH_ACC, ?COUCH_GAMES, Id, Rev),
    delete_iter(T, Counter-1).

%%%===================================================================
%%% CoachDb code
%%%===================================================================
%% Proper map and reduce function for okey game.
%% Map calculates expected score (ES) of each player in 4 players game
%%  and compares it to actual score. for single game actual score is 0.0 or 0.25 or 1.0
%%  (for math details see ELO rating for chess)
%%  based on both values change (delta) of rating is computed.
%% Reduce sums deltas from different games.
%%
%% Reduce function:
% function(keys, values, rereduce) {
%   if (!rereduce) {
%       var acc = [];
%       for (var i=0; i < values.length; i++) {
%           var spotted = false;
%           for (var j=0; j<acc.length; j++) {
%               if (values[i].player_id == acc[j].player_id) {
%                   spotted = true;
%                   acc[j].delta += values[i].delta;
%               }
%           }
%           if (!spotted) {
%               acc.push(values[i]);
%           }
%       }
%       return acc;
%   }
%   if (rereduce) {
%       var acc = values[0];
%       for (var k=1; k < values.length; k++) {
%           var cur = values[k];
%           var spotted = false;
%           for (var i=0; i<cur.length; i++) {
%               for (var j=0; j<acc.length; j++) {
%                   if (cur[i].player_id == acc[j].player_id) {
%                       spotted = true;
%                       acc[j].value.delta += cur[i].delta;
%                   }
%               }
%               if (!spotted) {
%                   acc.push(cur[i]);
%               }
%           }
%       }
%       return acc;
%   }
% }
%%
%%
%% Map function
% function(doc) {
% if (doc.rectype === "OkeyGameResults") {
%   var res = eval(uneval(doc.results));
%   for (var i = 0; i < res.length; i++) {
%       res[i].ES = 0.0;
%   }
%   for (var i = 0; i < res.length; i++) {
%       var Ri = res[i];
%       for (var j = 0; j < res.length; j++) {
%           var Rj = res[i];
%           var Pow = Math.pow(10, (Ri.skill_level - Rj.skill_level)/400);
%           var Ei = 1.0 / (1.0 + Pow);
%           Ri.ES += Ei / 8.0;
%       }
%   }
%   for (var i = 0; i < res.length; i++) {
%       var Ri = res[i];
%       Ri.delta = 32*(Ri.skill_delta - Ri.ES);
%   }
%
%   for (var i = 0; i < doc.results.length; i++) {
%       emit(res[i].player_id, res[i]);
%   }
%   }
% }
