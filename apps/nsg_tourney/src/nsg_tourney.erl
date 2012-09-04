%%%-------------------------------------------------------------------
%%% @author Dennis Novikov <dennis.novikov@gmail.com>
%%% @copyright Paynet Internet ve Bilisim Hizmetleri A.S. All Rights Reserved.
%%% @doc
%%% Tournament application API module
%%% @end
%%%-------------------------------------------------------------------

-module(nsg_tourney).


-include("tournament.hrl").


-export([start_tournament/2
        ,start_tournament/3
        ]).


-spec start_tournament(Game::atom(), Params::list()) ->
                              {ok, TManager::pid()}
                            | {error, Reason::term()}.
%% @doc
%% Start tournament, return tournament manager PID
%% @end
start_tournament(Game, Params) ->
    ID = id_generator:get_id(),
    start_tournament(ID, Game, Params).

-spec start_tournament(ID::term(), Game::atom(), Params::list()) ->
                              {ok, TManager::pid()}
                            | {error, Reason::term()}.
%% @doc
%% Start tournament, return tournament manager PID
%% @end
start_tournament(ID, Game, Params) ->
    case valid_params(Params) of
        true ->
            nsg_tourney_master:start_tournament(ID, Game, Params);
        false ->
            lager:error([{tournament, ID}],
                        "Bad tournament params: ~w", Params),
            {error, {invalid_params,{ID, Game, Params}}}
    end.


%% ---------------
%% Local functions

valid_params(_Params) ->
    true.
