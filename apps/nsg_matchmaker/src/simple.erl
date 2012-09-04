%%----------------------------------------------------------------------
%% @author Paul Peregud <paulperegud@gmail.com>
%% @copyright Paynet Internet ve Bilisim Hizmetleri A.S. All Rights Reserved.
%% @doc
%% Very simple users grouping.
%% @end
%%-------------------------------------------------------------------
-module(simple).

%% API
-export([prepare_request/2, match/3]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
match(List, _DFI, Requirements) ->
    ReqPlayers = proplists:get_value(players, Requirements),
    case length(List) of
        L when L >= ReqPlayers ->
            {Party, _Other} = lists:split(ReqPlayers, List),
            {ok, Party};
        _ ->
            false
    end.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
prepare_request(Session, _GameAtom) when is_pid(Session) ->
    none.

%%%===================================================================
%%% Internal functions
%%%===================================================================
