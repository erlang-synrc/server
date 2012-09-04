%%%-------------------------------------------------------------------
%%% @author Dennis Novikov <dennis.novikov@gmail.com>
%%% @copyright Paynet Internet ve Bilisim Hizmetleri A.S. All Rights Reserved.
%%% @doc
%%% Tournament instance processes starter, not supervized
%%% @end
%%%-------------------------------------------------------------------

-module(nsg_tourney_starter).


-export([start/3]).


start(Client, ID, Params) ->
    proc_lib:spawn_link(fun() -> start_t_procs(Client, ID, Params) end).


%%%===================================================================
%%% Internal functions
%%%===================================================================

start_t_procs(Client, ID, Params) ->
    case nsg_tourney_inst_sup:add_tournament(Client, ID, Params) of
        {ok, _TProcSup} ->
            %% tournament manager will report success
            ignore;
        {error, E} ->
            nsg_tourney_master:tournament_failed(Client, ID, E)
    end.
