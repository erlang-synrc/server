-module(nsg_proxy_lucky).
-behaviour(gen_server).
-include("setup.hrl").
-compile(export_all).
-record(state, {}).

start_link(Game, Params) -> gen_server:start_link(?MODULE, [Game, Params], []).

init([Game, Params]) ->
    {ok, _, Pid} =
        case Game of
            tavla -> game_manager:create_game(fl_lucky, Params);
            okey ->  game_manager:create_game(game_okey_ng_trn_lucky, Params)
        end,
    ?INFO("Lucky Started for ~p: Pid = ~p",[Game,Pid]),
    link(Pid),
    {ok, #state{}}.

handle_call(Request, From, State) -> Reply = ok, {reply, Reply, State}.
handle_cast(Msg, State) -> {noreply, State}.
handle_info(Info, State) -> {noreply, State}.
terminate(Reason, State) ->  ok.
code_change(OldVsn, State, Extra) -> {ok, State}.

