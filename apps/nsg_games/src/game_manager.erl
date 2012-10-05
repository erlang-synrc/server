-module(game_manager). % simple game manager, starts root game processes according to settings
-author('Maxim Sokhatsky <maxim@synrc.com>').

-include_lib("nsg_srv/include/requests.hrl").
-include_lib("nsm_db/include/table.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include_lib("nsx_config/include/log.hrl").

-export([start/0, stop/1, create_table/2, create_table/3, add_game/1, counter/1, get_requirements/2,
         get_relay/1, get_relay_mod_pid/1, subscribe/3, subscribe/2, unsubscribe/2,game_requirements/1]).

-export([create_game/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, { game_tavla = 0, game_okey = 0 }).

create_game(GameFSM, Params) ->
    GameId = id_generator:get_id(),
    {ok, Pid} = gen_server:call(?MODULE, {create_game, GameFSM, Params, GameId}),
    {ok, GameId, Pid}.

create_table(GameFSM, PlayerIds) -> create_table(GameFSM, [], PlayerIds).
create_table(GameFSM, Params, PlayerIds) ->
    GameId = id_generator:get_id(),
    {ok, Pid} = gen_server:call(?MODULE, {create_table, GameFSM, Params, GameId, PlayerIds}),
    {ok, GameId, Pid}.

get_tables(GameId) -> qlc:e(qlc:q([Val || {{_,_,_Key},_,Val=#game_table{id = Id}} <- gproc:table(props), GameId == Id ])).
get_relay_pid(GameId) -> case get_tables(GameId) of [] -> undefined;
    [#game_table{game_process = P} | _] -> ?INFO("GameRelay: ~p",[P]), P end.
get_relay_mod_pid(GameId) -> case get_tables(GameId) of [] -> undefined;
    [#game_table{game_process = P, game_module = M} | _] ->  ?INFO("GameRelay: ~p",[{M,P}]), {M,P} end.
subscribe(Pid, GameId, PlayerId) -> gen_server:call(?MODULE, {subscribe, Pid, GameId, PlayerId}).
subscribe(Pid, GameId) -> gen_server:call(?MODULE, {subscribe, Pid, GameId}).
get_relay(GameId) -> gen_server:call(?MODULE, {get_relay, GameId}).
unsubscribe(Pid, GameId) -> gen_server:cast(?MODULE, {unsubscribe, Pid, GameId}).
game_requirements(GameAtom) -> GameAtom:get_requirements().
add_game(Game) -> gen_server:cast(?MODULE, {add_game, Game}).
remove_game(Game) -> gen_server:cast(?MODULE, {remove_game, Game}).
counter(Game) -> gen_server:call(?MODULE, {game_counter, Game}).
start() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop(Ref) -> gen_server:cast(Ref, stop).
init([]) -> {ok, #state{}}.

handle_call({get_relay, Topic}, _From, State) -> Res = get_relay_pid(Topic), {reply, Res, State};

handle_call({game_counter, FSM}, _From, State) ->
    {reply, case FSM of game_tavla -> State#state.game_tavla; game_okey -> State#state.game_okey; _ -> 0 end, State};

handle_call({create_table, GameFSM, Params, Topic, PlayerIds}, _From, State) ->
    {Res, State1} = create_game_monitor(Topic, {lobby, GameFSM}, Params, PlayerIds, State),
    {reply, Res, State1};

handle_call({create_game, GameFSM, Params, Topic}, _From, State) ->
    {Res, State1} = create_game_monitor2(Topic, GameFSM, Params, State),
    {reply, Res, State1};

handle_call({create_chat, Topic, Players}, _From, State) ->
    {Srv, State1} = create_game_monitor(Topic, chat, [], Players, State),
    {reply, Srv, State1};

handle_call({subscribe, Pid, Topic}, _From, State) ->
    RelayPid = get_relay_pid(Topic),
    case RelayPid of
         undefined -> ok;
         X -> relay:subscribe(RelayPid, Pid)
    end,
    {reply, RelayPid, State};

handle_call({subscribe, Pid, Topic, PlayerId}, _From, State) ->
    RelayPid = get_relay_pid(Topic),
    case RelayPid of
         undefined -> ok;
         X -> relay:subscribe(RelayPid, Pid, PlayerId)
    end,
    {reply, RelayPid, State};

handle_call({unsubscribe, Pid, Topic}, _From, State) ->
    RelayPid = get_relay_pid(Topic),
    case RelayPid of
         undefined -> ok;
         X -> relay:unsubscribe(RelayPid, Pid)
    end,
    {reply, RelayPid, State};

handle_call(Event, From, State) -> {stop, {unknown_call, Event, From}, State}.

handle_cast(stop, State) -> {stop, normal, State};
handle_cast(Event, State) -> {stop, {unknown_cast, Event}, State}.

handle_info({'DOWN', _, process, Pid, Reason}, State) ->
    ?INFO("Game Monitor ~p has died with reason: ~p", [Pid, Reason]),
    {noreply, State};

handle_info({add_game, FSM}, State) ->
    Tavlas = State#state.game_tavla + case FSM of game_tavla -> 1; _ -> 0 end,
    Okeys  = State#state.game_okey  + case FSM of game_okey -> 1; _ -> 0 end,
    {noreply, State#state{game_tavla = Tavlas, game_okey = Okeys}};

handle_info({remove_game, FSM}, State) ->
    Tavlas = State#state.game_tavla + case FSM of game_tavla -> -1; _ -> 0 end,
    Okeys  = State#state.game_okey  + case FSM of game_okey -> -1; _ -> 0 end,
    {noreply, State#state{game_tavla = Tavlas, game_okey = Okeys}};

handle_info(Info, State) -> {stop, {unknown_info, Info}, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

game_monitor_module(GameFSM, GameMode) ->
    case {GameFSM, GameMode} of
        {game_tavla, paired} -> paired_tavla;
        _ -> relay
    end.

get_requirements(GameFSM,M) -> (game_monitor_module(GameFSM, M)):get_requirements(GameFSM,M).

-spec create_game_monitor(string(), pid(), [any()], [pid()], #state{}) -> {{'ok', pid()} | {'error', any()}, #state{}}.
create_game_monitor(Topic, {lobby,GameFSM}, Params, Players, State) ->
    GameMode = proplists:get_value(game_mode, Params, standard),
    ?INFO("Create Root Game Process (Game Monitor): ~p Mode: ~p",[GameFSM, GameMode]),
    RelayInit = (game_monitor_module(GameFSM,GameMode)):start(Topic, {lobby,GameFSM}, Params, Players, self()),
    case RelayInit of 
        {ok, Srv} ->
            Ref = erlang:monitor(process, Srv),
            {{ok, Srv}, State};
        {error, Reason} ->
            {{error, Reason}, State}
    end.

create_game_monitor2(Topic, GameFSM, Params, State) ->
    ?INFO("Create Root Game Process (Game Monitor2): ~p Params: ~p",[GameFSM, Params]),
    RelayInit = GameFSM:start(Topic, Params),
    case RelayInit of 
        {ok, Srv} ->
            Ref = erlang:monitor(process, Srv),
            {{ok, Srv}, State};
        {error, Reason} ->
            {{error, Reason}, State}
    end.
