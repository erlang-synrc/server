%%%-------------------------------------------------------------------
%%% @author Paul Peregud <pawel@saturn.lan>
%%% @copyright (C) 2011, Paul Peregud
%%% @doc
%%%
%%% @end
%%% Created : 18 Apr 2011 by Paul Peregud <pawel@saturn.lan>
%%% Based on Topic manager by Oortle. See copyright notice below
%%%-------------------------------------------------------------------
-module(game_manager).

%%%
%%% Topic manager
%%%

-include_lib("nsg_srv/include/requests.hrl").
-include_lib("alog/include/alog.hrl").

-export([publish/2, subscribe/2, subscribe/3, unsubscribe/2,
         start/0, stop/1]).

-export([create_game/2, create_tournament/2, create_table/2, create_table/3, flashify/1, add_game/1, counter/1,
         is_topic/1, get_relay/1, game_requirements/1, simulate_delay/2, wait_for_resume/0]).

-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-record(state, {
          topic_xref,
          server_xref,
          game_tavla = 0,
          game_okey = 0
         }).

%% creates game for users that are already logged into gameserver
create_game(GameFSM, Players) ->
    create_game(GameFSM, [], Players).

create_tournament(GameFSM, Players) ->
    create_tournament(GameFSM, [], Players).

create_tournament(GameFSM, Params, Players) ->
    Topic = id_generator:get_id(),
    case gen_server:call(?MODULE, {create_tournament, GameFSM, Params, Topic, Players}) of
        {ok, _Srv} ->
%            [ subscribe(Pid, Topic) || Pid <- Players ],
%            relay:signal(Srv, created),
            {ok, Topic};
        {error, Reason} ->
            ?INFO("creation failed for players ~p", [Players]),
            {error, Reason}
    end.

%% creates game for users that are already logged into gameserver
create_game(GameFSM, Params, Players) ->
    Topic = id_generator:get_id(),
    case gen_server:call(?MODULE, {create_game, GameFSM, Params, Topic, Players}) of
        {ok, Srv} ->
            [ subscribe(Pid, Topic) || Pid <- Players ],
            relay:signal(Srv, created),
            {ok, Topic};
        {error, Reason} ->
            ?INFO("creation failed for players ~p", [Players]),
            {error, Reason}
    end.

%% creates game, that can be joined by users later
create_table(GameFSM, PlayerIds) ->
    create_table(GameFSM, [], PlayerIds).
create_table(GameFSM, Params, PlayerIds) ->
    ?INFO("GameFSM: ~p, Params ~p, PlayerId ~p",[GameFSM, Params, PlayerIds]),
    Topic = id_generator:get_id(),
    {ok, Pid} = gen_server:call(?MODULE, {create_table, GameFSM, Params, Topic, PlayerIds}),
    {ok, Topic, Pid}.

is_topic(Topic) ->
    gen_server:call(?MODULE, {is_topic, Topic}).

get_relay(Topic) ->
    gen_server:call(?MODULE, {get_relay, Topic}).

game_requirements(GameAtom) ->
    GameAtom:get_requirements().

publish(Msg, Topic) ->
    gen_server:cast(?MODULE, {publish, Msg, Topic}).

subscribe(Pid, Topic) ->
    gen_server:call(?MODULE, {subscribe, Pid, Topic}).

add_game(Game) ->
    gen_server:cast(?MODULE, {add_game, Game}).

remove_game(Game) ->
    gen_server:cast(?MODULE, {remove_game, Game}).

counter(Game) ->
    gen_server:call(?MODULE, {game_counter, Game}).

subscribe(Pid, Topic, PlayerId) ->
    gen_server:call(?MODULE, {subscribe, Pid, Topic, PlayerId}).

unsubscribe(Pid, Topic) ->
    gen_server:cast(?MODULE, {unsubscribe, Pid, Topic}).

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop(Ref) ->
    gen_server:cast(Ref, stop).

init([]) ->
    State = #state{
      topic_xref = dict:new(),
      server_xref = dict:new()
     },
    {ok, State}.

handle_cast(stop, State) ->
    {stop, normal, State};


handle_cast({unsubscribe, Pid, Topic}, State) ->
    case dict:find(Topic, State#state.topic_xref) of
        {ok, Srv} ->
            %% found a topic server
            relay:unsubscribe(Srv, Pid);
        _ ->
            ignore
    end,
    {noreply, State};

handle_cast({publish, Msg, Topic}, State) ->
    {{ok, Srv}, State1} = ensure_server(Topic, State),
    relay:publish(Srv, Msg),
    {noreply, State1};

handle_cast(Event, State) ->
    {stop, {unknown_cast, Event}, State}.

handle_call({subscribe, Pid, Topic}, _From, State) ->
    {{ok, Srv}, State1} = ensure_server(Topic, State),
    relay:subscribe(Srv, Pid),
    {reply, Srv, State1};

handle_call({subscribe, Pid, Topic, PlayerId}, _From, State) ->
    {{ok, Srv}, State1} = ensure_server(Topic, State),
    relay:subscribe(Srv, Pid, PlayerId),
    {reply, Srv, State1};

handle_call({create_game, GameFSM, Params, Topic, Players}, _From, State) ->
    {Res, State1} = ensure_server(Topic, GameFSM, Params, Players, State),
    ?INFO("Create game res: ~p", [Res]),
    {reply, Res, State1};

handle_call({game_counter, FSM}, _From, State) ->
    {reply, case FSM of game_tavla -> State#state.game_tavla; game_okey -> State#state.game_okey; _ -> 0 end, State};

handle_call({create_tournament, GameFSM, Params, Topic, Players}, _From, State) ->
    {Res, State1} = ensure_server(Topic, GameFSM, Params, Players, State),
    {reply, Res, State1};

handle_call({create_table, GameFSM, Params, Topic, PlayerIds}, _From, State) ->
    {Res, State1} = ensure_server(Topic, {lobby, GameFSM}, Params, PlayerIds, State),
    {reply, Res, State1};

handle_call({create_chat, Topic, Players}, _From, State) ->
    {Srv, State1} = ensure_server(Topic, chat, [], Players, State),
    {reply, Srv, State1};

handle_call({is_topic, Topic}, _From, #state{topic_xref = TRefs} = State) ->
    Res = case dict:find(Topic, TRefs) of
        {ok, _Srv} ->
            true;
        _ ->
            false
    end,
    {reply, Res, State};

handle_call({get_relay, Topic}, _From, #state{topic_xref = TRefs} = State) ->
    Res = case dict:find(Topic, TRefs) of
              {ok, Srv} ->
                  {ok, Srv};
              _ ->
                  false
          end,
    {reply, Res, State};

handle_call(Event, From, State) ->
    {stop, {unknown_call, Event, From}, State}.

handle_info({'DOWN', _, process, Pid, Reason},
           #state{topic_xref = TXrefs, server_xref = SXrefs} = State) ->
    ?INFO("monitored pid ~p has died with reason: ~p", [Pid, Reason]),
    %% topic manager died, we don't care why
    State1 =
        case dict:find(Pid, SXrefs) of
            {ok, {Topic, SrvMonRef}} ->
                erlang:demonitor(SrvMonRef),
                TXrefs1 = dict:erase(Topic, TXrefs),
                SXrefs1 = dict:erase(Pid, SXrefs),
                State#state{topic_xref = TXrefs1, server_xref = SXrefs1};
            _ ->
                State
        end,
    {noreply, State1};

handle_info({add_game, FSM}, State) ->
    Tavlas = State#state.game_tavla + case FSM of game_tavla -> 1; _ -> 0 end,
    Okeys  = State#state.game_okey  + case FSM of game_okey -> 1; _ -> 0 end,
    {noreply, State#state{game_tavla = Tavlas, game_okey = Okeys}};

handle_info({remove_game, FSM}, State) ->
    Tavlas = State#state.game_tavla + case FSM of game_tavla -> -1; _ -> 0 end,
    Okeys  = State#state.game_okey  + case FSM of game_okey -> -1; _ -> 0 end,
    {noreply, State#state{game_tavla = Tavlas, game_okey = Okeys}};

handle_info(Info, State) ->
    {stop, {unknown_info, Info}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec ensure_server(string(), #state{}) -> {{'ok', pid()}, #state{}}.
ensure_server(Topic,
              #state{topic_xref = TXrefs, server_xref = SXrefs} = State) ->
    case dict:find(Topic, TXrefs) of
        {ok, Srv} ->
            %% found a topic server
            {{ok, Srv}, State};
        _ ->
            {ok, Srv} = relay:start(Topic, chat, [], self()),
            Ref = erlang:monitor(process, Srv),
            TXrefs1 = dict:store(Topic, Srv, TXrefs),
            SXrefs1 = dict:store(Srv, {Topic, Ref, chat}, SXrefs),
            {{ok, Srv},State#state{topic_xref = TXrefs1, server_xref = SXrefs1}}
    end.

-spec ensure_server(string(), pid(), [any()], [pid()], #state{}) ->
                           {{'ok', pid()} | {'error', any()}, #state{}}.
ensure_server(Topic, GameFSM, Params, Players,
              #state{topic_xref = TXrefs, server_xref = SXrefs } = State) ->
    case dict:find(Topic, TXrefs) of
        {ok, Srv} ->
            %% found a topic server
            {{ok, Srv}, State};
        _ ->
            %% start a new topic server
            case relay:start(Topic, GameFSM, Params, Players, self()) of
                {ok, Srv} ->
                    ?INFO("Relay pid: ~p", [Srv]),
                    Ref = erlang:monitor(process, Srv),
                    TXrefs1 = dict:store(Topic, Srv, TXrefs),
                    SXrefs1 = dict:store(Srv, {Topic, Ref, GameFSM}, SXrefs),
                    State1 = State#state{topic_xref = TXrefs1,
                                         server_xref = SXrefs1},
                    {{ok, Srv}, State1};
                {error, Reason} ->
                    {{error, Reason}, State}
            end
    end.

time_to_sleep(_, Delay) ->
    erlang:trunc((Delay / 3) * 2).

simulate_delay(Action, Delay) ->
    TheDelay = time_to_sleep(Action, Delay),
    receive
        #game_paused{action = <<"pause">>} ->
            wait_for_resume()
    after TheDelay ->
            ok
    end.

wait_for_resume() ->
    receive
        #game_paused{action = <<"resume">>} ->
            ok
    end.

flashify(R) when is_tuple(R) ->
    [RecName | Rest] = tuple_to_list(R),
    Rest1 = lists:map(fun
                          (X) -> flashify(X)
                      end, Rest),
    list_to_tuple([RecName | Rest1]);
flashify([{Key, _Value} | _] = P) when is_atom(Key) ->
    lists:map(fun
                  ({K, V}) when is_atom(K) -> {K, flashify(V)}
              end, P);
flashify(A) when A == true -> A;
flashify(A) when A == false -> A;
flashify(A) when A == null -> A;
flashify(A) when A == undefined -> A;
flashify(A) when is_atom(A) ->
    list_to_binary(atom_to_list(A));
flashify(Other) ->
    Other.
