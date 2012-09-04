%%%-------------------------------------------------------------------
%%% @author Dennis Novikov <dennis.novikov@gmail.com>
%%% @copyright Paynet Internet ve Bilisim Hizmetleri A.S. All Rights Reserved.
%%% @doc
%%% Object matcher - match up to required number of objects from queue
%%% based on their metrics and hard/soft restrictions.
%%% @end
%%%-------------------------------------------------------------------

-module(nsg_matchmaker_queue).

-behaviour(gen_server).

-include_lib("alog/include/alog.hrl").


%% API
-export([start/1         % Start standalone
        ,start/2         % Start standalone, named
        ,start_link/1    % Start supervized
        ,start_link/2    % Start supervized as named queue
        ,stop/1          % Stop queue
        ,enqueue/3       % Put object to the queue
        ,match/5         % Perform object matching
        ,match_metrics/4 % Perform metrics matching
        ,dequeue/2       % Remove objects from queue
        ]).

-export([start_players_matcher/0
        ,match_player/5
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ===========
%% Definitions


-type metric_name() :: atom().
-type metric() :: {Name::metric_name(), Vlaue::term()}.
-type match_filter() :: eq | {percent_range, pos_integer()} | undfeined.
-type filter_power() :: soft | hard.
-type matcher() :: {Name::metric_name(), Filter::match_filter()}.
-record(matchinfo, {metric                 :: metric_name()
                   ,base_value             :: term()
                   ,filter                 :: match_filter()
                   ,power                  :: filter_power()
                   }).


-record(state, {store        :: ets:tid()            % Object store/queue
               ,metrics_order:: tuple(metric_name()) % All metrics in
                                                     %  ascending priority order
               ,metric_count :: pos_integer()        % Length of 'metrics' list
               }).


%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(AllMetrics::[metric_name()]) -> {ok, pid()}
                                               | {error, term()}
                                               | ignore.
%%--------------------------------------------------------------------
%% @doc
%% Starts the server, same as start_link(undefined, AllMetrics)
%%
%% @spec start_link(AllMetrics::[metric_name()]) ->
%%    {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(AllMetrics) ->
    start_link(undefined, AllMetrics).


-spec start_link(QueueName::atom(),
                 AllMetrics::[metric_name()]) -> {ok, pid()}
                                               | {error, term()}
                                               | ignore.
%%--------------------------------------------------------------------
%% @doc
%% Starts the server with AllMetrics given in descending priority order,
%% queue is named as MatchQueueName.
%%
%% @spec start_link(QueueName::atom(), AllMetrics::[metric_name()]) ->
%%    {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(MatchQueueName, AllMetrics) when is_atom(MatchQueueName) ->
    case MatchQueueName of
        undefined ->
            gen_server:start_link(?MODULE, [undefined, AllMetrics], []);
        _ ->
            gen_server:start_link({local, MatchQueueName}, ?MODULE,
                                  [MatchQueueName, AllMetrics], [])
    end.


start(AllMetrics) ->
    start(undefined, AllMetrics).


start(MatchQueueName, AllMetrics) when is_atom(MatchQueueName) ->
    case MatchQueueName of
        undefined ->
            gen_server:start(?MODULE, [undefined, AllMetrics], []);
        _ ->
            gen_server:start({local, MatchQueueName}, ?MODULE,
                             [MatchQueueName, AllMetrics], [])
    end.


-spec stop(MatchQueue::atom()|pid()) -> ok.
%% @doc
%% Stop matcher queue process
%% @end
stop(MatchQueue) ->
    gen_server:call(MatchQueue, stop, infinity).


-spec enqueue(MatcherQueue::atom()|pid(),
              ObjID::term(), ObjMetrics::[metric()]) ->
                     ok
                   | {error, Reason}
    when
      Reason :: already_queued | term().
%% @doc Enqueue object with given ID and metrics
%% @end
enqueue(MatchQueue, ObjID, ObjMetrics) when is_list(ObjMetrics) ->
    gen_server:call(MatchQueue, {enqueue, ObjID, ObjMetrics}, infinity).


-spec match(MatcherQueue::atom()|pid(), ObjID::term(),
            MatchesRequired::pos_integer(),
            HardMatchers::[matcher()], SoftMatchers::[matcher()]) ->
                   {MatchedObjects::integer(), Objects::[term()]}
                 | {error, Reason}
    when
      Reason :: not_queued | term().
%% @doc Find matches for an object.
%% If no other matching objects found, returns {1, [ObjID]}
%% @end
match(MatcherQueue, ObjID, MatchesRequired, HardMatchers, SoftMatchers)
  when is_integer(MatchesRequired), MatchesRequired > 0,
       is_list(HardMatchers), is_list(SoftMatchers) ->
    gen_server:call(MatcherQueue,
                    {match, ObjID, MatchesRequired, HardMatchers, SoftMatchers},
                    infinity).


-spec match_metrics(MatcherQueue::atom()|pid(),
                    MatchesRequired::pos_integer(),
                    HardMatchers::[matcher()], SoftMatchers::[matcher()]) ->
                   {MatchedObjects::integer(), Objects::[term()]}
                 | {error, Reason}
    when
      Reason :: not_queued | term().
%% @doc Find closest matches based on matchers
%% @end
match_metrics(MatcherQueue, MatchesRequired, HardMatchers, SoftMatchers)
  when is_integer(MatchesRequired), MatchesRequired > 0,
       is_list(HardMatchers), is_list(SoftMatchers) ->
    %% TODO: implement metrics matching
    gen_server:call(
      MatcherQueue,
      {match_metrics, MatchesRequired, HardMatchers, SoftMatchers},
      infinity).



-spec dequeue(MatcherQueue::atom()|pid(), ObjID::[term()]) -> ok
                                                            | {error, term()}.
%% @doc Enqueue object with given ID and metrics
%% @end
dequeue(MatchQueue, ObjIDs) when is_list(ObjIDs) ->
    gen_server:call(MatchQueue, {dequeue, ObjIDs}, infinity).



%% Player specific, example

%% Player metrics in ascending priority order
-define(PLAYER_METRICS,
        [game_type, skill, membership, sex, age, country, city]).

-define(PLAYER_QUEUE, ns_pmq).

start_players_matcher() ->
    start(?PLAYER_QUEUE, ?PLAYER_METRICS).


-spec match_player(Player::term(), PMetrics::[metric()],
                   TotalPlayers::pos_integer(),
                   GameOpts::[metric()],
                   HardMatchers::[matcher()]) ->
                          {MatchedPlayers::integer(), Players::[term()]}
                        | {error, Reason::term()}.
%% @doc finds player matches, wrapping call to match/3
%% @end
match_player(Player, PMetrics, TotalPlayers, GameOpts, HardMatchers) ->
    GameMetrics = game_metrics(GameOpts),
    GamePlayerMetrics = GameMetrics ++ PMetrics,
    HardMatchers =
        game_matchers(GameMetrics) ++ HardMatchers,
    ok = enqueue(?PLAYER_QUEUE, Player, GamePlayerMetrics),
    match(?PLAYER_QUEUE, Player, TotalPlayers, HardMatchers, PMetrics).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Name, AllMetrics]) ->
    ?NOTICE("Match queue (name: ~w) is starting (~w) with metric list: ~w",
                 [Name, self(), AllMetrics]),
    PS = make_store(),
    {ok, #state{store = PS,
                metrics_order = list_to_tuple(lists:reverse(AllMetrics))
               }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({enqueue, ObjID, ObjMetrics}, From,
            #state{store  = ObjStore, metrics_order = AllMetrics} = State) ->
    Key = make_key(ObjID, normalize_metrics(ObjMetrics, AllMetrics)),
    ?DBG([], "Enqueue object ~w with key ~w", [ObjID, Key]),
    case store_object(Key, ObjID, {From, ObjMetrics}, ObjStore) of
        false ->
            {reply, {error, already_queued}, State};
        ObjStore1 ->
            {reply, ok, State#state{store = ObjStore1}}
    end;
handle_call({dequeue, ObjIDs}, _From,
            #state{store  = ObjStore, metrics_order = AllMetrics} = State) ->
    ?DBG([], "Dequeue objects ~w", [ObjIDs]),
    ObjStore1 = erase_objects(ObjIDs, AllMetrics, ObjStore),
    {reply, ok, State#state{store = ObjStore1}};
handle_call({match, ObjID, MatchesRequired, HardMatchers, SoftMatchers}, _From,
            #state{store  = ObjStore,
                   metrics_order = AllMetrics} = State) ->
    MatchResult =
        case prep_match(ObjStore, ObjID, AllMetrics,
                        HardMatchers, SoftMatchers) of
            {ok, Key, MatcherInfos} ->
                ?DBG("Requested ~w matches for obj ~w, key ~w,"
                            " soft matchers: ~w",
                            [MatchesRequired, ObjID, Key, MatcherInfos]),
                MState = initial_state(Key, MatcherInfos),
                case do_match(ObjStore, Key, MatchesRequired, MState) of
                    {error, _} = MatchErr ->
                        MatchErr;
                    {MatchesFound, KeysFound} ->
                        ObjsFound = [key_to_obj_id(K) || K <- KeysFound],
                        {MatchesFound, ObjsFound}
                end;
            {error, _} = PrepErr ->
                PrepErr
        end,
    ?INFO("Match result for ~w objects, object ~w: ~w",
                 [MatchesRequired, ObjID, MatchResult]),
    {reply, MatchResult, State};
handle_call({match_metrics, _MatchesRequired, _HardMatchers, _SoftMatchers},
            _From, #state{store  = _ObjStore,
                          metrics_order = _AllMetrics} = State) ->
    {reply, {error, not_implemented}, State};
handle_call(stop, From, State) ->
    ?NOTICE("Shutdown requested for the matcher queue by ~w", [From]),
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(Reason, #state{store = S}) ->
    All = all_objects_data(S),
    ?NOTICE([],"Matcher queue shutdown: ~w", [Reason]),
    spawn(fun() -> [match_not_found(From)
                    || {_ObjID, {From, _Metrics}} <- All]
          end),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


%% Player related

game_metrics(GameOpts) ->
    [{game_type, erlang:phash2(lists:sort(GameOpts))}].


game_matchers(GameMetrics) ->
    [game_matcher(N) || {N,_V} <- GameMetrics].

game_matcher(game_type) ->
    {game_type, eq}.


%% Generic matching

%% Store

make_key(ObjID, NormalizedMetrics) ->
    [Val || {_Name, Val} <- NormalizedMetrics] ++ [ObjID].


key_to_obj_id(Key) ->
    lists:last(Key).


make_store() ->
    {dict:new(), ets:new(objstore, [ordered_set, protected])}.


next_key({_ODict, OTab}, Direction, BaseKey) ->
    K =
        case Direction of
            up ->
                ets:next(OTab, BaseKey);
            down ->
                ets:prev(OTab, BaseKey)
        end,
    case K of
        '$end_of_table' ->
            false;
        _ ->
            K
    end.


key_exists({_Odict, OTab}, Key) ->
    [] =/= ets:lookup(OTab, Key).


store_object(Key, ObjID, ObjData, {ODict, OTab}) ->
    case dict:is_key(ObjID, ODict) of
        true ->
            false;
        false ->
            true = ets:insert_new(OTab, {Key, ObjID}),
            {dict:store(ObjID, ObjData, ODict), OTab}
    end.


retrive_object_data(ObjID, {ODict, _OTab}) ->
    case dict:find(ObjID, ODict) of
        error ->
            false;
        {ok, ObjData} ->
            ObjData
    end.


erase_objects(ObjIDs, AllMetrics, ObjStore) ->
    case obj_refs(ObjIDs, AllMetrics, ObjStore) of
        false ->
            false;
        Refs ->
            Erase =
                fun({K, ID}, Store) ->
                        erase_object(K, ID, Store)
                end,
            lists:foldl(Erase, ObjStore, Refs)
    end.


erase_object(Key, ObjID, {ODict, OTab}) ->
    ets:delete(OTab, Key),
    {dict:erase(ObjID, ODict), OTab}.


obj_refs(ObjIDs, AllMetrics, ObjStore) ->
    obj_refs(ObjIDs, AllMetrics, ObjStore, []).

obj_refs([], _AllMetrics, _ObjStore, Refs) ->
    Refs;
obj_refs([ObjID|Rest], AllMetrics, ObjStore, Refs) ->
    case retrive_object_data(ObjID, ObjStore) of
        false ->
            false;
        {_From, ObjMetrics} ->
            NormMetrics = normalize_metrics(ObjMetrics, AllMetrics),
            Ref = {make_key(ObjID, NormMetrics), ObjID},
            obj_refs(Rest, AllMetrics, ObjStore, [Ref|Refs])
    end.


all_objects_data({ODict, _OTab}) ->
    dict:to_list(ODict).


%% Metric/matcher handling

%% Get metric values in fixed order with placeholders for missing ones
normalize_metrics(ObjMetrics, AllMetrics) ->
    normalize_metrics(ObjMetrics, AllMetrics, 1, []).

%% Result is metric values in descending priority order, as used in key
normalize_metrics(_ObjMetrics, AllMetrics, MetricNo, Normalized)
  when MetricNo > tuple_size(AllMetrics) ->
    Normalized;
normalize_metrics(ObjMetrics, AllMetrics, MetricNo, Normalized) ->
    Metric = element(MetricNo, AllMetrics),
    Normalized1 =
        case lists:keyfind(Metric, 1, ObjMetrics) of
            false ->
                [{Metric, undefined}|Normalized];
            {_Metric, Value} ->
                [{Metric, Value}|Normalized]
        end,
    normalize_metrics(ObjMetrics, AllMetrics, MetricNo + 1, Normalized1).


prep_match(ObjStore, ObjID, AllMetrics, HardMatchers, SoftMatchers) ->
    case retrive_object_data(ObjID, ObjStore) of
        false ->
            {error, not_queued};
        {_EnqFrom, ObjMetrics} ->
            NormMetrics = normalize_metrics(ObjMetrics, AllMetrics),
            Key = make_key(ObjID, NormMetrics),
            case make_matchinfos(HardMatchers, SoftMatchers, NormMetrics) of
                {ok, MatcherInfos} ->
                    {ok, Key, MatcherInfos};
                {error, _} = E ->
                    E
            end
    end.


make_matchinfos(HardMatchers, SoftMatchers, NormMetrics) ->
    case check_matchers(HardMatchers, SoftMatchers) of
        ok ->
            Hard = [#matchinfo{metric = N, filter = F, power = hard}
                    || {N, F} <- HardMatchers],
            Soft = [#matchinfo{metric = N, filter = F, power = soft}
                    || {N, F} <- SoftMatchers],
            {ok, ordered_matchinfo(Hard ++ Soft, NormMetrics)};
        {error, _ } = E ->
            E
    end.

ordered_matchinfo(MatcherInfos, NormMetrics) ->
    PickMInfo =
        fun({MName, MVal}, OrderedMI) ->
                case lists:keyfind(MName, #matchinfo.metric, MatcherInfos) of
                    false ->
                        [#matchinfo{metric = MName,
                                    base_value = MVal,
                                    power = soft} | OrderedMI];
                    M ->
                        [M#matchinfo{base_value = MVal} | OrderedMI]
                end
        end,
    %% Order matchinfos according to key metrics order
    lists:reverse(lists:foldl(PickMInfo, [], NormMetrics)).


check_matchers(_Hard, _Soft) ->
    %% TODO: replace stub with actual sainity checks
    ok.


%% Matching object
do_match(ObjStore, Key, MatchesRequired, MState) ->
    case key_exists(ObjStore, Key) of
        false ->
            ?ERROR("Matching failed - key ~w not found", [Key]),
            {error, not_queued};
        true ->
            do_match(ObjStore, Key, MatchesRequired, MState, 1, [Key])
    end.


do_match(_ObjStore, _Key, MatchesRequired, _MState, MatchesFound, KeysFound)
  when MatchesFound >= MatchesRequired ->
    fit_result(MatchesRequired, MatchesFound, KeysFound);
do_match(ObjStore, Key, MatchesRequired, MState, MatchesFound, KeysFound) ->
    case extend_range(ObjStore, MState) of
        false ->
            {MatchesFound, KeysFound};
         MState1 ->
            {NewKeyCount, NewKeys} = new_keys(MState, MState1),
            %% prepend new keys - fit_result/3 depends on it
            do_match(ObjStore, Key, MatchesRequired, MState1,
                     MatchesFound + NewKeyCount, NewKeys ++ KeysFound)
    end.


%% Drop excessive keys from result
fit_result(MatchesRequired, MatchesRequired, KeysFound) ->
    {MatchesRequired, KeysFound};
fit_result(MatchesRequired, MatchesFound, [_|KeysFound]) ->
    fit_result(MatchesRequired, MatchesFound - 1, KeysFound).


%% Range calculations

-record(matcher_state,
        {xdown = true     :: boolean()      % Extensible with smaller keys
        ,lesser_keys = [] :: [term()]       % Lesser keys found on last range
                                            %  extension
        ,xup = true       :: boolean()      % Extensible with greater keys
        ,greater_keys = []:: [term()]       % Greater keys found on last range
                                            %  extension
        ,depth = 0        :: pos_integer()  % Check depth
        ,matchinfo = []   :: [#matchinfo{}] % Matcherinfos
        }).


initial_state(Key, MatchInfos) ->
    #matcher_state{xdown = true, lesser_keys = [Key],
                   xup = true, greater_keys = [Key],
                   depth = length(MatchInfos),
                   matchinfo = MatchInfos
                  }.


new_keys(#matcher_state{lesser_keys = LK1, greater_keys = GK1},
         #matcher_state{lesser_keys = LK2, greater_keys = GK2}) ->
    NewKeys = ( LK2 -- LK1 ) ++ ( GK2 -- GK1 ),
    {length(NewKeys), NewKeys}.


extend_range(_ObjStore, #matcher_state{xdown = false, xup = false,
                                       matchinfo = MI} = MState) ->
    case lists:last(MI) of
        #matchinfo{power = hard} ->
            %% Can not extend range for a hard matcher, stop matching
            false;
        #matchinfo{} ->
            %% Range is not extendable on the current level, descend
            descend(MState)
    end;
extend_range(ObjStore, #matcher_state{xdown = XDown, lesser_keys = LK,
                                      xup = XUp, greater_keys = GK,
                                      matchinfo = MI} = MState) ->
    %% This is a limiting matcher level, can only extend range
    %% within matcher limmit
    {XDown1, LK1} =
        limit(try_extend(ObjStore, down, XDown, LK), MI),
    {XUp1, GK1} =
        limit(try_extend(ObjStore, up, XUp, GK), MI),
    MState#matcher_state{xdown = XDown1,
                         lesser_keys = next_keys(XDown1, LK, LK1),
                         xup = XUp1,
                         greater_keys = next_keys(XUp1, GK, GK1)
                        }.


next_keys(StillExtendable, OldKeys, NewKeys) ->
    if
        StillExtendable -> NewKeys;
        true -> OldKeys
    end.


descend(#matcher_state{matchinfo = MI, depth = D} = MState) ->
    MILength = length(MI),
    MI2 = lists:sublist(MI, MILength - 1),
    MState#matcher_state{xdown = true, xup = true,
                         depth = D - 1,
                         matchinfo = MI2
                        }.


%% TODO: consider making PrevKeys orddict and doing multisearch
%% and multi-extend, e.g. for group membership - insert key
%% for every group user is member of.
try_extend(_ObjStore, _Direction, _, []) ->
    {false, []};
try_extend(_ObjStore, _Direction, false, _PrevKeys) ->
    {false, []};
try_extend(ObjStore, Direction, true, [LastKey|_PrevKeys]) ->
    %% Keep PrevKeys in reverse order by prepending
    case next_key(ObjStore, Direction, LastKey) of
        false ->
            {false, []};
        NextKey ->
            {true, [NextKey]}
    end.


limit({_, []} = X, _MI) ->
    X;
limit({Extendable, Keys}, MI) ->
    {WithinLimit, Keys1} = limit_keys(Keys, MI),
    {(Extendable andalso WithinLimit), Keys1}.

limit_keys(Keys, MI) ->
    limit_keys(Keys, MI, true, []).

limit_keys([], _MI, WithinLimit, FilteredKeys) ->
    {WithinLimit, FilteredKeys};
limit_keys([K|Keys], MI, WithinLimit, FilteredKeys) ->
    case key_within_limit(K, MI) of
        false ->
            limit_keys(Keys, MI, false, FilteredKeys);
        true ->
            limit_keys(Keys, MI, WithinLimit, [K|FilteredKeys])
    end.

key_within_limit(_Key, []) ->
    true;
key_within_limit([KE|KeyElements], [#matchinfo{base_value = BV
                                              ,filter = F
                                              }
                                   |MI]) ->
    case filter(F, BV, KE) of
        true ->
            key_within_limit(KeyElements, MI);
        false ->
            false
    end.


filter(undefined, _, _) -> true;
filter(eq, V, V) -> true;
filter({percent_range, X}, BaseVal, Val) ->
    Delta = BaseVal * X / 100,
    (Val >= (BaseVal - Delta)) andalso (Val =< (BaseVal + Delta));
filter(_, _, _) -> false.


%% Utils

%% Report failed matching
match_not_found(From) ->
    gen_server:reply(From, not_found).
