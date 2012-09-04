%%----------------------------------------------------------------------
%% @author Paul Peregud <paulperegud@gmail.com>
%% @copyright Paynet Internet ve Bilisim Hizmetleri A.S. All Rights Reserved.
%% @doc
%% Manages queues of requests for matchmaking. Pairing is done in ?LIB
%% module.
%% Dummy module that stores state for matchmaker
%% @end
%%-------------------------------------------------------------------
-module(matchmaker_state).

-behaviour(gen_server).

%% API
-export([get_tab/0, start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-include_lib("nsg_srv/include/logging.hrl").
-include("matchmaking.hrl").

-record(state, {
          requests
         }).

%%%===================================================================
%%% API
%%%===================================================================

get_tab() ->
    gen_server:call(?SERVER, get_tab).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    Ets = ets:new(requests, [public, ordered_set, {keypos, #request.ref}]),
    ?PP("Ets: ~p", [Ets]),
    {ok, #state{requests = Ets}}.

handle_call(get_tab, _From, State) ->
    Tab = State#state.requests,
    {reply, Tab, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
