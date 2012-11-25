-module(user_counter).
-behaviour(gen_server).
-include("setup.hrl").
-include("common.hrl").

-export([start_link/0, user_count/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-record(state, {user_count=0}).

user_count() ->
  gen_server:call(?SERVER, user_count).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  {ok, #state{}}.

handle_call(user_count, _From, State)->
  {reply, integer_to_list(State#state.user_count), State};
handle_call(_Request, _From, State) ->
  {reply, unknown, State}.

handle_cast(Msg, State) ->
  {noreply, State}.

handle_info({inc_user, Pid}, State)->
  erlang:monitor(process, Pid),
  {noreply, State#state{user_count=State#state.user_count+1}};
handle_info({'DOWN', _Ref, process, _Pid, _Reason}, State)->
  {noreply, State#state{user_count=State#state.user_count-1}};
handle_info(Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.