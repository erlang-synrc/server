-module(user_counter).
-behaviour(gen_server).
-include("setup.hrl").
-include("common.hrl").
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("nsm_db/include/config.hrl").
-include_lib("nsm_db/include/user.hrl").

-export([start_link/0, main/0, event/1, user_count/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-record(state, {user_count=0}).

main()->
  case wf:q(dec_user) of
    undefined -> [];
    PidStr ->
      ?INFO("kill comet process: ~p~n", [PidStr]),
      exit(list_to_pid(PidStr), kill),[]
  end.

event(_) -> ok.

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

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({inc_user, Pid}, State)->
  ?INFO("Inc users: ~p ~p~n", [State, Pid]),
  erlang:monitor(process, Pid),
  Count = #user_count{count=State#state.user_count+1},
  nsx_msg:notify(["system", "put"], Count),
  {noreply, State#state{user_count=Count#user_count.count}};
handle_info({'DOWN', _Ref, process, Pid, _Reason}, State)->
  ?INFO("Dec users: ~p ~p~n", [State, Pid]),
  Count = #user_count{count=State#state.user_count-1},
  nsx_msg:notify(["system", "put"], Count),
  {noreply, State#state{user_count=Count#user_count.count}};
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
