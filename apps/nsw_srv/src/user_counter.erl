-module(user_counter).
-author('Maxim Sokhatsky <maxim@synrc.com>').
-behaviour(gen_server).
-include_lib("nsm_db/include/user.hrl").
-export([start_link/0, user_count/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-define(SERVER, ?MODULE).
-record(state, {user_count=0,last_check={0,0,0}}).

user_count() -> gen_server:call(?SERVER, user_count).
start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) -> {ok, #state{last_check=now()}}.

handle_call(user_count, _From, State)->
  {L,T} = case timer:now_diff(now(),State#state.last_check) div 1000000 > 60 * 1 of
       true ->  {Users,B} = lists:partition(fun({_,_,A}) -> is_list(A) end, qlc:e(gproc:table())),
                {length(Users),now()};
       false -> {State#state.user_count,State#state.last_check}
   end,
  {reply, L, #state{user_count = L, last_check = T}};

handle_call(_Request, _From, State) -> {reply, unknown, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
