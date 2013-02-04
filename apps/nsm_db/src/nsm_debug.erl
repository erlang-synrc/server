-module(nsm_debug).
-author('Maxim Sokhatsky <maxim@synrc.com>').

-compile(export_all).

dump_raw() ->
  [{Pid, describe(Pid), length(dict(Pid)), Mem} || {Pid, Mem} <- top(full_memory, 5)].

dump() ->
  Pid = spawn(fun() ->
    exit(dump_raw())
  end),
  erlang:monitor(process, Pid),
  receive
    {'DOWN', _, _, Pid, Reply} -> Reply
  after
    4000 -> erlang:exit(Pid,kill),io:format("Failed to dump info~n")
  end.

dict(Pid) ->
  element(2, process_info(Pid, dictionary)).

describe(Pid) ->
  case proplists:get_value(name, dict(Pid)) of
    undefined ->
      proplists:get_value(registered_name, process_info(Pid), Pid);
    Name ->
      Name
  end.  

binary_memory(Pid) ->
  lists:sum([Mem || {_, Mem, _} <- element(2,process_info(Pid, binary))]).

proc_info(Pid, full_memory) ->
  proc_info(Pid, memory) + proc_info(Pid, binary);
  
proc_info(Pid, binary) ->
  binary_memory(Pid);

proc_info(Pid, Sort) ->
  element(2, process_info(Pid, Sort)).

top(Sort) ->
  DirtyList = [{Pid,(catch proc_info(Pid,Sort))} || Pid <- processes()],
  lists:reverse(lists:keysort(2, [{Pid,Count} || {Pid,Count} <- DirtyList, is_number(Count)] )).

top(Sort, Limit) ->
  lists:sublist(top(Sort), Limit).

limited(Sort, Limit) ->
  [{Pid,Count} || {Pid,Count} <- top(Sort), Count >= Limit].

kill(Sort, Limit) ->
  [erlang:exit(Pid,kill) || {Pid, _Count} <- limited(Sort, Limit)].

full_info(Sort) -> full_info(Sort, 10).
full_info(Sort, Limit) ->
  [{Pid, process_info(Pid)} || {Pid, _Count} <- top(Sort, Limit)].


get_state(Name) when is_atom(Name) -> get_state(whereis(Name));

get_state(Server) when is_pid(Server) ->
  Stat1 = fun(Pid) -> {status, _, _, Items} = sys:get_status(Pid), lists:nth(5,Items) end,
  Stat2 = fun(Pid) -> element(2, lists:nth(3,Stat1(Pid))) end,
  Stat3 = fun(Pid) -> proplists:get_value("State", Stat2(Pid)) end,
  Stat3(Server).
