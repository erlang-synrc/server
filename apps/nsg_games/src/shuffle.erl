-module(shuffle).
-author('Maxim Sokhatsky <maxim@synrc.com>').
-compile(export_all).

init_first_round(PlayersCount,PlayersPerBoard) ->
    [ lists:seq(Table * PlayersPerBoard, (Table + 1) * PlayersPerBoard - 1)
    || Table <- lists:seq(0, PlayersCount div PlayersPerBoard - 1) ].

shift_round(Tables, Players, PlayersPerBoard) ->
    [ [ lists:nth(Player + 1,
        lists:nth((Table + Player) rem (Players div PlayersPerBoard) + 1,Tables))
    || Player <- lists:seq(0, PlayersPerBoard - 1) ] || Table <- lists:seq(0, length(Tables) - 1) ].

generate_tournament_plan(1, _, PlayersCount, PlayersPerBoard, _) ->
    Tables = init_first_round(PlayersCount, PlayersPerBoard),
    generate_tournament_plan(2, Tables, PlayersCount, PlayersPerBoard, Tables);
generate_tournament_plan(Round, _Tables, PlayersCount, PlayersPerBoard, Rounds) 
    when PlayersCount div PlayersPerBoard =:= Round - 1 -> Rounds;
generate_tournament_plan(Round, Tables, PlayersCount, PlayersPerBoard, Rounds) ->
    Next = shift_round(Tables, PlayersCount, PlayersPerBoard),
    generate_tournament_plan(Round + 1, Next, PlayersCount, PlayersPerBoard, Rounds ++ Next).

dump_tables(ArraysList, Check) ->
    lists:map(fun(A) ->
               io:format("[ "),
                  lists:map(fun(I) ->
                       io:format("~3.16B ",[I])
                     end, A),
               io:format("], ~p ~n",[Check])
              end, ArraysList).

prev_contains(List, Item, Dict) ->
    lists:foldl(fun(A,Acc) -> Acc or (contains(Item, dict:fetch(A,Dict))) end, false, List).

contains(A, List) ->
    lists:any(fun(B) -> B =:= A end, List).

dump_dict(Dict) ->
    [ begin
        List = dict:fetch(Key,Dict),
        io:format("~p: ~p,",[Key,List])
    end || Key <- dict:fetch_keys(Dict)],
    ok.

check_table(List,Dict) ->
    R = lists:foldr(fun(A,Acc) ->
        Acc and lists:foldr(fun(B,Bcc) ->
            HashA = dict:fetch(A,Dict),
            HashB = dict:fetch(B,Dict),
            case (A =:= B) of 
                false -> 
%                 io:format("A,B ~p ~p~n",[{A,B},true]),
%                 io:format("contains(A,HashB) ~p ~p~n",[{A,HashB},not contains(A,HashB)]),
%                 io:format("contains(B,HashA) ~p ~p~n",[{B,HashA},not contains(B,HashA)]),
%                 io:format("prev_contains(List,B,Dict) ~p ~p~n",[{List,B},not prev_contains(List,B,Dict)]),
%                 dump_dict(Dict),
                       case
                         not contains(A,HashB) and not contains(B,HashA)
                         and not prev_contains(List,A,Dict) of
                            true -> Bcc;
                            false -> false
                        end;
                true -> Bcc
            end
        end, true, List)
    end, true, List),
%    io:format("check table: ~p~n",[R]),
    R.

filter_plan([], _Hash, C, Tables) -> {C,Tables};
filter_plan(Tables, H, C, _Tables) ->
    [Table | Rest] = Tables,
    {CC,H2,NewTables} = case _Check = check_table(Table, H) of
        true -> 
%                dump_tables([Table], Check),
                H3 = lists:foldl(fun(A,Acc) -> 
                        lists:foldl(fun(B,Bcc) ->
                            AList = dict:fetch(A,Bcc),
                            BList = dict:fetch(B,Bcc),
                            D = case contains(B,AList) or (A =:= B) of
                                true -> Bcc;
                                false -> dict:store(A,[B|AList],Bcc)
                            end,
                            D1 = case contains(A,BList) or (A =:= B) of
                                true -> D;
                                false -> dict:store(A,[B|AList],D)
                            end,
                            D1
                        end, Acc, Table)
                    end, H, Table),
                 {1,H3,_Tables ++ [Table]};
        false -> 
%                 dump_tables([Table], Check),
                 {0,H,_Tables}
    end,
    filter_plan(Rest,H2,C+CC,NewTables).

gen_dict(-1,H) -> H;
gen_dict(T,H) -> gen_dict(T - 1, dict:store(T,[],H)).

generate_tournament(PlayersCount,PlayersPerBoard) ->
    Tables= generate_tournament_plan(1, [], PlayersCount, PlayersPerBoard, []),
    H = gen_dict(PlayersCount,dict:new()),
    filter_plan(Tables,H,0,[]).

start() ->
    {C,_Tables} = generate_tournament(20,4),
    io:format("Plan:~n"),
%    dump_tables(_Tables,""),
    io:format("Tables: ~p~n",[{C}]),
    ok.

