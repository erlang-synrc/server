-module(utils).

-compile(export_all).

% -export([convert_if/2, lists_replace/3, apply_defauls/2]).

convert_if("undefined", _) ->
    undefined;
convert_if(undefined, _) ->
    undefined;
convert_if(L, list) when is_atom(L) ->
    atom_to_list(L);
convert_if(L, list) when is_integer(L) ->
    integer_to_list(L);
convert_if(L, list) when is_float(L) ->
    lists:flatten(io_lib:format("~f", [L]));
convert_if(L, list) when is_binary(L) ->
    binary_to_list(L);
convert_if(L, list) when is_list(L) ->
    L;
convert_if(L, binary) when is_list(L) ->
    iolist_to_binary(L);
convert_if(B, binary) ->
    B.

now_to_seconds(Now) ->
    CData0 = calendar:now_to_local_time(Now),
    calendar:datetime_to_gregorian_seconds(CData0).

date_to_text({Date0, Time0}) ->
    [Y,M,D] = tuple_to_list(Date0),
    [H,Min,S] = tuple_to_list(Time0),
    io_lib:fwrite("~b/~b/~b ~b:~b:~b", [Y, M, D, H, Min, S]).


-spec days_in_month(undefined|integer()|list(), undefined|integer()|list()) -> integer().
days_in_month(Month, Year) when (is_integer(Month) orelse Month==undefined)
									andalso (is_integer(Year)  orelse Year==undefined) ->
	case Month of
		2 -> %% leap year?
			case (catch calendar:is_leap_year(Year)) of
				false -> 28;
				_     -> 29 %% true or undefined
			end;
		M when is_integer(M) -> %% day from month
			case lists:member(M,[4,6,9,11]) of
				true  -> 30;
				false -> 31
			end;
		_ -> 31 %% undefined
	end;
days_in_month(Month, Year) when is_list(Month), is_list(Year) ->
	[M, Y] = [ case E of "undefined" -> undefined;
				   _ -> erlang:list_to_integer(E)
			   end || E <- [Month, Year] ],
	days_in_month(M, Y).


lists_replace(A, B, List) ->
    lists:map(fun
                  (X) when X =:= A -> B;
                  (X) -> X
              end, List).
apply_defauls(DefaultProplist, NewProplist) ->
    lists:foldl(fun({Key, Val}, Acc) ->
                        lists:keystore(Key, 1, Acc, {Key, Val})
                end, DefaultProplist, NewProplist).

sha(Raw) ->
    lists:flatten(
      [io_lib:format("~2.16.0b", [N]) || <<N>> <= crypto:sha(Raw)]).

sha_upper(Raw) ->
    SHA = sha(Raw),
    string:to_upper(SHA).

%% extended uuid, additional hash added
uuid_ex() ->
  R1 = random:uniform(round(math:pow(2, 48))) - 1,
  R2 = random:uniform(round(math:pow(2, 12))) - 1,
  R3 = random:uniform(round(math:pow(2, 32))) - 1,
  R4 = random:uniform(round(math:pow(2, 30))) - 1,
  R5 = erlang:phash({node(), now()}, round(math:pow(2, 32))),

  UUIDBin = <<R1:48, 4:4, R2:12, 2:2, R3:32, R4: 30>>,
  <<TL:32, TM:16, THV:16, CSR:8, CSL:8, N:48>> = UUIDBin,

  lists:flatten(io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~2.16.0b~2.16.0b-~12.16.0b-~8.16.0b",
                              [TL, TM, THV, CSR, CSL, N, R5])).
