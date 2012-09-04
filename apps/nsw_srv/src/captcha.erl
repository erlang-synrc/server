-module(captcha).

-export([generate/1,
         format/1,
         check/2]).

generate(Id) ->
    FirstNumber = crypto:rand_uniform(1,10),
    SecoundNumber = crypto:rand_uniform(1,10),
    wf:session({captcha, Id, first_number}, FirstNumber),
    wf:session({captcha, Id, secound_number}, SecoundNumber),
    wf:session({captcha, Id, result}, FirstNumber+SecoundNumber),
    ok.

-spec check(atom(), term()) -> true|false.
check(Id, Result) ->
    case integer_to_list(wf:session({captcha, Id, result})) of
        Result ->
	    true;
	_ ->
	    false
    end.

format(Id) ->
    FirstNumber = wf:session({captcha, Id, first_number}),
    SecoundNumber = wf:session({captcha, Id, secound_number}),
    io_lib:fwrite("~b+~b=?", [FirstNumber, SecoundNumber]).
