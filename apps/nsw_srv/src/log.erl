%%% @author JLarky <jlarky@punklan.net>
%%% @copyright (C) 2011, JLarky
%%% @copyright Paynet Internet ve Bilisim Hizmetleri A.S. All Rights Reserved.
%%% @doc
%%% Logger interface that provides simple http interface to logging system.
%%% @end
%%% Created : 13 Jul 2011 by JLarky <jlarky@punklan.net>

-module(log).

-include_lib("loger.hrl").
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("nsm_srv/include/user.hrl").
-include_lib("nsm_srv/include/config.hrl").
-include("gettext.hrl").

main() ->
    A = (catch main_()),
    %%?PRINT(A),
    A.

main_() ->
    %% to test that
    %% Go to any kakaranet page
    %% run in js-console
    %% $.ajax({
    %%  type: 'POST',
    %%  url: '/log',
    %%  data: {level:"debug", format:"any string"},
    %%  success: function(a,b,c) {console.log(a,b,c)},
    %%  error: function(a,b,c) {console.log(a,b,c)}
    %% });
    wf:header("Content-Type", "application/json"),
    Format = wf:q(format),

    try
	case wf:q(level) of
	    "emergency" -> ?EMERGENCY(Format, [], flash);
	    "alert" -> ?ALERT(Format, [], flash);
	    "critical" -> ?CRITICAL(Format, [], flash);
	    "error" -> ?ERROR(Format, [], flash);
	    "warning" -> ?WARNING(Format, [], flash);
	    "notice" -> ?NOTICE(Format, [], flash);
	    "info" -> ?INFO(Format, [], flash);
	    "debug" -> ?DBG(Format, [], flash)
	end,
	"{\"result\":\"ok\"}"
    catch
	error:badarg ->
	    "{\"error\":\"badformat\"}";
	error:{case_clause, _} ->
	    "{\"error\":\"badpriority\"}"
    end.

