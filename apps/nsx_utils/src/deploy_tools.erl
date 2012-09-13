-module(deploy_tools).

-export([code_reload/0,
         update_db/0,
         eval/1, eval/2,
         web_post_deploy/0,
         server_post_deploy/0]).

code_reload() ->
    MList = code:all_loaded(),
    {ok, Pwd} = file:get_cwd(),
    lists:map(fun({Module, Path}) when is_list(Path) ->
                      case lists:prefix(Pwd, Path) of
                          true ->
                              code:purge(Module),
                              {module, _} = code:load_file(Module),
                              load_module_everywhere(nodes(), Module);
                          false ->
                              no_reloaded
                      end;
                 (_) ->
                      no_reloaded
              end, MList).


update_db() ->
    nsm_db_update:update_db().

load_module_everywhere(Nodes, Mod) ->
    case code:get_object_code(Mod) of
	{_Module, Bin, Fname} ->
            rpc:eval_everywhere(Nodes, code, load_binary, [Mod, Fname, Bin]);
	Other ->
	    Other
    end.

%%FIX: there should be a better way to reload gettext's DB
web_post_deploy() ->
    [ application:Op(App) || Op <- [stop, start],
                             App <- [gettext] ].

server_post_deploy() ->
    %% [ application:Op(App) || Op <- [stop, start],
    %%                          App <- [gettext] ].
    ok.

eval(S) ->
    eval(S, []).
eval(S, Environ) ->
    {ok,Scanned,_} = erl_scan:string(S),
    {ok,Parsed} = erl_parse:parse_exprs(Scanned),
    erl_eval:exprs(Parsed,Environ).
