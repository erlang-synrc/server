-module(app_util).

-compile(export_all).

start(App) ->
    start(App, permanent).
start(App, Type) ->
    application:load(App),
    start_deps(App),
    case application:start(App, Type) of
        ok ->
            ok;
        {error,{already_started, _}} ->
            ok;
        Error ->
            erlang:error(Error)
    end.


start_deps(Application) ->
    application:load(Application),
    {ok, Apps} = application:get_key(Application, applications),
    [ start(App) || App <- Apps ].
