-module(i18n_route_handler).
% -extends(dynamic_route_handler).
% Can't be extend module because dynamic_route_handler should exports mroe functions.
-behaviour (route_handler).

-include_lib("nitrogen_core/include/wf.hrl").
-include("gettext.hrl").
-include("loger.hrl").

-export ([
          init/2,
          finish/2
         ]).

%% @doc
%% i18n_route_handler works exactly as nitrogen's {@link dynamic_router_hanlder} except that it
%% translates the path to english before routing.
%% @end

init(_Config, State) ->
    % Get the path...
    RequestBridge = wf_context:request_bridge(),
    BasePath = RequestBridge:path(),

    Path =
        try uri_translator:translate(BasePath)
        catch
            _:{unknown_language, _} ->
%		?PRINT("uknown_Language"),
                "404"; %% It should lead to a 404
            _:{unknown_translation, _, _} ->
%		?PRINT("unknown traslation"),
                "404" %% It should lead to a 404
        end,

%    ?DBG("User: ~p~nBasePath: ~p~nPath: ~p~n",
%            [wf:user(), BasePath, Path]),

    % Convert the path to a module. If there are no routes defined, then just
    % convert everything without an extension to a module.
    % Otherwise, look through all routes for the first matching route.
    {Module, PathInfo} = route(re:replace(Path, "-", "_", [{return, list}, global])),
    {Module1, PathInfo1} = check_for_404(Module, PathInfo, Path),
    % ?PRINT({BasePath, {Module, PathInfo}, {Module1, PathInfo1}}),

    wf_context:page_module(Module1),
    wf_context:path_info(PathInfo1),

    %% CAUTION! This will work only with custom handler which implements
    %% aad_path_params/0 function. After this call we will have parameters from
    %% path in list of the regular params
    wf_handler:call(query_handler, add_path_params, []),

    {ok, State}.

finish(_Config, State) ->
    {ok, State}.

route("/") ->
    {index, []};

route(Path) ->
    IsStatic = (filename:extension(Path) /= []),
    case IsStatic of
        true ->
            % Serve this up as a static file.
            {static_file, Path};

        false ->
            Path1 = string:strip(Path, both, $/),
            Tokens = string:tokens(Path1, "/"),
            % Check for a loaded module. If not found, then try to load it.
            case try_load_module(Tokens) of
                {Module, PathInfo} ->
                    {Module, PathInfo};
                undefined ->
                    {web_404, Path1}
            end
    end.

try_load_module(Tokens) -> try_load_module(Tokens, []).
try_load_module([], _ExtraTokens) -> undefined;
try_load_module(Tokens, ExtraTokens) ->
    %% Get the module name...
    ModuleName = string:join(Tokens, "_"),
    Module = try
        list_to_existing_atom(ModuleName)
    catch _:_ ->
        case erl_prim_loader:get_file(ModuleName ++ ".beam") of
            {ok, _, _} -> list_to_atom(ModuleName);
            _ -> list_to_atom("$not_found")
        end
    end,

    %% Load the module, check if it exports the right method...
    code:ensure_loaded(Module),
    case erlang:function_exported(Module, main, 0) of
        true ->
            PathInfo = string:join(ExtraTokens, "/"),
            {Module, PathInfo};
        false ->
            next_try_load_module(Tokens, ExtraTokens)
    end.

next_try_load_module(Tokens, ExtraTokens) ->
    Tokens1 = lists:reverse(tl(lists:reverse(Tokens))),
    ExtraTokens1 = [hd(lists:reverse(Tokens))|ExtraTokens],
    try_load_module(Tokens1, ExtraTokens1).

check_for_404(static_file, _PathInfo, Path) ->
    {static_file, Path};

check_for_404(Module, PathInfo, Path) ->
    % Make sure the requested module is loaded. If it
    % is not, then try to load the web_404 page. If that
    % is not available, then default to the 'file_not_found_page' module.
    case code:ensure_loaded(Module) of
        {module, Module} -> {Module, PathInfo};
        _ ->
            case code:ensure_loaded(web_404) of
                {module, web_404} -> {web_404, Path};
                _ -> {file_not_found_page, Path}
            end
    end.
