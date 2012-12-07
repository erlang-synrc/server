-module (client).
-author('Maxim Sokhatsky <maxim@synrc.com>').
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("nsm_db/include/config.hrl").
-include_lib("nsm_db/include/table.hrl").
-include("setup.hrl").
-include("loger.hrl").
-include("gettext.hrl").

route() -> ["game_name"].

main() ->
    case wf:user() /= undefined of
        true  -> main_authorized();
        false -> wf:redirect_to_login(?_U("/login"))
    end.

main_authorized() -> #template { file=code:priv_dir(nsw_srv)++"/templates/dummy_kakaranet.html" }.

generate_token0() ->
    T0 = crypto:rand_bytes(100),
    T = base64:encode(T0),
    T.

adobe_client() ->
    ?INFO("Adobe Client: ~p",[?_U(wf:q(game_name))]),
    io_lib:fwrite("\"/testauth/~s.swf?v=03122012.1\"~n",[?_U(wf:q(game_name))]).

token() ->
    U = wf:user(),
    T = rpc:call(?GAMESRVR_NODE,auth_server,store_token,[generate_token0(),U]),
    GameType = ["flashvars.gameType = \"", ?_U(wf:q(game_name)),"\";"],

    Debug = case nsm_db:get(config,"is_production",false) of
                 {ok,true} -> "";
                 {ok,false} -> wf:f("flashvars.debugMode = \"true\";\n")
            end,

    ?INFO("Starting CLient: ~p",[{?SERVER_HOST,?SERVER_PORT}]),

    [
        io_lib:fwrite("var flashvars = {};~n", []),
        Debug,
        io_lib:fwrite("flashvars.tokenKey = encodeURIComponent(\"~s\");~n", [T]),
        io_lib:fwrite("flashvars.port = ~b;~n", [?SERVER_PORT]),
        io_lib:fwrite("flashvars.locale = '~s';~n", [site_utils:detect_language()]),
        io_lib:fwrite(GameType, []),

        case wf:q(id) of
             undefined -> "";
             Id ->        A = wf:session(Id),
                          GameId = case A of undefined -> Id; _ -> list_to_integer(A) end,
                          Host = lists:nth(GameId div 1000000,?SERVER_HOST),
                          ?INFO("Connect to Game Host: ~p",[Host]),
                          [
                            io_lib:fwrite("flashvars.host = \"~s\";~n",[Host]),
                            io_lib:fwrite("flashvars.gameId = ~p;~n", [GameId])
                          ]
        end
    ].
