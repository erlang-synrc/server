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

main_authorized() -> #template { file=code:priv_dir(nsp_srv)++"/templates/dummy_kakaranet.html" }.

generate_token0() ->
    T0 = crypto:rand_bytes(100),
    T = base64:encode(T0),
    T.

adobe_client() ->
    ?INFO("Adobe Client: ~p",[?_U(wf:q(game_name))]),
    io_lib:fwrite("\"/testauth/~s.swf?v=20130129.1\"~n",[?_U(wf:q(game_name))]).

token() ->
    U = wf:user(),
    GameType = ["flashvars.gameType = \"", ?_U(wf:q(game_name)),"\";"],

    Debug = case nsm_db:get(config,"is_production",false) of
                 {ok,true} -> "flashvars.debugMode = \"false\";\n";
                 {ok,false} -> "flashvars.debugMode = \"true\";\n"
            end,

    ?INFO("Starting CLient: ~p",[{?SERVER_HOST,?SERVER_PORT}]),

    [
        io_lib:fwrite("var flashvars = {};~n", []),
        io_lib:fwrite(Debug,[]),
        io_lib:fwrite("flashvars.port = ~b;~n", [?SERVER_PORT]),
        io_lib:fwrite("flashvars.locale = '~s';~n", [site_utils:detect_language()]),
        io_lib:fwrite(GameType, []),

        case wf:q(id) of
             undefined -> "";
             Id ->        A = wf:session(Id),
                          GameId = case A of undefined -> list_to_integer(Id); _ -> list_to_integer(A) end,
                          ?INFO("GameId: ~p",[GameId]),
                          Zone = GameId div 1000000,
                          Host = case Zone of
                                      4 -> "127.0.1.1";
                                      _ -> lists:nth(Zone,?SERVER_HOST)
                                 end,
                          T = nsm_queries:store_token([GameId,generate_token0(),U]),
                          [
                            io_lib:fwrite("flashvars.host = \"~s\";~n",[Host]),
                            io_lib:fwrite("flashvars.tokenKey = encodeURIComponent(\"~s\");~n", [T]),
                            io_lib:fwrite("flashvars.gameId = ~p;~n", [GameId])
                          ]
        end
    ].
