-module (client).
-author('Maxim Sokhatsky <maxim@synrc.com>').
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("nsm_srv/include/config.hrl").
-include_lib("nsm_srv/include/table.hrl").
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

token() ->
    U = wf:user(),
    T = rpc:call(?APPSERVER_NODE,zealot_auth,generate_token,[U]),
    GameType = case wf:q('__submodule__') of
                    "batak" -> "flashvars.gameType = \"batak\";~n";
                    "king"  -> "flashvars.gameType = \"king\";~n";
                    "tavla" -> "flashvars.gameType = \"tavla\";~n";
                    "okey"  -> "flashvars.gameType = \"okey\";~n";
                    "sorbi" -> "flashvars.gameType = \"sorbi\";~n";
                          _ -> ""
            end,
    Debug = case rpc:call(?APPSERVER_NODE,zealot_db,get,[config,"is_production",false]) of
                 {ok,true} -> "";
                 {ok,false} -> wf:f("flashvars.debugMode = \"true\";\n")
            end,
    [
        io_lib:fwrite("var flashvars = {};~n", []),
        Debug,
        io_lib:fwrite("flashvars.tokenKey = encodeURIComponent(\"~s\");~n", [T]),
        io_lib:fwrite("flashvars.port = ~b;~n", [?SERVER_PORT]),
        io_lib:fwrite("flashvars.locale = '~s';~n", [site_utils:detect_language()]),
        io_lib:fwrite("flashvars.host = \"~s\";~n", [case ?SERVER_HOST of
                                                        "188.40.111.154" -> "test.kakaranet.com";
                                                        X -> X
                                                  end]),
        io_lib:fwrite(GameType, []),

        case wf:q(id) of
             undefined -> "";
             Id ->        GameId = list_to_integer(wf:session(Id)),
                          io_lib:fwrite("flashvars.gameId = ~b;~n", [GameId])
        end
    ].
