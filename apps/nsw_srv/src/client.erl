-module (client).
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
     GameType =
        case wf:q('__submodule__') of
            "batak" -> "flashvars.gameType = \"batak\";~n";
            "king"  -> "flashvars.gameType = \"king\";~n";
            "tavla" -> "flashvars.gameType = \"tavla\";~n";
            "okey"  -> "flashvars.gameType = \"okey\";~n";
            "sorbi" -> "flashvars.gameType = \"sorbi\";~n";
            _ ->
                ""
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
         undefined ->
             "";
         Id ->

              GameId = list_to_integer(wf:session(Id)),
%              Tab = view_table:get_table(list_to_integer(Id)),
%              ?INFO("Tab: ~p",[GameId]),
              io_lib:fwrite("flashvars.gameId = ~b;~n", [GameId])

%              ?INFO("Lookup just created table: ~p",[Tab]),
%              ?INFO("Client Tab: ~p",[Tab]),
%              case Tab of
%             case rpc:call(?APPSERVER_NODE,table_manager,get_table,[list_to_integer(Id)]) of
%		 {error,not_found} ->
%		     "alert('"++?_T("Table not found")++"');";
%		 {ok,Table} ->
%		     GameId = Tab#game_table.gameid,
 %                    ?INFO("GameID: ~p",[GameId]),
%		     io_lib:fwrite("flashvars.gameId = ~b;~n", [GameId])
%	     end
     end
    ].
