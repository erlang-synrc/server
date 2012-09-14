-define(WEBSERVER_NODE, game_opt:get_web_srv()).
-define(APPSERVER_NODE, game_opt:get_app_srv()).
-define(GAMESERVER_NODE, game_opt:get_game_srv()).
-define(COUCH_ACC, {game_opt:get_couch_srv(), game_opt:get_couch_port()}).
-define(COUCH_GAMES, nsm_db:get(config, "couchdb/couch_games", "okey_games") ).
-define(COUCH_SKILL, "okey_games").
-define(MNESIA_INITIAL, fun() -> db:start_kakaserver() end).
-define(MNESIA_INITIAL_DEFAULT, fun() -> db:sharing_mnesia(?WEBSERVER_NODE) end).
-define(SET_COUCH_ACC, fun() -> zealot_db:put({config,"couchdb/couch_acc", ?COUCH_ACC}) end).
-define(SET_COUCH_GAMES, fun() -> zealot_db:put(config,"couchdb/couch_games", "okey_games"}) end).
