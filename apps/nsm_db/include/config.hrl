-define(MAX_BAD_OPERATION, 5).

-define(SMTP_USER, db_opt:get_smtp_user()).
-define(SMTP_PASSWD, db_opt:get_smtp_pass()).
-define(SMTP_HOST, db_opt:get_smtp_host()).
-define(SMTP_PORT, db_opt:get_smtp_port()).
-define(SMTP_SSL, db_opt:get_smtp_ssl()).

-define(GAMESERVER_NODE, db_opt:get_game_srv()).
-define(WEBSERVER_NODE, db_opt:get_web_srv()).
-define(RIAKSERVER, node()).
-define(DBA, db_opt:get_dba()).
-define(VERSION, db_opt:get_version()).

-record(config, {key, value}).
