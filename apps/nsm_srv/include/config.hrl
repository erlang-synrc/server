-define(MAX_BAD_OPERATION, 5).

-define(SMTP_USER, app_opt:get_smtp_user()).
-define(SMTP_PASSWD, app_opt:get_smtp_pass()).
-define(SMTP_HOST, app_opt:get_smtp_host()).
-define(SMTP_PORT, app_opt:get_smtp_port()).
-define(SMTP_SSL, app_opt:get_smtp_ssl()).

-define(GAMESERVER_NODE, app_opt:get_game_srv()).
-define(WEBSERVER_NODE, app_opt:get_web_srv()).
%-define(RIAKSERVER, app_opt:get_riak_srv()).
-define(RIAKSERVER, node()).
-define(DBA, app_opt:get_dba()).
-define(VERSION, app_opt:get_version()).

-record(config, {key, value}).
