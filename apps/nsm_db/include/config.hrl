-define(MAX_BAD_OPERATION, 5).

%-define(SMTP_USER, db_opt:get_smtp_user()).
%-define(SMTP_PASSWD, db_opt:get_smtp_pass()).
%-define(SMTP_HOST, db_opt:get_smtp_host()).
%-define(SMTP_PORT, db_opt:get_smtp_port()).
%-define(SMTP_SSL, db_opt:get_smtp_ssl()).

-define(APPSERVER_NODE, nsx_opt:get_env(nsm_db, app_srv_node, "app@srv5.kakaranet.com")).
-define(GAMESRVR_NODE, nsx_opt:get_env(nsm_db, game_srv_node, "game@srv5.kakaranet.com")).
-define(WEBSERVER_NODE, nsx_opt:get_env(nsm_db, web_srv_node, "web@srv5.kakaranet.com")).
-define(DBA, nsx_opt:get_env(nsm_db, dba, nsm_riak)).
-define(VERSION, nsx_opt:get_env(nsm_db, version, "1")).
-define(RIAKSERVER_NODE, nsx_opt:get_env(nsm_db, riak_srv_node, "app@srv5.kakaranet.com")).

-record(config, {key, value}).
