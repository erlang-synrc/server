-module(db_opt).
-compile(export_all).

get_smtp_user() -> g(nsm_db:get(config, "smtp/user",     "noreply@kakaranet.com")).
get_smtp_pass() -> g(nsm_db:get(config, "smtp/password", "unknown")).
get_smtp_host() -> g(nsm_db:get(config, "smtp/host",     "smtp.kakaranet.com")).
get_smtp_port() -> g(nsm_db:get(config, "smtp/port",     587)).
get_smtp_ssl()  -> g(nsm_db:get(config, "smtp/with_ssl", false)).

get_app_srv()  -> nsx_opt:get_env(nsm_db,app_srv_node, 'app@rigdzin.cc').
get_game_srv() -> nsx_opt:get_env(nsm_db,game_srv_node, 'game@rigdzin.cc').
get_web_srv()  -> nsx_opt:get_env(nsm_db,web_srv_node, 'web@rigdzin.cc').
get_riak_srv() -> nsx_opt:get_env(nsm_srv,riak_srv_node, 'store@rigdzin.cc').
get_version()  -> nsx_opt:get_env(nsm_srv,version, "1").
get_dba()      -> nsx_opt:get_env(nsm_srv,dba, nsm_riak).
get_pass_init_db()      -> nsx_opt:get_env(nsm_db,pass_init_db, true).

get_default_quota() -> g(nsm_db:get(config, "accounts/default_quota",  300)).
get_quota_limit_soft() -> g(nsm_db:get(config, "accounts/quota_limit/soft",  -20)).
get_quota_limit_hard() -> g(nsm_db:get(config, "accounts/quota_limit/hard",  -100)).

g({ok, V}) -> V;
g(Other) -> throw({unexpected_reply, Other}).
