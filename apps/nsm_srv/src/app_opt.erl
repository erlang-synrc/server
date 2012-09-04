-module(app_opt).
-compile(export_all).

get_smtp_user() -> g(zealot_db:get(config, "smtp/user",     "noreply@kakaranet.com")).
get_smtp_pass() -> g(zealot_db:get(config, "smtp/password", "unknown")).
get_smtp_host() -> g(zealot_db:get(config, "smtp/host",     "smtp.kakaranet.com")).
get_smtp_port() -> g(zealot_db:get(config, "smtp/port",     587)).
get_smtp_ssl()  -> g(zealot_db:get(config, "smtp/with_ssl", false)).

get_game_srv() -> nsx_opt:get_env(nsm_srv,game_srv_node, 'game@rigdzin.cc').
get_web_srv()  -> nsx_opt:get_env(nsm_srv,web_srv_node, 'web@rigdzin.cc').
get_riak_srv() -> nsx_opt:get_env(nsm_srv,riak_srv_node, 'store@rigdzin.cc').
get_version()  -> nsx_opt:get_env(nsm_srv,version, "1").
get_dba()      -> nsx_opt:get_env(nsm_srv,dba, zealot_mnesia).

%% default quota assignment
get_default_quota() -> g(zealot_db:get(config, "accounts/default_quota",  300)).
%% quota limits
get_quota_limit_soft() -> g(zealot_db:get(config, "accounts/quota_limit/soft",  -20)).
get_quota_limit_hard() -> g(zealot_db:get(config, "accounts/quota_limit/hard",  -100)).

%% local functions

%% get value from result tuple
g({ok, V}) ->
	V;
g(Other) ->
	throw({unexpected_reply, Other}).
