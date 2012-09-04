-module(game_opt).
-compile(export_all).

get_game_srv() -> nsx_opt:get_env(nsg_srv,game_srv_node, 'game@rigdzin.cc').
get_game_port() -> nsx_opt:get_env(nsg_srv,game_srv_port, 7999).
get_web_srv()  -> nsx_opt:get_env(nsg_srv,web_srv_node, 'web@rigdzin.cc').
get_app_srv()  -> nsx_opt:get_env(nsg_srv,app_srv_node, 'app@rigdzin.cc').
get_riak_srv() -> nsx_opt:get_env(nsg_srv,riak_srv_node, store@rigdzin.cc).
get_version()  -> nsx_opt:get_env(nsg_srv,version, "1").
get_couch_srv() -> nsx_opt:get_env(nsg_srv,couch_srv_node, "test.kakaranet.com").
get_couch_port() -> nsx_opt:get_env(nsg_srv,couch_srv_port, 5984).
