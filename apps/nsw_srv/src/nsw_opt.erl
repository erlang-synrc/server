-module(nsw_opt).
-compile(export_all).

get_game_srv()  -> nsx_opt:get_env(nsw_srv,game_srv_node, 'game@rigdzin.cc').
get_game_host() -> nsx_opt:get_env(nsw_srv,game_srv_host, "kakaranet.com").
get_game_port() -> nsx_opt:get_env(nsw_srv,game_srv_port, 7999).
get_web_srv()   -> nsx_opt:get_env(nsw_srv,web_srv_node, 'web@rigdzin.cc').
get_app_srv()   -> nsx_opt:get_env(nsw_srv,app_srv_node, 'app@rigdzin.cc').
get_riak_srv()  -> nsx_opt:get_env(nsw_srv,riak_srv_node, kakastore@rigdzin.cc).
get_http_addr() -> nsx_opt:get_env(nsw_srv,http_address, "http://kakaranet.com:7788").
get_fb_id()     -> nsx_opt:get_env(nsw_srv,fb_id, "154227314626053").
get_fb_secret() -> nsx_opt:get_env(nsw_srv,fb_secret, "cf9d49958ee536dd75f15bf8ca541965").
get_jspack()    -> nsx_opt:get_env(nsw_srv,jspack, "min").
get_csspack()   -> nsx_opt:get_env(nsw_srv,csspack, "min").

