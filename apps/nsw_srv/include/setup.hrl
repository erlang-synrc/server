%-define(APPSERVER_NODE, nsx_opt:get_env(nsw_srv,app_srv_node, 'app@rigdzin.cc')).
%-define(GAMESRVR_NODE, nsx_opt:get_env(nsw_srv,game_srv_node, 'game@rigdzin.cc')).
-define(GAMEHOST, nsx_opt:get_env(nsw_srv,game_srv_host, '127.0.1.1')).
-define(FORGET_TOKEN_EXPIRED, 86400).
-define(INVITE_CODE_EXPIRED, 172800). %% 48h
-define(SERVER_PORT, nsx_opt:get_env(nsw_srv,game_srv_port, 7999)).
-define(SERVER_HOST, nsx_opt:get_env(nsw_srv, game_srv_host, "kakaranet.com")).
-define(HTTP_ADDRESS, nsx_opt:get_env(nsw_srv,http_address, "http://kakaranet.com")).
-define(HTTPS_ADDRESS, nsx_opt:get_env(nsw_srv,https_address, "https://kakaranet.com")).
-define(FEED_PAGEAMOUNT, 20).

% demo_id 176025532423202, kakaranet_id 154227314626053
% demo secret df0ed1f649bf974189947caf832ffa01
-define(FB_APP_ID, nsx_opt:get_env(nsw_srv, fb_id, "154227314626053")).
-define(FB_APP_SECRET, nsx_opt:get_env(nsw_srv, fb_secret, "cf9d49958ee536dd75f15bf8ca541965")).
-define(FB_REDIRECT_URI, nsx_opt:get_env(nsw_srv, fb_redirect_uri, "http://doxtop.cc:8000")).
%-define(FB_LOGIN_URI, "https://www.facebook.com/dialog/oauth/?client_id="++?FB_APP_ID++"&redirect_uri="++?FB_REDIRECT_URI++"/?facebook=true").

