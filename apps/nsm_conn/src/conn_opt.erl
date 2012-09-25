-module(conn_opt).
-compile(export_all).

get_listen_ip()      -> nsx_opt:get_env(nsm_conn,ip, "127.0.0.1").

