-module(nsx_mibs_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    application:stop(otp_mibs),
    application:stop(snmp),
    mnesia:change_table_copy_type(schema, node(), disc_copies),
    nsx_mibs_db:init(),

    %% Initialize table for demo purposes
    nsx_mibs_db:set_req_per_sec("Serv", 2000),
    nsx_mibs_db:set_online("GameSrv", 1000),

    application:start(snmp),
    application:start(otp_mibs),
    nsx_mibs_sup:start_link().

stop(_State) ->
    ok.
