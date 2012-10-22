-module(nsx_mibs_db).
-include("tables.hrl").
-include("GAME-MIB.hrl").
-include("DHT-MIB.hrl").

-export([init/0, set_req_per_sec/2, set_online/2]).

init()->
    mnesia:create_table(dhtTable, [{snmp, [{key, string}]},
	{attributes, record_info(fields, dhtTable)}, {disc_copies, [node()]} ]),

    mnesia:create_table(gameTable, [{snmp, [{key, string}]},
	{attributes, record_info(fields, dhtTable)},{disc_copies, [node()]} ]).

set_req_per_sec(Server, ReqCount)->
    R = #dhtTable{dName=Server, reqPerSec=ReqCount},
    mnesia:dirty_write(dhtTable,R).

set_online(Server, Online)->
    R = #gameTable{gName=Server, online=Online},
    mnesia:dirty_write(gameTable,R).