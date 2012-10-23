-module(nsx_mibs_db).
-include("tables.hrl").
-include("GAME-MIB.hrl").

-export([init/0, set_online/2]).

init()->
    mnesia:create_table(gameTable, [{snmp, [{key, string}]},
	{attributes, record_info(fields, gameTable)},{disc_copies, [node()]} ]).

set_online(Server, Online)->
    R = #gameTable{gName=Server, online=Online},
    mnesia:dirty_write(gameTable,R).