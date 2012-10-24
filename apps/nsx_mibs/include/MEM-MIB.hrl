%%% This file was automatically generated by snmpc_mib_to_hrl version 4.22.1
%%% Date: 24-Oct-2012::19:08:27
-ifndef('MEM-MIB').
-define('MEM-MIB', true).

%% Oids

-define(mem, [1,3,6,1,4,1,88888,1,1,2]).

-define(memTable, [1,3,6,1,4,1,88888,1,1,2,1]).

-define(memEntry, [1,3,6,1,4,1,88888,1,1,2,1,1]).
-define(id, 1).
-define(total, 2).
-define(used, 3).
-define(memStatus, 4).


%% Range values
-define(low_id, 0).
-define(high_id, 255).


%% Enum definitions from memStatus
-define(memStatus_destroy, 6).
-define(memStatus_createAndWait, 5).
-define(memStatus_createAndGo, 4).
-define(memStatus_notReady, 3).
-define(memStatus_notInService, 2).
-define(memStatus_active, 1).

%% Default values

-endif.
