-module(nsg_matchmaker_queue_test).

-include_lib("eunit/include/eunit.hrl").

-define(M, nsg_matchmaker_queue).

startstop_test_() ->
    Q = qq1,
    [{ "starting",
      ?_assertMatch({ok, _},
                     ?M:start(Q, [c,b,a]))
     },
     {"stopping",
      ?_assertMatch(ok,
                    ?M:stop(Q))
     }
    ].


enqueue_test_() ->
    Q = qq2,
    {setup,
     fun() -> 
             logger_setup(),
             {ok, PID} = ?M:start(Q, [a,b,c]), PID
     end,
     fun(PID) -> ?M:stop(PID) end,
     [
      {"enqueue",
       ?_assertMatch(ok,
                     ?M:enqueue(Q, 101, [{a,1},{b,2},{c,cc}]))
      }
     ]
    }.


match_test_() ->
    Q = qq3,
    ObjID = 101,
    NotQueuedObjID = 102,
    {setup,
     fun() -> 
             logger_setup(),
             {ok, PID} = ?M:start(Q, [a,b,c]),
             ok = ?M:enqueue(Q, ObjID, [{b,2},{c,cc},{a,1}]),
             PID
     end,
     fun(PID) -> ?M:stop(PID) end,
     % match test instantiator
     fun(_PID) ->
             [
              {"match - just an object",
               ?_assertMatch({1, [ObjID]},
                             ?M:match(Q, ObjID, 4, [{a,eq}], [{b, eq}]))
              }
             ,{"match - not queued",
               ?_assertMatch({error, not_queued},
                             ?M:match(Q, NotQueuedObjID, 4, [{a,eq}], [{b, eq}]))
              }
             ]
     end
    }.


match2_test_() ->
    Q = qq4,
    ObjID = 101,
    ObjID2 = 102,
    ObjID3 = 103,
    {setup,
     fun() -> 
             logger_setup(),
             {ok, PID} = ?M:start(Q, [a,b,c]),
             ok = ?M:enqueue(Q, ObjID, [{a,1},{b,2},{c,cc}]),
             ok = ?M:enqueue(Q, ObjID2, [{a,1},{b,2},{c,dd}]),
             ok = ?M:enqueue(Q, ObjID3, [{a,1},{b,4},{c,ee}]),
             PID
     end,
     fun(PID) -> ?M:stop(PID) end,
     % match test instantiator
     fun(_PID) ->
             [
              {"match 1",
               ?_assertMatch({1, [ObjID]},
                             ?M:match(Q, ObjID, 1, [{a,eq}], [{b, eq}]))
              }
             ,{"match 2",
               ?_assertMatch({2, [ObjID2,ObjID]},
                             ?M:match(Q, ObjID, 2, [{a,eq}], [{b, eq}]))
              }
             ,{"hard match 2",
               ?_assertMatch({2, [ObjID2,ObjID]},
                             ?M:match(Q, ObjID, 2, [{a,eq},{b,eq}], []))
              }
             ,{"hard match 2 when 3 requested",
               ?_assertMatch({2, [ObjID2,ObjID]},
                             ?M:match(Q, ObjID, 3, [{a,eq},{b,eq}], []))
              }
             ]
     end
    }.


match_dequeue4_test_() ->
    Q = qq5,
    ObjID = 500,
    ObjIDs = lists:seq(1,1000),
    DeqObjIDs = [ObjID, 501, 502, 503],
    {setup,
     fun() -> 
             logger_setup(),
             {ok, PID} = ?M:start(Q, [a,b,c]),
             [ok = ?M:enqueue(Q, O, [{a,1},{b,2},{c,3+O},{d,O}])
              || O <- ObjIDs],
             PID
     end,
     fun(PID) -> ?M:stop(PID) end,
     % match test instantiator
     fun(_PID) ->
             [
              {"match all, ignoring a soft matcher",
               ?_assertMatch({4, [_,_,_,_]},
                             ?M:match(Q, ObjID, 4, [{a,eq},{b,eq}], [{c, eq}]))
              },
              {"match all, ignoring a soft matcher (non-contiguous)",
               ?_assertMatch({4, [_,_,_,_]},
                             ?M:match(Q, ObjID, 4, [{a,eq},{b,eq}], [{d, eq}]))
              },
              {"dequeue 4",
               ?_assertMatch(ok,
                             ?M:dequeue(Q, DeqObjIDs))
              },
              {"match error for dequeued object",
               ?_assertMatch({error, not_queued},
                             ?M:match(Q, ObjID, 4, [{a,eq}], [{b, eq}]))
              }
             ]
     end
    }.



logger_setup() ->
    ok = lager:start(),
    lager:set_loglevel(lager_console_backend, debug),
    ok.
