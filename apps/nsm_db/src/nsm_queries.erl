-module(nsm_queries).
-include_lib("nsx_config/include/log.hrl").
-include_lib("nsm_db/include/table.hrl").
-include_lib("stdlib/include/qlc.hrl").
-compile(export_all).

get_single_tables(Setting,UId,GameFSM,_Convert, LeftList) ->
    GetPropList = fun(Key,Setngs) -> 
                   case Setngs of
                        undefined -> undefined;
                        _Else -> proplists:get_value(Key, Setngs)
                   end end,

    Rounds = GetPropList(rounds, Setting),
    GameType = GetPropList(game_mode, Setting),
    Speed = GetPropList(speed, Setting),
    Game = GetPropList(game, Setting),
    PaidOnly = GetPropList(paid_only, Setting),
    Lucky = false,

    MaxUsers = case GameFSM of "tavla" -> 2; "okey" -> 4 end,

    Check = fun(Param,Value) -> 
                   case Param of
                        undefined -> true;
                        _Else -> Param == Value
                   end end,

    Cursor = fun(Id,FilterFree,FilterUser) ->
                qlc:cursor(qlc:q([V || {{_,_,_K},_,V=#game_table{creator=C,
                                                   rounds=R, game_type=G,
                                                   users=U, game_speed=S,
                                                   game_mode=GT,
                                                   paid_only = PO,
                                                   feel_lucky = L}} <- gproc:table(props),
                           FilterFree(MaxUsers - length(U)),
                           FilterUser(C,Id),
                           Check(Game,G),
                           Check(Speed,S),
                           Check(GameType,GT),
                           Check(Rounds,R),
                           Check(Lucky, L),
                           Check(PaidOnly, PO)])
                )
    end,
    OneAvailable   = fun(N) -> N == 1 end,
    TwoAvailable   = fun(N) -> N == 2 end,
    ThreeAvailable = fun(N) -> N == 3 end,
    MoreAvailable  = fun(N) -> N > 3 end,
    NotAvailable   = fun(N) -> N == 0 end,
    Others         = fun(IterUser,CurrentUser) -> IterUser =/= CurrentUser end,
    Own            = fun(IterUser,CurrentUser) -> IterUser == CurrentUser end,

    case LeftList of
        one_other -> qlc:next_answers(Cursor(UId, OneAvailable, Others), 10);
        one_own -> qlc:next_answers(Cursor(UId, OneAvailable, Own), 10);
        two_other -> qlc:next_answers(Cursor(UId, TwoAvailable, Others), 10);
        two_own -> qlc:next_answers(Cursor(UId, TwoAvailable, Own), 10);
        three_other -> qlc:next_answers(Cursor(UId, ThreeAvailable, Others), 10);
        three_own -> qlc:next_answers(Cursor(UId, ThreeAvailable, Own), 10);
        more_other -> qlc:next_answers(Cursor(UId, MoreAvailable, Others), 10);
        more_own -> qlc:next_answers(Cursor(UId, MoreAvailable, Own), 10);
        nomore_other -> qlc:next_answers(Cursor(UId, NotAvailable, Others), 10);
        nomore_own -> qlc:next_answers(Cursor(UId, NotAvailable, Own), 10)
    end.


map_reduce(Module, Fun, Args)->
  lists:flatten([ case rpc:call(Node, Module, Fun, Args) of
      {badrpc, _Reason} -> [];
      R -> R
    end || Node <- nsx_opt:get_env(nsm_db, nodes, [])]).
