-module(nsm_queries).
-include_lib("nsm_db/include/table.hrl").
-include_lib("stdlib/include/qlc.hrl").
-compile(export_all).

get_single_tables(Setting,UId,GameFSM,Convert) ->

    GetPropList = fun(Key,Setngs) -> 
                   case Setngs of
                        undefined -> undefined;
                        _Else -> proplists:get_value(Key, Setngs)
                   end end,

    Rounds = GetPropList(rounds, Setting),
    GameType = GetPropList(game_mode, Setting),
    Speed = GetPropList(speed, Setting),
    Game = GetPropList(game, Setting),
    Lucky = false,

    FilterAllUsers = case GetPropList(users, Setting) of
        undefined -> [];
        {multiple, ManyUsers} -> ManyUsers;
        SingleUser -> [SingleUser]
    end,

    FilterAnyUser = case GetPropList(group, Setting) of
        undefined -> [];
        GroupId -> 
            [UId || UId <- nsm_groups:list_group_members(GroupId)]
    end,

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
                                                   feel_lucky = L}} <- gproc:table(props),
                           FilterFree(MaxUsers - length(U)),
                           FilterUser(C,Id),
                           Check(Game,G),
                           Check(Speed,S),
                           Check(GameType,GT),
                           Check(Rounds,R),
                           Check(Lucky, L)])
                )
    end,
    OneAvailable   = fun(N) -> N == 1 end,
    TwoAvailable   = fun(N) -> N == 2 end,
    ThreeAvailable = fun(N) -> N == 3 end,
    MoreAvailable  = fun(N) -> N > 3 end,
    NotAvailable   = fun(N) -> N == 0 end,
    Others         = fun(IterUser,CurrentUser) -> IterUser =/= CurrentUser end,
    Own            = fun(IterUser,CurrentUser) -> IterUser == CurrentUser end,

    OneLeftListOther = qlc:next_answers(Cursor(UId, OneAvailable, Others), 10),
    OneLeftListOwn = qlc:next_answers(Cursor(UId, OneAvailable, Own), 10),
    TwoLeftListOther = qlc:next_answers(Cursor(UId, TwoAvailable, Others), 10),
    TwoLeftListOwn = qlc:next_answers(Cursor(UId, TwoAvailable, Own), 10),
    ThreeLeftListOther = qlc:next_answers(Cursor(UId, ThreeAvailable, Others), 10),
    ThreeLeftListOwn = qlc:next_answers(Cursor(UId, ThreeAvailable, Own), 10),
    MoreLeftListOther = qlc:next_answers(Cursor(UId, MoreAvailable, Others), 10),
    MoreLeftListOwn = qlc:next_answers(Cursor(UId, MoreAvailable, Own), 10),
    NoMoreLeftListOther = qlc:next_answers(Cursor(UId, NotAvailable, Others), 50),
    NoMoreLeftListOwn = qlc:next_answers(Cursor(UId, NotAvailable, Own), 10),

    QLC = OneLeftListOwn ++ OneLeftListOther ++
          TwoLeftListOwn ++ TwoLeftListOther ++
          ThreeLeftListOwn ++ ThreeLeftListOther ++
          MoreLeftListOwn ++ MoreLeftListOther ++
          NoMoreLeftListOwn ++ NoMoreLeftListOther.

