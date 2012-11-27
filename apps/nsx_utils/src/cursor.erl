-module(cursor).

-export([size/1,
         current_size/1,
         pos/1,
         new/1,
         new/2,
         next/2,
         prev/2,
         page_next/2,
         page_prev/2,
         get_page/3]).

-include("types.hrl").
-include_lib("eunit/include/eunit.hrl").

-record(cursor,
       {pos = 0,
        current_size = 0,
        data_size = 0,
        cbefore = [],
        ccurrent = [],
        cafter = []}).

new(List) ->
    #cursor{cafter = List, data_size = length(List)}.

new(List, K) ->
    #cursor{cafter = lists:keysort(K, List), data_size = length(List)}.

size(Cursor) ->
    Cursor#cursor.data_size.

current_size(Cursor) ->
    Cursor#cursor.current_size.

pos(Cursor) ->
    Cursor#cursor.pos.

page_next(PageSize, Cursor) ->
    #cursor{pos = Pos} = Cursor,
    Jump = PageSize - (Pos rem PageSize),
    next(Jump, Cursor).

page_prev(PageSize, Cursor) ->
    #cursor{pos = Pos} = Cursor,
    Jump = case Pos rem PageSize of
                0 -> PageSize;
                X -> X
           end,
    prev(Jump, Cursor).

next(N0, C) ->
    N = case length(C#cursor.cafter) of
        Len when N0 > Len -> Len;
        _ -> N0
    end,
    {Result, NewAfter} = lists:split(N, C#cursor.cafter),
    NewBefore = lists:reverse(C#cursor.ccurrent, C#cursor.cbefore),
    NewPos = length(NewBefore),
    {ok, Result, C#cursor{pos = NewPos, cbefore = NewBefore, ccurrent = Result, cafter = NewAfter}}.

prev(N0, C) ->
    N = case length(C#cursor.cbefore) of
        Len when N0 > Len -> Len;
        _ -> N0
    end,
    {Result0, NewBefore} = lists:split(N, C#cursor.cbefore),
    Result = lists:reverse(Result0),
    NewAfter = C#cursor.ccurrent ++ C#cursor.cafter,
    NewPos = length(NewBefore),
    {ok, Result, C#cursor{pos = NewPos, cbefore = NewBefore, ccurrent = Result, cafter = NewAfter}}.

get_page(PageNumber0, PageSize, Cursor) ->
    MinPage = 1,
    MaxPage = 1+(Cursor#cursor.data_size),
    PageNumber = min(MaxPage, max(MinPage, PageNumber0)),
    CurrentPage = 1+(length(Cursor#cursor.cbefore) div PageSize),
    {ok, _, NewCursor} =
        case Cursor of
#cursor{ccurrent=[],cbefore=[]} -> % need to be initialized
cursor:page_next(PageSize,Cursor);
_ -> {ok, '_', Cursor}
end,
    rewind(PageNumber-CurrentPage, PageSize, NewCursor).

rewind(0, _PageSize, Cursor) ->
    {ok, Cursor#cursor.ccurrent, Cursor};
rewind(1, PageSize, Cursor) ->
    page_next(PageSize, Cursor);
rewind(-1, PageSize, Cursor) ->
    page_prev(PageSize, Cursor);
rewind(Posit, PageSize, Cursor) when Posit > 1 ->
    {ok, _, NewCursor} = page_next(PageSize, Cursor),
    rewind(Posit-1, PageSize, NewCursor);
rewind(Negat, PageSize, Cursor) when Negat < -1 ->
    {ok, _, NewCursor} = page_prev(PageSize, Cursor),
    rewind(Negat+1, PageSize, NewCursor).


get_page_test() ->
    TM = 19,
    TestData = lists:seq(1, TM),
    Cursor = new(TestData),
    PageSize = 10,

    %% manual check
    ?assertEqual({ok, lists:seq(1,10), Cursor#cursor{ccurrent=lists:seq(1,10),cafter=lists:seq(11,TM)}},
get_page(1, PageSize, Cursor)),
    ?assertEqual({ok, lists:seq(11,TM), Cursor#cursor{cbefore=lists:seq(10,1,-1),ccurrent=lists:seq(11,TM),cafter=[],pos=PageSize}},
get_page(2, PageSize, Cursor)),

    %% test getting same page from different cursor state
    {ok, _, NC1} = get_page(2, PageSize, Cursor),
    [ ?assertEqual(get_page(PN, PageSize, Cursor), get_page(PN, PageSize, NC1)) || PN <- lists:seq(-10, 10) ],
    ok.
