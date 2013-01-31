-module(user_counter).
-author('Maxim Sokhatsky <maxim@synrc.com>').
-behaviour(gen_server).
-include("uri_translator.hrl").
-include_lib("nsm_db/include/user.hrl").
-include_lib("nsm_gifts/include/common.hrl").
-include_lib("nsm_db/include/tournaments.hrl").
-export([start_link/0, user_count/0, joined_users/1, write_cache/2, get_word/1, get_translation/1, gifts/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-define(SERVER, ?MODULE).
-record(state, {user_count=0,last_check=undefined,tour_cache,translations,words,gifts}).

gifts(Min,Max) -> gen_server:call(?SERVER, {gifts,Min,Max}).
user_count() -> gen_server:call(?SERVER, user_count).
joined_users(TID) -> gen_server:call(?SERVER, {joined_users,TID}).
write_cache(TID,PlayRecord) -> gen_server:call(?SERVER, {write_cache,TID,PlayRecord}).
start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
get_word(Word) -> gen_server:call(?SERVER, {get_word,Word}).
get_translation({Lang,Translation}) -> gen_server:call(?SERVER, {get_translation,{Lang,Translation}}).

init([]) -> 
    State = #state{tour_cache=dict:new(),translations=dict:new(),words=dict:new(),gifts=nsm_db:all(gifts)},
    NewState = lists:foldl(fun({English, Lang, Word},ST) ->

                      Words0 = dict:store(English,
             #ut_word{english = English, lang = "en", word = English},
                       ST#state.words),

                      Words = dict:store(Word,
             #ut_word{english = Word, lang = Lang, word = Word},
                       Words0),

                      Translations0 = dict:store(Lang ++ "_" ++ Word,
             #ut_translation{source = {Lang, Word}, word = English},
                       ST#state.translations),

                      Translations1 = dict:store("en_" ++ English,
             #ut_translation{source = {"en", English}, word = English},
                       Translations0),

                      Translations = dict:store(Lang ++ "_" ++ English,
             #ut_translation{source = {Lang, English}, word = Word},
                       Translations1),

                      ST#state{words = Words, translations = Translations}

%                          ok = nsm_db:put(#ut_word{english = English, lang = "en",  word = English}),
%                          ok = nsm_db:put(#ut_word{english = Word,    lang = Lang,  word = Word}),
%                          ok = nsm_db:put(#ut_translation{source = {Lang, Word},    word = English}),
%                          ok = nsm_db:put(#ut_translation{source = {"en", English}, word = English}),
%                          ok = nsm_db:put(#ut_translation{source = {Lang, English}, word = Word}),

    end, State, ?URI_DICTIONARY),
   {ok, NewState}.

handle_call({get_word,Word}, _From, State)->
  Return = case dict:find(Word,State#state.words) of 
       error -> {error,notfound}; 
       R -> R end,
  {reply, Return, State};

handle_call({gifts,MinPrice,MaxPrice}, _From, State)->
   R = [ Gift || Gift <-State#state.gifts, Gift#gift.enabled_on_site, (Gift#gift.kakush_point >= MinPrice) and (Gift#gift.kakush_point =< MaxPrice)],
  {reply, R, State};

handle_call({get_translation,{Lang,Translation}}, _From, State)->
  Return = case dict:find(Lang ++ "_" ++ Translation,State#state.translations) of 
       error -> {error,notfound}; 
       R -> R end,
  {reply, Return, State};

handle_call(user_count, _From, State)->
  {L,T} = case State#state.last_check == undefined orelse 
               timer:now_diff(now(),State#state.last_check) div 1000000 > 60 * 5 of
       true ->  Users = webutils:online_users(), %lists:partition(fun({_,_,A}) -> is_list(A) end, qlc:e(gproc:table())),
                {length(Users),now()};
       false -> {State#state.user_count,State#state.last_check}
   end,
  {reply, L, State#state{user_count = L, last_check = T}};

handle_call({joined_users,TID}, _From, State)->
  {LS,TS} = case dict:find(TID,State#state.tour_cache) of 
       error -> {[],undefined}; 
       {ok,Val} -> Val end,
   {JU,T} = case TS == undefined orelse timer:now_diff(now(),TS) div 1000000 > 60 * 2 of
        true ->  Full = nsm_tournaments:joined_users(TID),
                 {Full,now()};
        false -> {LS,TS}
    end,
    Dict = dict:store(TID,{JU,T},State#state.tour_cache),
  {reply, JU, State#state{tour_cache=Dict}};

handle_call({write_cache,TID,Item}, _From, State)->
  {LS,TS} = case dict:find(TID,State#state.tour_cache) of 
       error -> {[],undefined}; 
       {ok,Val} -> Val end,
   {JU,T} = case TS == undefined of
        true ->  {[],undefined};
        false -> Found = lists:keyfind(Item#play_record.who,#play_record.who,LS),
                 NewList = case Found of 
                                false -> [Item] ++ LS;
                                F -> lists:keyreplace(Item#play_record.who,#play_record.who,LS,Item) end,
                 {NewList,now()}
    end,
    Dict = dict:store(TID,{JU,T},State#state.tour_cache),
  {reply, JU, State#state{tour_cache=Dict}};

handle_call(_Request, _From, State) -> {reply, unknown, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
