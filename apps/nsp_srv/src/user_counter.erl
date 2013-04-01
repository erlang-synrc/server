-module(user_counter).
-author('Maxim Sokhatsky <maxim@synrc.com>').
-behaviour(gen_server).
-include("uri_translator.hrl").
-include_lib("nsm_db/include/user.hrl").
-include_lib("nsx_config/include/log.hrl").
-include_lib("nsm_gifts/include/common.hrl").
-include_lib("nsm_db/include/tournaments.hrl").
-export([start_link/0, user_count/0, joined_users/1, write_cache/2, get_word/1,
         get_translation/1, gifts/2, tournaments/0, register_user/1, packages/0,groups/0,
         retournaments/0,repackages/0,regroups/0, active_users_top/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-define(SERVER, ?MODULE).
-record(state, {user_count=0,packages,last_check=undefined,tour_cache,translations,words,gifts,tournaments,active_users,active_users_top,groups}).

% this is actually a cache layer

gifts(Min,Max) -> gen_server:call(?SERVER, {gifts,Min,Max}).
tournaments() -> gen_server:call(?SERVER, tournaments).
packages() -> gen_server:call(?SERVER, packages).
groups() -> gen_server:call(?SERVER, groups).
retournaments() -> gen_server:call(?SERVER, retournaments).
repackages() -> gen_server:call(?SERVER, repackages).
regroups() -> gen_server:call(?SERVER, regroups).
register_user(Pid) -> gen_server:call(?SERVER, {register_user,Pid}).
user_count() -> gen_server:call(?SERVER, user_count).
joined_users(TID) -> gen_server:call(?SERVER, {joined_users,TID}).
write_cache(TID,PlayRecord) -> gen_server:call(?SERVER, {write_cache,TID,PlayRecord}).
start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
get_word(Word) -> gen_server:call(?SERVER, {get_word,Word}).
get_translation({Lang,Translation}) -> gen_server:call(?SERVER, {get_translation,{Lang,Translation}}).
active_users_top() -> gen_server:call(?SERVER, active_users_top).

init([]) -> 
    State = #state{tour_cache=dict:new(),translations=dict:new(),words=dict:new(),active_users=0},
    NewState = lists:foldl(fun({English, Lang, Word},ST) ->

                      Words0 = dict:store(English,
             #ut_word{english = English, lang = "en", word = English}, ST#state.words),
                      Words = dict:store(Word,
             #ut_word{english = Word, lang = Lang, word = Word}, Words0),
                      Translations0 = dict:store(Lang ++ "_" ++ Word,
             #ut_translation{source = {Lang, Word}, word = English}, ST#state.translations),
                      Translations1 = dict:store("en_" ++ English,
             #ut_translation{source = {"en", English}, word = English}, Translations0),
                      Translations = dict:store(Lang ++ "_" ++ English,
             #ut_translation{source = {Lang, English}, word = Word}, Translations1),
                      ST#state{words = Words, translations = Translations}

    end, State, ?URI_DICTIONARY),
   {ok, NewState}.

handle_call({get_word,Word}, _From, State)->
  Return = case dict:find(Word,State#state.words) of 
       error -> {error,notfound}; 
       R -> R end,
  {reply, Return, State};

handle_call({gifts,MinPrice,MaxPrice}, _From, State)->
   Gifts = case State#state.gifts of
        undefined -> nsm_db:all(gifts);
        A -> A end,
   R = [ Gift || Gift <-Gifts, Gift#gift.enabled_on_site, (Gift#gift.kakush_point >= MinPrice) and (Gift#gift.kakush_point =< MaxPrice)],
  {reply, R, State#state{gifts=Gifts}};

handle_call(tournaments, _From, State)->
   Tournaments = case State#state.tournaments of
        undefined -> nsm_db:all(tournament);
        A -> A end,
  {reply, Tournaments, State#state{tournaments=Tournaments}};

handle_call(packages, _From, State)->
   Tournaments = case State#state.packages of
        undefined -> nsm_db:all(membership_package);
        A -> A end,
  {reply, Tournaments, State#state{packages=Tournaments}};

handle_call(groups, _From, State)->
   Tournaments = case State#state.groups of
        undefined -> nsm_db:all(group);
        A -> A end,
  {reply, Tournaments, State#state{groups=Tournaments}};

handle_call(retournaments, _From, State)->
   Tournaments = nsm_db:all(tournament),
  {reply, ok, State#state{tournaments=Tournaments}};

handle_call(repackages, _From, State)->
   Tournaments = nsm_db:all(membership_package),
  {reply, ok, State#state{packages=Tournaments}};

handle_call(regroups, _From, State)->
   Tournaments = nsm_db:all(group),
  {reply, ok, State#state{groups=Tournaments}};

handle_call({register_user,Pid}, _From, State)->
  erlang:monitor(process,Pid),
   ?INFO("UP counter: ~p",[Pid]),
  Users = State#state.active_users,
  {reply, Users, State#state{active_users = Users + 1}};

handle_call({get_translation,{Lang,Translation}}, _From, State)->
  Return = case dict:find(Lang ++ "_" ++ Translation,State#state.translations) of 
       error -> {error,notfound}; 
       R -> R end,
  {reply, Return, State};

handle_call(user_count, _From, State)->
  {L,T} = case State#state.last_check == undefined orelse 
               timer:now_diff(now(),State#state.last_check) div 1000000 > 25 of
       true ->  Users = webutils:online_users(), 
                {length(Users),now()};
       false -> {State#state.user_count,State#state.last_check}
   end,
  {reply, L, State#state{user_count = L, last_check = T}};

handle_call(active_users_top, _From, State)->
  Top = case State#state.active_users_top of
    undefined -> nsm_db:all(active_users_top);
    T -> T
  end,
  {reply, lists:map(fun(U)-> {U#active_users_top.user_id, nsm_accounts:user_paid(U#active_users_top.user_id)} end, Top), State#state{active_users_top=Top}};
%handle_call(user_count, _From, State)->
%  {reply, State#state.active_users, State};

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
handle_info({'DOWN', Ref, _Type, Pid, _Info}, State) ->
     Users = State#state.active_users,
     ?INFO("DOWN counter: ~p",[{Ref, _Type, Pid, _Info}]),
     erlang:demonitor(Ref),
     {noreply, State#state{active_users = Users - 1}};

handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
