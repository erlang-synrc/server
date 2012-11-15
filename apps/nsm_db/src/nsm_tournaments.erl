-module(nsm_tournaments).

-include("user.hrl").
-include("tournaments.hrl").

-export([create/2,create/12,     % { create the tournament }
         destroy/1,              % { purge tournament with all team placeholders and played games }
         join/2,                 % { put users in waiting queue }
         waiting_player/1,       % { dequeue first waiting_player }
         start/1,                % { move users from wating queue to teams }
         remove/2,               % { unsubscribe user from tournaments in all sences }
         all/0,                  % { return all registered tournaments }
         joined_users/1,         % { all wating and active (if tournament started) users }
         user_tournaments/1,     % { all tournaments in which user participates }
         list_users_per_team/1,  % { list of played games of a team with all user replacements }
         get/1,                  % { get tournament from db }
         user_is_team_creator/2, % { if user have team creator priority }
         create_team/1,
         user_joined/2,          % { if user is already joined the tournament }
         active_users/1,         % { list of active users (ready and on lobby page) }
         chat_history/1]).       % { tournament chat history }

create_team(Name) ->
    TID = nsm_db:next_id("team",1),
    ok = nsm_db:put(Team = #team{id=TID,name=Name}),
    TID.

create(UID, Name) -> create(UID, Name, "", date(), time(), 100, 100, undefined, pointing, game_okey, 8, slow).
create(UID, Name, Desc, Date, Time, Players, Quota, Awards, Type, Game, Tours, Speed) ->
    TID = nsm_db:next_id("tournament",1),
    CTime = erlang:now(),
    ok = nsm_db:put(#tournament{name = Name,
                                   id = TID,
                                   description = Desc,
                                   quota = Quota,
                                   players_count = Players,
                                   start_date = Date,
                                   awards = Awards,
                                   creator = UID,
                                   created = CTime,
                                   game_type = Game,
                                   type = Type,
                                   tours = Tours,
                                   speed = Speed,
                                   start_time = Time,
                                   status = created,
                                   owner = UID}),

    TID.

get(TID) ->
    case nsm_db:get(tournament, TID) of
        {ok, Tournament} -> Tournament;
        {error, not_found} -> #tournament{};
        {error, notfound} -> #tournament{}
    end.

start(_TID) -> ok.
destroy(_TID) -> erlang:error(notimpl).
join(UID, TID) -> nsm_db:join_tournament(UID, TID).
remove(_UID, _TID) -> ok.
waiting_player(TID) -> nsm_db:tournament_pop_waiting_player(TID).
joined_users(TID) -> nsm_db:tournament_waiting_queue(TID).
user_tournaments(UID) -> nsm_db:user_tournaments(UID).
user_joined(TID, UID) -> {error, notfound} =/= nsm_db:membership(UID, TID).
active_users(TID) -> nsm_srv_tournament_lobby:active_users(TID).
chat_history(TID) -> nsm_srv_tournament_lobby:chat_history(TID).
all() -> nsm_db:all(tournament).
user_is_team_creator(_UID, _TID) -> true.
list_users_per_team(_TeamID) -> [].
