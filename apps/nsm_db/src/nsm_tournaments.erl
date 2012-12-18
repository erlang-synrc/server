-module(nsm_tournaments).
-include("user.hrl").
-include("tournaments.hrl").
-compile(export_all).

create_team(Name) ->
    TID = nsm_db:next_id("team",1),
    ok = nsm_db:put(Team = #team{id=TID,name=Name}),
    TID.

create(UID, Name) -> create(UID, Name, "", date(), time(), 100, 100, undefined, pointing, game_okey, standard, 8, slow).
create(UID, Name, Desc, Date, Time, Players, Quota, Awards, Type, Game, Mode, Tours, Speed) ->
    NodeAtom = nsx_opt:get_env(nsm_db,game_srv_node,'game@doxtop.cc'),
    TID = rpc:call(NodeAtom,id_generator,get_id,[]), % nsm_db:next_id("tournament",1),
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
                                   game_mode = Mode,
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
join(UID, TID) -> nsm_db:join_tournament(UID, TID).
remove(UID, TID) -> nsm_db:leave_tournament(UID, TID).
waiting_player(TID) -> nsm_db:tournament_pop_waiting_player(TID).
joined_users(TID) -> nsm_db:tournament_waiting_queue(TID).
user_tournaments(UID) -> nsm_db:user_tournaments(UID).
user_joined(TID, UID) -> 
    AllJoined = [UId || #play_record{who = UId} <- joined_users(TID)],
    lists:member(UID, AllJoined).
all() -> nsm_db:all(tournament).
user_is_team_creator(_UID, _TID) -> true.
list_users_per_team(_TeamID) -> [].
destroy(TID) -> nsm_db:delete_by_index(play_record, <<"play_record_tournament_bin">>, TID),
                          nsm_db:delete(tournament,TID).
clear() -> [destroy(T#tournament.id) || T <- nsm_db:all(tournament)].
lost() -> lists:usort([erlang:element(3, I) || I <- nsm_db:all(play_record)]).
