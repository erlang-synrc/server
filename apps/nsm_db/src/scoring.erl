-module(scoring).
-author('Maxim Sokhatsky <maxim@synrc.com>').
-include("user.hrl").
-include("scoring.hrl").
-include_lib("nsx_config/include/log.hrl").

-export([add_score/3,score_entries/1, add_personal_score/7, score_test/0]).

add_score(PlayerId, ScoringRecord, Temp) ->
    PlayerScoring = case nsm_db:get(player_scoring, PlayerId) of 
       {ok,R} -> R;
       {error,notfound} -> 
           nsm_db:put(A = #player_scoring{id=PlayerId,agregated_score=0}),
           A
    end,
    EntryId = nsm_db:next_id("scoring_record",1),
    Prev = undefined,
    Top = case Temp of
    	temp -> PlayerScoring#player_scoring.temp;
        perm -> PlayerScoring#player_scoring.permanent
    end,
    case Top of
        undefined ->
            Next = undefined;
	X ->
	    case nsm_db:get(scoring_record, erlang:integer_to_list(X)) of
	       {ok, TopEntry} ->
		    Next = TopEntry#scoring_record.id,
		    EditedEntry = #scoring_record{
		              id = TopEntry#scoring_record.id,
                      condition = TopEntry#scoring_record.condition,
                      game_type = TopEntry#scoring_record.game_type,
                      game_kind = TopEntry#scoring_record.game_kind,
                      game_id = TopEntry#scoring_record.game_id,
                      score_kakaush = TopEntry#scoring_record.score_kakaush,
                      score_points = TopEntry#scoring_record.score_points,
                      custom = TopEntry#scoring_record.custom,
                      timestamp = TopEntry#scoring_record.timestamp,
                      who = TopEntry#scoring_record.who,
                      all_players = TopEntry#scoring_record.all_players,
                      next = TopEntry#scoring_record.next,
                      prev = EntryId},
                    nsm_db:put(EditedEntry); % update prev entry
            {error,notfound} -> Next = undefined
	    end
    end,

    {T,P} = case Temp of
        temp -> {EntryId,PlayerScoring#player_scoring.permanent};
        perm -> {PlayerScoring#player_scoring.temp,EntryId}
    end,

    Entry  = ScoringRecord#scoring_record{id = EntryId,
                    who = PlayerId,
                    next = Next,
                    prev = Prev},

    nsm_db:put(PlayerScoring#player_scoring{
          temp=T, permanent=P,
          agregated_score = PlayerScoring#player_scoring.agregated_score + 
             case Temp of
                perm -> Entry#scoring_record.score_points;
                temp -> 0
             end}),

    case nsm_db:put(Entry) of
        ok ->
            {ok, Entry}
    end.

score_entries(PlayerId) ->
    RA = nsm_db:get(player_scoring, PlayerId),
    case RA of
        {ok,RO} -> traverse_score_entries(RO#player_scoring.permanent, []);
        {error, _} -> []
    end.

traverse_score_entries(undefined, Result) ->
    Result;
traverse_score_entries(Next, Result) ->
    RA = nsm_db:get(scoring_record, Next),
    case RA of
	{ok,RO} -> traverse_score_entries(RO#scoring_record.next, Result ++ [RO]);
	{error,notfound} -> Result
    end.


add_personal_score(UId, Games, Wins, Loses, Disconnects, Points, AverageTime) ->
    {_, PS} = nsm_db:get(personal_score, UId),
    {NGames, NWins, NLoses,  NDisconnects, NPoints, NAverageTime} = case PS of
        notfound ->
            {Games, Wins, Loses, Disconnects, Points, AverageTime};
        _ -> 
            {
                PS#personal_score.games + Games, 
                PS#personal_score.wins + Wins,
                PS#personal_score.loses + Loses,
                PS#personal_score.disconnects + Disconnects,
                PS#personal_score.points + Points,
                case PS#personal_score.games of
                    0 -> 0;
                    Gs -> (PS#personal_score.average_time * Gs + AverageTime) / (PS#personal_score.games + Games)
                end
            }
    end,
    nsm_db:put(#personal_score{
        uid = UId,
        games = NGames,
        wins = NWins,
        loses = NLoses,
        disconnects = NDisconnects,
        points = NPoints,
        average_time = NAverageTime
    }).


score_test() ->
    SR1 = #scoring_record{
        game_id = 1000403,
        who = "demo1",
        all_players = ["demo1", "(robot)"],
        game_type = standart,
        game_kind = tavla,
        score_points = 12,
        score_kakaush = 34,
        timestamp = erlang:now()
    },
    SR2 = #scoring_record{
        game_id = 1000404,
        who = "demo1",
        all_players = ["demo1", "(robot)", "demo2"],
        game_type = standart,
        game_kind = tavla,
        score_points = 56,
        score_kakaush = 78,
        timestamp = erlang:now()
    },
    SR3 = #scoring_record{
        game_id = 1000405,
        who = "demo1",
        all_players = ["demo1", "(robot)", "demo2", "maxim"],
        game_type = standart,
        game_kind = tavla,
        score_points = 90,
        score_kakaush = 12,
        timestamp = erlang:now()
    },
    add_score("demo1", SR1, perm),
    add_score("demo1", SR2, perm),
    add_score("demo1", SR3, perm),
    [
        score_entries("demo1"),
        nsm_db:all(player_scoring),
        nsm_db:all(scoring_record)
    ].
