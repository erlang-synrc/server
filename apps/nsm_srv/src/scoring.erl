-module(scoring).

-include("user.hrl").
-include("scoring.hrl").

-export([add_score/3,score_entries/1]).

add_score(PlayerId, ScoringRecord, Temp) ->
    PlayerScoring = case zealot_db:get(player_scoring, PlayerId) of 
       {ok,R} -> R;
       {error,notfound} -> 
           zealot_db:put(A = #player_scoring{id=PlayerId,agregated_score=0}),
           A
    end,
    EntryId = zealot_db:next_id("scoring_record",1),
    Prev = undefined,
    Top = case Temp of
	temp -> PlayerScoring#player_scoring.temp;
        perm -> PlayerScoring#player_scoring.permanent
    end,
    case Top of
        undefined ->
            Next = undefined;
	X ->
	    case zealot_db:get(scoring_record, erlang:integer_to_list(X)) of
	       {ok, TopEntry} ->
		    Next = TopEntry#scoring_record.id,
		    EditedEntry = #scoring_record{
		      id = TopEntry#scoring_record.id,
                      condition = TopEntry#scoring_record.condition,
                      game_type = TopEntry#scoring_record.game_type,
                      game_kind = TopEntry#scoring_record.game_kind,
                      game_id = TopEntry#scoring_record.game_id,
                      score = TopEntry#scoring_record.score,
                      custom = TopEntry#scoring_record.custom,
                      who = TopEntry#scoring_record.who,
                      next = TopEntry#scoring_record.next,
                      prev = EntryId},
                    zealot_db:put(EditedEntry); % update prev entry
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

    zealot_db:put(PlayerScoring#player_scoring{
          temp=T, permanent=P,
          agregated_score = PlayerScoring#player_scoring.agregated_score + 
             case Temp of
                perm -> Entry#scoring_record.score;
                temp -> 0
             end}),

    case zealot_db:put(Entry) of
        ok ->
            {ok, Entry}
    end.

score_entries(PlayerId) ->
    RA = zealot_db:get(player_scoring, PlayerId),
    case RA of
        {ok,RO} -> traverse_score_entries(RO#player_scoring.permanent, []);
        {error, _} -> []
    end.

traverse_score_entries(undefined, Result) ->
    Result;
traverse_score_entries(Next, Result) ->
    RA = zealot_db:get(scoring_record, Next),
    case RA of
	{ok,RO} -> traverse_score_entries(RO#scoring_record.next, Result ++ [RO]);
	{error,notfound} -> Result
    end.
