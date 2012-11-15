-record(team,
        {name, % { team name for now will bu just first player username }
         id,
         play_record, % { linked list history of played users under that ticket }
         type }).

-record(tournament,
        {name, % { tournament name }
         id,
         game_type,
         description,
         creator,
         created,
         start_date,
         start_time,
         end_date,
         status, % { activated, ongoing, finished }
         quota,
         tours,
         awards, 
         teams :: list(#team{}), % { bulk list of fixed teams/placeholders created on tournament start from waiting_queue }
         waiting_queue, % { play_record, added here when user wants to join tournament }
         last,
         owner,
         players_count,
         speed,
         type }). % { eliminatin, pointing, etc }

-record(play_record, % { tournament_player, game_record, tournament_info, choose your name :) }
        {who, % { user }
         id,
         tournament, % { tournament in which user played }
         team, % { team under which user player tournament }
         game_id, % { game id that user played under that team }
         entry_id,
         score_points,
         next,
         prev}).

