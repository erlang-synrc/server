
-type 'Color'()   :: integer(). %% 'black' or 'red'
-type 'Position'()   :: integer(). %% 'black' or 'red'

-record('TavlaPlace', { count :: integer } ).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%     EVENTS      %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
-record('TavlaPlayerScore', {
           player_id :: 'PlayerId'(),
           reason :: atom(),
           winner = <<"none">>  :: binary(), %% similar to skill_delta
           score :: integer()
                         }).

-record('TavlaGameResults', {
           table_id  :: integer(),
           game_id :: integer(),
           players :: list(#'TavlaPlayerScore'{})
                     }).

-record('PlayerTavlaStats', {
          playerId, %% : int
          playerName, %% : String;
          level, %% : int; Number
          levelName, %% : String;
          badges, %% : Array; Array of int between [1; 5],
          skill,     %% : int;
          score,     %% : int;
          totalWins, %% : int;
          totalLose, %% : int;
          totalDisconnects, %% : int;
          overalSuccessRatio, %% : Number;
          averagePlayDuration %% : Number;
          }).

-record('TavlaPlayer', {
          pid                      :: pid(),
          player_id                :: any(),
          player_info              :: #'PlayerInfo'{},
          skill                    :: integer(),
	  collected		   :: integer(),
	  color                    :: integer()
         }).

-record(tavla_game_info, {
          game_type :: atom(),
          table_name :: binary(),
          game_mode = undefined :: atom(),
          set_no :: integer, %% number of current set
          table_id  :: integer(),
          tables_num :: integer(),
          current_round :: integer(),
          rounds :: integer(),
          players :: list(#'PlayerInfo'{}),
          speed             :: atom(),      %% [slow, normal, fast, blitz]
          turn_timeout      :: integer(),   %% timeout value for player turn
          challenge_timeout :: integer(),   %% timeout value for challenge
          ready_timeout     :: integer(),   %% timeout value for ready msg
          timeout :: integer(),   %% timeout value for player turn
          mul_factor        :: pos_integer(),
          slang_flag        :: boolean(),
          observer_flag     :: boolean(),
          pause_enabled = true :: boolean(),
          social_actions_enabled = true :: boolean(),
          tournament_type = standalone :: tournament()
                         }).

-record(tavla_color_info, {
          name :: any(),
          table_id  :: integer(),
          color :: integer()
                     }).

-record(tavla_board, {
          id :: integer(),
          name :: any(),
          players :: list(#'PlayerInfo'{}),
          main = false :: boolean()
                     }).

-record(tavla_player_ready, {
          table_id  :: integer(),
          player :: 'PlayerId'()
                     }).

-record(tavla_game_started, {
          table_id  :: integer(),
          board :: list(tuple(integer(), integer()) | null),
          another_boards :: list(#tavla_board{}),
          players :: list(#tavla_color_info{})
          }).

-record(tavla_next_turn, {
          table_id  :: integer(),
          player :: #'PlayerInfo'{}
                     }).

-record(tavla_rolls, {
          table_id  :: integer(),
          player  :: 'PlayerId'(),
          color :: integer(),
          dices   :: list(integer())
                     }).

-record(tavla_moves, {
          table_id  :: integer(),
          player       :: 'PlayerId'(),
          from         :: 'Position'(),
          to           :: 'Position'(),
          hits = false :: boolean()
                          }).

-record(tavla_vidoes, {
          table_id  :: integer(),
          player   :: 'PlayerId'()
                      }).

-record(tavla_accepts, {
          table_id  :: integer(),
          player   :: 'PlayerId'(),
          accept   :: boolean()
                      }).

-record(tavla_timeouts, {
          table_id  :: integer(),
          player  :: 'PlayerId'()
                      }).

-record(tavla_series_ended, {}).

-record(tavla_game_ended, {
          table_id  :: integer(),
          winner  :: 'PlayerId'(),
          results :: #'TavlaGameResults'{}
                     }).


%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%     ACTION      %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(tavla_ready, {table_id  :: integer()}).

-record(tavla_roll, {table_id :: any()}).

-record('TavlaAtomicMove', { table_id::integer(),from :: 'Position'(), to :: 'Position'() } ).
-record(tavla_move, { table_id  :: integer(), moves :: list(#'TavlaAtomicMove'{}), player :: 'PlayerId'() }).

-record(tavla_skip, {table_id  :: integer()}).


-record(tavla_request, {table_id  :: integer()}).

-record(tavla_vido, {table_id :: integer()}).

-record(tavla_vido_request, { table_id :: integer(),
          from :: 'PlayerId'(),
          to   :: 'PlayerId'()
                    }).

-record(tavla_vido_answer, { table_id  :: integer(),
          from :: 'PlayerId'(),
          to   :: 'PlayerId'(),
          answer :: boolean()
                    }).

-record(tavla_ack, { table_id  :: integer(),
       type :: atom(),
       from :: 'PlayerId'(),
       to   :: 'PlayerId'(),
       answer :: boolean()}).

-record(tavla_accepts_vido, { table_id  :: integer(),
          accept :: boolean()
                    }).

-record(tavla_surrender, { table_id  :: integer()}).

-record(tavla_surrender_request, { table_id  :: integer(),
          from :: 'PlayerId'(),
          to   :: 'PlayerId'()
                    }).

-record(tavla_surrender_answer, { table_id  :: integer(),
          from :: 'PlayerId'(),
          to   :: 'PlayerId'(),
          answer :: boolean()
                    }).

-record(tavla_accept_timeout, { table_id  :: integer(),
          accept = true:: boolean()
         }).

-record(tavla_game_player_state, { table_id  :: integer(),
          whos_move     :: 'PlayerId'(),
          game_state    :: atom(),
          places        :: list(#'TavlaPlace'{} | null),
          pile_height   :: integer(),
          current_round :: integer(),
          game_sub_type :: atom(),
          next_turn_in  :: integer() | atom()
          %% number of milliseconds until next turn or 'infinity'
         }).

-record('TavlaSetState', {
          round_cur,
          round_max,
          set_cur,
          set_max
         }).

-record('TavlaTimeouts', {
          speed             :: atom(),      %% [slow, normal, fast, blitz]
          turn_timeout      :: integer(),   %% timeout value for player turn
          challenge_timeout :: integer(),   %% timeout value for challenge
          ready_timeout     :: integer(),   %% timeout value for ready msg
          rematch_timeout   :: integer()    %% timeout value for general api #rematch{} msg
         }).
