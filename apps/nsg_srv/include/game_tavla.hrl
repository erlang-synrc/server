
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
          set_no :: integer, %% number of current set
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
          observer_flag     :: boolean()
                     }).

-record(tavla_color_info, {
          name :: any(),
          color :: integer()
                     }).

-record(tavla_player_ready, {
          player :: 'PlayerId'()
                     }).

-record(tavla_game_started, {
          board :: list(tuple(integer(), integer()) | null),
          players :: list(#tavla_color_info{})
          }).

-record(tavla_next_turn, {
          player :: #'PlayerInfo'{}
                     }).

-record(tavla_rolls, {
          player  :: 'PlayerId'(),
          dices   :: list(integer())
                     }).

-record(tavla_moves, {
          player       :: 'PlayerId'(),
          from         :: 'Position'(),
          to           :: 'Position'(),
          hits = false :: boolean()
                          }).

-record(tavla_vidoes, {
          player   :: 'PlayerId'()
                      }).

-record(tavla_accepts, {
          player   :: 'PlayerId'(),
          accept   :: boolean()
                      }).

-record(tavla_timeouts, {
          player  :: 'PlayerId'()
                      }).

-record(tavla_series_ended, {}).

-record(tavla_game_ended, {
          winner  :: 'PlayerId'(),
          results :: #'TavlaGameResults'{}
                     }).


%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%     ACTION      %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(tavla_ready, {}).

-record(tavla_roll, {}).

-record('TavlaAtomicMove', { from :: 'Position'(), to :: 'Position'() } ).
-record(tavla_move, { moves :: list(#'TavlaAtomicMove'{}), player :: 'PlayerId'() }).

-record(tavla_skip, {}).


-record(tavla_request, {}).

-record(tavla_vido, {}).

-record(tavla_vido_request, {
          from :: 'PlayerId'(),
          to   :: 'PlayerId'()
                    }).

-record(tavla_vido_answer, {
          from :: 'PlayerId'(),
          to   :: 'PlayerId'(),
          answer :: boolean()
                    }).

-record(tavla_ack, {
       type :: atom(),
       from :: 'PlayerId'(),
       to   :: 'PlayerId'(),
       answer :: boolean()}).

-record(tavla_accepts_vido, {
          accept :: boolean()
                    }).

-record(tavla_surrender, {}).

-record(tavla_surrender_request, {
          from :: 'PlayerId'(),
          to   :: 'PlayerId'()
                    }).

-record(tavla_surrender_answer, {
          from :: 'PlayerId'(),
          to   :: 'PlayerId'(),
          answer :: boolean()
                    }).

-record(tavla_accept_timeout, {
          accept = true:: boolean()
         }).

-record(tavla_game_player_state, {
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
