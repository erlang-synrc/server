-module(game_lastmatch_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-include_lib("nsg_srv/include/logging.hrl").
-include_lib("nsg_srv/include/conf.hrl").

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

-define(MAX_RESTART,    5).
-define(MAX_TIME,      60).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    W = game_lastmatch,
    Worker = {W,
              {W, start_link, []},
              temporary, brutal_kill, worker, [W]},
    {ok, { {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME}, [Worker]} }.
