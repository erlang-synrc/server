-module(session_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-include_lib("nsg_srv/include/logging.hrl").
-include_lib("nsg_srv/include/conf.hrl").

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(MAX_RESTART,    5).
-define(MAX_TIME,      60).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    W = game_session,
    Worker = {W,
              {W, start_link, []},
              temporary, brutal_kill, worker, [W]},
    {ok, { {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME}, [Worker]} }.

