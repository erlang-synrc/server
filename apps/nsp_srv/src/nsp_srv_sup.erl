-module(nsp_srv_sup).
-behaviour(supervisor).
-export([start_link/0,init/1]).
-include_lib("nsm_db/include/config.hrl").
-include_lib("nsm_db/include/tournaments.hrl").
-include("setup.hrl").
-include("loger.hrl").

-define(CHILD(M, F, A, Type), {M, {M, F, A}, permanent, 5000, Type, [M]}).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
%    net_kernel:connect(?APPSERVER_NODE),
%    net_kernel:connect(?GAMESRVR_NODE),
%    rpc:call(?APPSERVER_NODE,nsm_srv_app,start_gproc,[]),
    application:start(gproc),
    nsm_db_sup:stop_riak(),
    application:stop(luke),
    application:stop(riak_core),
    application:stop(riak_sysmon),
    Restart = permanent,
    Shutdown = 2000,
    Type = worker,
    DChild = {user_counter, {user_counter, start_link, []}, Restart, Shutdown, Type, [user_counter]},
    gettext_server:start(),
    gettext:change_gettext_dir(code:priv_dir(nsp_srv)),
    gettext:recreate_db(),
    {ok, { {one_for_one, 5, 10}, [DChild]} }.
