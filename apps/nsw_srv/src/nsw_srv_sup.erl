-module(nsw_srv_sup).
-behaviour(supervisor).
-compile(export_all).
-include_lib("nsm_db/include/tournaments.hrl").
-include("setup.hrl").
-include("loger.hrl").

-define(CHILD(M, F, A, Type), {M, {M, F, A}, permanent, 5000, Type, [M]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).



init([]) ->
    net_kernel:connect(?APPSERVER_NODE),
    net_kernel:connect(?GAMESRVR_NODE),

    rpc:call(?GAMESRVR_NODE,nsg_srv_app,stop_gproc,[]),
    application:stop(gproc),

    rpc:call(?APPSERVER_NODE,nsm_srv_app,start_gproc,[]),
    rpc:call(?GAMESRVR_NODE,nsg_srv_app,start_gproc,[]),
    application:start(gproc),

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,
    DChild = {user_counter, {user_counter, start_link, []}, Restart, Shutdown, Type, [user_counter]},

    gettext_server:start(),
    gettext:change_gettext_dir(code:priv_dir(nsw_srv)),
    gettext:recreate_db(),

%    LuckyChild = {nsw_srv_lucky_sup,
%                  {nsw_srv_lucky_sup, start_link, []},
%                  permanent, 2000, supervisor, [nsw_srv_lucky_sup]},

    {ok, { {one_for_one, 5, 10}, [DChild]} }.
