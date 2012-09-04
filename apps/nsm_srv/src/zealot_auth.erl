%%%-------------------------------------------------------------------
%%% File    : zealot_auth.erl
%%% Author  : peterflis <pawel_flis@silversoft.pl>
%%% Description : Implements basic user management, including user
%%% creation, login, login with facebook, generating tokens for
%%% gameserver and getting users' info.
%%%
%%% Created : 22 Feb 2011 by peterflis <pawel_flis@silversoft.pl>
%%%-------------------------------------------------------------------
-module(zealot_auth).

-behaviour(gen_server).

%% API
-export([start_link/0,
         generate_token/1,
         get_user_info/1,
         register/1,
         update_gamestate/2,
		 login/1,
		 login_fb/1]).

-include_lib("config.hrl").
-include_lib("user.hrl").
-include_lib("alog/include/alog.hrl").

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-record(state, {}).


%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

generate_token(U) ->
    gen_server:call(?SERVER, {generate_token, U}).

get_user_info(UserId) ->
    gen_server:call(?SERVER, {get_user_info, UserId}).

register(User) ->
    gen_server:call(?SERVER, {register, User}).

update_gamestate(GaId, State) ->
    gen_server:cast(?SERVER, {update_gamestate, GaId, State}).

login(Credentials) ->
    gen_server:call(?SERVER, {login, Credentials}).

login_fb(Credentials) ->
	gen_server:call(?SERVER, {login_fb, Credentials}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------

handle_call({register, #user{} = RegisterData}, _From, State) ->
    Reply = case users:register(RegisterData) of
		{ok, register} -> {ok, register};
		{error, Error} -> {error, Error}
	    end,
    {reply, Reply, State};

handle_call({login_fb, Data}, _From, State) ->
	FbId =  proplists:get_value(username, Data),
    UserName =
        case users:get_user({facebook, FbId}) of
            {ok, User} ->
                case User#user.status of
                    ok ->
                        users:login_posthook(User#user.username),
                        {ok, User#user.username};
                    banned ->
                        {error, banned};
                    _ ->
                        {error, unknown}
                end;
            Other ->
                Other
        end,
    {reply, UserName, State};

handle_call({login, Data}, _From, State) ->
    UserName = proplists:get_value(username, Data),
    Password = proplists:get_value(password, Data),
    HashedPassword = utils:sha(Password),

    Reply =
        case users:get_user(UserName) of
            {ok, #user{password = HashedPassword, username = U} = User } ->
                case User#user.status of
                    ok ->
                        users:login_posthook(UserName),
                        {ok, U};
                    not_verified ->
                        {error, not_verified};
                    banned ->
                        {error, banned};
                    _ ->
                        {error, unknown}
                end;
            {ok, _} ->
                {error, incorrect_password};
            {error, not_found} ->
                {error, user_not_found};
            {error, notfound} ->
                {error, user_not_found}
        end,
    {reply, Reply, State};

handle_call({get_user_info, UserId}, _From, State) ->
    Reply =
        case users:get_user(UserId) of
            {ok, User} ->
                UserInfo = build_user_info(User),
                {ok, UserInfo};
            {error, not_found} ->
                {error, user_not_found};
            {error, notfound} ->
                {error, user_not_found}
        end,
    {reply, Reply, State};

handle_call(get_all_user, _From, State) ->
    {reply, {ok, zealot_db:all(user)}, State};

handle_call({generate_token, User}, _From, State) ->
    Token = generate_token0(),
    io:fwrite("saving token: ~p, ~p to ~p machine~n", [User, Token, ?GAMESERVER_NODE]),
    Res = rpc:call(?GAMESERVER_NODE, auth_server, store_token, [Token, User]),
    io:fwrite("with result :~p~n", [Res]),
    {reply, Token, State};

handle_call(Request, _From, State) ->
    Reply = {error, unrecognized_call, Request},
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({update_gamestate, GaId, NewGameState}, State) ->
    io:fwrite("GaId: ~p~nState:~p~n~n", [GaId, NewGameState]),
    case NewGameState of
	finished ->
	    table_manager:delete_table({gameId, GaId});
	_ ->
	    ok
    end,
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

generate_token0() ->
    T0 = crypto:rand_bytes(100),
    T = base64:encode(T0),
    T.


build_user_info(#user{username = UserName,
                      name = Name,
                      surname = Surname,
                      age = Age,
                      sex = Sex} = User) ->
    Skill = case rpc:call(?GAMESERVER_NODE, score_db, get_skill, [UserName]) of
		{badrpc, SkError} ->
		    alog:error("Error in RPC: ~w", [SkError]),
		    "undefined";
		{error, not_found} -> "undefined";
		{error, notfound} -> "undefined";
                {ok, S} -> S
            end,

    Score = case rpc:call(?GAMESERVER_NODE,score_db,get_game_points,[okey,UserName] ) of
		{badrpc, SError} ->
		    alog:error("Error in RPC: ~w", [SError]),
		    "undefined";
		{error, not_found} -> "undefined";
		{error, notfound} -> "undefined";
                {ok, SC} -> proplists:get_value(game_points, SC)
            end,

    #user_info{username = UserName,
               name = Name,
               surname = Surname,
               age = Age,
               avatar_url = rpc:call(?WEBSERVER_NODE,avatar,get_avatar,[User, small]),
               sex = Sex,
               skill = Skill,
               score = Score}.

