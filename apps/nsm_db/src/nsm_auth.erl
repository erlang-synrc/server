-module(nsm_auth).
-compile(export_all).
-include("config.hrl").
-include("user.hrl").
-include_lib("nsx_config/include/log.hrl").

register(#user{} = RegisterData) ->
    case nsm_users:register(RegisterData) of
                 {ok, register} -> {ok, register};
                 {error, Error} -> {error, Error}
            end.

login_fb(Data) ->
    FbId =  proplists:get_value(username, Data),
    UserName =
        case nsm_users:get_user({facebook, FbId}) of
            {ok, User} ->
                case User#user.status of
                    ok ->
                        nsm_users:init_mq_for_user(User#user.username),
                        nsm_users:login_posthook(User#user.username),
                        {ok, User#user.username};
                    banned ->
                        {error, banned};
                    _ ->
                        {error, unknown}
                end;
            Other ->
                Other
        end,
    UserName.

login(Data) ->
    UserName = proplists:get_value(username, Data),
    Password = proplists:get_value(password, Data),
    HashedPassword = utils:sha(Password),

    Reply =
        case nsm_users:get_user(UserName) of
            {ok, #user{password = HashedPassword, username = U} = User } ->
                case User#user.status of
                    ok ->
                        nsm_users:init_mq_for_user(UserName),
                        nsm_users:login_posthook(UserName),
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
    Reply.

get_user_info(UserId) ->
    Reply =
        case nsm_users:get_user(UserId) of
            {ok, User} ->
                UserInfo = build_user_info(User),
                {ok, UserInfo};
            {error, not_found} ->
                {error, user_not_found};
            {error, notfound} ->
                {error, user_not_found}
        end,
    Reply.

get_all_user() -> {ok,nsm_db:all(user)}.

generate_token(User) ->
    Token = generate_token0(),
    ?INFO("saving token: ~p, ~p to ~p machine~n", [User, Token, ?GAMESRVR_NODE]),
    Res = rpc:call(?GAMESRVR_NODE, auth_server, store_token, [Token, User]),
    ?INFO("with result :~p~n", [Res]),
    Token.

update_gamestate(GaId, NewGameState) ->
    ?INFO("GaId: ~p~nState:~p~n~n", [GaId, NewGameState]),
    case NewGameState of
         finished -> table_manager:delete_table({gameId, GaId});
         _ -> ok
    end.

generate_token0() -> T0 = crypto:rand_bytes(100), T = base64:encode(T0), T.

build_user_info(#user{username = UserName,
                      name = Name,
                      surname = Surname,
                      age = Age,
                      sex = Sex} = User) ->
    #user_info{username = UserName,
               name = Name,
               surname = Surname,
               age = Age,
               avatar_url = get_avatar(User, small),
               sex = Sex,
               skill = 0,
               score = 0}.

get_avatar(#user{avatar = Avatar}, Size) ->
    get_avatar(Avatar, Size);
get_avatar(Avatar, Size) ->
    case Avatar of
        #avatar{big = Big} when Size =:= big -> Big;
        #avatar{small = Small} when Size =:= small -> Small;
        #avatar{tiny = Tiny} when Size =:= tiny -> Tiny;
        _ -> case Size of
                 big -> "/images/no_avatar_big.jpg";
                 small -> "/images/no_avatar_small.jpg";
                 tiny -> "/images/no_avatar_tiny.jpg"
             end
    end.
