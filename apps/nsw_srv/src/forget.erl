-module(forget).

-include_lib("setup.hrl").
-include_lib("nsm_db/include/user.hrl").
-include_lib("nsm_db/include/config.hrl").


-export([init_forget/1,
         check_token/1]).


-spec init_forget(#user{}) -> ok.
init_forget(User) ->
    UId = User#user.username,
    Mail = User#user.email,
    Token = create_token(),
    {Subject, Content} = create_message(UId, Token),
    Time = erlang:now(),

    Record = #forget_password{token = Token,
                              uid = UId,
                              create = Time},

%    ok = rpc:call(?APSERVER_NODE,nsm_db,put,[Record]),
    nsx_msg:notify(["db", "user", UId, "put"], Record),
    nsx_msg:notify_email(Subject, Content, Mail).


-spec check_token(string()) -> {'error', atom()} | {ok, string()}.
check_token(Token) ->
    Time = calendar:datetime_to_gregorian_seconds(erlang:localtime()),

    case nsm_db:get(forget_password, Token) of
        {error, notfound} ->
            {error, bad_token};
        {ok, Data} ->
            case utils:now_to_seconds(Data#forget_password.create) of
                Create when Time-Create >= ?FORGET_TOKEN_EXPIRED ->

                    {error, token_expired};
                _ ->
                    {ok, Data#forget_password.uid}
            end
    end.

-spec create_token() -> string().
create_token() ->
%    T0 = crypto:rand_bytes(20),
%    binary_to_list(base64:encode(T0)).
    [hd(erlang:integer_to_list(Nibble, 16)) || << Nibble:4 >> <= crypto:rand_bytes(20)].

-spec create_message(string() | record(user), string()) -> {string(), iolist()}.
create_message(#user{username = UId}, Url) ->
    create_message(UId, Url);
create_message(UId, Token) ->
    Subject = "Your forget password - Kakaranet.com",
    Text = "Hello ~s.\nTo change your password, click on the link:\n~s",

    %%FIX: use/create module to generate URI
    Url = lists:concat(["/login/forget/token/", mochiweb_util:quote_plus(Token)]),
    Content = io_lib:fwrite(Text, [UId, Url]),

    {Subject, Content}.





