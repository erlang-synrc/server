%%----------------------------------------------------------------------
%% @author Vladimir Baranov <baranoff.vladimir@gmail.com>
%% @copyright Paynet Internet ve Bilisim Hizmetleri A.S. All Rights Reserved.
%% @doc
%%    Email worker
%% @end
%%--------------------------------------------------------------------
-module(nsm_bg_worker_email).

-behaviour(nsm_bg_gen_worker).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("nsx_config/include/log.hrl").
-include_lib("nsm_db/include/membership_packages.hrl").
-include_lib("nsm_db/include/config.hrl").
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([]).

%% nsm_bg_gen_worker callbacks
-export([init/1, handle_notice/3, get_opts/1, handle_info/2]).

-record(state, {smtp_options = []}).

%% ====================================================================
%% External functions
%% ====================================================================


%% ====================================================================
%% Server functions
%% ====================================================================

init([]) ->
    %% store smtp options in worker state
    SMTPOptions = read_smtp_options(),
    {ok, #state{smtp_options = SMTPOptions}}.



handle_notice(["email", "send"], {Subject, Content, To},
              #state{smtp_options = Options} = State) ->
    ?INFO("email(~p): send email to ~s. Subject:~s",
          [self(), To, Subject]),
    mail:send(Subject, Content, To, Options),
    {noreply, State};

%% send multipart
handle_notice(["email", "send"], {Subject, TextContent, HTMLContent, To},
              #state{smtp_options = Options} = State) ->
    ?INFO("email(~p): send multipart email to ~s. Subject:~s",
          [self(), To, Subject]),
    mail:send_multipart(Subject, TextContent, HTMLContent, To, Options),
    {noreply, State};

handle_notice(["purchase", User, PurchaseId, PaymentType, PurchaseState],
              #membership_purchase{} = _Purchase,
              #state{smtp_options = Options} = State) ->
    case nsm_db:get(config, "purchase/notifications/email") of
        [] ->
            ?INFO("email(~p): purchase notifications recipients list is empty",
                 [self()]);

        {ok, #config{value = SendTo}} ->
            Message = io_lib:format(
                        "Purchase ~s state has been changed.\nNew state: ~s.\n"
                        "Payment type: ~s\nUser: ~s",
                        [PurchaseId, PurchaseState, PaymentType, User]),
            Subject = io_lib:format("Purchase ~s state has been changed: ~s",
                            [PurchaseId, PurchaseState]),
            mail:send(lists:flatten(Subject), lists:flatten(Message), SendTo, Options);

        {error, Reason} ->
            ?ERROR("email(~p): unable to get purchase notification recipients: ~p",
                  [self(), Reason])
    end,
    {noreply, State};

handle_notice(Route, Payload, State) ->
    ?INFO("email(~p): notice received. Route: ~p, Payload: ~p",
          [self(), Route, Payload]),
    {noreply, State}.

handle_info(Info, State) ->
    ?INFO("reanimator(~p): handle info: ~p",
          [self(), Info]),
    {noreply, State}.


get_opts(_State) ->
    [{routes, [[email, '*'],
               [purchase, '*', '*', '*', '*']]},
     {queue, <<"notice.email.1">>},
     {queue_options, [auto_delete]}].

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
%% FIXME: read options from db
read_smtp_options() -> [].
%% read_smtp_options() ->
%%     {ok, User}    = nsm_db:get(config, "smtp/user",     "noreply@kakaranet.com"),
%%     {ok, Pass}    = nsm_db:get(config, "smtp/password", "unknown"),
%%     {ok, Server}  = nsm_db:get(config, "smtp/host",     "smtp.kakaranet.com"),
%%     {ok, Port}    = nsm_db:get(config, "smtp/port",     587),
%%     {ok, WithSSL} = nsm_db:get(config, "smtp/with_ssl", false),
%%
%%     [{user, User},
%%      {password, Pass},
%%      {server, Server},
%%      {port, Port},
%%      {with_ssl, WithSSL}].
