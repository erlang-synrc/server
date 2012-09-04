%%%-------------------------------------------------------------------
%%% @author pawelflis <pawel_flis@silversoft.pl>
%%% @author Gleb Peregud <gleber.p@gmail.com>
%%% @author Fernando Benavides <fernando.benavides@inakanetworks.com>
%%% @author JLarky <jlarky@punklan.net>
%%% @copyright Paynet Internet ve Bilisim Hizmetleri A.S. All Rights Reserved.
%%% @doc
%%% Antispam logs and throttle for frequent requests (not sure where it used).
%%% @end
%%% Created :  10 May 2011 by pawelflis <pawel_flis@silversoft.pl>
%%%-------------------------------------------------------------------
-module(antispam).

-export([log/2,
         log/3]).

-include_lib("user.hrl").
-include_lib("config.hrl").

-type ip() :: {integer(), integer(), integer(), integer()}.

-spec log({ip(), atom()}, any()) -> ok.
log(Key, Log) ->
   log(Key, Log, undefined).

-spec log({ip(), atom()}, any(), string()) -> ok.
log(Key, Log, UId) ->
    Record = #prohibited{ip = Key,
                         activity = Log,
                         time = erlang:now(),
                         uid = UId},
    zealot_db:put(Record),
    check_operation(Key).



%%FIX: {feature, forget_password} is hardcoded, which is bad, maybe
%%Action should be put there?
%%FIX: bad operations should have expire time, i.e. if they are older
%%than something they should not be counted
-spec check_operation({string(), atom()}) -> ok.
check_operation({Ip, _Action} = Key) ->
    case zealot_db:select(prohibited, Key) of
        [] ->
            ok;
        Data ->
            case length(Data) of
                ?MAX_BAD_OPERATION ->
                    acl:define_access({ip, Ip}, {feature, forget_password}, deny);
                _ ->
                    ok
            end
    end.

