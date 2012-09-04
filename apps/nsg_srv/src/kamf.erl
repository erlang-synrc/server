-module(kamf).

-define(ENCODER, amf3).
-define(L_TO_A(X),
        list_to_atom(X)).

%%FIX: is there any reason to export encode/decode?
%%looks like in all cases it is used along with "construct",
%%"reconstruct", why not merge them? Also "construct" / "reconstruct"
%%are bad names - just by reading them I can't get in what direction
%%they transform data

%% amf encoding
-export([encode/1, decode/1]).

%% tcp stream <-> kamf packet construction and reconstruction
-export([construct/1, reconstruct/1, reconstruct/2]).

%% amf encodable #object <-> erlang record of #record.name type
-export([object_to_record/1, record_to_object/1]).

-export([create_request/2, create_message/2]).
%% creating Kamf packets
-export([create_request/3, create_response/2,
         create_message/3, create_fatalerror/3]).

% %%FIX: not used outside and seems to be internal function
% -export([to_encodable/1]).

-include("classes.hrl").
-include("logging.hrl").
-include("kamf.hrl").

%%FIX: change naming of the functions to be consistent
construct(Data) when is_binary(Data) ->
    MessageLength = size(Data),
    <<?KAMF_VERSION:8/integer-big, MessageLength:16/integer-big,
     Data:MessageLength/binary>>.

reconstruct(Data) ->
    reconstruct(Data, <<>>).
reconstruct(New, Saved) ->
    reconstruct(New, Saved, []).
reconstruct(New, Saved, Acc) ->
    case <<Saved/binary, New/binary>> of
        <<?KAMF_VERSION:8/integer-big, MessageLength:16/integer-big,
         Body:MessageLength/binary, Rest/binary>> ->
            reconstruct(Rest, <<>>, [Body | Acc]);
        NS ->
            {done, lists:reverse(Acc), NS}
    end.


%%FIX: these should not be exported. They are kamf-specific. I think
%%abstract records for request/response/message/fatalerror should be
%%created and transformed into/from AMF objects in encoding/decoding
%%phase
create_request(Id, Rec) when is_tuple(Rec)->
    create_request(Id, api_utils:name(Rec), api_utils:members(Rec)).
create_request(Id, Method, Args) ->
    #'KamfRequest'{id = Id, method = Method, args = Args}.
create_response(Id, {error, Reason}) ->
    #'KamfResponse'{id = Id, success = false, result = Reason};
create_response(Id, Result) ->
    #'KamfResponse'{id = Id, success = true, result = Result}.
create_message(Id, Rec) ->
    create_message(Id, api_utils:name(Rec), api_utils:members(Rec)).
create_message(Id, EventType, Args) ->
    #'KamfMessage'{id = Id, event_type = EventType, args = Args}.
create_fatalerror(Id, Type, Reason) ->
    #'KamfFatalError'{id = Id, type = Type, reason = Reason}.





%%FIX: in ideal case this module should have two functions exported:
%% 1) erlang terms -> amf terms
%% 2) amf terms -> erlang terms
%% and no other functions
encode(Data) ->
    ?ENCODER:encode(Data).

record_to_object(Record)  ->
    Name = api_utils:name(Record),
    Members = api_utils:members(Record),
    record_to_object0(Name, Members).

record_to_object0(Name, Members) when is_list(Name) ->
    record_to_object0(list_to_binary(Name), Members);
record_to_object0(Name, Members) when is_atom(Name) ->
    record_to_object0(list_to_binary(atom_to_list(Name)), Members);
record_to_object0(Name, Members) when is_binary(Name) ->
    CheckedMembers = [ kv_to_amf(Key, Value) || {Key, Value} <- Members],
    #object{name = Name, members = CheckedMembers}.

kv_to_amf(Key,Value) when is_binary(Key) ->
    kv_to_amf(binary_to_atom(Key, latin1), Value);
kv_to_amf(Key,Value) when is_atom(Key) ->
    case string:rstr(atom_to_list(Key), "_datetime") of
        0 ->
            {list_to_binary(atom_to_list(Key)), transform_to_amf_term(Value)};
        _ ->
            {list_to_binary(atom_to_list(Key)), {date, float(Value), 0}}
    end.

transform_to_amf_term(A) when A == true -> A;
transform_to_amf_term(A) when A == false -> A;
transform_to_amf_term(A) when A == null -> A;
transform_to_amf_term(A) when A == undefined -> A;
transform_to_amf_term(A) when is_atom(A) ->
    list_to_binary(atom_to_list(A));

transform_to_amf_term(T) when is_tuple(T) ->
    record_to_object(T);
transform_to_amf_term([{A, B} | Rest]) when is_atom(A) ->
    to_ecma_array([{A, B} | Rest], []);

transform_to_amf_term(L) when is_list(L) ->
    to_strict_list(L);

%% skip otherwise, it will encode unchanged
transform_to_amf_term(T) ->
    T.

to_ecma_array([{A, B} | Rest], Acc) when is_atom(A) ->
    to_ecma_array(Rest, [ kv_to_amf(A, B) | Acc]);
to_ecma_array([], Acc) ->
    lists:reverse(Acc).

to_strict_list(L) ->
    [ transform_to_amf_term(X) || X <- L ].






decode(Data)->
    ?ENCODER:decode(Data).

object_to_record(#object{name = Bin, members = Members0} = _Obj) ->
    try
        Tag = ?L_TO_A(binary_to_list(Bin)),
        Members = lists:map(fun({Key, Value}) ->
                                    amf_to_kv(Key, Value)
                            end, Members0),
        api_utils:to_known_record(Tag, Members)
    catch
        A:B ->
            ?PP("object_to_record(~p) failed with msg: ~p", [_Obj, {A, B}]),
            erlang:error(not_recognized)
    end.

transform_from_amf_term(Obj) when is_record(Obj, object)->
    object_to_record(Obj);
transform_from_amf_term([{Key, _Value} | _Tail] = List) when is_binary(Key) ->
    from_ecma_array(List, []);
transform_from_amf_term(L) when is_list(L) ->
    from_strict_list(L);
transform_from_amf_term(T) ->
    T.

from_ecma_array([{A, B} | Rest], Acc) when is_binary(A) ->
    from_ecma_array(Rest, [amf_to_kv(A, B) | Acc]);
from_ecma_array([], Acc) ->
    lists:reverse(Acc).

from_strict_list(L) ->
    [ transform_from_amf_term(X) || X <- L ].


amf_to_kv(Key, Value) when is_binary(Key) ->
    amf_to_kv(binary_to_atom(Key, latin1), Value);
amf_to_kv(Key, {date, Value, TZ}) when is_atom(Key) ->
    true = (0 /= string:rstr(atom_to_list(Key), "_datetime")),
    0 = TZ,
    {Key, trunc(Value)};
amf_to_kv(Key, Value) when is_atom(Key) ->
    {Key, transform_from_amf_term(Value)}.
