-module(feedformat).

-compile(export_all).

-include("feed.hrl").

-export([format/1]).

-include_lib("nsx_config/include/log.hrl").

%PUBLIC BETA we have to turn &amp; into & and %27 to '
replace(String, Dirt, Icecream) ->
    Pos = string:str(String, Dirt),
    case Pos of
        0 ->
            String;
        _ ->
            string:left(String, Pos-1) ++ Icecream ++ replace(string:right(String, length(String) - length(Dirt) - Pos + 1), Dirt, Icecream)
    end.

split(String, Separator) ->
    Pos = string:str(String, Separator),
    case Pos of
        0 ->
            [String];
        _ ->
            [string:left(String, Pos-1)] ++ split(string:right(String, length(String) - length(Separator) - Pos + 1), Separator)
    end.

replace_out_tags(String, Dirt, Icecream) ->
    OpenTagPairs = split(String, ">"), % bla bla bla <a href='#'
    string:join([   
        begin
            OutIn = split(OTPair, "<"),
            case length(OutIn) of
                0 ->
                    "";
                1 -> 
                    OTPair;
                2 ->
                    NewOutIn = [replace(hd(OutIn), Dirt, Icecream)] ++ [hd(tl(OutIn))],
                    string:join(NewOutIn, "<");
                _ ->
                    throw(wtf_error)
            end
        end
         || OTPair <- OpenTagPairs
    ], ">").




registered_handlers() ->
    [format_oembed].

-spec format(#entry{}) -> #entry{}.
format(Entry) ->
    %PUBLIC BETA This is a dirty hack. He should keep both formated and unformated descriptions in #entry
    case string:str(Entry#entry.description, "<a href=\"") of
        0 ->
            %PUBLIC BETA 
            Entry1_5 = Entry#entry{description = replace(Entry#entry.description, "%", "pRcNt")},
            Entry2 = Entry1_5#entry{description = replace(Entry1_5#entry.description, "&amp;", "&")},
            Entry3 = lists:foldl(fun(H, #entry{description=Text} = E) ->
                            Indexes = H:search(Text),
                            format_all(H, E, Indexes)
                        end, Entry2, registered_handlers()),
            Entry4 = Entry3#entry{description = replace_out_tags(Entry3#entry.description, "&", "&amp;")},
            Entry4#entry{description = replace(Entry4#entry.description, "pRcNt", "%")};
        _ ->
            Entry   % it has been formated earlier
    end.

format_all(Handler, Entry, Indexes) ->
    format_all0(Handler, Entry, Indexes, 0).

format_all0(_Handler, Entry, [], _) ->
    Entry;

format_all0(Handler, #entry{description=T}=E, [Thing | Rest], Delta) ->
    Element = get_by_index(Thing, T, Delta),
    %% io:fwrite("Element: ~p~nDelta: ~p~n", [Element, Delta]),
    {NewElement, ModEntry} = Handler:format(Element, E),
    {FormattedText, NewDelta} = replace_thing(T, Thing, NewElement, Delta),
    NewEntry = ModEntry#entry{description = FormattedText},
    format_all0(Handler, NewEntry, Rest, NewDelta).

replace_thing(T, {Start, Stop}, El, Del) ->
    Before = string:sub_string(T, 1, Start-1+Del),
    After = string:sub_string(T, Stop+1+Del),
    %% io:fwrite("Before: ~p~nNew: ~s~nAfter: ~p~n~n~n", [Before, El,After]),
    NewText = lists:flatten(lists:concat([Before, El, After])),

    Delta = length(El) - (Stop-Start)+Del-1,
    {NewText, Delta}.

get_by_index({Start, Stop}, T, Delta) ->
    string:sub_string(T, Start+Delta, Stop+Delta).
    

