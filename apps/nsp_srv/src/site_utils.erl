-module(site_utils).

-compile(export_all).

-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("nsm_db/include/user.hrl").
-include_lib("nsm_db/include/feed.hrl").
-include("elements/records.hrl").
-include("common.hrl").
-include("setup.hrl").

-spec reset_language() -> 'ok'.
reset_language() ->
    erase(gettext_language).

-spec detect_language() -> string().
detect_language() ->
    case get(gettext_language) of
        undefined ->
            L = [ 
                  fun() -> wf:session(lang) end,
                  fun() -> wf:cookie("lang") end,
                  fun() -> "tr" end ],
            V = try_f(L),
            case catch wf:session(lang, V) of
                {'EXIT', _} ->
                    ok;
                _ ->
                    put(gettext_language, V)
            end,
            V;
        V -> V
    end.

-spec try_f([fun()]) -> any().
try_f([F|L]) ->
    try F() of
        undefined ->
            try_f(L);
        X ->
            X
    catch
        _:_ ->
            try_f(L)
    end.


-spec join(list(), any()) -> list().
join([], _Sep) ->
    [];
join([X], _Sep) ->
    [X];
join([X | L], Sep) ->
    [X, Sep | join(L, Sep)].


-spec as_str(any()) -> string().
as_str(S) ->
    lists:flatten(io_lib:format("~s", [utils:convert_if(S, list)])).

-spec as_str_list([any()]) -> string() | undefined.
as_str_list(S) when S == undefined ->
    undefined;

as_str_list(S) ->
    site_utils:join([ lists:flatten(io_lib:format("~s", [utils:convert_if(X, list)])) || X <- S ], ", ").


x(_X) ->
    ok.

show_if(X) ->
    case X of
        undefined -> false;
        false -> false;
        [] -> false;
        _ -> true
    end.

-spec game_speed_to_string(atom()) -> string().
game_speed_to_string(fast) -> ?_T("Fast");
game_speed_to_string(normal) -> ?_T("Normal");
game_speed_to_string(slow) -> ?_T("Slow");
game_speed_to_string(_) -> ?INFO("Uknown Game Speed!!!"), ?_T("Unknown").


-spec game_mode_to_string(atom()) -> string().
game_mode_to_string(color) -> ?_T("Color");
game_mode_to_string(evenodd) -> ?_T("Even/Odd");
game_mode_to_string(standard) -> ?_T("Standard");
game_mode_to_string(paired) -> ?_T("Pair");
game_mode_to_string(countdown) -> ?_T("Countdown from 10");
game_mode_to_string(kakaratavla) -> ?_T("Kakara Tavla");
game_mode_to_string(_) -> ?INFO("Uknown Game Mode!!!"), ?_T("Unknown").

-spec game_to_string(atom()) -> string().
game_to_string(game_okey) -> ?_T("okey");
game_to_string(game_tavla) -> ?_T("tavla");
game_to_string(game_batak) -> ?_T("batak");
game_to_string(game_sorbi) -> ?_T("sorbi");
game_to_string(game_king) -> ?_T("king");
game_to_string(_) -> ?_T("unsupported").


group_link(Gid)     when is_list(Gid) ->      lists:concat([?_U("/wall"), "/type/group/id/", Gid]).
user_link(Username) when is_list(Username) -> lists:concat([?_U("/wall"), "/type/user/id/",  Username]);
user_link(Username) -> "".



user_vcard(Username) when is_list(Username) ->
    {ok, User} = nsm_users:get_user(Username),
    Avatar = avatar:get_avatar(User, small),
    #link{body=[#image{image=Avatar, class =
        case nsm_accounts:user_paid(Username) of
            true -> "paid_user_avatar";
            _ -> ""
        end
    }, #span{text=username_upper(Username)}],
	  url=site_utils:user_link(Username)}.


username_upper() ->
    username_upper(wf:user()).

username_upper([H|String]) ->
    string:to_upper([H]) ++ String.


date_to_text(Date) ->
    io_lib:fwrite("~4..0b/~2..0b/~2..0b", tuple_to_list(Date)).

time_to_text(Time) ->
    io_lib:fwrite("~2..0b:~2..0b:~2..0b", tuple_to_list(Time)).

local_time_to_text({D, H}) ->
    Date = date_to_text(D),
    Hour = time_to_text(H),
    io_lib:fwrite("~s  ~s", [Hour, Date]).

feed_time_tuple({D, H}) ->
    local_time_to_text({D,H}).

-spec check_date_correct({integer()|string(),integer()|string(),integer()|string()}) -> {ok|error, {integer()|string(),integer()|string(),integer()|string()}}.
check_date_correct({SYear,SMonth,SDay} = OrigDate) ->
    case catch begin
		   Day = wf:to_integer(SDay),
		   Month = wf:to_integer(SMonth),
		   Year = wf:to_integer(SYear),
		   {calendar:datetime_to_gregorian_seconds({{Year, Month, Day},{0,0,0}}), {Year, Month, Day}}
	       end of
	{N,Date} when is_number(N) ->
	    {ok, Date};
	_ ->
	    {error, OrigDate}
    end.

element_value(Element) ->
    case wf:q(Element) of
        "undefined" -> undefined;
        Other -> Other
    end.

element_value(Element, integer) ->
    case wf:q(Element) of
        "undefined" -> undefined;
        Other -> wf:to_integer(Other)
    end.

linkify_name(robot, _) ->
    ?_T("robot");
linkify_name(Name, normal) ->
    #link{text=Name, url=site_utils:user_link(Name)};
linkify_name(Name, parent) ->
    URI = site_utils:user_link(Name),
    #link{text=Name, actions=#event{type=click,
                                    actions=#script{script=open_in_parent(URI)}}}.

-spec textify_settings(list()) -> string().
%% @doc Transforms settings into human readable string. Used for
%% describing table options.
textify_settings(Settings) ->
    Set = lists:keysort(1, Settings),
    TS = [ textify_setting_(Option) || Option <- Set ],
    S = lists:filter(fun(E) -> E=/="" end, TS),
    string:join(S, ", ").

textify_setting_({game, Game}) ->
    site_utils:game_to_string(Game);
textify_setting_({game_mode, Mode}) ->
    site_utils:game_mode_to_string(Mode);
textify_setting_({speed, Speed}) ->
    site_utils:game_speed_to_string(Speed);
textify_setting_({rounds, Rounds}) ->
    wf:f("rounds: ~b", [Rounds]);
textify_setting_(_Option) ->
    "". %% ignore

-spec simple_pickle(term()) -> string().
%% @doc Simplified version of wf:pickle used to create string usable as class
%% or id of element.
simple_pickle(Data) ->
    Byte = term_to_binary(Data, [compressed]),
    Encoded = m_b64_e(base64:encode(Byte), <<>>),
    binary_to_list(Encoded).

% modified_base64_encode/1
%       - Replace '+' and '/' with '-' and '_', respectively.
% - Strip '='.
m_b64_e(<<>>, Acc) -> Acc;
m_b64_e(<<$+, Rest/binary>>, Acc) -> m_b64_e(Rest, <<Acc/binary, $->>);
m_b64_e(<<$/, Rest/binary>>, Acc) -> m_b64_e(Rest, <<Acc/binary, $_>>);
m_b64_e(<<$=, Rest/binary>>, Acc) -> m_b64_e(Rest, Acc);
m_b64_e(<<H,  Rest/binary>>, Acc) -> m_b64_e(Rest, <<Acc/binary, H>>).

open_in_parent(URI) ->
    io_lib:fwrite("opener.location = \"~s\"", [URI]).


invite_message(InviterUser, InviteLink, InviterNote, NewUser) ->
    invite_message(InviterUser, InviteLink, InviterNote, NewUser, site_utils:detect_language()).

-spec invite_message(string(), string(), string(), string(), string()) -> {string(), string()}.
invite_message(InviterUser, InviteLink, InviterNote, NewUser, Lang) ->
    Subject = ?TXT2("Kakaranet Membership", Lang),
    Content = ?STXT2("Dear $invited_user_name$,\n\n"
		     "Our user $inviter_name$, invited you to Kakaranet Social Game Zone.\n"
		     "You can sign in by clicking the link above:\n\n"
		     "$invitation_code_link$\n\n"
		     "$inviter_note$\n\n"
		     "Please note that we are in closed private beta.\n"
		     "Most of functionality of the site is unfinished and heavily testing.\n\n"
		     "Note: This email is sending by one of our users. We hate SPAM and never let it happens.\n"
		     "If you don't know this user, someone probably mis-typed your email address. You can ignore this message, and we apologize for the inconvenience. \n\n\n"
		     "Thank you for join in,\n"
		     "Kakaranet Team\n",
		     [{invitation_code_link, InviteLink},
		      {inviter_name, InviterUser},
		      {invited_user_name, NewUser},
		      {inviter_note, InviterNote}
		     ], Lang),
    {Subject, Content}.


-spec generate_code() -> string().
generate_code() ->
    <<A:(16*8), _/binary>> = crypto:rand_bytes(16),
    lists:sublist(lists:flatten(io_lib:format("~25.36.0b", [A])), 1, 32).

-spec nl_to_br(iolist()) -> iolist().
nl_to_br(Text) ->
    lists:map(fun(E) ->
        case E of
            $\n -> "<br/>\n";
            _ -> E
        end
    end, Text).

-spec traverse_criteria_elements(list(), tuple() | atom()) -> {ok, term()} | {error, term()}.
traverse_criteria_elements(Html, Tag) ->
    case (catch traverse_list(Html, Tag)) of
	{ok, Element} ->
	    {ok, Element};
	error ->
	    error
    end.

%% @private
traverse_list([], _) ->
    error;
traverse_list([H|Html], Tag) when is_tuple(H) ->
    case element(1, H) of
	slider ->
	    case H#slider.id of
		Tag -> throw({ok, H});
		_   -> ok
	    end;
	draggable_new ->
	    case H#draggable_new.tag of
		Tag -> throw({ok, H});
		_   -> ok
	    end;
	checkbox when size(Tag) == 2 ->
	    {CBTag, _} = Tag,
	    case H#checkbox.postback of
		{cb_change, _Body, _Id, CBTag} ->
		    throw({ok, H});
		_ -> ok
	    end;
	droppable ->
	    traverse_list(H#droppable.body, Tag);
	panel ->
	    traverse_list(H#panel.body, Tag);
	table ->
	    traverse_list(H#table.rows, Tag);
	tablerow ->
	    traverse_list(H#tablerow.cells, Tag);
	tablecell ->
	    traverse_list(H#tablecell.body, Tag);
	I when I==span orelse I==br -> %% implicit ignore
	    traverse_list(Html, Tag);
	_NonImplicitIgnore ->
	    ok
    end,
    traverse_list(Html, Tag);
traverse_list(Element, Tag) when is_tuple(Element) ->
    traverse_list([Element], Tag);
traverse_list(_String, _Tag) -> %% in case when we have text in body.
    error.


-spec postback_to_js_string(any) -> string().
%% @doc Creates js-sctring that will fire postback. Can be used like
%% that: String = postback_to_js_string(test_event), wf:wire(wf:f("setTimeout(function() {~s;}, 1000);", String)).
postback_to_js_string(Postback) ->
    postback_to_js_string(?MODULE, Postback).
postback_to_js_string(Module, Postback) ->
    %% cool way to create JavaScript code which does postback call
    Anchor = wf_context:anchor(), ValidationGroup = wf_context:event_validation_group(),
    _Postback_js = wf_event:generate_postback_script(Postback, Anchor, ValidationGroup, Module, undefined).

-spec get_usort_user(list(), list()) -> list().
get_usort_user(Groups, Users) ->
    GroupsUsers =
        lists:foldl(fun(Group, Acc) ->
                            UsersInGroup = nsm_groups:list_group_members(Group),
                            lists:umerge(lists:usort(UsersInGroup), Acc)
                    end, [], Groups),
    lists:umerge(lists:usort(Users), GroupsUsers).


-spec js_close_on_click(string()) -> record(event).
js_close_on_click(ID) ->
    #event{type=click, actions="objs('"++ID++"').hide();"}.

create_url_invite(Code) ->
    lists:concat([?_U("/invite"), "/", Code]).

base64_encode_to_url(String) ->
    Utf8 = unicode:characters_to_binary(String),
    wf:url_encode( base64:encode_to_string(Utf8) ).

base64_decode_from_url(EncodedUrl) ->
    base64:decode_to_string( wf:url_decode(EncodedUrl) ).

long_integer_to_list(I) when is_integer(I) ->
    long_integer_to_list(integer_to_list(I));
long_integer_to_list(S) ->
    LS = length(S),
    case LS > 3 of 
        true -> long_integer_to_list(lists:sublist(S, 1, LS-3)) ++ " " ++ lists:sublist(S, LS-2, 3);
        false -> S
    end.

decode_letters(undefined) -> "";
decode_letters("") -> "";
decode_letters(In) ->
    case is_list(hd(In)) of
        true ->
            decode_letters(hd(In));
        _ ->
            ling:replace_a_lot(In, [ 
                {[286], "Ğ"},    % 'unicode'
                {[287], "ğ"},
                {[304], "İ"},
                {[305], "ı"},
                {[350], "Ş"},
                {[351], "ş"},

                {[246], "ö"},    % both latin-5 and 'unicode'
                {[214], "Ö"},
                {[252], "ü"},
                {[220], "Ü"},

                {[231], "ç"},    % latin-5
                {[199], "Ç"},
                {[240], "ğ"},
                {[208], "Ğ"},

                {[253], "ı"},
                {[221], "İ"},
                {[254], "ş"},
                {[222], "Ş"}
            ])
    end.

decode_amp(In) ->
    ling:replace(In, "&amp;", "&").

decode_entities(In) ->
    ling:replace_a_lot(In, [
        {"&lt;", "<"},
        {"&gt;", ">"},
        {"&quot;", "'"},

        {": medium", ": small"} % this is a dirty hack for making description fit into a page. 
                                % It should be eradicated with decent design.
    ]).

assume_eq(In) ->
    ling:replace_a_lot(In, [
        {"style'", "style='"},
        {"align'", "align='"},
        {"src'", "src='"}
    ]).

decode_html(In) ->
    ling:replace_a_lot(decode_letters(assume_eq(decode_entities(decode_amp(decode_amp(In))))), [
        {"line-height:", "ignore:"},    % particular fixes for gifts html
        {"<--", "<!--"},
        {"/images/cuzdan", "http://www.enilginc.com/images/cuzdan"}
    ]).

clean_stuff([]) -> [];
clean_stuff([H | T]) -> 
    case ((H >= $a) and (H =< $z)) or ((H >= $A) and (H =< $Z)) or ((H >= $0) and (H =< $9)) or (H == $_) of
        true -> [H] ++ clean_stuff(T);
        false -> clean_stuff(T)
    end.

validate_group_id(GId) ->
    clean_stuff(ling:replace_a_lot(GId, [
        {"Ğ", "G"},
        {"ğ", "g"},
        {"İ", "I"},
        {"ı", "i"},
        {"Ş", "S"},
        {"ş", "s"},
        {"ö", "o"},
        {"Ö", "O"},
        {"ü", "u"},
        {"Ü", "U"},
        {"ç", "c"},
        {"Ç", "C"},
        {" ", "_"}])).


