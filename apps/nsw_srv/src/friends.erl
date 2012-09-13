%% -*- mode: nitrogen -*-
-module (friends).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("nsm_db/include/user.hrl").
-include_lib("nsm_db/include/feed.hrl").
-include_lib("alog/include/alog.hrl"). 
-include("elements/records.hrl").
-include("gettext.hrl").
-include("setup.hrl").

-define(FRIENDSPERPAGE, 20).

main() ->
    case wf:user() /= undefined of
        true  -> main_authorized();
        false -> wf:redirect_to_login(?_U("/login"))
    end.

main_authorized() ->
    UserOrNot = wf:q('of'),
    case UserOrNot of
        undefined ->
            UserName = wf:user();
        MrX ->
            UserName = MrX
    end,
    case catch rpc:call(?APPSERVER_NODE,nsm_users,get_user,[UserName]) of
        {ok, UserInfo} ->
            wf:state(user, UserInfo),
            wf:state(feed_owner, {user, UserName}),
            dashboard:main_authorized();
        Reason ->
            ?ERROR("unable to get user info: User=~p, Reason=~p", [UserName, Reason]),
            wf:redirect("404")
    end,
    webutils:js_for_main_authorized_game_stats_menu(),
    #template { file=code:priv_dir(nsw_srv)++"/templates/bare.html" }.

title() -> webutils:title(?MODULE).

body() ->
    #template{file=code:priv_dir(nsw_srv)++"/templates/view-user.html"}.

content()     -> content(1).
content(Page) -> content(Page, ?_T("FRIENDS"), wf:user(), {nsm_users, list_subscription}).
content(Page, Title, UId, ModFun) ->
    wf:wire(wf:f("objs('search_textbox')"
             ".bind('keyup keydown change', function()"
             "{var $this=objs('search_textbox');var l = parseInt($this.attr('value').length);"
             " if(l > 0){objs('sendsearch').css('background','url(/images/grn-shr-btn.png) no-repeat');objs('sendsearch').css('cursor','pointer');}"
             " if(l <= 0){objs('sendsearch').css('background','url(/images/gre_shr_btn.png) no-repeat');objs('sendsearch').css('cursor','default');}"
             " })"
             ".bind('keypress', function(e)"
             "{var code = e.keyCode || e.which;"
             " if (code == 13) { if (!e.shiftKey) {objs('sendsearch').trigger('click'); return false;}}" %% send postback
             " if (code != 116 && code != 46 && code > 40 || code == 32) return $(this).trigger('change').attr('value').length < ~b" %% deny only text keys
             "})",
             [100])),
    [
        #panel{class="top-space", body=[
%PHASE1            #panel{class="search-cell", body=[
%                #form{body=[
%                    #panel{class="text", body=#textbox{id="search_textbox"}},
%                    #button{class="btn-submit-mkh", id="sendsearch", postback={search_friend}, text=?_T("Search")}
%                ]}
%            ]},
            #h1{text=Title}
        ]},
        #panel{id="friends_content", body=getPageContent(Page, UId, ModFun)}
    ].


get_friends() ->
    User = wf:state(user),
    webutils:get_friends(User).

get_groups() ->
    User = wf:state(user),
    webutils:get_groups(User).

user_info() ->
    UserOrNot = wf:q('of'),
    case UserOrNot of
        undefined ->
            webutils:get_ribbon_menu();
        _ ->
            view_user:user_info()
    end.

getPageContent(Page) ->
    getPageContent(Page, wf:user(), {nsm_users, list_subscription}).

getPageContent(Page, UId, ModFun) when is_number(Page) ->
    [
        friends_search_form(),
        get_users_view(get_subscribed_users(Page, UId, ModFun), true, Page)
    ];
getPageContent(Page, UId, ModFun) when is_list(Page) ->
    [
        friends_search_form(),
        get_users_view(get_subscribed_users(Page, UId, ModFun), false, 0)
    ].

friends_search_form() ->
%PHASE1 no friends search yet
%    #panel{class="sub-space", body=[
%        #panel{class="sort-form", body=[
%            #form{body=[
%                #label{text=?_T("View")},
%                #panel{class="sel", body=[
%                    #dropdown{id="lb01", class="cs-3", options=[
%                        #option{text=?_T("All subscribtions")}
%                    ]}
%                ]}
%            ]}
%        ]},
%        #list{class="menu-1", body=[
%            #listitem{style="background:none", body=#link{url="#", text=?_T("New friend list")}},
%            #listitem{body=#link{url="#", text=?_T("Find/invite friends")}}
%        ]}
%    ]}.
    [].

get_users_view(UsersView, false, _)         -> #list{class="user-list", body=[UsersView]};
get_users_view(UsersView, true, PageNumber) ->
    NextButton = if
        length(UsersView) < ?FRIENDSPERPAGE %PUBLIC BETA this is wrong, but will do
             -> #listitem{body=#link{text=">", url="javascript:void(0)", class="inactive"}};
        true -> #listitem{body=#link{text=">", postback={page, PageNumber + 1}}}
    end,
    PrevButton = case PageNumber of
        I when is_integer(I),I>1 -> #listitem{body=#link{text="<", postback={page, PageNumber - 1}}};
        _                        -> #listitem{body=#link{text="<", url="javascript:void(0)", class="inactive"}}
    end,
    [
        #list{class="user-list", body=[UsersView]},
        #panel{class="paging-2", body=[
            #panel{class="center", body=[
                #list{body=[
                    PrevButton,
                    #listitem{body=#link{class="inactive", url="javascript:void(0)", text=io_lib:format("~b",[PageNumber])}},
                    NextButton
                ]}
            ]}
        ]}
    ].

get_subscribed_users()     -> get_subscribed_users(1).
get_subscribed_users(Page) -> 
    get_subscribed_users(Page, wf:user(), {nsm_users, list_subscription}).

get_subscribed_users(Page,UId, {Mod,Fun}) when is_number(Page) ->
    AltUser = wf:q('of'),
    case AltUser of
        undefined ->
            UserId = UId;
        MrX ->
            UserId = MrX
    end,
    case rpc:call(?APPSERVER_NODE, Mod, Fun, [UserId, Page, ?FRIENDSPERPAGE]) of
        [] ->
            #label{text=?_T("Nothing on this page"), style="margin-left:2em;"};
        Sub ->
            ?PRINT(Sub),
            [friends_view(X) || X <- Sub]
    end;
get_subscribed_users(Query,UId, {Mod,Fun}) when is_list(Query) ->
    case rpc:call(?APPSERVER_NODE, Mod, Fun, [UId, Query]) of
        [] ->
            ?_T("Can't find anything matching your request");
        Sub ->
            ?PRINT(Sub),
            [friends_view(X) || X <- Sub]
    end.


friends_view(#subscription{who = Who, whom = WhoName}) -> friends_view(Who, WhoName);
friends_view(#user{username = Who, name = Name, surname = Surname})     -> friends_view(Who, io_lib:format("~s ~s", [Name, Surname]));
friends_view({Who,WhoName}) -> friends_view(Who,WhoName);
friends_view(Who) -> friends_view(Who, Who).

user_short_description(WhoName) ->
    case WhoName of
        undefined -> [];
        UserName ->
            {ok, UInfo} = rpc:call(?APPSERVER_NODE,nsm_users, get_user, [{username, UserName}]),
            #user{name=UName, surname=ULastName, age=UAge, sex=USex, location=ULoc, education=UEdu} = UInfo,
            if
                ULastName == undefined, UName == undefined ->
                    URealName = UserName;
                ULastName == undefined ->
                    URealName = UName;
                UName == undefined ->
                    URealName = ULastName;
                true ->
                    URealName = 
                    case is_binary(UName) of
                        true -> binary_to_list(UName);
                        false -> UName
                    end ++
                    [" "] ++ 
                    case is_binary(ULastName) of
                        true -> binary_to_list(ULastName);
                        false -> ULastName
                    end
            end,
            UDOB = case UAge of
                undefined ->
                    undefined;
                {undefined, undefined, undefined} ->
                    undefined;
                {_,_,_} ->
                    site_utils:date_to_text(UAge);
                _ ->
                    ""  % for NYH on srv3
            end,

            UOI = fun(undefined) -> undefined;
                     (Info) when is_atom(Info) -> atom_to_list(Info);
                     (Info) when is_integer(Info) -> integer_to_list(Info);
                     (Info) when is_list(Info) -> lists:flatten(Info);
                     (Info) when is_binary(Info) -> binary_to_list(Info);
                     (_Info) -> ""   % for NYH on srv3
            end,                    
            string:join([Ok || Ok <- [UOI(URealName), UOI(USex), UOI(UDOB), UOI(ULoc), UOI(UEdu)], Ok =/= undefined], ", ")
    end.

friends_view(Who_, WhoName_) ->
%PUBLIC BETA WTF!? Who is the current user name and WhoName is the target user id
%                  Ok. Due to a data structure changes, we have TWO different inputs for this function
%                  Friends page gives us a WhoName as a target user id
%                  And group members gives us a structure with a full name as a WhoName
    ?INFO(" ~p ~p",[Who_, WhoName_]),
    case is_list(hd(WhoName_)) of
        true -> % we have Who as a traget user id
            WhoName = Who_;
        false -> % we have WhoName as a traget user id
            WhoName = WhoName_
    end,

    SUId = wf:temp_id(),
    SubUnsubItem = case wf:user() of
        undefined -> [];
        User ->
            case rpc:call(?APPSERVER_NODE,nsm_users, is_user_subscribed, [User, WhoName]) of
                true   -> #listitem{id=SUId, body=#link{url="javascript:void(0)",
                                text=?_T("Unsubscribe"), title=?_T("You can stop seeing this users posts in your feed"),
                                    actions=#event { type=click, postback={unsubscribe, User, WhoName, SUId} } }};
                false -> #listitem{id=SUId, body=#link{url="javascript:void(0)",
                                text=?_T("Subscribe"), title=?_T("You can start seeing this users posts in your feed"),
                                    actions=#event { type=click, postback={subscribe, User, WhoName, SUId} } }}
            end
    end,
    ShortDescription = user_short_description(WhoName),
    #listitem{class="user-item", body=[
%PUBLIC BETA We don't have this fully operational for now: url=io_lib:format("/dashboard/user/~s",[WhoName]),
       #link{class="entity", url=site_utils:user_link(WhoName), body=[
            #panel{class="img", body=#image{image=webutils:get_user_avatar(WhoName, "big"), style="width:96px;height:96px"}},
            #panel{class="user-name", body=WhoName}
       ]},
       #panel{class="tooltip-1", body=[
            #panel{class="t", body=[
                #panel{class="c", body=[
                    #panel{class="frame", body=[
                        #panel{class="img", body=[
                            #image{image=webutils:get_user_avatar(WhoName, "big"), style="width:96px;height:96px"}
                        ]},
                        #panel{class="descr", body=[
                            io_lib:format("<strong class=\"user-name\">~s</strong>", [WhoName]), %PHASE1
                            #label{class="user-description", text=ShortDescription},
                            #br{},
                            #br{},
                            #list{class="func-list", body=[
                                #listitem{body=#link{url=io_lib:format("/dashboard/filter/direct/tu/~s",[WhoName]), text=?_T("Send direct message"), title=?_T("You can send a message only this user will read")}},
%PHASE1                                #listitem{body=#link{url="#", text=?_T("Recommend friends")}},
%PHASE1                                #listitem{body=[#link{url="#", text=?_T("Personal")}, " (", #link{url="#", text=?_T("edit")}, ")"]},
                                %#listitem{body=#link{url="#", text=?_T("Unsubscribe")}}
                                SubUnsubItem
                            ]}
                        ]}
                    ]}
                ]},
                #panel{class="b", body="&nbsp;"}
            ]}
       ]}
    ]}.


split_subs([], A) -> A;
split_subs(L, A)  when length(L) =< 5 -> A ++ [L];
split_subs(L, A)  ->
    {L2, L3} = lists:split(5, L),
    split_subs(L3, A ++ [L2]).

friends_row(FL) -> #panel{body=[ show_friend(X) || X <- FL]}.

show_friend(#subscription{whom = Who}) ->
    #panel{style="float:left;padding-left:5px",
        body=[
            #image{image=webutils:get_user_avatar(Who, "big")}
    ]}.

big() ->
    [
     #panel { id="main_area", style="margin:0px;",  body=[subscribe()] }
    ].

subscribe() ->
    UId = wf:user(),
    SubList =
        case rpc:call(?APPSERVER_NODE,nsm_users, list_subscription, [UId]) of
            [] ->
                ?_T("You are not subscribed to anyone");
            Sub ->
                [?_T("You are subscribed to:"),
                 #br{},
                 view_subscribe(Sub)]
        end,
    [view_add_friend(), SubList].


view_add_friend() ->
    [
     #label{text=?_T("Search users")++":"},
     #textbox_autocomplete {tag=friend_search},
     #flash{}
    ].

create_user_lists() ->
    {ok, Users} = gen_server:call(zealot_auth, get_all_user),
    Users.


view_subscribe(SubList) ->
    Source = [ [#link{text=Who,
                      url=site_utils:user_link(Who)}, #br{}]
               || #subscription{whom = Who} <- SubList ],
    lists:flatten(Source).

event(Event) ->
    ?PRINT({"FE:", Event}),
    case wf:user() of
	undefined -> wf:redirect_to_login(?_U("/login"));
        User      -> inner_event(Event, User)
    end.

inner_event(view_sub, _) ->
    wf:update(main_area, subscribe());

inner_event({subscription, FrId}, _) ->
    UId = wf:user(),
    ok = rpc:call(?APPSERVER_NODE, nsm_users, subscribe_user, [UId, FrId]),
    wf:update(main_area, subscribe()),
    Msg = io_lib:fwrite(?_T("You have subscribed to '~s'."), [FrId]),
    wf:flash(Msg);

inner_event(search_friend, _) ->
    SearchStr = wf:q("search_textbox"),
    SearchedUsers = case rpc:call(?APPSERVER_NODE,nsm_users, search_user, [SearchStr]) of
        [] ->
            #panel{body=?_T("We could not find any users matching the search") ++ " \"" ++ SearchStr ++ "\""};
        Sub ->
            [friends_view(X) || X <- Sub]
    end,
    wf:update(friends_content, get_users_view(SearchedUsers, false, 0));

inner_event({page, N}, _) ->
    ActualNumber = if
        N < 1 -> 1;
        true  -> N
    end,
    wf:update(friends_content, getPageContent(ActualNumber));


inner_event({unsubscribe, _, Who, SUId}, User1) ->
    rpc:call(?APPSERVER_NODE, nsm_users, remove_subscribe, [User1, Who]),
    wf:update(SUId,
       #link{url="javascript:void(0)", text=?_T("Subscribe"), title=?_T("You can start seeing this users posts in your feed"),
            actions=#event{type=click, postback={subscribe, User1, Who, SUId}}}),
    wf:wire("blur();"),
    wf:wire("qtip_all_links();");


inner_event({subscribe, _, Who, SUId}, User1)   ->
    rpc:call(?APPSERVER_NODE, nsm_users, subscribe_user, [User1, Who]),
    wf:update(SUId,
        #link{url="javascript:void(0)", text=?_T("Unsubscribe"), title=?_T("You can stop seeing this users posts in your feed"),
            actions= #event{type=click, postback={unsubscribe, User1, Who, SUId}}}),
    wf:wire("blur();"),
    wf:wire("qtip_all_links();");


inner_event(Any, _)->
    webutils:event(Any).



autocomplete_enter_event(SearchTerm, friend_search) ->
    Data = create_user_lists(),
    List = [ {struct,[{id, list_to_binary(UId) }, {label, list_to_binary(UId)}, {value, list_to_binary(UId)}]} ||
               #user{username = UId} <- Data,
               string:str(string:to_lower(UId), string:to_lower(SearchTerm)) > 0 ],
    mochijson2:encode(List).

autocomplete_select_event({struct, [{<<"id">>, _ },{<<"value">>, Value}]}, friend_search) ->
    TextQuestion = io_lib:fwrite(?_T("Do you want to subscribe to '~s'?"), [Value]),
    wf:flash([#label {text=TextQuestion, style="display: inline;"},
            #button {text=?_T("Yes"), class="inputButton", postback={subscription, binary_to_list(Value)}}]),
    ok.


