%% -*- mode: nitrogen -*-
-module (friends).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("nsm_db/include/user.hrl").
-include_lib("nsm_db/include/feed.hrl").
-include_lib("nsx_config/include/log.hrl"). 
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
    case catch nsm_users:get_user(UserName) of
        {ok, UserInfo} ->
            wf:state(user, UserInfo),
            wf:state(feed_owner, {user, UserName}),
            UserList = [FrId || #subs{whom=FrId} <- nsm_users:list_subscr(UserName)],
            wf:state(userlist, lists:sort(UserList)),
            wf:state(userlist_count, length(UserList)),
            wall:main();
        Reason ->
            ?ERROR("unable to get user info: User=~p, Reason=~p", [UserName, Reason]),
            wf:redirect("404")
    end.

title() -> webutils:title(?MODULE).

body() -> [
  #panel{class="page-content", body=webutils:quick_nav()},
  #panel{class="page-content page-canvas", style="overflow:auto;", body=[
    #panel{class=aside, body=[
      user_info(),
      wall:get_friends(),
      get_groups()
    ]},
    #panel{class=friendlist, body=content(1)}
  ]}].

content(Page) -> content(Page, ?_T("FRIENDS")).
content(Page, Title) ->
    [
        #panel{class="top-space", body=[
            #h1{text=Title},
            #textbox{id=nick_filter, style="width:84px; height:19px; border-radius:3px; border: 1px solid rgb(201, 201, 201); float:left;"},
            #link{postback=filter_by_nick, class="set-table-name", text=?_T("Filter by nick"), style="float:left; font-weight:bold; padding-left:8px; padding-top:5px;"}
        ]},
        #panel{id="friends_content", body=getPageContent(Page)}
    ].

get_groups() ->
    User = wf:state(user),
    webutils:get_groups(User).

user_info() ->
    UserOrNot = wf:q('of'),
    case UserOrNot of
        undefined ->
            wall:get_ribbon_menu();
        _ ->
            view_user:user_info()
    end.

getPageContent(Page) ->
    [
        get_users_view(get_subscribed_users(Page), Page)
    ].


get_users_view(UsersView, PageNumber) ->
    NextButton = case PageNumber < (wf:state(userlist_count) div ?FRIENDSPERPAGE + 1) of
        true -> #listitem{body=#link{text=">", postback={page, PageNumber + 1}}};
        false -> #listitem{body=#link{text=">", url="javascript:void(0)", class="inactive"}}
    end,
    PrevButton = case PageNumber of
        I when is_integer(I),I>1 -> #listitem{body=#link{text="<", postback={page, PageNumber - 1}}};
        _                        -> #listitem{body=#link{text="<", url="javascript:void(0)", class="inactive"}}
    end,
    [
        #list{class="user-list", body=[UsersView]},
        case wf:state(userlist_count) > ?FRIENDSPERPAGE of
            true ->
                #panel{class="paging-2", body=[
                    #panel{class="center", body=[
                        #list{body=[
                            PrevButton,
                            NextButton,
                            #listitem{class="inactive", style="width:84px;", body=
                                #link{text="Go to page", postback=go_to_page}
                            },
                            #listitem{class="inactive", style="width:36px;", body=
                                #textbox{id=pager_textbox, style="width:36px; height:19px; text-align:right; border-radius:3px; border: 1px solid rgb(201, 201, 201);", text=integer_to_list(PageNumber)}
                            },
                            #listitem{class="inactive", style="padding-top:2px;", body=
                                "&nbsp;/&nbsp;" ++ integer_to_list(wf:state(userlist_count) div ?FRIENDSPERPAGE + 1)
                            }
                        ]}
                    ]}
                ]};
            false -> 
                []
        end
    ].

get_subscribed_users()     -> get_subscribed_users(1).
get_subscribed_users(Page) ->
    case lists:sublist(wf:state(userlist), (Page-1)*?FRIENDSPERPAGE + 1, ?FRIENDSPERPAGE) of
        [] ->
            #label{text=?_T("Nothing on this page"), style="margin-left:2em;"};
        Sub ->
            [friends_view(X) || X <- Sub]
    end.

user_short_description(UId) ->
    case UId of
        undefined -> [];
        UserName ->
             case nsm_users:get_user(UserName) of
            {ok, UInfo} ->
            #user{age=UAge, sex=USex, location=ULoc, education=UEdu} = UInfo,
            URealName = nsm_users:user_realname_user(UInfo),
            UDOB = case UAge of
                undefined ->
                    undefined;
                {undefined, undefined, undefined} ->
                    undefined;
                {_,_,_} ->
                    site_utils:date_to_text(UAge);
                _ ->
                    ""
            end,
            UOI = fun(undefined) -> undefined;
                     (Info) when is_atom(Info) -> atom_to_list(Info);
                     (Info) when is_integer(Info) -> integer_to_list(Info);
                     (Info) when is_list(Info) -> lists:flatten(Info);
                     (Info) when is_binary(Info) -> binary_to_list(Info);
                     (_) -> ""
            end,                    
            string:join([Ok || Ok <- [UOI(URealName), UOI(USex), UOI(UDOB), UOI(ULoc), UOI(UEdu)], Ok =/= undefined], ", ")
            ;
            _ -> ""
            end
    end.


friends_view(#subs{whom = Who}) -> friends_view(Who);
friends_view(#user{username = Who}) -> friends_view(Who);
friends_view({Who, _}) -> friends_view(Who);

friends_view(UId) ->
    SUId = wf:temp_id(),
    SubUnsubItem = case wf:user() of
        undefined -> [];
        User ->
            case nsm_users:is_user_subscr(User, UId) of
                true   -> #listitem{id=SUId, body=#link{url="javascript:void(0)",
                                text=?_T("Unsubscribe"), title=?_T("You can stop seeing this users posts in your feed"),
                                    actions=#event { type=click, postback={unsubscribe, User, UId, SUId} } }};
                false -> #listitem{id=SUId, body=#link{url="javascript:void(0)",
                                text=?_T("Subscribe"), title=?_T("You can start seeing this users posts in your feed"),
                                    actions=#event { type=click, postback={subscribe, User, UId, SUId} } }}
            end
    end,
    ShortDescription = user_short_description(UId),
    #listitem{class="user-item", body=[
       #link{class="entity", url=site_utils:user_link(UId), body=[
            #panel{class="img", body=#image{image=webutils:get_user_avatar(UId, "big"), style="width:96px;height:96px", class=
                case nsm_accounts:user_paid(UId) of
                    true -> "paid_user_avatar";
                    _ -> ""
                end
            }},
            #panel{class="user-name", body=UId}
       ]},
       #panel{class="tooltip-1", body=[
            #panel{class="t", body=[
                #panel{class="c", body=[
                    #panel{class="frame", body=[
                        #panel{class="img", body=[
                            #image{image=webutils:get_user_avatar(UId, "big"), style="width:96px;height:96px", class=
                                case nsm_accounts:user_paid(UId) of
                                    true -> "paid_user_avatar";
                                    _ -> ""
                                end
                            }
                        ]},
                        #panel{class="descr", body=[
                            io_lib:format("<strong class=\"user-name\">~s</strong>", [UId]), %PHASE1
                            #label{class="user-description", text=ShortDescription},
                            #br{},
                            #br{},
                            #list{class="func-list", body=[
                                #listitem{body=#link{url=io_lib:format("/wall/filter/direct/tu/~s",[UId]), text=?_T("Send direct message"), title=?_T("You can send a message only this user will read")}},
                                SubUnsubItem
                            ]}
                        ]}
                    ]}
                ]},
                #panel{class="b", body="&nbsp;"}
            ]}
       ]}
    ]}.


show_friend(#subs{whom = Who}) ->
    #panel{style="float:left;padding-left:5px",
        body=[
            #image{image=webutils:get_user_avatar(Who, "big"), class=
                case nsm_accounts:user_paid(Who) of
                    true -> "paid_user_avatar";
                    _ -> ""
                end
            }
    ]}.


event(Event) ->
    case wf:user() of
	undefined -> wf:redirect_to_login(?_U("/login"));
        User      -> inner_event(Event, User)
    end.


inner_event({page, N}, _) ->
    ActualNumber = if
        N < 1 -> 1;
        true  -> N
    end,
    wf:update(friends_content, getPageContent(ActualNumber));

inner_event(go_to_page, User) ->
    Page = list_to_integer(wf:q(pager_textbox)),
    case (Page > 0) and (Page =< wf:state(userlist_count) / ?FRIENDSPERPAGE + 1) of
        true -> inner_event({page, Page}, User);
        false -> ok
    end;

inner_event(filter_by_nick, User) ->
    Filter = wf:q(nick_filter),
    UserList = [FrId || #subs{whom=FrId} <- nsm_users:list_subscr(User)],
    FilteredUserList = [UId || UId <- UserList, string:str(UId, Filter) /= 0],
    wf:state(userlist, lists:sort(FilteredUserList)),
    wf:state(userlist_count, length(FilteredUserList)),
    event({page, 1});

inner_event({unsubscribe, _, Who, SUId}, User1) ->
    nsx_msg:notify(["subscription", "user", User1, "remove_subscribe"], {Who}),
    wf:update(SUId,
       #link{url="javascript:void(0)", text=?_T("Subscribe"), title=?_T("You can start seeing this users posts in your feed"),
            actions=#event{type=click, postback={subscribe, User1, Who, SUId}}}),
    wf:wire("blur();"),
    wf:wire("qtip_all_links();");


inner_event({subscribe, _, Who, SUId}, User1)   ->
    nsx_msg:notify(["subscription", "user", User1, "subscribe_user"], {Who}),
    wf:update(SUId,
        #link{url="javascript:void(0)", text=?_T("Unsubscribe"), title=?_T("You can stop seeing this users posts in your feed"),
            actions= #event{type=click, postback={unsubscribe, User1, Who, SUId}}}),
    wf:wire("blur();"),
    wf:wire("qtip_all_links();");


inner_event(Any, _)->
    webutils:event(Any).

