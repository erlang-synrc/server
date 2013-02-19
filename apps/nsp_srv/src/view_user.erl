%% -*- mode: nitrogen -*-
-module (view_user).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("nsm_db/include/user.hrl").
-include_lib("nsm_db/include/feed.hrl").

-include("setup.hrl").
-include("common.hrl").

main() ->
  case wf:user() of
    undefined -> wf:redirect_to_login(?_U("/login"));
    _User -> main_authorized()
  end.

main_authorized() ->
  UserName = wf:q(id),
  case catch nsm_users:get_user(UserName) of
    {ok, UserInfo} ->
      wf:state(user, UserInfo),
      wf:state(feed_owner, {user, UserName}),
      dashboard:main();
    Reason ->
      ?ERROR("unable to get user info: User=~p, Reason=~p", [UserName, Reason]),
      wf:redirect(?_U("/404"))
  end.

title() -> webutils:title(?MODULE).

body() ->
  User = wf:state(user),
  #panel{class="page-content page-canvas", style="overflow:auto;margin-top:20px;", body=[
    "<section id=\"content\">", dashboard:feed(User#user.username), "</section>",
    #panel{class="aside", body=[
      #panel{id=aside,body=[
        user_info(),
        webutils:get_friends(User),
        webutils:get_groups(User)
      ]}
    ]}
  ]}.

user_info() ->
    Info = wf:state(user),
    DOB = case Info#user.age of
        undefined ->
            "not entered";
        {undefined, undefined, undefined} ->
            "not entered";
        {_,_,_} ->
            site_utils:date_to_text(Info#user.age)
    end,
    Ava = webutils:get_user_avatar(Info#user.username, "big"),

    Who = Info#user.username,
    SUId = wf:temp_id(),
    SubUnsubItem = case wf:user() of
        undefined -> [];
        User ->
            case nsm_users:is_user_subscr(User, Who) of
                true   -> #listitem{id=SUId, body=#link{url="javascript:void(0)",
                                text=?_T("Unsubscribe"), title=?_T("You can stop seeing this users posts in your feed"), 
                                    actions=#event { type=click, postback={unsubscribe, User, Who, SUId} } }};
                false -> #listitem{id=SUId, body=#link{url="javascript:void(0)",
                                text=?_T("Subscribe"), title=?_T("You can start seeing this users posts in your feed"),
                                    actions=#event { type=click, postback={subscribe, User, Who, SUId} } }}
            end
    end,
    DirectMessageItem = #listitem{body=#link{url=io_lib:format("/dashboard/filter/direct/tu/~s",[Who]), 
        text=?_T("Send direct message"), title=?_T("You can send a message only this user will read")}},

    % fixing "undefined" fields
    UserName = case Info#user.name of
        undefined ->
            ?_T("not entered");
        _ ->
            site_utils:decode_letters( Info#user.name )
    end,
    UserSurname = case Info#user.surname of
        undefined ->
            ?_T("not entered");
        _ ->
            site_utils:decode_letters( Info#user.surname )
    end,
    UserSex = case utils:convert_if(Info#user.sex, list) of
        undefined ->
            ?_T("not entered");
        _ ->
            utils:convert_if(Info#user.sex, list)
    end,

    EntriesCount  = feed:get_entries_count(Info#user.username),
    CommentsCount = feed:get_comments_count(Info#user.username),
    LikesCount    = feed:get_user_likes_count(Info#user.username),

    #panel{class="box user-info", body=[
        #h3{text=Info#user.username},

        case nsm_accounts:user_paid(Info#user.username) of
            true -> #panel{class=paid_user_avatar_img, body=#image{image=Ava}};
            _ -> #panel{class=img, body=#image{image=Ava}}
        end,

        #panel{body=[
	        #list{class=user_info, style="margin-left:0px;", body=[
                #listitem{body=[?_T("Name")++": ",#span{text=UserName}]},
                #listitem{body=[?_T("Surname")++": ",#span{text=UserSurname}]},
                #listitem{body=[?_T("Date of birth")++": ",#span{text=DOB}]},
                #listitem{body=[?_T("Gender")++": ",#span{text=UserSex}]},
                #listitem{body=["<br />"]},

                #listitem{body=[?_T("Subscriptions")++": ",#span{text=integer_to_list(length(nsm_users:list_subscr(Info#user.username))) }]},
                #listitem{body=[?_T("Subscribers")++": ",#span{text=integer_to_list(length(nsm_users:list_subscr_me(Info#user.username))) }]},
                #listitem{body=[?_T("Entries")++": ",#span{text=integer_to_list(EntriesCount) }]},
                #listitem{body=[?_T("Comments")++": ",#span{text=integer_to_list(CommentsCount) }]},
                #listitem{body=[?_T("Likes")++": ",#span{text=integer_to_list(LikesCount) }]}
            ]}
        ]},
        #panel{style="margin-left:33px;", body=[
            DirectMessageItem,
            SubUnsubItem,
            case nsm_acl:check_access(wf:user(), {feature, admin}) of
	            allow -> 
                    [
                        #br{},
                        #br{},
                        #span{text=?_T("Admin")++":"},
                        #br{},
                        case nsm_affiliates:is_existing_affiliate(Info#user.username) of
                            true -> ?_T("This user is an affiliate");
                            false -> #link{text=?_T("Make this user affiliate"), postback={make_affiliate, Info#user.username}}
                        end
                    ];
	            _ -> []
            end,
            "<br />"
        ]}
   ]}.

view_user(Users) ->
    Source = [ #panel{style="text-align: left;", body = site_utils:user_vcard(UId)}
               || #subscription{whom = UId} <- Users ],
    lists:flatten(Source).

api_event(Name, Tag, Args)->
  webutils:api_event(Name, Tag, Args).

event({subscribe,_,_,_}=Event) ->
    friends:event(Event);
event({unsubscribe,_,_,_}=Event) ->
    friends:event(Event);

event({make_affiliate, User}) ->
    nsx_msg:notify(["affiliates", "user", User, "create_affiliate"], {}),
    wf:redirect("");

event(Other) -> dashboard:event(Other).

textboxlist_event(SearchTerm)-> dashboard:textboxlist_event(SearchTerm).

more_entries(Entry) -> dashboard:more_entries(Entry).

finish_upload_event(X1, X2, X3, X4) ->
    dashboard:finish_upload_event(X1, X2, X3, X4).

inplace_textbox_event(Tag, Value, FeedEntry) ->
    dashboard:inplace_textbox_event(Tag, Value, FeedEntry).
