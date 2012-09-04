%% -*- mode: nitrogen -*-
-module (view_all_users).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("nsm_srv/include/user.hrl").
-include_lib("nsm_srv/include/feed.hrl").
-include("gettext.hrl").
-include("setup.hrl").
-include("elements/records.hrl").


-define(FRIENDSOURCE, "kakaranet").

main() ->
    case wf:user() /= undefined of
        true  -> main_authorized();
        false -> wf:redirect_to_login(?_U("/login"))
    end.

main_authorized() ->
    dashboard:main_authorized().

title() -> webutils:title(?MODULE).

body() ->
    #template{file=code:priv_dir(nsw_srv)++"/templates/view-group.html"}.

content() ->
    content(1).

content(Page) ->
    friends:content(Page,?_T("All users of Kakaranet"), ?FRIENDSOURCE, {groups,list_user_with_name_in_group}).

getPageContent(Page) ->
    friends:getPageContent(Page, ?FRIENDSOURCE, {groups,list_user_with_name_in_group}).

group_info() ->
    Info = rpc:call(?APPSERVER_NODE,groups,get_group,[?FRIENDSOURCE]),
    Ava = webutils:get_group_avatar(Info#group.username, "big"),
    Description = #span{id=group_info_description, style="font-size:11pt;", text=Info#group.description},
    MembersCount = integer_to_list(rpc:call(?APPSERVER_NODE, groups, get_members_count, [Info#group.username])),

    #panel{class="box user-info", body=[
        #h3{id=group_info_name, style="letter-spacing:0px;", text=Info#group.name},
        Description,
        #br{},
        #br{},
        #panel{class=img, body=#image{image=Ava}},
        #panel{class="all-user-box-text", body=[
            ?_T("There is") ++ " " ++ MembersCount ++ " " ++ ?_T("people here!")
        ]},
        #br{}
    ]}.

get_members() ->
    [].

event({page, N}) ->
    wf:update(friends_content, getPageContent(N));

event({subscribe,_,_,_}=Event) ->
    friends:event(Event);
event({unsubscribe,_,_,_}=Event) ->
    friends:event(Event);

event(Other) ->
    view_group:event(Other).
