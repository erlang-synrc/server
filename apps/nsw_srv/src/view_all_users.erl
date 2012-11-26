%% -*- mode: nitrogen -*-
-module (view_all_users).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("nsm_db/include/user.hrl").
-include_lib("nsm_db/include/feed.hrl").
-include("gettext.hrl").
-include("setup.hrl").
-include("elements/records.hrl").


-define(FRIENDSOURCE, "kakaranet").

main() -> dashboard:main().

title() -> webutils:title(?MODULE).

body() ->
    #template{file=code:priv_dir(nsw_srv)++"/templates/view-group.html"}.

content() ->
    content(1).

content(Page) ->
    friends:content(Page,?_T("All users of Kakaranet"), ?FRIENDSOURCE, {friends, list_group_members_paged}).

getPageContent(Page) ->
    friends:getPageContent(Page, ?FRIENDSOURCE, {friends, list_group_members_paged}).

group_info() ->
    {ok, Info} = nsm_groups:get_group(?FRIENDSOURCE),
    Ava = webutils:get_group_avatar(Info#group.username, "big"),
    Description = #span{id=group_info_description, style="font-size:11pt;", text=Info#group.description},
    MembersCount = nsm_groups:group_members_count(Info#group.username),

    #panel{class="box user-info", body=[
        #h3{id=group_info_name, style="letter-spacing:0px;", text=Info#group.name},
        Description,
        #br{},
        #br{},
        #panel{class=img, body=#image{image=Ava}},
        #panel{class="all-user-box-text", body=[
            ?_T("There is") ++ " " ++ integer_to_list(MembersCount) ++ " " ++ ?_T("people here!")
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
