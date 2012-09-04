%% -*- mode: nitrogen -*-
-module (view_members).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("nsm_srv/include/user.hrl").
-include_lib("nsm_srv/include/feed.hrl").
-include("gettext.hrl").
-include("setup.hrl").
-include("elements/records.hrl").

main() ->
    case wf:user() /= undefined of
        true  -> main_authorized();
        false -> wf:redirect_to_login(?_U("/login"))
    end.

group_info(I) ->
    view_group:group_info(I).
group_info() ->
    view_group:group_info().

main_authorized() ->
    dashboard:main_authorized().

title() -> webutils:title(?MODULE).

body() ->
    view_group:body().

content() ->
    content(1).

content(Page) ->
    Id = wf:q(id),
    friends:content(Page,?_TS("Members of $group$", [{group,group_info(name)}]),Id,{groups,list_user_with_name_in_group}).

getPageContent(Page) ->
    Id = wf:q(id),
    friends:getPageContent(Page, Id, {groups,list_user_with_name_in_group}).

get_members() ->
    [
        incoming_invites(),
        outgoing_invites(),
        []
    ].

incoming_invites() ->
    view_group:incoming_invites().

outgoing_invites() ->
%PUBLIC DEMO No invites - no block!
%    Invites = [],
%    [#panel{class="box", body=[
%       #h3{text=?_T("Invite to group"), style="letter-spacing:0px;"},
%       #list{class="list-photo", body=[ Invites ]}
%    ]}].
    [].


group_edit_form(Owner) ->
    view_group:group_edit_form(Owner).

show_editgroup_content() ->
    view_group:show_editgroup_content().

event({change_language,SL}) ->  %PUBLIC BETA this is here just to fix not working language selector bug ASAP
    webutils:event({change_language, SL});

event({page, N}) ->
    ActualNumber = if
        N < 1 -> 1;
        true  -> N
    end,
    wf:update(friends_content, getPageContent(ActualNumber));

event({subscribe,_,_,_}=Event) ->
    friends:event(Event);
event({unsubscribe,_,_,_}=Event) ->
    friends:event(Event);

event({search_friend}=Event) ->
    %friends:event(Event);
    SearchStr = wf:q("search_textbox"),
    wf:update(friends_content, getPageContent(SearchStr));

event(Other) ->
    view_group:event(Other).

finish_upload_event(X1, X2, X3, X4) ->
    dashboard:finish_upload_event(X1, X2, X3, X4).

autocomplete_enter_event(SearchTerm, SearchField) ->
    view_group:autocomplete_enter_event(SearchTerm, SearchField).

