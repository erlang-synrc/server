%% -*- mode: nitrogen -*-
-module (view_members).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("nsm_db/include/user.hrl").
-include_lib("nsm_db/include/feed.hrl").
-include("gettext.hrl").
-include("setup.hrl").
-include("elements/records.hrl").

main() ->
    case wf:user() /= undefined of
        true  -> main_authorized();
        false -> wf:redirect_to_login(?_U("/login"))
    end.

main_authorized() ->
    dashboard:main_authorized().

title() -> webutils:title(?MODULE).

body() ->
    view_group:body().

group_info() ->
    view_group:group_info().

content() ->
    content(1).

content(Page) ->
    GId = wf:q(id),
    {ok, Group} = nsm_groups:get_group(GId),
    friends:content(Page,?_TS("Members of $group$", [{group,Group#group.name}]),GId,{friends, list_group_members_paged}).

getPageContent(Page) ->
    Id = wf:q(id),
    friends:getPageContent(Page, Id, {nsm_groups,list_group_members}).

get_members() ->
    [
        incoming_invites(),
        outgoing_invites(),
        []
    ].

incoming_invites() ->
    view_group:incoming_invites().

outgoing_invites() ->
    [].


group_edit_form(Owner) ->
    view_group:group_edit_form(Owner).

show_editgroup_content() ->
    view_group:show_editgroup_content().

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

event(Other) ->
    view_group:event(Other).

finish_upload_event(X1, X2, X3, X4) ->
    dashboard:finish_upload_event(X1, X2, X3, X4).

autocomplete_enter_event(SearchTerm, SearchField) ->
    view_group:autocomplete_enter_event(SearchTerm, SearchField).

