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
    GId = wf:q(id),
    UserList = [UId || UId <- nsm_groups:list_group_members(GId)],
    wf:state(userlist, lists:sort(UserList)),
    wf:state(userlist_count, length(UserList)),
    {ok, Group} = nsm_groups:get_group(GId),
    wf:state(curgroup, Group#group.name),
    wall:main().

title() -> webutils:title(?MODULE).

body() ->
  GId = wf:q(id),
  UId = wf:user(),
  {_, Group} = nsm_groups:get_group(GId),
  #panel{class="page-content page-canvas", style="margin-top:20px;height:770px;",
   body=[    "<section id=\"content\">",
      case Group of
        notfound -> [];
        _ -> [
          case nsm_groups:user_has_access(UId, GId) of
            true -> content(1);
            false -> []
          end
        ]
      end,
    "</section>",
    #panel{class="aside", body=[
      #panel{id=aside,body=[
        wall:group_info()
      ]}
    ]}
  ]}.


content(Page) ->
    friends:content(Page,?_TS("Members of $group$", [{group, wf:state(curgroup)}])).

getPageContent(Page) ->
    friends:getPageContent(Page).

get_members() ->
    [
        incoming_requests(),
        outgoing_invites(),
        []
    ].

incoming_requests() ->
    wall:incoming_requests().

outgoing_invites() ->
    [].


group_edit_form(Owner) ->
    wall:group_edit_form(Owner).

show_editgroup_content() ->
    wall:show_editgroup_content().

api_event(Name, Tag, Args)->
  webutils:api_event(Name, Tag, Args).

event(filter_by_nick) ->
    Filter = wf:q(nick_filter),
    GId = wf:q(id),
    UserList = [UId || UId <- nsm_groups:list_group_members(GId)],
    FilteredUserList = [UId || UId <- UserList, string:str(UId, Filter) /= 0],
    wf:state(userlist, lists:sort(FilteredUserList)),
    wf:state(userlist_count, length(FilteredUserList)),
    event({page, 1});

event({page, N}) ->
    friends:event({page, N});

event(go_to_page) ->
    friends:event(go_to_page);

event({subscribe,_,_,_}=Event) ->
    friends:event(Event);
event({unsubscribe,_,_,_}=Event) ->
    friends:event(Event);

event(Other) ->
    wall:event(Other).

finish_upload_event(X1, X2, X3, X4) ->
    wall:finish_upload_event(X1, X2, X3, X4).

autocomplete_enter_event(SearchTerm, SearchField) ->
    wall:autocomplete_enter_event(SearchTerm, SearchField).

