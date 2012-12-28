%% -*- mode: nitrogen -*-
-module (element_view_system_entry).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("nsm_db/include/user.hrl").
-include_lib("nsm_db/include/table.hrl").
-include_lib("nsm_db/include/feed.hrl").
-include("records.hrl").
-include("gettext.hrl").
-include("setup.hrl").

%% Move the following line to records.hrl:

reflect() -> record_info(fields, view_system_entry).

render_element(#view_system_entry{type={system, type1}, entry=E}) ->
    #notice{type=message, position=right, title=?_T("Kakaranet system message"), body=E#entry.description};

render_element(#view_system_entry{type={system, type2}, entry=E}) ->
    #notice{type=message, position=left, title=?_T("Kakaranet system message"), body=E#entry.description};

render_element(#view_system_entry{type={system, type3}, entry=E}) ->
    #notice{type=message, position=left, title=E#entry.description};

render_element(#view_system_entry{type={system, type4}, entry=E}) ->
    #notice{type=error, is_short=false, body=E#entry.description};

render_element(#view_system_entry{type={system, type5}, entry=E}) ->
    #notice{type=success, is_short=false, body=E#entry.description};

render_element(#view_system_entry{type={system, type6}, entry=E}) ->
    #notice{type=info, is_short=false, body=E#entry.description};

render_element(#view_system_entry{type={system, type7}, entry=E}) ->
    #notice{type=error, is_short=true, body=E#entry.description};

render_element(#view_system_entry{type={system, type8}, entry=E}) ->
    #notice{type=success, is_short=true, body=E#entry.description};

render_element(#view_system_entry{type={system, type9}, entry=E}) ->
    #notice{type=info, is_short=true, body=E#entry.description};

render_element(#view_system_entry{type={system, _}, entry=E}) ->
    #notice{type=message, position=top, title=?_T("Kakaranet system message"), body=get_description(E#entry.description, E)}.


get_description(#game_table{owner=Username,name=Tablename,game_type=Gametype,id=TId,sets=Sets,rounds=Rounds}=T, E) ->
    LocalTime = calendar:now_to_local_time(E#entry.created_time),
    Time = site_utils:local_time_to_text(LocalTime),
    T = E#entry.description,
    Info = webutils:table_info(table_manager:game_table_to_settings(T#game_table{users=[]})),

    Url = lists:concat([?_U("/view-table"), "?id=", T#game_table.id]),
    Script = webutils:new_window_js(Url),
    Action = #event{type=click,
        actions=#script{script=Script}},
    Desc = #p{body=?_TS("Our player $username$, has created '$tablename$' for "
        "$gametype$ game. Game specs: ", [
            {username, Username},
            {tablename, Tablename},
            {gametype, Gametype}])
        },
    {ok, TableActive} = table_manager:is_active(TId),
    ViewJoin = case TableActive of
        true -> site_utils:table_per_user_point(wf:user(), Sets, Rounds);
        false -> false
    end,

    Class = case TableActive of
        true -> view_system_entry_new_table;
        false -> view_system_entry_new_table_no_active
    end,
    [#panel{class=Class, body=[
        #grid_7{style="text-align: left; margin: 0px;", omega=true, body=[
            #h3{text=?_T("New game")},

            #grid_clear{},
            #panel{style="margin-left: 15px;" ,body=[Desc, Info], class="entry_description"},
            #panel{style="float:right;", body=[
                #button{text=?_T("Take your seat"), actions=Action, show_if=ViewJoin}
                ]},
            #grid_clear{},
            #label {text=Time, class="entry_time"}

            %% comments_element(E#entry.entry_id, Comments, Anchor)
        ]}]
    }];
get_description(Description, _) -> io_lib:format("<p>~s</p>",[Description]).

render_element_old(#view_system_entry{type={system, new_table}, entry=E}) ->
    LocalTime = calendar:now_to_local_time(E#entry.created_time),
    Time = site_utils:local_time_to_text(LocalTime),
    T = E#entry.description,

    Username = T#game_table.owner,
    Tablename = T#game_table.name,
    Gametype = T#game_table.game_type,
    TId = T#game_table.id,

    Sets = T#game_table.sets,
    Rounds = T#game_table.rounds,



    Info = webutils:table_info(table_manager:game_table_to_settings(T#game_table{users=[]})),

    Url = lists:concat([?_U("/view-table"), "?id=", T#game_table.id]),
    Script = webutils:new_window_js(Url),
    Action = #event{type=click,
        actions=#script{script=Script}},


    Desc = #p{body=?_TS("Our player $username$, has created '$tablename$' for "
        "$gametype$ game. Game specs: ", [
            {username, Username},
            {tablename, Tablename},
            {gametype, Gametype}])
        },
    {ok, TableActive} = table_manager:is_active(TId),
    ViewJoin = case TableActive of
        true -> site_utils:table_per_user_point(wf:user(), Sets, Rounds);
        false -> false
    end,

    Class = case TableActive of
        true -> view_system_entry_new_table;
        false -> view_system_entry_new_table_no_active
    end,

    [#panel{class=Class, body=[
        #grid_7{style="text-align: left; margin: 0px;", omega=true, body=[
            #h3{text=?_T("New game")},

            #grid_clear{},
            #panel{style="margin-left: 15px;" ,body=[Desc, Info], class="entry_description"},
            #panel{style="float:right;", body=[
                #button{text=?_T("Take your seat"), actions=Action, show_if=ViewJoin}
                ]},
            #grid_clear{},
            #label {text=Time, class="entry_time"}

            %% comments_element(E#entry.entry_id, Comments, Anchor)
        ]},

        #grid_clear{},
        #hr{class="entry_hr"}]
    }].

