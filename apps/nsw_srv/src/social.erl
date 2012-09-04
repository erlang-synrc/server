-module(social).

-include("gettext.hrl").
-include("setup.hrl").

%% -*- mode: nitrogen -*-

-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("nsm_srv/include/user.hrl").
-include_lib("nsm_srv/include/config.hrl").

main() -> #template { file=code:priv_dir(nsw_srv)++"/templates/700.html" }.

title() ->
    ?_T("Social").

body() ->
    [
     #hr{class="hr"},

     left_form(),
     #hr{class="hr"},

     icon_form(),

     bottom_box()
    ].



%%%%%%%%  left_form/0 %%%%%%%%%%
left_form() ->
    #container_12 { body=[#grid_7{alpha=true,
                                  body=[#panel{class="static_page_big_image social_big_image"},
                                        #panel{body=left_form_text(), style="text-align:left;"}],
                                  style="text-align:left;"},

                          #grid_5{omega=true,
                                  body=[login_fb(),
                                        #panel{class="static_tour_continue", body=#link{url=?_U("/matchmaker"), text=?_T("Continue tour >>")}}],
                                  class="sosyal_form"}]}.


login_fb() ->
    login:login_fb().

left_form_text() ->
    ["&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",
     ?_T("Social Club of Pinkies"),
     "<br><br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",
     ?_T("<ul>\n"
         "<li>Keep connected without being disturbed by anyone</li>\n"
         "<li>Create your own world</li>\n"
         "<li>Share your thoughts, files or music with friends</li>\n"
         "<li>Create your own interest groups</li>\n"
         "<li>Create your own music channels<li>\n"
         "</ul>")].


%%%%%%%%  END sosyal_form/0 %%%%%%%%%%

running_games(Type) ->
    {ok, Games} = rpc:call(?APPSERVER_NODE,table_manager,get_only_game,[Type]),
    ?_TS("Running $type$ games: $count$ ", [{count, length(Games)},
                                            {type, Type}]). %% $"


%%%%%%%%  icon_form/0 %%%%%%%%%%

icon_form() ->
    #container_12 {body=[#panel{class="icon_form",
                                body=[#link{url=?_U("/gifts"),
                                            body=#panel{class="static_page_bottom_icon icon_blue"}},
                                      #link{url=?_U("/tournaments"),
                                            body=#panel{class="static_page_bottom_icon icon_orange"}},
                                      #link{url=?_U("/matchmaker"),
                                            body=#panel{class="static_page_bottom_icon icon_cyan"}}

                                     ]}
                        ]}.


%%%%%%%%  END icon_form/0 %%%%%%%%%%



%%%%%%%%  bottom_box/0 %%%%%%%%%%

bottom_box() ->
    #container_12 {body=#grid_10{prefix=1,
                                 body=[webutils:gifts_info_box(),
                                       webutils:tournament_info_box(),
                                       webutils:matchmaker_info_box()]}}.

%%%%%%%%  END bottom_box/0 %%%%%%%%%%









event(Other) ->
    webutils:event(Other).
