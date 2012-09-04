%%% @author Pawel Flis <pawel_flis@silversoft.pl>
%%% @copyright (C) 2011, Gleb Peregud
%%% @doc
%%%
%%% This module implements a login page with login, login with
%%% facebook and registration features.
%%%
%%% Logging in with Facebook is implemented using Graph API using
%%% erlfb module. After user clicks at "Login with facebook" page he
%%% get's redirected to page where he authorizes application. When
%%% authorization is completed "code" is supplied to this page which
%%% is used to fetch access token. Access token is used to fetch
%%% user's details and depending if user exists locally it appropriate
%%% record will be created or not. After this user is logged in using
%%% internal login.
%%%
%%% @end
%%% Created : 13 July 2011 by Pawel Flis <pawel_flis@silversoft.pl>

-module(newindex).

-include("gettext.hrl").

-compile(export_all).

-export([main/0]).

-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("nsm_srv/include/user.hrl").
-include_lib("nsm_srv/include/config.hrl").

-include("elements/records.hrl").


main() ->
    %% case wf:q(facebook) of
    %%     "true" ->
    %%         case wf:q(code) /= undefined of
    %%             true ->
    %%                 facebook_login();
    %%             false ->
    %%                 main_notauthorized()
    %%         end;
    %%     _ ->
    %%         case wf:q(verify) /= undefined of
    %%             true ->
    %%                 verification_account();
    %%             false ->
    %%                 main_notauthorized()
    %%         end
    %% end.
    main_notauthorized().

main_notauthorized() -> #template { file=code:priv_dir(nsw_srv)++"/templates/bare.html" }.

title() ->
    ?_T("Kakaranet.com").

body() ->
    [#container_12{class=bubble_background, body=main_content()},
     slide_bare(),
     #grid_clear{},
     #panel{class=game_background,
            body=#container_12{body=games()}},
     #grid_clear{},
     #container_12{class=bottom_left, body=[]},
     #grid_clear{}
     %% #login_lightbox{},
     %% #register{},
     %% #show_lightbox{}
     %% #register{}
    ].


main_content() ->
    #panel{body=[#easy_slider{pages=[#easy_slider_page{body=slides(one)},
                                     #easy_slider_page{body=slides(two)},
                                     #easy_slider_page{body=slides(three)}
                                    ]}
                 ]}.

slides(one) ->
    [#grid_5{alpha=true, class=main_man},
     #grid_7{omega=true, class=[main_text],
             body=[#label{class=[sosyal_text], text=?_T("Lay back and relax")},
                   #label{class=[uzatin_text], text=?_T("In our social complex!")},
                   #label{class=gotham_rounded, text=?_T("• Interact with only people you've selected")},
                   #label{class=gotham_rounded, text=?_T("• Create your own gaming world. Play without disturbed by anyone.")},
                   #label{class=gotham_rounded, text=?_T("• Share your ideas, music, files about everything.")},
                   #label{class=gotham_rounded, text=?_T("• We are positive discriminators for women")},
                   #link{body="Show register",
                         actions=#event{type=click, actions=#show{target=register_lightbox}}},
                   #link{body="Show login",
                         actions=#event{type=click, actions=#show{target=login_lightbox}}},
                   #link{body="Show login", postback=blah}



                  ]}
    ];

slides(two) ->
    [slides(one),
     #h1{text="SITE 2"}];

slides(three) ->
    [slides(one),
     #h1{text="SITE 3"}].

slide_bare() ->
    #panel{class=slide_bare,
           body=[#panel{class=slide_bare_content,
                        body="links"}
                ]}.

games() ->
    #grid_12{alpha=true, omega=true, body=#image{image="/images/new/default/games-fake.png"}}.

event(blah) ->
    wf:wire(#show_lightbox{action=register});





event(Other) ->
    webutils:event(Other).
