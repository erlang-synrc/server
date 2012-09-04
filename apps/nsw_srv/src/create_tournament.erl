-module(create_tournament).
-compile(export_all).

-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("nsm_srv/include/user.hrl").
-include_lib("nsm_srv/include/config.hrl").
-include_lib("nsm_srv/include/feed.hrl").
-include_lib("nsm_srv/include/table.hrl").

-include("elements/records.hrl").
-include("loger.hrl").
-include("setup.hrl").
-include("gettext.hrl").

main() ->
    case wf:user() /= undefined of
        true  -> main_authorized();
        false -> wf:redirect_to_login("/")
    end.

main_authorized() ->
    webutils:add_script("/nitrogen/js/input-type-file.js"),
    #template { file=code:priv_dir(nsw_srv)++"/templates/bare.html" }.

title() ->
    ?_T("Create Tournament").

body() ->
    [#panel{class="list-top-photo-h", body=webutils:get_hemen_nav(tournament)},
     #section{class="create-area", body=#section{class="create-block",
            body=[
                #panel{id="welcome_text", body=?_T("Please select game type.")},
                inn_body()
    ]}}].

inn_body() ->
    wf:wire("objs('settings').css({overflow:'hidden'}).height(0);"),
    TournOption = [{"Pointing", ?_T("Pointing")},
                   {"Election", ?_T("Election")}],
    Tourn = webutils:list_to_options(TournOption),
    #panel{id="settings", class="settings", body=[
        "<form>",
        #panel{class=grid_7, body=[
            #label{text=?_T("Tournament name")},
            #panel{class="text wide", body=#textbox{id=tournament_name}}
        ]},
        #panel{class=grid_7, body=[
            #label{text=?_T("Tournament description")},
            #panel{class="text wide",body=#textbox{id=tournament_description}}
        ]},
        #panel{class=grid_7, body=[
            #label{text=?_T("Tournament image")},
            #panel{class="text wide", body=#textbox{id=tournament_image}}
        ]},

        #panel{class=col, body=[
            #label{text=?_T("Tournament type")},
            #panel{class="sel", body=#dropdown{class="cs-1 selectArea", options=Tourn, id=tournament_type}}
        ]},
        #panel{class=col, body=[
            #label{text=?_T("No. of quota")},
            #panel{class=text,body=#textbox{id=quota}}
        ]},
        #panel{class=col, body=[
            #label{text=?_T("Something")++":"},
            #radiogroup{id=radio, body=[
                #radio{id=first, text=?_T("Only first"), value="1", checked=true}, #br{},
                #radio{id=second, text=?_T("First and second"), value="2"}
            ]}
        ]},
        #panel{class=col, body=[
            #label{text=?_T("Gift range")},
            "gift range"
        ]},
        #panel{class=col, body=[
            #label{text=?_T("No. of players")},
            #panel{class=text,body=#textbox{id=players_num}}
        ]},
        #panel{class=col, body=[
            #label{text=?_T("Start date")},
            #panel{class=text,body=#textbox{id=date}}
        ]},
        "</form>",
        #grid_clear{},
        #table{class="gifts", rows=[
            #tablerow{cells=[
                #tablecell{body=[#panel{class="gift-cell", body=[
                    #panel{class="gift-cell-2",body=[
                        #image{image="/images/tournament/tourn_gift.png"},
                        #panel{body="Pierre Cardini Erkek Kol Saatl"}]}]}]}
                || _ <- lists:seq(1,5)]}
          || _ <- lists:seq(1,3)]},
        #panel{class="btn-holder", body=[
            #panel{class="ar", body=[
                #panel{ class="publish-fb", body=[#checkbox{id=checkbox_fb, text="Publish to Facebook", checked=true}]},
                #panel{body=[#button{class="btn-reset", text="Cancel", postback=tournament_cancel},
                             #button{class="btn-submit", text="Create", postback=tournament_create}]}
            ]}
        ]},
        #grid_clear{}
    ]}.

event(Event) ->
    case wf:user() of
        undefined ->
            wf:redirect_to_login("/");
        _User ->
            u_event(Event)
    end.

u_event({game, Game}) ->
    wf:state(game_type, Game),
    wf:wire(#hide{target=welcome_text}),
    wf:wire("objs('settings').height('auto');");

u_event(tournament_create) ->
    create_tournament();
u_event(Other) ->
    webutils:event(Other).




create_tournament() ->
    User = wf:user(),
    TournamentName = wf:q(tournament_name),
    Description =  wf:q(tournament_description),
    _Reply = rpc:call(?APPSERVER_NODE, tournaments, create,
                      [User, TournamentName, Description]).
