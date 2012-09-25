-module (profile).
-compile(export_all).

-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("nsm_db/include/user.hrl").
-include_lib("nsm_db/include/tournaments.hrl").
-include_lib("nsm_db/include/accounts.hrl").
-include_lib("nsm_db/include/scoring.hrl").
-include_lib("nsm_db/include/membership_packages.hrl").
-include_lib("elements/records.hrl").

-include("common.hrl").
-include("setup.hrl").

-define(ORDERS_PER_PAGE, 10).

main() ->
    webutils:add_script("/nitrogen/js/form.js"),
    webutils:add_script("/nitrogen/js/input-type-file.js"),
    webutils:add_script("/nitrogen/blockui.js"),
    #template { file=code:priv_dir(nsw_srv)++"/templates/bare.html" }.

title() -> webutils:title(?MODULE).

body() ->
    case wf:user() of
	undefined ->
	    wf:redirect_to_login("/");
	_User ->
	    main_authorized()
    end.

main_authorized() ->
    Col = #panel{class=col, body=section_body(get_current_section())},
    PreLinks = [
	     {profile,	?_U("/profile"), 	?_T("Profile")},
	     {gifts,	?_U("/profile/gifts"), 	?_T("Gifts")},
	     {account,	?_U("/profile/account"),?_T("Account")},
	     {stats,	?_U("/profile/stats"), 	?_T("Stats")},
	     {invite,	?_U("/profile/invite"), ?_T("Invite")},
	     {tournament,?_U("/profile/tournament"), ?_T("New Tournament")},
%	     {tournament2,?_U("/create-tournament"), ?_T("// Tournament")},
	     {user_tournaments,?_U("/profile/user-tournaments"), ?_T("Tournaments")}
	    ],
    Links = case rpc:call(?APPSERVER_NODE,nsm_affiliates, is_existing_affiliate, [wf:user()]) of 
        true ->
            case rpc:call(?APPSERVER_NODE,nsm_affiliates, is_able_to_look_details, [wf:user()]) of
                true -> PreLinks ++ [{affiliates,?_U("/profile/affiliates"), ?_T("Affiliates")}];
                false -> PreLinks
            end;
        false -> PreLinks
    end,
    Aside = #aside{body=#list{body=[#listitem{class=case get_current_section() of S -> active; _ -> "" end,
					      body=#link{text=T, url=U}} || {S, U,T} <- Links]}},
    [#panel{class="list-top-photo-h", body=webutils:get_hemen_nav()},
     #section{class="profile-block", body=[Col, Aside]}
    ].

get_current_section() ->
    case wf:q('__submodule__') of
	undefined -> profile;
	Page -> list_to_existing_atom(Page)
    end.


callback(#api{anchor = Id, name = Name}, DataVar) ->
    wf:f("obj('~s').~p(~s)", [Id, Name, DataVar]).

api_event(savePackage, Anchor, Data) ->
    ?INFO("Api Event: ~p ~p",[Anchor, Data]).

section_body(profile) ->
    User0 = webutils:user_info(),
    User = undef_to_empty_str(User0),

    CityList = webutils:city_list(),
    City = webutils:list_to_options(CityList, User#user.location),

    EduOptions = [{"school",	?_T("Elementary School")},
		  {"highschool",?_T("High School")},
                  {"bsc",	?_T("B.Sc.")},
                  {"msc",	?_T("M.Sc.")},
                  {"phd",	?_T("Phd")}],
    Edu = webutils:list_to_options(EduOptions, User#user.education),

    GenderOption = [{"male",	?_T("Male")},
		    {"female",	?_T("Female")}],
    Gender = webutils:list_to_options(GenderOption, User#user.sex),

    {Year0, Month0, Day0} =
        case User#user.age of
            {_, _, _} = D ->
                D;
            _ ->
                {undefined, undefined, undefined}
        end,

    {Y0, _,_} = erlang:date(),
    ViewYear = Y0-18,

    Day = webutils:create_option_with_number({1,31}, Day0, undefined),
    Month = webutils:create_option_with_number({1,12}, Month0, undefined),
    Year = webutils:create_option_with_number({ViewYear,1900}, Year0, undefined),

    ColL = [
	    #panel{class=row,body=[#label{text=?_T("Name")},
				   #panel{class=text,body=#textbox{id=profile_name, text=User#user.name}}]},
	    #panel{class=row,body=[#label{text=?_T("Last Name")},
				   #panel{class=text,body=#textbox{id=profile_surname, text=User#user.surname}}]},
	    #panel{class=row,body=[#label{text=?_T("E-mail")},
				   #panel{class=text,body=#textbox{id=profile_email,text=User#user.email}}]},
	    "<dl class=\"dlist-2\"><dt>"++?_T("Username")++"</dt><dd>"++site_utils:username_upper()++"</dd></dl>",
	    #panel{class=row,body=[#label{text=?_T("New password")},#panel{class=text,body=#password{id=password1}}]},
	    #panel{class=row,body=[#label{text=?_T("New password (again)")},#panel{class=text,body=#password{id=password2}}]},
	    #panel{class=row,body=[#label{text=?_T("Date of Birth")++":"},
				   #panel{class=sel,body=#dropdown{class="cs-1", options=Day, id=profile_day}},
				   #panel{class=sel,body=#dropdown{class="cs-1", options=Month, id=profile_month}},
				   #panel{class=sel,body=#dropdown{class="cs-1", options=Year, id=profile_year}}
				   ]},
	    #panel{class=row,body=[#label{text=?_T("Gender")++":"},
				   #panel{class=sel,body=#dropdown{class="cs-1", options=Gender, id=profile_sex}}]},
	    #panel{class=row,body=[#label{text=?_T("City"++":")},
				   #panel{class="sel sel-2",body=#dropdown{class="cs-1", options=City, id=profile_city}}
				   ]},
	    #panel{class=row,body=[#label{text=?_T("Education"++":")},
				   #panel{class="sel sel-2",body=#dropdown{class="cs-1", options=Edu, id=profile_edu}}
				   ]},
	    #panel{class="btn-holder", body=#panel{class=ar, body=[
		#button{class="btn-reset", text=?_T("Cancel"), postback=profile_cancel},
		#button{class="btn-submit", text=?_T("Save"), postback=profile_save}
	    ]}}
    ],

    AvatarP = avatar_update_box(User),
    Services = [
	{"/images/img-51.png", "Facebook", add},
	{"/images/img-52.png", "Twitter", add},
	{"/images/img-53.png", "Tumblr", del},
	{"/images/img-54.png", "Vimeo", add},
	{"/images/img-55.png", "RSS", add}
    ],
    ServicesP = #panel{class=cell,body=[
				    #h3{text=?_T("Services")},
				    #list{class="soc-list",body=[
					begin
					#listitem{class=png, body=[#image{image=Img},#span{text=Text},
					    case Butt of
						add ->
						    #link{class="btn", text=["<span>+</span>",?_T("Add")],
							  html_encode = false, postback={service, Text}};
						del ->
						    #link{class="btn btn-2", text=?_T("Edit"),
							  postback={service, Text}}
					    end
					    ]}
					end || {Img, Text, Butt} <- Services ]
				    }
	    ]},

    ColR = [ AvatarP, ServicesP ],
    [
     #h1{text=?_T("Profile Information")},
     #panel{id=profile_info},
     #panel{class="profile-info", body=[#panel{class="col-l",body=["<form>", ColL, "</form>"]},
					#panel{class="col-r",body=ColR}
				       ]}
    ];
section_body(gifts) ->
	"<div class='for_blocking'>
        <h1><a href=\"#\" class=\"link\">Tüm Hediyeler</a>Hediyeler</h1>
	    <div class=\"inform-block\">
		    <dl>
			    <dt>Hediye Puanınız:</dt>
			    <dd>38.540</dd>
		    </dl>
	    </div>
	    <h2 class=\"ttl\"><span>Puanlarımla Alabileceğim Hediyeler</span></h2>
	    <ul class=\"prod-list\">
		    <li>
			    <div class=\"box\">
				    <h2 class=\"head\">Puan: 35.000</h2>
				    <div class=\"img\"><a href=\"#\"><img src=\"/images/img-41.jpg\" alt=\"\" width=\"118\" height=\"140\"></a></div>
			    </div>
			    <strong class=\"prod-name\"><a href=\"#\">Samsung Galaxy Gio S5660<br>2 Gb Hafıza Kartı...</a></strong>
			    <a href=\"#\" class=\"btn\">Hediyeyi Al</a>
		    </li><li>
			    <div class=\"box\">
				    <h2 class=\"head\">Puan: 25.000</h2>
				    <div class=\"img\"><a href=\"#\"><img src=\"/images/img-45.jpg\" alt=\"\" width=\"148\" height=\"120\"></a></div>
			    </div>
			    <strong class=\"prod-name\"><a href=\"#\">Fujifilm AV150 14.0MP<br>2.7\"LCD Dijital Fotoğraf...</a></strong>
			    <a href=\"#\" class=\"btn\">Hediyeyi Al</a>
		    </li>
	    </ul>
	    <h2 class=\"ttl\"><span>Daha Önce Aldığım Hediyeler</span></h2>
	    <ul class=\"prod-list\">
		    <li>
			    <div class=\"box\">
				    <div class=\"img\"><a href=\"#\"><img src=\"/images/img-49.jpg\" alt=\"\" width=\"112\" height=\"144\"></a></div>
			    </div>
			    <strong class=\"prod-name\"><a href=\"#\">Pierre Cardin Erkek Kol Saati</a></strong>
		    </li>
	    </ul>
    </div>
    <script>
        $('div.for_blocking').block({
            message: '<h1>" ++ ?_T("This feature is not yet available in beta.") ++
                "</h1><h2>" ++ ?_T("You will see this part very soon.") ++ "</h2>',
            css: { border: '3px solid #a00' }
        });
    </script>";
section_body(user_tournaments) ->
    User = wf:user(),
    Tournaments = rpc:call(?APPSERVER_NODE, nsm_tournaments, user_tournaments, [User]),

    ?INFO("TS: ~p", [Tournaments]),

    TournamentItem =
    fun({T, NumOfPLayers})->
        ?DBG("T: ~p", [T]),
        Name      = T#tournament.name,
        StartDate = T#tournament.start_date,
        _Status   = T#tournament.status,
        _Awards   = T#tournament.awards,
        GameType  = T#tournament.game_type,

        GameTypeImg = case T#tournament.game_type of
            batak ->
                "/images/tournament/slider_batak.png";
            okey ->
                "/images/tournament/slider_okey.png";
            tavla ->
                "/images/tournament/slider_tavla.png";
            %% no image specified for other game types
            _ ->
                ""
        end,


        AwardImage = "/images/img-45.jpg",

        #list{class="tour-item", body=[
            #listitem{body=#image{image=GameTypeImg}},
            #listitem{class=name, body=#h3{text = Name}},
            [#listitem{body=wf:f("<span>~s:</span>~s",[Key, Val])} ||
            {Key, Val} <- [
                    {?_T("Game type"),          wf:to_list(GameType)},
                    {?_T("Starting date"),      convert_date(StartDate)},
                    {?_T("Num of players"),     wf:to_list(NumOfPLayers)},
                    {?_T("Max num of players"), "infinity"}]],
            #listitem{class=line},
            #listitem{class=award, body=[#image{image=AwardImage}]}
        ]}
    end,

    [
        #panel{class="for_blocking", style="overflow:hidden;", body=[
            "<h1>", #link{class=link, text=?_T("Tournaments")},"</h1>",
            #panel{class="inform-block", body=[
                "<dl>
                <dt>Nearest Tournament:</dt>
                <dd>TAVLA 2 May 2012 </dd><dt>12:00AM EEST</td>
                </dl>"]},

            #panel{class="profile-tournaments", body=
                [TournamentItem(T) || T <- Tournaments]
            }
        ]},
        "<script>
            $('div.for_blocking').block({
                message: '<h1>" ++ ?_T("This feature is not yet available in beta.") ++
                    "</h1><h2>" ++ ?_T("You will see this part very soon.") ++ "</h2>',
                css: { border: '3px solid #a00' }
            });
        </script>"
    ];

section_body(account) ->
    Username = wf:user(),
    {ok, Quota}  = rpc:call(?APPSERVER_NODE, nsm_accounts, balance, [Username, ?CURRENCY_QUOTA]),

%    Id = wf:temp_id(),
%    ApiSave = #api{anchor = Id, tag = Id, name = savePackage, delegate = ?MODULE},
%    wf:wire(ApiSave),
%    Script = wf:f(" { ~s } ",[callback(ApiSave,"self == top")]),
%    wf:wire(Id, Script),

    ?INFO("wf:session: ~p",[wf:session(is_facebook)]),

    Orders = rpc:call(?APPSERVER_NODE, nsm_db, get_purchases_by_user,
        [Username, ?ORDERS_PER_PAGE, [?MP_STATE_DONE]]),

    [
        #h1{text=?_T("Account")},
	#panel{class="inform-block", body = [
            "<dl>
            <dt>"++ ?_T("Remaining Quota") ++":</dt>
            <dd>"++wf:to_list(Quota)++"</dd>
            </dl>",
            case wf:session(is_facebook) of
                 true -> "";
                 _ ->  #link{class=btn, url=?_U("/price-table"), text=?_T("Üyelİk Yenİle")}
            end
        ]},

	#panel{class="profile-info", body =
            #form{body=[
                "<fieldset>",
                "<h2 class=\"ttl\"><span>"++?_T("Account information")++"</span></h2>",
                #panel{class=row, body = [
                    #label{text = ?_T("Address")},
                    #panel{class=text, body = #textbox{}} ]},
                #panel{class=row, body = [
                    #label{text = ?_T("City")},
                    #panel{class=text, body = #textbox{}} ]},
                #panel{class=row, body = [
                    #label{text = ?_T("District")},
                    #panel{class=text, body = #textbox{}} ]},
                #panel{class=row, body = [
                    #label{text = ?_T("Postal code")},
                    #panel{class=text, body = #textbox{}} ]},
                #panel{class="row", body=[
                    #panel{class="btn-holder", body=[
			#panel{class="ar", body=[
                            #button{class="btn-reset", text=?_T("Cancel")},
                            #button{class="btn-submit", text=?_T("Save")}
			]}
                    ]}
                ]},

                order_history(Orders),
                "</fieldset>"
                ]}
        }
    ];

section_body(invite) ->
    captcha:generate(invite),
    RSpan = " <span class=\"req\">*</span>",
    [#h1{text=?_T("Invite")},
     #panel{id=invite_info},
     #panel{class="invite-form", body=[
	"<form>",
	#panel{class="row", body=[
		#label{text=?_T("Invite code URL")},
		#panel{class=text, body=[#textbox{id=invite_generate_url}]},
		#link{class=btn, text=?_T("Generate link"), postback=generate_invite}
	]},
	#panel{class="block", body=[
		#panel{class="row", body=[
			#label{text=[?_T("Recipient e-mail address"),RSpan], html_encode = false},
			#panel{class=text, body=#textbox{id=mail_invite, class="text"}}
		]},
		#panel{class="row", body=[
			#label{text=[?_T("Enter his/her name"), RSpan], html_encode = false},
			#panel{class=text, body=#textbox{id=name_invite, class="text"}}
		]},
		#panel{class="row", body=[
			#label{text=[?_T("Question"), RSpan], html_encode = false},
			#panel{class=frame, body=[
				#span{id=invite_captcha, class="add-lbl", text=captcha:format(invite)},
				#panel{class="text text-2", body=#textbox{id=invite_captcha_result}}
			]}
		]},
		#panel{class="row", body=[
			#label{text="Varsa, mesaja ekleme yapabilirsiniz"},
			#panel{class="textarea", body=#textarea{id=text_invite, style="resize:vertical;"}}
		]},
		#panel{class="row", body=[
		#panel{class="btn-holder", body=[
			#panel{class="ar", body=[
				#button{class="btn-reset", text=?_T("Cancel"), postback=invite_cancel},
				#button{class="btn-submit", text=?_T("Send"), postback=invite_send}
			]}
		]}]}
	]},
	"</form>"]}
	];
section_body(tournament) ->

    User0 = webutils:user_info(),
    User = undef_to_empty_str(User0),

    CityList = webutils:city_list(),
    _City = webutils:list_to_options(CityList, User#user.location),

    EduOptions = [{"school",	?_T("Elementary School")},
		  {"highschool",?_T("High School")},
                  {"bsc",	?_T("B.Sc.")},
                  {"msc",	?_T("M.Sc.")},
                  {"phd",	?_T("Phd")}],
    _Edu = webutils:list_to_options(EduOptions, User#user.education),

    GenderOption = [{"male",	?_T("Male")},
		    {"female",	?_T("Female")}],
    _Gender = webutils:list_to_options(GenderOption, User#user.sex),

    {Year0, Month0, Day0} =
        case User#user.age of
            {_, _, _} = D ->
                D;
            _ ->
                {undefined, undefined, undefined}
        end,

    {Y0, _,_} = erlang:date(),
    ViewYear = Y0,

    Day = webutils:create_option_with_number({1,31}, Day0),
    Month = webutils:create_option_with_number({1,12}, Month0),
    Year = webutils:create_option_with_number({ViewYear+2,ViewYear}, Year0),

    Tourn = [#option{value=pointing, text=?_T("Pointing"), selected=true},
                   #option{value=election, text=?_T("Election")}],
%    Tourn = webutils:list_to_options(TournOption),
    Tour = #tournament{name="",description="",quota=100,start_date=date(),type=election,players_count=100},

    Games = [#option{value=game_okey, text=?_T("Okey"), selected=true},
                   #option{value=game_tavla, text=?_T("Tavla")}],
%    Games = webutils:list_to_options(GameOption),

    {Y11,M1,D1} = date(),

    ColL = [
            #panel{class=row,body=[
	        #panel{class="row", body=[
		    #label{text=?_T("Game type")},
                    #panel{class="sel", body=#dropdown{class="cs-1 selectArea", options=Games, id=game_type, value=game_okey}}
		]},
	        #panel{class="row", body=[
	            #label{text=?_T("Name")},
		    #panel{class=text,body=#textbox{id=tournament_name, text=Tour#tournament.name}}
		]},
	        #panel{class="row", body=[
	            #label{text=?_T("Description")},
		    #panel{class=text,body=#textbox{id=tournament_description, text=Tour#tournament.description}}
		]},
	        #panel{class="row", body=[
	            #label{text=?_T("Quota")},
		    #panel{class=text,body=#textbox{id=tournament_quota, text=Tour#tournament.quota}}
		]},
                #panel{class=row,body=[#label{text=?_T("Start Date:")},
				   #panel{class=sel,body=#dropdown{class="cs-1", options=Day, id=tournament_day,value=D1}},
				   #panel{class=sel,body=#dropdown{class="cs-1", options=Month, id=tournament_month,value=M1}},
				   #panel{class=sel,body=#dropdown{class="cs-1", options=Year, id=tournament_year,value=Y11}}
				   ]},
	        #panel{class="row", body=[
		    #label{text=?_T("Tournament type")},
                    #panel{class="sel", body=#dropdown{class="cs-1 selectArea", options=Tourn, id=tournament_type,value=pointing}}
		]},
	        #panel{class="row", body=[
                    #label{text=?_T("No. of players")},
                    #panel{class=text,body=#textbox{id=max_players}}
		]},

	        #panel{class="row", body=[
                    #panel{class="btn-holder", body=[
                         #panel{class="ar", body=[
                              #panel{ class="publish-fb", body=[#checkbox{id=checkbox_fb, text="Publish to Facebook", checked=true}]},
                              #panel{body=[#button{class="btn-reset", text="Cancel", postback=cancel_tournament},
                              #button{class="btn-submit", text="Create", postback=create_tournament}]}
                         ]}
                    ]}
        	]}
	    ]}
    ],

    _AvatarP = avatar_update_box(User),

    ColR = [  ],
    [
    "<div class='for_blocking'>",
     #h1{text=?_T("Create Tournament")},
     #panel{id=create_tour_info},
     #panel{class="profile-info", body=[#panel{class="col-l",body=["<form>", ColL, "</form>"]},
					#panel{class="col-r",body=ColR}
				       ]},

	#br{},
	"<h2 class=\"ttl\"><span>Gifts</span></h2>"
		"<ul class=\"prod-list\">
		<li>
			<div class=\"box\">
				<h2 class=\"head\">Points: 35.000</h2>
				<div class=\"img\"><a href=\"#\"><img src=\"/images/img-41.jpg\" alt=\"\" width=\"118\" height=\"140\"></a></div>
			</div>
			<strong class=\"prod-name\"><a href=\"#\">Samsung Galaxy Gio S5660<br>2 Gb Hafıza Kartı...</a></strong>
		</li><li>
			<div class=\"box\">
				<h2 class=\"head\">Points: 25.000</h2>
				<div class=\"img\"><a href=\"#\"><img src=\"/images/img-45.jpg\" alt=\"\" width=\"148\" height=\"120\"></a></div>
			</div>
			<strong class=\"prod-name\"><a href=\"#\">Fujifilm AV150 14.0MP<br>2.7\"LCD Dijital Fotoğraf...</a></strong>
		</li>
	</ul>
    </div>
    <script>
        $('div.for_blocking').block({
            message: '<h1>" ++ ?_T("This feature is not yet available in beta.") ++
                "</h1><h2>" ++ ?_T("You will see this part very soon.") ++ "</h2>',
            css: { border: '3px solid #a00' }
        });
    </script>"
    ];
%    create_tour_body()];

section_body(affiliates) ->
    #panel{id=page_content, body=[
        affiliates:paged_content(wf:user())
    ]};

section_body(_) ->
    Scores = rpc:call(?APPSERVER_NODE, scoring, score_entries, [wf:user()]),
    STotalScore = integer_to_list(lists:sum([S#scoring_record.score_points || S <- Scores])),
    STotalKakaush = integer_to_list(lists:sum([S#scoring_record.score_kakaush || S <- Scores])),
    [
        #h1{text=?_T("Player statistics")},
        case Scores of 
            [] ->
                #span{text=?_T("You haven't played any games yet.")};
            ScoreList ->
                #panel{ class="affiliates-box", body=[
                    #table {rows=[
                        [#tablerow { cells=[
                            #tableheader { text=?_T("When"), style="padding:4px 10px;" },
                            #tableheader { text=?_T("Other players"), style="padding:4px 10px;" },
                            #tableheader { text=?_T("Game"), style="padding:4px 10px;" },
                            #tableheader { text=?_T("Scores"), style="padding:4px 10px;" },
                            #tableheader { text=?_T("Kakaush"), style="padding:4px 10px;" }
                        ]}] ++
                        [
                            begin
                                SDate = site_utils:feed_time_tuple(calendar:now_to_local_time(S#scoring_record.timestamp)),
                                SCompany = string:join( lists:subtract(S#scoring_record.all_players, [wf:user()]), ", "),
                                SGame = [atom_to_list(S#scoring_record.game_kind) ++ " ",
                                    #span{text="(" ++ atom_to_list(S#scoring_record.game_type) ++ ")", style="font-size:11px;"}],
                                SScores = integer_to_list(S#scoring_record.score_points),
                                SKakaush = integer_to_list(S#scoring_record.score_kakaush),
                                #tablerow { cells=[
                                    #tablecell {body = SDate, style="padding:4px 10px;"},
                                    #tablecell {body = SCompany, style="padding:4px 10px;"},
                                    #tablecell {body = SGame, style="padding:4px 10px;"},
                                    #tablecell {body = SScores, style="padding:4px 10px; text-align:center;"},
                                    #tablecell {body = SKakaush, style="padding:4px 10px; text-align:center;"}
                                ]}
                            end
                        || S <- ScoreList] ++
                        [
                            #tablerow { cells=[
                                #tablecell {body = ""},
                                #tablecell {body = ""},
                                #tablecell {body = ?_T("Total:"), style="padding:12px 10px; text-align:right; font-weight:bold;"},
                                #tablecell {body = STotalScore, style="padding:12px 10px; text-align:center;"},
                                #tablecell {body = STotalKakaush, style="padding:12px 10px; text-align:center;"}
                            ]}
                        ]
                    ]}
                ]}
        end        
%        #panel{class="inform-block", style="width:490px; ", body=
%        "<h1>"++?_T("This feature is not available in beta version")++"</h1>
%         <h2>"++?_T("But we are working on it. Wait for a while - it will all be here. Every kind of statistics with all that tables and diagrams. All that stuff smart people with glasses call 'information design'.")++"</h2>
%        "}
        
    ].

%%%% update avatar %%%%
avatar_update_box(User) ->
    Avatar = avatar:get_avatar_by_username(User#user.username, big),
    #panel{class=cell,body=[
			    #h3{text=?_T("Avatar")},
			    #panel{id=avatarholder, class=photo, body=#image{image=Avatar}},
			    #panel{class=file, body=[
				#upload { class="file-input-area", tag=upload_avatar, show_button=false },
				#textbox{class="file-text", text=?_T("Update")},
				#link{class="button", text=?_T("Update")}
			    ]}
			   ]}.

start_upload_event(_Tag) ->
    wf:wire("$('.profile-block .file-text').val('Uploading...');"),
    wf:replace(avatarholder,
               #panel{id=avatarholder, class=photo,
                      body=#image{image="/images/spinner.gif"}}).

finish_upload_event(_Tag, undefined, _, _) ->
    flash(info, profile_info, ?_T("Please select a file"));

finish_upload_event(_Tag, OrigFile, LocalFile, _Node) ->
    wf:wire("$('.profile-block .file-text').val('"++OrigFile++"');"),
    User = webutils:user_info(),
    case avatar:process_uploaded_avatar(User#user.username, OrigFile, LocalFile) of
	{error, Error} -> flash(error, profile_info, ?_TS("Error: $error$ ", [{error, Error}]));
	{ok, Avatar} ->
            wf:state(new_avatar, Avatar),
            wf:replace(header_user_avatar,
                       #image{image=avatar:get_avatar(Avatar, tiny), 
                            id=header_user_avatar, style="width:23px;height:23px", alt="#"}),
            wf:replace(avatarholder,
                       #panel { id=avatarholder, class=photo,
                                body=#image{image=avatar:get_avatar(Avatar, big)} }),
            UserWithAvatar = User#user{avatar = Avatar},
            rpc:call(?APPSERVER_NODE, nsm_db, put,[UserWithAvatar])
    end.

%%%% end update avatar %%%%

event(Event) ->
    case wf:user() of
	undefined ->
	    wf:redirect_to_login("/");
	_User ->
	    u_event(Event)
    end.

u_event({is_facebook, Flag}) ->
    ?INFO("is_facebook: ~p",[Flag]);

u_event({is_facebook, _, A}) ->
    ?INFO("is_facebook p: ~p",[A]);

u_event({show_contract_details, ContractId, PanelId}) ->
    affiliates:inner_event({show_contract_details, ContractId, PanelId}, "");

u_event({details_page, ContractId, Page, PanelId}) ->
    affiliates:inner_event({details_page, ContractId, Page, PanelId}, "");

u_event({hide_contract_details, PanelId}) ->
    affiliates:inner_event({hide_contract_details, PanelId}, "");

u_event({show_user_details, PurchasesList, PanelId}) ->
    affiliates:inner_event({show_user_details, PurchasesList, PanelId}, "");

u_event({hide_user_details, PanelId}) ->
    affiliates:inner_event({hide_user_details, PanelId}, "");

u_event({contracts_page, Page, UserID}) ->
    affiliates:inner_event({contracts_page, Page, UserID}, "");


u_event({nothing}) ->
    ok;

u_event(profile_save) ->
    OrigUser = webutils:user_info(),
    NewName = wf:q(profile_name),
    NewSurname = wf:q(profile_surname),
    NewEmail = wf:q(profile_email),
    Passwd1 = wf:q(password1),
    Passwd2 = wf:q(password2),

    Sex = site_utils:element_value(profile_sex),
    City = site_utils:element_value(profile_city),
    Edu = site_utils:element_value(profile_edu),

    Y = site_utils:element_value(profile_year, integer),
    M = site_utils:element_value(profile_month, integer),
    D = site_utils:element_value(profile_day, integer),

    BDate = {Y,M,D},

    User1 =  OrigUser#user{name = NewName,
			   surname = NewSurname,
			   sex = Sex,
			   location = City,
			   education = Edu,
			   age = BDate},
    try begin
	  User2 =
	      case {Passwd1, Passwd2} of
		  {"", ""} -> User1;
		  {P, P} ->
		      case length(Passwd1) < 6 of
			  true ->
			      throw({error, ?_T("Password is to short. Six letters needed.")});
			  false -> User1#user{password = utils:sha(Passwd1)}
		      end;
		  _ -> throw({error, ?_T("Passwords don't match.")})
	      end,
	  User3 =
	      case validator_is_email:validate("", NewEmail) of
		  true ->
		      User2#user{email = NewEmail};
		  false ->
		      throw({error, ?_T("Email is not valid.")})
	      end,

	      case wf:state(new_avatar) /= undefined of
		  true -> User3#user{avatar = wf:state(new_avatar)};
		  false -> User3
	      end
	end
    of
      #user{} = NewUser ->
	  ?PRINT({NewUser}),
	  wf:session(user_info, NewUser),
	  ok = rpc:call(?APPSERVER_NODE,nsm_users,update_user,[NewUser]),
	  flash(info, profile_info, ?_T("Saved!"))
    catch
      {error, Error} ->
	  flash(error, profile_info, Error)
    end;

u_event(profile_cancel) ->
    u_event(not_done_yet);

u_event(generate_invite) ->
    User = webutils:user_info(),
    {ok, InviteCode} = rpc:call(?APPSERVER_NODE,invite,generate_code,[User]),
    Url = site_utils:create_url_invite(InviteCode),
    wf:set(invite_generate_url, Url),
    u_event(not_done_yet);

u_event(create_tournament) ->
    User = webutils:user_info(),
    TourName = wf:q(tournament_name),
    TourType = wf:q(tournament_type),
    TourDesc = wf:q(tournament_description),
    GameType = case wf:q(game_type) of
      "game_okey" -> game_okey;
      "game_tavla" -> game_tavla;
      "game_batak" -> game_batak;
      "game_king" -> game_king;
      _ -> game_tavla
    end,
    MaxPlayers = erlang:list_to_integer(wf:q(max_players)),
    Quota = erlang:list_to_integer(wf:q(tournament_quota)),
    Y = site_utils:element_value(tournament_year, integer),
    M = site_utils:element_value(tournament_month, integer),
    D = site_utils:element_value(tournament_day, integer),
    case rpc:call(?APPSERVER_NODE,tournaments,create,[
                                  User#user.username,
                                  TourName,
                                  TourDesc,
                                  {Y,M,D},
                                  MaxPlayers,
                                  Quota,
                                  undefined,
                                  TourType, GameType]) of
        {error,X} -> flash(error, create_tour_info, io_lib:format("~p",[X]));
        _X ->
           rpc:call(?APPSERVER_NODE,tournaments,join,[User#user.username,_X]),
           flash(info, create_tour_info, ?_T("OK"))
    end,
    ok;

u_event(invite_send) ->
    case captcha:check(invite, wf:q(invite_captcha_result)) of
	true ->
	    Mail = wf:q(mail_invite),
	    UserName = wf:q(name_invite),
	    Text = wf:q(text_invite),
	    User = webutils:user_info(),
	    case rpc:call(?APPSERVER_NODE,invite,send_invite_email,[User, Mail, UserName, Text]) of
		{error, wrong_email} ->
		    flash(error, invite_info, ?_T("Wrong e-mail"));
		{error, wrong_username} ->
		    flash(error, invite_info, ?_T("Wrong username."));
		{ok, _} ->
		    captcha:generate(invite),
		    wf:update(invite_captcha, captcha:format(invite)),
		    wf:set(invite_captcha_result, ""),
		    flash(info, invite_info, ?_T("Invite sent!"))
	    end;
	false ->
	    flash(error, invite_info, ?_T("Bad captcha!"))
    end;

u_event(invite_cancel) ->
    u_event(not_done_yet);

u_event({service, _}) ->
    u_event(not_done_yet);

u_event(not_done_yet) ->
    flash(info, profile_info, ?_T("Not implemented."));

u_event({more_orders, PurchaseId}) ->
    Username = wf:user(),
    Orders0 = rpc:call(?APPSERVER_NODE, nsm_db, get_purchases_by_user,
        [Username, PurchaseId, ?ORDERS_PER_PAGE+1, [?MP_STATE_DONE]]),
    %% first element already rendered as last element of the current list
    case Orders0 of
        [_] ->
            wf:update(more_button_holder,  []);
        [_|Orders] ->
            wf:insert_bottom(orders_list, [order_list_item(MP) || MP <- Orders]),
            wf:update(more_button_holder,  orders_more_button(Orders))
    end;

u_event(Any) ->
    webutils:event(Any).

%%%%%% internals

undef_to_empty_str(Rec) when is_tuple(Rec) ->
    [Atom | List0] = tuple_to_list(Rec),
    List = [ case X of undefined -> ""; O -> O end || X <- List0 ],
    list_to_tuple([Atom | List]).

%title() ->
%    ?_T("Create Tournament").

create_tour_body() ->
    [
    %#panel{class="list-top-photo-h", body=webutils:get_hemen_nav(tournament)},
     #section{class="create-area", body=#section{class="create-block",
           body=[
                 #panel{id="welcome_text", body=?_T("Please select game type.")},
                 inn_body()
                 ]}}].

inn_body() ->
    TournOption = [{"Pointing", ?_T("Pointing")},
                   {"Election", ?_T("Election")}],
    Tourn = webutils:list_to_options(TournOption),

%    wf:wire("objs('settings').css({overflow:'hidden'}).height(0);"),
    #panel{id="settings", class="settings", body=[
        "<form>",
        #panel{class=col, body=[
            #label{text=?_T("Tournament type")},
            #panel{class="sel", body=#dropdown{class="cs-1 selectArea", options=Tourn, id=tournament_type}}
        ]},
        #panel{class=col, body=[
            #label{text=?_T("No. of quota")},
            #panel{class=text,body=#textbox{id=quota}}
        ]},
        #panel{class=col, body=[
            #label{text=?_T("Something:")},
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

order_history(Orders) ->
    [
        "<h2 class=\"ttl\"><span>"++?_T("My order history")++"</span></h2>",
        #list{class="history-list", id=orders_list, body =
            [order_list_item(MP) || MP <- Orders]},
        #panel{id=more_button_holder, class="btn-holder", body = orders_more_button(Orders)}
    ].

order_list_item(#membership_purchase{membership_package = Package} = MP) ->
    PackageId = Package#membership_package.id,
    %% get package by id from database to get actual avalilable_for_sale option state
    AvailableForSale =
    case rpc:call(?APPSERVER_NODE, nsm_db, get, [membership_package, PackageId]) of
        {ok, P} ->
            P#membership_package.available_for_sale;
        _ ->
            false
    end,

    PaymentType = Package#membership_package.payment_type,
    PackageId = Package#membership_package.id,
    No = Package#membership_package.no,
    Quota = Package#membership_package.quota,
    {PurchaseDateRaw, _} = calendar:now_to_datetime(MP#membership_purchase.start_time),
    PurchaseDate = site_utils:date_to_text(PurchaseDateRaw),
    Button = case AvailableForSale of
        true ->
            Url = ?_U(lists:concat(["/buy/", PaymentType, "/package_id/", PackageId])),
            [#link{class=btn, url=Url,  text=?_T("Buy it")}];
        _ ->
            []
    end,
    Text = ?_TS("Package $number$, $quota$ quotas, $date$",
        [{number, wf:to_list(No)}, {quota,  wf:to_list(Quota)}, {date, PurchaseDate}]),

    #listitem{body = [#span{text=Text} | Button]}.

orders_more_button(Orders) ->
%    ?PRINT({length(Orders), ?ORDERS_PER_PAGE}),
    case length(Orders) < ?ORDERS_PER_PAGE of
        %% last orders, don't show more button
        true ->
            [];
        false ->
            Last = lists:last(Orders),
%            ?PRINT(Last),
            #button{class="btn-submit", text = ?_T("More"), postback = {more_orders, Last#membership_purchase.id}}
    end.




convert_date({_A, _B, _C} = Now) ->
    httpd_util:rfc1123_date(calendar:now_to_datetime(Now));
convert_date(Other) ->
    wf:f("~p", [Other]).

flash(Type, Id, Text) ->
    case Type of
        error ->
            wf:update(Id, #notice{type=message, delay = 3000, body = Text});
        info ->
            wf:update(Id, #notice{type=message, delay = 3000, body = Text})
    end.
