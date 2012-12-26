-module (profile).
-compile(export_all).

-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("nsm_db/include/user.hrl").
-include_lib("nsm_db/include/tournaments.hrl").
-include_lib("nsm_db/include/accounts.hrl").
-include_lib("nsm_db/include/scoring.hrl").
-include_lib("nsm_db/include/membership_packages.hrl").
-include_lib("nsm_db/include/common.hrl").
-include_lib("nsm_db/include/invite.hrl").
-include_lib("nsm_db/include/config.hrl").
-include_lib("elements/records.hrl").

-include("common.hrl").
-include("setup.hrl").

-define(ORDERS_PER_PAGE, 10).

main() ->
    tw_utils:app_callback(),
    webutils:add_script("/nitrogen/js/form.js"),
    webutils:add_script("/nitrogen/js/input-type-file.js"),
    webutils:add_script("/nitrogen/blockui.js"),
    #template { file=code:priv_dir(nsp_srv)++"/templates/bare.html" }.

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
       {profile,         ?_U("/profile"),                  ?_T("Profile")},
       {gifts,           ?_U("/profile/gifts"),            ?_T("Gifts")},
       {account,         ?_U("/profile/account"),          ?_T("Account")},
       {stats,           ?_U("/profile/stats"),            ?_T("Stats")},
       {invite,          ?_U("/profile/invite"),           ?_T("Invite")}
    ],
    Links = case nsm_affiliates:is_existing_affiliate(wf:user()) of 
        true ->
            case nsm_affiliates:is_able_to_look_details(wf:user()) of
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
    ?INFO("Api Event: ~p ~p",[Anchor, Data]);
api_event(Name, Tag, Args)->
    webutils:api_event(Name, Tag, Args).

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
				   #panel{class=text,body=#textbox{id=profile_name, text=site_utils:decode_letters(User#user.name)}}]},
	    #panel{class=row,body=[#label{text=?_T("Last Name")},
				   #panel{class=text,body=#textbox{id=profile_surname, text=site_utils:decode_letters(User#user.surname)}}]},
%% 	    #panel{class=row,body=[#label{text=?_T("E-mail")},
%% 				   #panel{class=text,body=#textbox{id=profile_email,text=User#user.email}}]},
            "<dl class=\"dlist-2\"><dt>"++?_T("E-mail")++"</dt><dd>"++ User#user.email ++"</dd></dl>",
            "<dl class=\"dlist-2\"><dt>"++?_T("Username")++"</dt><dd>"++ wf:user() ++"</dd></dl>",
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

    Services = #panel{class=cell,body=[
      #h3{text=?_T("Services")},
      #list{class="soc-list",body=[
        fb_utils:service_item(),
        tw_utils:service_item()
      ]}
    ]},
    [
     #h1{text=?_T("Profile Information")},
     #panel{id=profile_info},
     #panel{class="profile-info", body=[
	    #panel{class="col-l",body=["<form>", ColL, "</form>"]},
	    #panel{class="col-r",body= [AvatarP, Services]}]}
    ];
section_body(gifts) ->
    AllGifts = lists:reverse(lists:sort(nsm_users:list_gifts_of(wf:user()))),
    [
        #h1{text=?_T("Gifts")},
        #list{class="history-list", id=orders_list, body = [
            begin
%                SDate = site_utils:feed_time_tuple(calendar:now_to_local_time(BoughtTime)),
                GiftIfAny = nsm_gifts_db:get_gift(GiftId),
                case element(1, GiftIfAny) of
                    ok ->
                        {ok, {ThisGift, _}} = GiftIfAny,
                        SName = site_utils:decode_letters(ThisGift#gift.gift_name),
                        SKakush = integer_to_list(ThisGift#gift.kakush_point),
%                        SPrice = integer_to_list(ThisGift#gift.kakush_currency),
                        #listitem{body = [
                            #span{body=[
                                 ?_TS("<b>$name$</b> worth $kakush$ kakush", [{name, SName}, {kakush, SKakush}])
%                                "<b>" ++ SName ++ " </b><font style='color:#777;'>(" ++ SDate ++ ")</font> " ++ ?_T("worth") ++ " " ++
%                                SPrice ++ " <font size=-1>TL</font> " ++ ?_T("for") ++ " " ++ SKakush ++ " " ++ ?_T("kakush")
                            ]},
                            #link{class=btn, postback={deliver, GiftId, BoughtTime, SName, SKakush},  text=?_T("Deliver")}
                        ]};
                    error ->    % some junk got into user_bought_gifts
                        ?ERROR("No such gift with id: ~p", [GiftId])
                end
            end

        || #user_bought_gifts{timestamp = BoughtTime, gift_id = GiftId} <- AllGifts]}
    ];


section_body(user_tournaments) ->
    User = wf:user(),
    Tournaments = nsm_tournaments:user_tournaments(User),

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
        #panel{%class="for_blocking", 
               style="overflow:hidden;", body=[
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
    {ok, Quota}  = nsm_accounts:balance(Username, ?CURRENCY_QUOTA),
    {ok, Kakush}  = nsm_accounts:balance(Username, ?CURRENCY_KAKUSH),
    {ok, KakushCurrency}  = nsm_accounts:balance(Username, ?CURRENCY_KAKUSH_CURRENCY),
%    Id = wf:temp_id(),
%    ApiSave = #api{anchor = Id, tag = Id, name = savePackage, delegate = ?MODULE},
%    wf:wire(ApiSave),
%    Script = wf:f(" { ~s } ",[callback(ApiSave,"self == top")]),
%    wf:wire(Id, Script),
    Orders = nsm_db:get_purchases_by_user(Username, ?ORDERS_PER_PAGE, [?MP_STATE_DONE,?MP_STATE_CONFIRMED]),
    {_, UA} = nsm_db:get(user_address, wf:user()),
    case UA of
        notfound ->
            Address="", City="", PostalCode="", Phone="", PersonalId="";
        _ -> 
            #user_address{address = Address, city = City, postal_code = PostalCode, phone = Phone, personal_id = PersonalId} = UA
    end,
    [
	#h1{text=?_T("Account")},
	#panel{class="inform-block", body = [
	    "<dt><dl>"++ ?_T("Remaining Kakush") ++": <dd>"++wf:to_list(Kakush)++"</dd>&nbsp;&nbsp;&nbsp;</dl>",
	    "<dl> "++ ?_T("Kakush Currency") ++": <dd>"++wf:to_list(KakushCurrency)++"</dd>&nbsp;TL&nbsp;&nbsp;&nbsp;</dl>",
	    "<dl> "++ ?_T("Quota") ++": <dd>"++wf:to_list(Quota)++"</dd></dl></dt>",
	    #link{class=btn, url=?_U("/price-table/credit-card"), text=?_T("Üyelİk Yenİle")}
	]},
	#panel{class="profile-info", body = [
	    #panel{class="col-l",body=[
                #form{body=[
                    "<fieldset>",
                    "<h2 class=\"ttl\"><span>"++?_T("Account information")++"</span></h2>",
                    #panel{class=row, body = [
                        #label{text = ?_T("Address")},
                        #panel{class=text, body = #textbox{id=user_address, text=Address}} ]},
                    #panel{class=row, body = [
                        #label{text = ?_T("City")},
                        #panel{class=text, body = #textbox{id=user_city, text=City}} ]},
                    #panel{class=row, body = [
                        #label{text = ?_T("Postal code")},
                        #panel{class=text, body = #textbox{id=user_postal_code, text=PostalCode}} ]},
                    #panel{class=row, body = [
                        #label{text = ?_T("Phone")},
                        #panel{class=text, body = #textbox{id=user_phone, text=Phone}} ]},
                    #panel{class=row, body = [
                        #label{body = ?_T("Personal&nbsp;ID#")},
                        #panel{class=text, body = #textbox{id=user_personal_id, text=PersonalId}} ]},
                    #panel{class="row", body=[
                        #panel{class="btn-holder", body=[
                            #panel{class="ar", body=[
                                #button{class="btn-reset", text=?_T("Cancel"), postback=user_address_cancel},
                                #button{class="btn-submit", text=?_T("Save"), postback=user_address_save}
                            ]}
                        ]}
                    ]},

                    order_history(Orders),
                    "</fieldset>"
                ]}
            ]},
            #panel{class="col-r",body=[
                #panel{class=cell,body=[
                    #h3{text=?_T("Convert kakush to quota")},
                    #panel{class=row, body=[
                        #label{text=?_T("Amount"), style="width:100px;"},
                        #panel{class=text, style="width:132px;", body = #textbox{id=kakush_to_quota, style="width:132px;"}},
                        #link{class="button", text=?_T("Convert"), style="margin:10px;", postback=convert_kakush_to_quota}
                    ]}
                ]}
            ]}
        ]}
    ];

section_body(invite) ->
    captcha:generate(invite),
    u_event(generate_invite),
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
				#button{class="btn-submit", text=?_T("Send"), postback=invite_send}
			]}
		]}]}
	]},
	"</form>"]},
    #h1{text=?_T("Sent invites")},
    #panel{id=invite_list, body=invite_list()}
    ];

section_body(affiliates) ->
    #panel{id=page_content, body=[
        affiliates:paged_content(wf:user())
    ]};

section_body(_) ->
    Scores = scoring:score_entries(wf:user()),
    STotalScore = integer_to_list(lists:sum([S#scoring_record.score_points || S <- Scores])),
    STotalKakaush = integer_to_list(lists:sum([S#scoring_record.score_kakaush || S <- Scores])),

    {_, PersonalScore} = nsm_db:get(personal_score, wf:user()),
    [
        #h1{text=?_T("Player statistics")},
        case PersonalScore of 
            notfound -> [];
            PS -> #panel{body=[
                #singlerow{style="width:700px; margin-left:36px; font-size:16px; ",cells=[
                    #tablecell{body=[
                        ?_T("Total games played: "),
                        #span{style="font-size:18px;", text=integer_to_list(PS#personal_score.games)}
                    ]},
                    #tablecell{body=[
                        ?_T("Success ratio: "),
                        #span{style="font-size:18px;", text=integer_to_list(100 * PS#personal_score.wins div PS#personal_score.games) ++ "%"}
                    ]}
                ]},
                #br{},
                #singlerow{style="width:400px; margin-left:36px; font-size:14px; ", cells=[
                    #tablecell{body=[
                        ?_T("Wins: "),
                        #span{style="font-size:16px;", text=integer_to_list(PS#personal_score.wins)}
                    ]},
                    #tablecell{body=[
                        ?_T("Loses: "),
                        #span{style="font-size:16px;", text=integer_to_list(PS#personal_score.loses)}
                    ]},
                    #tablecell{body=[
                        ?_T("Disconnects: "),
                        #span{style="font-size:16px;", text=integer_to_list(PS#personal_score.disconnects)}
                    ]}
                ]},
                #br{},
                #br{},
                #br{}
            ]}
        end,
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
    ].

% invite list separated for easy update
invite_list() ->
    SentInvites = nsm_db:invite_code_by_issuer(wf:user()),
    #list{class="history-list", id=orders_list, body = [
        begin
            Recipient = case RecipientOrUndefined of
                undefined -> ?_T("anyone");
                _ -> RecipientOrUndefined
            end,
            SDate = site_utils:feed_time_tuple(calendar:now_to_local_time(CreateDate)),
            case CreatedUser of
                undefined ->
                    #listitem{body = [
                        #span{body=[
                            "<b>" ++ Code ++ " </b><font style='color:#777;'>(" ++ SDate ++ ")</font> " ++ ?_T("Invited") ++ " " ++ Recipient                        
                        ]},
                        #link{class=btn, postback={invite_resend, Recipient},  text=?_T("Resend")}
                    ]};
                _ ->
                    #listitem{body = [
                        #span{body=[
                            "<b>" ++ Code ++ " </b><font style='color:#777;'>(" ++ SDate ++ ")</font> " ++ ?_T("Invited") ++ " " ++ 
                            Recipient ++ " " ++ ?_T("became") ++ CreatedUser ++ " " ++ ?_T("on Kakaranet")
                        ]}
                    ]}
            end
        end

    || #invite_code{code=Code, create_date=CreateDate, recipient=RecipientOrUndefined, created_user=CreatedUser} <- SentInvites]}.


%%%% update avatar %%%%
avatar_update_box(User) ->
    Avatar = avatar:get_avatar_by_username(User#user.username, big),
    #panel{class=cell,body=[
			    #h3{text=?_T("Avatar")},
			    #panel{id=avatarholder, class=photo, body=#image{image=Avatar, class=
                    case nsm_accounts:user_paid(User#user.username) of
                        true -> "paid_user_avatar";
                        _ -> ""
                    end
                }},
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
                            id=header_user_avatar, style="width:23px;height:23px", alt="#", class=
                                case nsm_accounts:user_paid(User#user.username) of
                                    true -> "paid_user_avatar";
                                    _ -> ""
                                end
            }),
            wf:replace(avatarholder,
                   #panel { id=avatarholder, class=photo,
                            body=#image{image=avatar:get_avatar(Avatar, big), class=
                                case nsm_accounts:user_paid(User#user.username) of
                                    true -> "paid_user_avatar";
                                    _ -> ""
                                end
                            } 
                    }
            ),
            UserWithAvatar = User#user{avatar = Avatar},
            nsx_msg:notify(["db", "user", User#user.username, "put"], UserWithAvatar)
    end.

%%%% end update avatar %%%%

event(Event) ->
  case wf:user() of 
    undefined ->  wf:redirect_to_login("/");
    _User -> u_event(Event)
  end.

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
    NewName = site_utils:decode_letters(wf:q(profile_name)),
    NewSurname = site_utils:decode_letters(wf:q(profile_surname)),
%    NewEmail = wf:q(profile_email),
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
%      ?INFO(" +++ ~p", [NewEmail]),
	  User3 = User2,
%	      case validator_is_email:validate("", NewEmail) of
%		  true ->
%		      User2#user{email = NewEmail};
%		  false ->
%		      throw({error, ?_T("Email is not valid.")})
%	      end,

	      case wf:state(new_avatar) /= undefined of
		  true -> User3#user{avatar = wf:state(new_avatar)};
		  false -> User3
	      end
	end
    of
      #user{} = NewUser ->
	  ?PRINT({NewUser}),
	  wf:session(user_info, NewUser),
      nsx_msg:notify(["subscription", "user", NewUser#user.username, "update_user"], {NewUser}),
	  flash(info, profile_info, ?_T("Saved!"))
    catch
      {error, Error} ->
	  flash(error, profile_info, Error)
    end;

u_event(profile_cancel) ->
    u_event(not_done_yet);

u_event(generate_invite) ->
    User = webutils:user_info(),
    {ok, InviteCode} = nsm_invite:generate_code(User),
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
    
    nsx_msg:notify(["tournaments", "user", User#user.username, "create_and_join"], {
        TourName,
        TourDesc,
        {Y,M,D},
        MaxPlayers,
        Quota,
        undefined,
        TourType, GameType}),       
    flash(info, create_tour_info, ?_T("OK"));

u_event(invite_send) ->
    case captcha:check(invite, wf:q(invite_captcha_result)) of
	true ->
	    Mail = wf:q(mail_invite),
	    UserName = wf:q(name_invite),
	    Text = wf:q(text_invite),
	    User = webutils:user_info(),
	    case nsm_invite:send_invite_email(User, Mail, UserName, Text) of
		{error, wrong_email} ->
		    flash(error, invite_info, ?_T("Wrong e-mail"));
		{error, wrong_username} ->
		    flash(error, invite_info, ?_T("Wrong username."));
		{ok, _} ->
		    captcha:generate(invite),
		    wf:update(invite_captcha, captcha:format(invite)),
		    wf:set(invite_captcha_result, ""),
		    wf:set(invite_generate_url, ""),
		    wf:set(mail_invite, ""),
		    wf:set(name_invite, ""),
		    wf:set(text_invite, ""),
		    flash(info, invite_info, ?_T("Invite sent!")),
            timer:sleep(500),
            wf:update(invite_list, invite_list())
	    end;
	false ->
	    flash(error, invite_info, ?_T("Bad captcha!"))
    end;

u_event({invite_resend, Mail}) ->
    UserName = ?_T("Gamer"),  % this is wrong
    Text = "",
    User = webutils:user_info(),
    case nsm_invite:send_invite_email(User, Mail, UserName, Text) of
	{error, wrong_email} ->
	    flash(error, invite_info, ?_T("Wrong e-mail"));
	{error, wrong_username} ->
	    flash(error, invite_info, ?_T("Wrong username."));
	{ok, _} ->
	    flash(info, invite_info, ?_T("Invite sent!")),
        timer:sleep(500),
        wf:update(invite_list, invite_list())
    end;

u_event({delete, twitter}) ->
  tw_utils:delete();
u_event({service, _}) ->
    u_event(not_done_yet);

u_event(not_done_yet) ->
    flash(info, profile_info, ?_T("Not implemented."));

u_event({more_orders, PurchaseId}) ->
    Username = wf:user(),
    Orders0 = nsm_db:get_purchases_by_user(Username, PurchaseId, ?ORDERS_PER_PAGE+1, [?MP_STATE_DONE]),
    %% first element already rendered as last element of the current list
    case Orders0 of
        [_] ->
            wf:update(more_button_holder,  []);
        [_|Orders] ->
            wf:insert_bottom(orders_list, [order_list_item(MP) || MP <- Orders]),
            wf:update(more_button_holder,  orders_more_button(Orders))
    end;

u_event(convert_kakush_to_quota) ->
    SKakush = wf:q(kakush_to_quota),
    Koef = 100 / nsm_db:get_config("curr_qouta_k", 2.39),
    try
        Kakush = list_to_integer(SKakush),
        {ok, HasKakush} = nsm_accounts:balance(wf:user(), ?CURRENCY_KAKUSH_CURRENCY),
        if 
            Kakush > HasKakush ->
                wf:wire(#alert{text=?_T("Sorry, you don't have that much kakush.")});
            Kakush =< 0 ->
                wf:wire(#alert{text=?_T("No.")});
            true ->
                nsm_accounts:transaction(wf:user(), ?CURRENCY_KAKUSH_CURRENCY, -Kakush, "Buying "++SKakush++" quota: give kakush"),
                nsm_accounts:transaction(wf:user(), ?CURRENCY_QUOTA, round(Kakush * Koef), "Buying "++SKakush++" quota: get quota"),
                wf:wire(#alert{text=?_T("You bought" ++ " " ++ integer_to_list(round(Kakush * Koef)) ++ " " ++ "quota" ++ ".")}),
                wf:redirect("/profile/account")
        end
    catch
        _:Error ->
            ?ERROR(Error),
            wf:wire(#alert{text=?_T("Please, write only digits.")})
    end;

u_event(user_address_save) ->
    Address = site_utils:decode_letters(wf:q(user_address)),
    City = site_utils:decode_letters(wf:q(user_city)),
    PostalCode = wf:q(user_postal_code),
    Phone = wf:q(user_phone),
    PersonalId = wf:q(user_personal_id),
    nsx_msg:notify(["db", "user", wf:user(), "put"], #user_address{
        username=wf:user(),
        address = Address,
        city = City,
        postal_code = PostalCode,
        phone = Phone,
        personal_id = PersonalId
    }),
    wf:wire(#alert{text=?_T("Saved")});

u_event(user_address_cancel) ->
    {_, UA} = nsm_db:get(user_address, wf:user()),
    case UA of
        #user_address{address = Address, city = City, postal_code = PostalCode, phone = Phone, personal_id = PersonalId} ->
            wf:set(user_address, Address),
            wf:set(user_city, City),
            wf:set(user_postal_code, PostalCode),
            wf:set(user_phone, Phone),
            wf:set(user_personal_id, PersonalId);
        _ ->
            wf:set(user_address, ""),
            wf:set(user_city, ""),
            wf:set(user_postal_code, ""),
            wf:set(user_phone, ""),
            wf:set(user_personal_id, "")
    end;

u_event({deliver, GiftId, GiftTimestamp, SName, SKakush}) ->
    {_, UA} = nsm_db:get(user_address, wf:user()),
    case UA of
        notfound ->
            Address="", City="", PostalCode="", Phone="", PersonalId="";
        _ -> 
            #user_address{address = Address, city = City, postal_code = PostalCode, phone = Phone, personal_id = PersonalId} = UA
    end,
    {_, UInfo} = nsm_users:get_user(wf:user()),
    case UInfo of   
        notfound -> 
            UName = "", USurname = "";
        _ ->
            UName = case UInfo#user.name of
                undefined -> "";
                OkName -> OkName
            end,
            USurname = case UInfo#user.surname of
                undefined -> "";
                OkSurname -> OkSurname
            end
    end,
    Body = [
        #panel{class=holder, style="margin-left:12px;", body=[
            #panel{style="font-size:18px; margin-top:30px;", body=
                ?_TS("So, you want to deliver <b>$name$</b> worth $kakush$ kakush", [{name, SName}, {kakush, SKakush}])
            },
            #singlerow{style="margin-top:26px; margin-bottom:16px;", cells=[
                #tablecell{style="vertical-align:top; width:200px; font-size:1.4em; padding-top:10px;", body=?_T("Please, check:")},
                #tablecell{body=[
                    #panel{class=row, body = [
                        #label{text = ?_T("Name")},
                        #panel{class=text, body = #textbox{id=delivery_name, text=UName}} ]},
                    #panel{class=row, body = [
                        #label{text = ?_T("Surname")},
                        #panel{class=text, body = #textbox{id=delivery_surname, text=USurname}} ]},
                    #panel{class=row, body = [
                        #label{text = ?_T("Address")},
                        #panel{class=text, body = #textbox{id=delivery_address, text=Address}} ]},
                    #panel{class=row, body = [
                        #label{text = ?_T("City")},
                        #panel{class=text, body = #textbox{id=delivery_city, text=City}} ]},
                    #panel{class=row, body = [
                        #label{text = ?_T("Postal code")},
                        #panel{class=text, body = #textbox{id=delivery_postal_code, text=PostalCode}} ]},
                    #panel{class=row, body = [
                        #label{text = ?_T("Phone")},
                        #panel{class=text, body = #textbox{id=delivery_phone, text=Phone}} ]},
                    #panel{class=row, body = [
                        #label{body = ?_T("Personal&nbsp;ID#")},
                        #panel{class=text, body = #textbox{id=delivery_personal_id, text=PersonalId}} ]}
                ]}
            ]},
            #singlerow{cells=[
                #tablecell{
                    body="", style="width:272px;"
                },
                #tablecell{
                    body=#cool_button{text=?_T("Deliver!"), postback={process_delivery, wf:user(), GiftId, GiftTimestamp, SName}, style="display:block;"}
                }
            ]},
            #grid_clear{}
        ]}
    ],
    wf:update(simple_panel, webutils:lightbox_panel_template(gift_lightbox, Body, hide_delivery_details)),
    wf:wire(simple_lightbox, #show{});

u_event(hide_delivery_details) ->
    wf:wire(simple_lightbox, #hide{});

u_event({process_delivery, UId, GiftId, GiftTimestamp, SName}) ->
    Orders = nsm_db:get_purchases_by_user(UId, undefined, all),
    HasNotConfirmed = fun(F, OL) ->
        case OL of
            [] -> false;
            [#membership_purchase{state=S}|T] -> case S of
                confirmed -> F(F, T);
                _ -> true
            end
        end
    end,
    case HasNotConfirmed(HasNotConfirmed, Orders) of
        true ->
            wf:wire(#alert{text=?_T("Sorry, not all of your payments are yet confirmed. We are working on it, please wait a little bit more.")}),
            wf:wire(simple_lightbox, #hide{});
        false ->
            case nsm_db:get(config, "delivery/notifications/email") of
                {error, notfound} ->
                    ?ERROR("Delivery error, delivery/notifications/email not set"),
                    wf:wire(#alert{text=?_T("Notification error while processing delivery!")});
                {ok, #config{value=Email}} ->
                    nsx_msg:notify(["gifts", "user", UId, "mark_gift_as_deliving"], {GiftId, GiftTimestamp}),
                    UName = wf:q(delivery_name),
                    USurname = wf:q(delivery_surname),
                    Address = wf:q(delivery_address),
                    City = wf:q(delivery_city),
                    PostalCode = wf:q(delivery_postal_code),
                    Phone = wf:q(delivery_phone),
                    PersonalId = wf:q(delivery_personal_id),
                    {Vendor, Gift} = GiftId,
                    MailContent = ?_TS("User $uname$ $usurname$ ($user$) wants his $gname$ ($vendor_id$, $gift_id$) delivered to $address$, $city$, $postal_code$. Tel.: $phone$. Pesonal id # of Turkish citizen: $personal_id$", 
                        [{uname, UName}, {usurname, USurname}, {user, UId}, {gname, SName}, {vendor_id, Vendor}, {gift_id, Gift}, {address, Address}, {city, City}, {postal_code, PostalCode}, {phone, Phone}, {personal_id, PersonalId}]),
                    MailSubj = ?_T("Kakaranet: new delivery"),
                    nsx_msg:notify_email(MailSubj, MailContent, Email),
                    wf:wire(#alert{text=?_T("Ok, the delivery is on its' way now!")}),
                    wf:wire(simple_lightbox, #hide{}),
                    wf:redirect("/profile/gifts")
            end
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
	fb_utils:pay_dialog(),
        "<h2 class=\"ttl\"><span>"++?_T("My order history")++"</span></h2>",
        #list{class="history-list", id=orders_list, body =
            [order_list_item(MP) || MP <- Orders]},
        #panel{id=more_button_holder, class="btn-holder", body = orders_more_button(Orders)}
    ].

order_list_item(#membership_purchase{membership_package = Package} = MP) ->
    PackageId = Package#membership_package.id,
    %% get package by id from database to get actual avalilable_for_sale option state
    AvailableForSale =
    case nsm_db:get(membership_package, PackageId) of
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
    OverLimit = nsm_membership_packages:check_limit_over(wf:user(), PackageId),
    Button = case AvailableForSale of
        true -> [buy_it_button(PaymentType, PackageId, OverLimit)];
        _ -> []
    end,
    Text = ?_TS("Package $number$, $quota$ quotas, $date$",
        [{number, wf:to_list(No)}, {quota,  wf:to_list(Quota)}, {date, PurchaseDate}]),

    #listitem{body = [#span{text=Text} | Button]}.

buy_it_button(facebook, PackageId, OverLimit)->
  case wf:session(is_facebook) of
    true ->
      #link{class="pay_fb_btn", text=?_T("Buy it"),
        actions=#event{type=click, actions=#script{script="pay_with_fb(\""
          ++ wf:user() ++ "\" ,\""
          ++ PackageId ++ "\","
          ++ atom_to_list(OverLimit) ++");"}}};
    _ ->
      #link{class="pay_fb_stub",text=" ", url=""}
  end;
buy_it_button(PaymentType, PackageId, _)->
    Url = ?_U(lists:concat(["/buy/", PaymentType, "/package_id/", PackageId])),
    #link{class=btn, url=Url,  text=?_T("Buy it")}.

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
