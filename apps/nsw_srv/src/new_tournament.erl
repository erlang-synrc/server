-module(new_tournament).
-compile(export_all).

-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("nsm_db/include/common.hrl").
-include_lib("nsm_db/include/config.hrl").
-include("setup.hrl").
-include("common.hrl").
-include("elements/records.hrl").

-define(GIFTSPERTOURPAGE, 20).
-define(MIN_PRIZE_PERCENT, 10).


main() ->
    case wf:user() /= undefined of
        true  -> case nsm_accounts:user_paid(wf:user()) of
            true -> main_authorized();
            false -> wf:redirect_to_login(?_U("/login"))
        end;
        false -> wf:redirect_to_login(?_U("/login"))
    end.

main_authorized() ->
    webutils:add_to_head({raw,
    "
        <link rel='stylesheet' href='/nitrogen/datepicker/css/datepicker.css' type='text/css' />
	    <script type='text/javascript' src='/nitrogen/datepicker/js/datepicker_tr.js'></script>

        <link rel='stylesheet' href='/nitrogen/timepicker/jquery.ui.timepicker.css' type='text/css' />
	    <script type='text/javascript' src='/nitrogen/timepicker/jquery.ui.timepicker.js'></script>

        <script>
        window.onload = function(){
            $('.wfid_tour_date').DatePicker({
	            format:'d.m.Y',
	            date: $('.wfid_tour_date').val(),
	            current: $('.wfid_tour_date').val(),
	            starts: 1,
	            position: 'bottom',
	            onBeforeShow: function(){
		            $('.wfid_tour_date').DatePickerSetDate($('.wfid_tour_date').val(), true);
	            },
	            onChange: function(formated, dates){
		            $('.wfid_tour_date').val(formated);
		            $('.wfid_tour_date').DatePickerHide();
	            }
            });
            $('.wfid_tour_time').timepicker({
                hourText: 'Saat',
                minuteText: 'Dakika',
                amPmText: ['AM', 'PM'],
                closeButtonText: 'Tamam',
                showCloseButton: true,
                timeSeparator: ':',
                duration: 300
            });
        };
        new Image('../images/tournament/new_tournament/btn_big_orange_hover.png');
        new Image('../images/tournament/new_tournament/btn_big_orange_pressed.png');
        </script>
    "}),
    wf:state(slider_min, 0),
    wf:state(slider_max, 50000),
    wf:state(slider_min_value, 0),
    wf:state(slider_max_value, 50000),
    #template { file=code:priv_dir(nsw_srv)++"/templates/bare.html" }.

   
title() -> webutils:title(?MODULE).

body() ->
    #template{file=code:priv_dir(nsw_srv)++"/templates/info_page.html"}.

content() ->
    {{Y, M, D}, {H, _, _}} = calendar:local_time(),
    SY = integer_to_list(Y),
    SM = integer_to_list(M),
    SD = integer_to_list(D),
    SNH = integer_to_list( (H+1) rem 24),
    wf:wire(#event{postback=prize_fund_changed}),
    [
        #panel{class="newtour_title", body=[
                #label{class="newtour_title_label", body=?_T("CREATE TOURNAMENT")}
            ]
        },
        #panel{id=top_selectors, style="height:440px; font-size:16px; ", body=[
            #label{style="position:absolute; left:-18px; top:84px; width:150px; text-align:right;", text=?_T("Tour. Name:")},
            #textbox{style="position:absolute; left:137px; top:77px; width:140px; height:28px; font-size:16px;", class="newtour_textbox", id=tournament_name},
            #label{style="position:absolute; left:300px; top:84px; width:100px; text-align:right;", text=?_T("Description:")},
            #textbox{style="position:absolute; left:405px; top:77px; width:508px; height:28px; font-size:16px;", class="newtour_textbox", id=tournament_desc},

            #label{style="position:absolute; left:-28px; top:145px; width:150px; text-align:right;", text=?_T("Game Type:")},
            #dropdown {id=tour_game, style="position:absolute; left:126px; top:138px; width:110px; height:32px; font-size:16px; padding-top:2px;", options=[
                        #option { text="OKEY" }
            ]},
            #label{style="position:absolute; left:253px; top:145px; width:100px; text-align:right;", text=?_T("Game Mode:")},
            #dropdown {id=tour_esli, style="position:absolute; left:357px; top:138px; width:110px; height:32px; font-size:16px; padding-top:2px;", options=[
                        #option { text=?_T("Standard"), value=standard },
                        #option { text=?_T("Even/Odd"), value=evenodd},
                        #option { text=?_T("Color"), value=color }
            ]},
            #label{style="position:absolute; left:486px; top:145px; width:120px; text-align:right;", text=?_T("Players total:")},
            #dropdown {postback=prize_fund_and_tours_and_quota_changed, id=tour_players, style="position:absolute; left:610px; top:138px; width:110px; height:32px; font-size:16px; padding-top:2px;", options=[
                        #option { text="2048" },
                        #option { text="1024" },
                        #option { text="512" },
                        #option { text="256" },
                        #option { text="128" },
                        #option { text="64" },
                        #option { text="16" }
            ]},
            #label{style="position:absolute; left:723px; top:145px; width:80px; text-align:right;", text=?_T("Quota:")},
            #dropdown {postback=prize_fund_and_tours_changed, id=tour_quota, style="position:absolute; left:807px; top:138px; width:110px; height:32px; font-size:16px; padding-top:2px;", options=[
                        #option { text="10" },
                        #option { text="8" },
                        #option { text="6" },
                        #option { text="4" },
                        #option { text="2" }
            ]},

            #label{style="position:absolute; left:-9px; top:197px; width:150px; text-align:right;", text=?_T("Tournament Type:")},
            #dropdown {postback=tournament_type_changed, id=tour_type, style="position:absolute; left:146px; top:190px; width:90px; height:32px; font-size:16px; padding-top:2px;", options=[
                        #option { text="Elemeli", value=elimination },
                        #option { text="POINTING", value=pointing }
            ]},

            #label{style="position:absolute; left:252px; top:197px; width:100px; text-align:right;", text=?_T("Game Speed")++":"},
            #dropdown {id=tour_speed, style="position:absolute; left:356px; top:190px; width:90px; height:32px; font-size:16px; padding-top:2px;", options=[
                        #option { text=?_T("Fast") },
                        #option { text=?_T("Normal") },
                        #option { text=?_T("Slow") }
            ]},

            #label{style="position:absolute; left:450px; top:197px; width:60px; text-align:right;", text=?_T("Date:")},
            #textbox{id=tour_date, class="newtour_textbox",
                style="position:absolute; left:515px; top:190px; width:140px; height:28px; font-size:16px;
                       background:url(../images/tournament/new_tournament/calendar_icon.png) no-repeat 118px 2px;",
                text= (SD ++ "." ++ SM ++ "." ++ SY)},
            #textbox{id=tour_time, class="newtour_textbox",
                style="position:absolute; left:665px; top:190px; width:80px; height:28px; font-size:16px;
                       background:url(../images/tournament/new_tournament/calendar_icon.png) no-repeat 118px 2px;",
                text= SNH ++ ":00"},

            #label{style="position:absolute; left:722px; top:197px; width:100px; text-align:right;", text=?_T("Tours")++":"},
            #dropdown {postback=prize_fund_changed, id=tour_tours, style="position:absolute; left:827px; top:190px; width:90px; height:32px; font-size:16px; padding-top:2px;", options=[
                        #option { text="6" },
                        #option { text="8" }
            ]},


            #label{style="position:absolute; left:200px; top:255px; width:100px; text-align:right;", text=?_T("Plan")++":"},
            #panel{style="position:absolute; left:310px; top:255px;", id=plan_desc, body=["..."]},

            #panel{style="height:1px; background-color:#c2c2c2; width:960px; margin-left:-25px; position:absolute; top:332px;", body=[]},
            #panel{class="newtour_title", style="top:307px;", body=[
                    #label{class="newtour_title_label", body=?_T("PRIZES TO CHOOSE")}
                ]
            },
            #label{style="position:absolute; left:42px; top:400px;", text=?_T("Cost Range:")},
            #label{id=slider_min_value, style="position:absolute; left:160px; top:379px;", text=""},
            #label{id=slider_max_value, style="position:absolute; left:260px; top:379px; text-align:right; width:100px;", text=""},
            #panel{id=slider_panel, style="position:absolute; left:160px; top:402px; width:200px; height:20px;", body=[
                #slider{range = true, id=newtour_slider, max=1000,
                    postback={?MODULE, {newtour_slider}},
                    values=[{min,0}, {max,1000}]
                }
            ]},
            #label{style="position:absolute; left:550px; top:390px;", text=?_T("Prizes:")},
            #panel{id=prize_1, style="position:absolute; left:620px; top:365px;", body=[
                #label{style="position:absolute; left:36px; top:-20px;", text="1"},
                #panel{class="newtour_prize_holder", body=
                    #link{postback=deselect_1_prize, body=#image{id=img_prize_1, style="max-width:80px; max-height:80px;", image="/images/tournament/new_tournament/question.png"}}
                }
            ]},
            #panel{id=prize_2, style="position:absolute; left:710px; top:365px;", body=[
                #label{style="position:absolute; left:36px; top:-20px;", text="2"},
                #panel{class="newtour_prize_holder", body=
                    #link{postback=deselect_2_prize, body=#image{id=img_prize_2, style="max-width:80px; max-height:80px;", image="/images/tournament/new_tournament/question.png"}}
                }
            ]},
            #panel{id=prize_3, style="position:absolute; left:800px; top:365px;", body=[
                #label{style="position:absolute; left:36px; top:-20px;", text="3"},
                #panel{class="newtour_prize_holder", body=
                    #link{postback=deselect_3_prize, body=#image{id=img_prize_3, style="max-width:80px; max-height:80px;", image="/images/tournament/new_tournament/question.png"}}
                }
            ]}
        ]},

        "<center>",
        #button{class="newtour_orange_button", text=?_T(" CREATE "), id=create_button_top, postback=create_pressed},
        "</center>", #br{},

        #panel{id=product_list, body=product_list_paged(1)},
        #br{},
        #panel{style="height:1px; background-color:#c2c2c2; width:860px; margin-left:25px;", body=[]},
        #br{},
        #br{},
        "<center>",
        #button{class="newtour_orange_button", text=?_T(" CREATE "), id=create_button, postback=create_pressed},
        "</center>"
    ].

product_list_paged(Page) ->
    MinPrice = wf:state(slider_min),
    MaxPrice = wf:state(slider_max),
    AllGiftsData = nsm_gifts_db:get_all_gifts(),
    FilteredGiftsData = [Gift || {Gift, _Obj} <- AllGiftsData, Gift#gift.enabled_on_site, (Gift#gift.real_price >= MinPrice * 100) and (Gift#gift.real_price =< MaxPrice * 100)],
    PageGiftsData = lists:sublist( 
        lists:sort(
            fun(A, B) -> 
                if
                    A#gift.our_price < B#gift.our_price -> true;
                    A#gift.our_price == B#gift.our_price -> A#gift.id =< B#gift.id;
                    true -> false
                end
            end,
            FilteredGiftsData
        ), 
        (Page-1) * ?GIFTSPERTOURPAGE + 1, ?GIFTSPERTOURPAGE),

    Buttons = case length(FilteredGiftsData) > ?GIFTSPERTOURPAGE of
        true ->
            #panel{class="paging-2", style="padding: 10px 0px 0px 0px;", body=[
                #panel{class="center", body=[
                    #list{body=[
                       case Page of
                           1 -> #listitem{body=#link{text="<", postback={nothing}, class="inactive"}};
                           _ -> #listitem{body=#link{text="<", postback={page, Page - 1}}}
                        end,
                        [case N of
                            Page -> #listitem{body=#link{text=integer_to_list(N), postback={nothing}, class="inactive", 
                                style="color:#444444; font-weight:bold;"}};
                            _ -> #listitem{body=#link{text=integer_to_list(N), postback={page, N}}}
                        end
                        || N <- lists:seq(1, (length(FilteredGiftsData) - 1) div ?GIFTSPERTOURPAGE + 1)],
                        case Page * ?GIFTSPERTOURPAGE >= length(FilteredGiftsData) of                 
                            true -> #listitem{body=#link{text=">", postback={nothing}, class="inactive"}};
                            false -> #listitem{body=#link{text=">", postback={page, Page + 1}}}
                        end
                   ]}
               ]}
            ]};
       false ->
            []
    end,
    [   
        "<ul class='prod-list'>",
        [
			["<li>
				<div class='box'>",
                    #singlerow { style="width:100%;", cells=[
                        #tablecell{body=
        					"<h2 class='head'>"++ ?_T("Price") ++ ":&nbsp;" ++ integer_to_list(OneGift#gift.kakush_currency) ++ "<br>" 
                            ++ ?_T("Kakush") ++ ":&nbsp;" ++ site_utils:long_integer_to_list(OneGift#gift.kakush_point) ++ "</h2>"
                        },
                        #tablecell{
                            style="text-align:right; background:#9d9d9d; color:#fff; 
                                   font-size:18px; padding-right:5px"
                        }
                    ]},
					"<div class='img'>",
                    #link{body=#image{image=OneGift#gift.image_small_url, style="max-width:144px; max-height:118px;"}, 
                        postback={show_details, OneGift#gift.description_long, OneGift#gift.image_big_url, OneGift#gift.id}},
                    "</div>
				    <strong class='prod-name' style='padding-bottom:15px; margin-top:-15px;'>",
                    #link{text=site_utils:decode_letters(OneGift#gift.gift_name),  
                        postback={show_details, OneGift#gift.description_long, OneGift#gift.image_big_url, OneGift#gift.id}},
                    "</strong>",
				"</div>",
            "</li>"]
            || OneGift <- PageGiftsData
        ],
        "</ul>",
        Buttons
    ].

get_prizes_total(Prizes) ->
    lists:sum([
        case Id of
            undefined -> 0;
            _ -> 
                {Status, Res} = nsm_gifts_db:get_gift(Id),
                case Status of 
                    ok -> {Gift, _} = Res, Gift#gift.our_price;
                    _ -> 0
                end
        end 
    || Id <- Prizes]).

get_cur_prize_fund() ->
    NPlayers = list_to_integer(wf:q(tour_players)),
    Quota = case wf:state(workaround_quota) of 
        undefined -> 
            list_to_integer(wf:q(tour_quota));
        Q -> 
            wf:state(workaround_quota, undefined),
            Q
    end,
    Tours = case wf:state(workaround_tours) of
        undefined ->
            list_to_integer(wf:q(tour_tours));
        T ->
            wf:state(workaround_tours, undefined),
            T
    end,
    case game_okey_ng_trn_elim:get_prize_fund(Quota, NPlayers, Tours) of
        {ok, PrizeFund} -> 100*PrizeFund;
        _ -> 0
    end.

reset_slider() ->
    PrizeFund = get_cur_prize_fund(),
    PrizePrices = get_prizes_total([wf:state(prize_1), wf:state(prize_2), wf:state(prize_3)]),
    MaxOrNot = (PrizeFund - PrizePrices),
    Min = PrizeFund * ?MIN_PRIZE_PERCENT div 100,
    Max = case Min > MaxOrNot of
        true -> Min;
        _ -> MaxOrNot
    end,
    wf:state(slider_min_value, Min div 100),
    wf:state(slider_max_value, Max div 100),
    {A, B, C, D} = nsm_gifts_db:get_factors(),
    {_, _, KMin} = nsm_gifts_tools:calc_prices(Min, A, B, C, D),
    {_, _, KMax} = nsm_gifts_tools:calc_prices(Max, A, B, C, D),
    wf:update(slider_min_value, site_utils:long_integer_to_list(KMin)),
    wf:update(slider_max_value, site_utils:long_integer_to_list(KMax)),
    event({newtour_slider}).

set_prize(No, Id, ImageUrl) ->
    SNo = integer_to_list(No),
    wf:state(list_to_atom("prize_"++SNo), Id),
    AImg = list_to_atom("img_prize_"++SNo),
    wf:replace(AImg, #image{id=AImg, style="max-width:80px; max-height:80px;", image=ImageUrl}).

api_event(Name, Tag, Args) ->  webutils:api_event(Name, Tag, Args).

event({newtour_slider}) ->
    Min1000 = list_to_integer(wf:q(newtour_slider_values_min)),
    Max1000 = list_to_integer(wf:q(newtour_slider_values_max)),
    F = wf:state(slider_min_value),
    T = wf:state(slider_max_value),
    wf:state(slider_min, F + ((T-F) * Min1000) div 1000),
    wf:state(slider_max, F + ((T-F) * Max1000) div 1000),
    wf:wire(#event{postback=update_product_list});

event(update_product_list) ->
    Players = list_to_integer(wf:q(tour_players)),
    Quota = list_to_integer(wf:q(tour_quota)),
    Tours = list_to_integer(wf:q(tour_tours)),

    {ok,PlanDesc1} = rpc:call(?GAMESRVR_NODE, game_okey_ng_trn_elim,get_plan_desc,[Quota,Players,Tours]),

    PlanI = lists:seq(1, length(PlanDesc1)),
    PlanDesc = #table{rows=[
        #tablerow{cells=
            [#tablecell{body=integer_to_list(I), style=case I of
                    1 -> "padding-bottom:4px; text-align:center; border-bottom:1px solid #AAA;";
                    _ -> "padding-bottom:4px; text-align:center; border-bottom:1px solid #AAA; border-left:1px solid #AAA;"
                end
            }
            || I <- PlanI]
        },
        #tablerow{cells=
            [#tablecell{body=lists:nth(I, PlanDesc1), style=case I of
                    1 -> "padding-left:6px; padding-right:6px; padding-top:4px; text-align:center;";
                    _ -> "padding-left:6px; padding-right:6px; padding-top:4px; text-align:center; border-left:1px solid #AAA;"
                end
            }
            || I <- PlanI]
        }
    ], style="margin-top:-13px;"},
    wf:update(plan_desc, PlanDesc),
    wf:update(product_list, product_list_paged(1));

event({page, Page}) ->
    wf:update(product_list, product_list_paged(Page));

event({show_details, Description, ImageUrl, Id}) ->
    Body = [
        #panel{class=holder, body=[
            "<center>",
            #image{image=ImageUrl, style="margin:10px; max-width:300px; max-height:300px;"},
            "</center>",
            site_utils:decode_html(Description),
            "<center>",
            #singlerow{cells=[
                #tablecell{
                    body=#cool_button{text=?_T("1-st prize"), postback={chose_prize, 1, Id, ImageUrl}, style="display:block;"}
                },
                #tablecell{
                    body=#cool_button{text=?_T("2-nd prize"), postback={chose_prize, 2, Id, ImageUrl}, style="display:block;"}
                },
                #tablecell{
                    body=#cool_button{text=?_T("3-rd prize"), postback={chose_prize, 3, Id, ImageUrl}, style="display:block;"}
                }
            ]},
            "</center>",
            #grid_clear{}
        ]}
    ],    
    wf:update(simple_panel, webutils:lightbox_panel_template(gift_lightbox, Body, hide_details)),
    wf:wire(simple_lightbox, #show{});

event({chose_prize, No, Id, ImageUrl}) ->
    PrizeFund = get_cur_prize_fund(),
    PrizePrices = get_prizes_total([wf:state(prize_1), wf:state(prize_2), wf:state(prize_3)]),
    MaxOrNot = (PrizeFund - PrizePrices),
    Min = PrizeFund * ?MIN_PRIZE_PERCENT div 100,
    Max = case Min > MaxOrNot of
        true -> Min;
        _ -> MaxOrNot
    end,
    ThisPrizeCost = get_prizes_total([Id]),
    case (Max >= ThisPrizeCost) and (Min =< ThisPrizeCost) of
        true ->
            set_prize(No, Id, ImageUrl),
            reset_slider(),
            event(hide_details);
        false ->
            wf:wire(#alert{text=?_T("Sorry, you need a bigger tournament for this prize.")}),
            event(hide_details)
    end;

event(browse_pressed) ->
    wf:wire(#alert{text=?_T("Not ready yet.")});

event(create_pressed) ->
    TourName = wf:q(tournament_name),
    TourDesc = wf:q(tournament_desc),
    {TourGame,TourGameType} = case wf:q(tour_game) of
        "OKEY" ->  {game_okey, case wf:q(tour_esli) of A when is_list(A) -> list_to_atom(A); _ -> standard end};
        "TAVLA" -> {game_tavla, standard}
    end,
    TourType = wf:q(tour_type),
    TFast = ?_T("Fast"),
    TNormal = ?_T("Normal"),
    TSlow = ?_T("Slow"),
    TourSpeed = case wf:q(tour_speed) of
         TFast -> fast;
         TNormal -> normal;
         TSlow -> slow
    end,
    TourDateSource = wf:q(tour_date),
    TourDateChunks = lists:reverse([list_to_integer(C) || C <- ling:split(TourDateSource, ".")]),
    TourDate = list_to_tuple(TourDateChunks),

    TourTimeSource = wf:q(tour_time),
    TourTimeChunks = [list_to_integer(C) || C <- ling:split(TourTimeSource, ":")],
    TourTime = list_to_tuple(TourTimeChunks ++ [0]),

    TourPlayers = list_to_integer(wf:q(tour_players)),
    TourQuota = list_to_integer(wf:q(tour_quota)),
    Tours = list_to_integer(wf:q(tour_tours)),
    Prize1 = wf:state(prize_1),
    Prize2 = wf:state(prize_2),
    Prize3 = wf:state(prize_3),
    if
        (Prize1 == undefined) ->
            wf:wire(#alert{text=?_T("Please, choose at least the main prize!")});
        (Prize2 == undefined) and (Prize3 /= undefined) ->
            wf:wire(#alert{text=?_T("Please, choose second prize!")});
        true ->
            case TourName == "" of
                true -> 
                    wf:wire(#alert{text=?_T("Please, provide tournament name!")}); 
                false ->
                    wf:replace(create_button, #panel{class="view_media_other_attachment", style="float:none", body=#panel{class=loading_spiner}}),
                    wf:replace(create_button_top, #panel{class="view_media_other_attachment", style="float:none", body=#panel{class=loading_spiner}}),
                    wf:wire(#event{postback={start_tournament, TourName, TourDesc, TourDate, TourTime, TourPlayers, TourQuota, Prize1, Prize2, Prize3, TourType, TourGame, TourGameType, Tours, TourSpeed}})
            end
    end;

event({start_tournament, TourName, TourDesc, TourDate, TourTime, TourPlayers, TourQuota, Prize1, Prize2, Prize3, TourType, TourGame, TourGameType, Tours, TourSpeed}) ->
    TID = nsm_tournaments:create(wf:user(), TourName, TourDesc, TourDate, TourTime, TourPlayers, TourQuota, [Prize1, Prize2, Prize3], TourType, TourGame, TourGameType, Tours, TourSpeed),
    nsm_tournaments:join(wf:user(), TID),
    AllowedUsers = lists:subtract(["maxim","sustel","ahmettez","serg","doxtop"], [wf:user()]),
    [case nsm_db:get(user,User) of
           {ok, _} -> nsm_tournaments:join(User,TID);
           {error, _} -> ?INFO("TOURNAMENT DEFAULT USERS SKIP: ~p",[User])
     end || User <- AllowedUsers],

    Zone = TID div 1000000,
    GameSrv = "game@srv" ++ integer_to_list(Zone) ++ ".kakaranet.com",
    NodeAtom = case Zone of
                   4 -> nsx_opt:get_env(nsm_db, game_srv_node, 'game@doxtop.cc');
                   _ -> list_to_atom(GameSrv)
              end,

    rpc:call(NodeAtom,nsm_srv_tournament_lobby_sup,start_lobby,[TID]),
    URL = ?_U("tournament/lobby/id/")++integer_to_list(TID),

    % notification via text messaging
    {TY, TM, TD} = TourDate,
    {Th, Tm, _} = TourTime,
    STourDesc = case TourDesc of
        "" -> "";
        _ -> " ("++TourDesc++")"
    end,
    STourDate = integer_to_list(TY) ++ "." ++ integer_to_list(TM) ++ "." ++ integer_to_list(TD),
    STourTime = integer_to_list(Th) ++ ":" ++ case Tm<10 of true -> "0"++integer_to_list(Tm); false -> integer_to_list(Tm) end,
    STourPlayers = integer_to_list(TourPlayers),
    STourQuota = integer_to_list(TourQuota),
    STourGame = site_utils:game_to_string(TourGame),
    SKakush = integer_to_list(get_prizes_total([wf:state(prize_1), wf:state(prize_2), wf:state(prize_3)])),
    STourType = case TourType of
        elimination -> ?_T("elimination");
        pointing -> ?_T("pointing");
        _ -> "?"
    end,
    Desc = lists:flatten( URL ++ "|" ++ wf:user() ++ "|" ++ TourName ++ "|" ++ STourDesc ++ "|" ++ STourDate ++ "|" ++ STourTime ++ "|" ++ STourPlayers ++ "|" ++ STourQuota ++ "|" ++ STourType ++ "|" ++ STourGame ++ "|" ++ SKakush),
    fb_utils:announce_tournament(wf:user(), TID),
    webutils:post_user_system_message(Desc),
    wf:redirect(URL);

event(tournament_type_changed) ->
    case wf:q(tour_type) of
        "elimination" ->
            wf:replace(tour_players, #dropdown {postback=prize_fund_and_tours_and_quota_changed, id=tour_players, style="position:absolute; left:610px; top:138px; width:110px; height:32px; font-size:16px; padding-top:2px;", options=[
                #option { text=integer_to_list(P) }
            || P <- [2048, 1024, 512, 256, 128, 64, 32, 16] ]});
        "pointing" ->
            wf:replace(tour_players, #dropdown {postback=prize_fund_and_tours_and_quota_changed, id=tour_players, style="position:absolute; left:610px; top:138px; width:110px; height:32px; font-size:16px; padding-top:2px;", options=[
                #option { text=integer_to_list(P) }
            || P <- [1020, 500, 400, 200, 300, 100, 60, 40] ]})
    end,
    wf:wire(#event{postback=prize_fund_and_tours_and_quota_changed});

event(prize_fund_and_tours_and_quota_changed) ->
    case wf:q(tour_type) of
        "elimination" ->
            case wf:q(tour_players) of
                "16" -> wf:state(workaround_quota, 10),
                    wf:replace(tour_quota, #dropdown {postback=prize_fund_and_tours_changed, id=tour_quota, style="position:absolute; left:807px; top:138px; width:110px; height:32px; font-size:16px; padding-top:2px;", options=[
                        #option { text="10" },
                        #option { text="8" }
                    ]});
                _ -> wf:state(workaround_quota, 10),
                    wf:replace(tour_quota, #dropdown {postback=prize_fund_and_tours_changed, id=tour_quota, style="position:absolute; left:807px; top:138px; width:110px; height:32px; font-size:16px; padding-top:2px;", options=[
                        #option { text="10" },
                        #option { text="8" },
                        #option { text="6" },
                        #option { text="4" },
                        #option { text="2" }
                    ]})
            end;
        "pointing" ->
            case wf:q(tour_players) of
                "40" -> wf:state(workaround_quota, 10),
                    wf:replace(tour_quota, #dropdown {postback=prize_fund_and_tours_changed, id=tour_quota, style="position:absolute; left:807px; top:138px; width:110px; height:32px; font-size:16px; padding-top:2px;", options=[
                        #option { text="10" }
                    ]});
                "60" -> wf:state(workaround_quota, 10),
                    wf:replace(tour_quota, #dropdown {postback=prize_fund_and_tours_changed, id=tour_quota, style="position:absolute; left:807px; top:138px; width:110px; height:32px; font-size:16px; padding-top:2px;", options=[
                        #option { text="10" },
                        #option { text="8" },
                        #option { text="6" }
                    ]});
                "100" -> wf:state(workaround_quota, 10),
                    wf:replace(tour_quota, #dropdown {postback=prize_fund_and_tours_changed, id=tour_quota, style="position:absolute; left:807px; top:138px; width:110px; height:32px; font-size:16px; padding-top:2px;", options=[
                        #option { text="10" },
                        #option { text="8" },
                        #option { text="6" }
                    ]});
                 _ -> wf:state(workaround_quota, 10),
                       wf:replace(tour_quota, #dropdown {postback=prize_fund_and_tours_changed, id=tour_quota, style="position:absolute; left:807px; top:138px; width:110px; height:32px; font-size:16px; padding-top:2px;", options=[
                        #option { text="10" },
                        #option { text="8" },
                        #option { text="6" },
                        #option { text="4" },
                        #option { text="2" }
                    ]})
             end
    end,
    wf:wire(#event{postback=prize_fund_and_tours_changed});

event(prize_fund_and_tours_changed) ->
    Tours = game_okey_ng_trn_elim:get_tours(list_to_integer(wf:q(tour_quota)), list_to_integer(wf:q(tour_players)) ), 

    ?INFO("Tours: ~p",[Tours]),
            case Tours of
                [] -> ?ERROR("No sush plan: ~p quota, ~p players!", [list_to_integer(wf:q(tour_quota)), list_to_integer(wf:q(tour_players))]);
                _ ->
                    wf:state(workaround_tours, hd(Tours)),
                    wf:replace(tour_tours, #dropdown {postback=prize_fund_changed, id=tour_tours, style="position:absolute; left:827px; top:190px; width:90px; height:32px; font-size:16px; padding-top:2px;", options=[
                         #option { text=integer_to_list(T) }
                    || T <- Tours]}),
                    event(prize_fund_changed)
            end;

event(prize_fund_changed) ->
    set_prize(1, undefined, "/images/tournament/new_tournament/question.png"),
    set_prize(2, undefined, "/images/tournament/new_tournament/question.png"),
    set_prize(3, undefined, "/images/tournament/new_tournament/question.png"),
    reset_slider();

event(deselect_1_prize) ->
    set_prize(1, undefined, "/images/tournament/new_tournament/question.png"),
    reset_slider();

event(deselect_2_prize) ->
    set_prize(2, undefined, "/images/tournament/new_tournament/question.png"),
    reset_slider();

event(deselect_3_prize) ->
    set_prize(3, undefined, "/images/tournament/new_tournament/question.png"),
    reset_slider();

event(hide_details) ->
    wf:wire(simple_lightbox, #hide{});

event(Any) ->
    webutils:event(Any).
