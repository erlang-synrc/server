%% -*- mode: nitrogen -*-
-module(new_tournament).
-compile(export_all).

-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("nsm_db/include/common.hrl").
-include("setup.hrl").
-include("common.hrl").
-include("elements/records.hrl").

-define(GIFTSPERTOURPAGE, 20).
-define(MIN_PRIZE_PERCENT, 10).


main() ->
    case wf:user() /= undefined of
        true  -> main_authorized();
        false -> wf:redirect_to_login(?_U("/login"))
    end.

main_authorized() ->
    webutils:add_to_head({raw,              % this goes to styles.css. Still here for convenience of editing
    "
        <style media='screen' type='text/css'>
            .newtour_title {
                width:250px; 
                height:43px; 
                background: url(../images/tournament/lobby/top_plask.png);
                position:absolute; 
                left:21px; 
                top:-7px; 
                text-align:center;
            }

            .newtour_title_label {
                font-size:18px; 
                color:#fff; 
                font-weight:bold; 
                line-height:42px;
            }

            .newtour_textbox {
                font-size:16px; 
                height:28px;
                border:1px solid #cdcdcd;
            }

            .newtour_orange_button {
                display:block; width:147px; height:52px; background: url(../images/tournament/new_tournament/btn_big_orange_normal.png);
                font:18px 'Gotham Rounded Bold','Trebuchet MS'; color:#fff; text-shadow:0 1px 1px #353535;
                border:0px; line-height:56px;
            }

            .newtour_orange_button:hover {
                display:block; width:147px; height:52px; background: url(../images/tournament/new_tournament/btn_big_orange_hover.png);
                font:18px 'Gotham Rounded Bold','Trebuchet MS'; color:#fff; text-shadow:0 1px 1px #353535;
                border:0px; line-height:56px;
            }

            .newtour_orange_button:active {
                display:block; width:147px; height:52px; background: url(../images/tournament/new_tournament/btn_big_orange_pressed.png);
                font:18px 'Gotham Rounded Bold','Trebuchet MS'; color:#fff; text-shadow:0 1px 1px #353535;
                border:0px; line-height:58px;
            }

            .newtour_browse_button {
                display:block; width:109px; height:28px; background: url(../images/tournament/new_tournament/btn_browse_normal.png);
                font:16px; border:0px; line-height:25px;
            }

            .newtour_browse_button:hover {
                display:block; width:109px; height:28px; background: url(../images/tournament/new_tournament/btn_browse_hover.png);
                font:16px; border:0px; line-height:25px;
            }

            .newtour_browse_button:active {
                display:block; width:109px; height:28px; background: url(../images/tournament/new_tournament/btn_browse_pressed.png);
                font:16px; border:0px; line-height:25px;
            }
        </style>

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
                #label{class="newtour_title_label", body="TURNUVA YARAT"}
            ]
        },
        #panel{id=top_selectors, style="height:420px; font-size:16px; ", body=[
            #label{style="position:absolute; left:42px; top:84px;", text="Turnuva Adı:"},
            #textbox{style="position:absolute; left:137px; top:77px; width:140px; height:28px; font-size:16px;", class="newtour_textbox", id=tournament_name},
            #label{style="position:absolute; left:300px; top:84px;", text="Açiklama:"},
            #textbox{style="position:absolute; left:375px; top:77px; width:180px; height:28px; font-size:16px;", class="newtour_textbox", id=tournament_desc},

            #panel{id=upload, class=file, body=[
                #label{style="position:absolute; left:575px; top:84px;", text="Turnuva Resmi:"},
                #textbox{style="position:absolute; left:692px; top:77px; width:110px; height:28px; font-size:16px;", class="newtour_textbox", id=tournament_official},
                #button{style="position:absolute; left:806px; top:77px; width:110px; height:32px; font-size:16px;", text="BROWSE", id=browse, postback=browse_pressed}
            ]},

            #label{style="position:absolute; left:42px; top:145px;", text="Oyun Türü:"},
            #dropdown {id=tour_game, style="position:absolute; left:126px; top:138px; width:110px; height:32px; font-size:16px; padding-top:2px;", options=[
                        #option { text="OKEY" },
                        #option { text="—" }
            ]},
            #label{style="position:absolute; left:281px; top:145px;", text="Oyun Tipi:"},
            #dropdown {id=tour_esli, style="position:absolute; left:357px; top:138px; width:110px; height:32px; font-size:16px; padding-top:2px;", options=[
                        #option { text="EŞLİ" },
                        #option { text="—" }
            ]},
            #label{style="position:absolute; left:514px; top:145px;", text="Oyun Sayısı:"},
            #dropdown {postback=prize_fund_changed, id=tour_players, style="position:absolute; left:610px; top:138px; width:110px; height:32px; font-size:16px; padding-top:2px;", options=[
                        #option { text="1024" },
                        #option { text="512" },
                        #option { text="256" },
                        #option { text="128" },
                        #option { text="64" },
                        #option { text="16" }
            ]},
            #label{style="position:absolute; left:764px; top:145px;", text="Kota:"},
            #dropdown {postback=prize_fund_changed, id=tour_quota, style="position:absolute; left:807px; top:138px; width:110px; height:32px; font-size:16px; padding-top:2px;", options=[
                        #option { text="10" },
                        #option { text="8" },
                        #option { text="6" },
                        #option { text="4" },
                        #option { text="2" }
            ]},
            #label{style="position:absolute; left:192px; top:197px;", text="Turnuva Türü:"},
            #dropdown {id=tour_type, style="position:absolute; left:296px; top:190px; width:100px; height:32px; font-size:16px; padding-top:2px;", options=[
                        #option { text="Elemeli" },
                        #option { text="—" }
            ]},
            #label{style="position:absolute; left:415px; top:197px;", text="Tarih:"},
            #textbox{id=tour_date, class="newtour_textbox",
                style="position:absolute; left:460px; top:190px; width:140px; height:28px; font-size:16px;
                       background:url(../images/tournament/new_tournament/calendar_icon.png) no-repeat 118px 2px;",
                text= (SD ++ "." ++ SM ++ "." ++ SY)},

            #textbox{id=tour_time, class="newtour_textbox",
                style="position:absolute; left:610px; top:190px; width:80px; height:28px; font-size:16px;
                       background:url(../images/tournament/new_tournament/calendar_icon.png) no-repeat 118px 2px;",
                text= SNH ++ ":00"},

            #panel{style="height:1px; background-color:#c2c2c2; width:960px; margin-left:-25px; position:absolute; top:282px;", body=[]},
            #panel{class="newtour_title", style="top:257px;", body=[
                    #label{class="newtour_title_label", body="ÖDÜLÜ BELİRLE"}
                ]
            },
            #label{style="position:absolute; left:42px; top:350px;", text="Hediye Aralığı:"},
            #label{id=slider_min_value, style="position:absolute; left:160px; top:329px;", text=""},
            #label{id=slider_max_value, style="position:absolute; left:260px; top:329px; text-align:right; width:100px;", text=""},
            #panel{id=slider_panel, style="position:absolute; left:160px; top:352px; width:200px; height:20px;", body=[
                #slider{range = true, id=newtour_slider, max=1000,
                    postback={?MODULE, {newtour_slider}},
                    values=[{min,0}, {max,1000}]
                }
            ]},
            #label{style="position:absolute; left:550px; top:350px;", text="Ödüller:"},
            #panel{id=prize_1, style="position:absolute; left:620px; top:315px; border:1px solid #cdcdcd;", body=[
                #label{style="position:absolute; left:36px; top:-20px;", text="1"},
                #link{postback=deselect_1_prize, body=#image{id=img_prize_1, style="width:80px; height:80px;", image="/images/tournament/new_tournament/question.png"}}
            ]},
            #panel{id=prize_2, style="position:absolute; left:710px; top:315px; border:1px solid #cdcdcd;", body=[
                #label{style="position:absolute; left:36px; top:-20px;", text="2"},
                #link{postback=deselect_2_prize, body=#image{id=img_prize_2, style="width:80px; height:80px;", image="/images/tournament/new_tournament/question.png"}}
            ]},
            #panel{id=prize_3, style="position:absolute; left:800px; top:315px; border:1px solid #cdcdcd;", body=[
                #label{style="position:absolute; left:36px; top:-20px;", text="3"},
                #link{postback=deselect_3_prize, body=#image{id=img_prize_3, style="width:80px; height:80px;", image="/images/tournament/new_tournament/question.png"}}
            ]}
        ]},

        "<center>",
        #button{class="newtour_orange_button", text="YARAT", id=create_button_top, postback=create_pressed},
        "</center>", #br{},

        #panel{id=product_list, body=product_list_paged(1)},
        #br{},
        #panel{style="height:1px; background-color:#c2c2c2; width:860px; margin-left:25px;", body=[]},
        #br{},
        #br{},
        "<center>",
        #button{class="newtour_orange_button", text="YARAT", id=create_button, postback=create_pressed},
        "</center>"
    ].

product_list_paged(Page) ->
    MinPrice = wf:state(slider_min),
    MaxPrice = wf:state(slider_max),
    AllGiftsData = nsm_gifts_db:get_all_gifts(),
    FilteredGiftsData = [Gift || {Gift, _Obj} <- AllGiftsData, Gift#gift.enabled_on_site, (Gift#gift.our_price / 100 >= MinPrice) and (Gift#gift.our_price / 100 =< MaxPrice)],
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
                            ++ ?_T("Kakuş") ++ ":&nbsp;" ++ integer_to_list(OneGift#gift.kakush_point) ++ "</h2>"
                        }
                    ]},
					"<div class='img'>",
                    #link{body=#image{image=OneGift#gift.image_small_url, style="width:144px; height:118px;"}, 
                        postback={show_details, OneGift#gift.description_long, OneGift#gift.image_big_url, OneGift#gift.id}},
                    "</div>
				    <strong class='prod-name' style='padding-bottom:15px; margin-top:-15px;'>",
                    #link{text=gifts:decode_letters(OneGift#gift.gift_name),  
                        postback={show_details, OneGift#gift.description_long, OneGift#gift.image_big_url, OneGift#gift.id}},
                    "</strong>",
				"</div>",
            "</li>"]
            || OneGift <- PageGiftsData
        ],
        "</ul>",
        Buttons
    ].

reset_slider() ->
    PrizePrices = lists:sum([
    case Id of
        undefined -> 0;
        _ -> 
            {ok, {Gift, _}} = nsm_gifts_db:get_gift(Id),
            Gift#gift.our_price
    end || Id <- [wf:state(prize_1), wf:state(prize_2), wf:state(prize_3)]]),

    NPlayers = list_to_integer(wf:q(tour_players)),
    Quota = list_to_integer(wf:q(tour_quota)),
    {ok, PrizeFund} = game_okey_ng_trn_elim:get_prize_fund(Quota, NPlayers),
    MaxOrNot = PrizeFund - PrizePrices,
    Min = PrizeFund * ?MIN_PRIZE_PERCENT div 100,
    Max = case Min > MaxOrNot of
        true -> Min;
        _ -> MaxOrNot
    end,
    wf:state(slider_min_value, Min),
    wf:state(slider_max_value, Max),
    wf:update(slider_min_value, integer_to_list(Min)),
    wf:update(slider_max_value, integer_to_list(Max)),
    event({newtour_slider}).

set_prize(No, Id, ImageUrl) ->
    SNo = integer_to_list(No),
    wf:state(list_to_atom("prize_"++SNo), Id),
    AImg = list_to_atom("img_prize_"++SNo),
    wf:replace(AImg, #image{id=AImg, style="width:80px; height:80px;", image=ImageUrl}).

event({newtour_slider}) ->
    Min1000 = list_to_integer(wf:q(newtour_slider_values_min)),
    Max1000 = list_to_integer(wf:q(newtour_slider_values_max)),
    F = wf:state(slider_min_value),
    T = wf:state(slider_max_value),
    wf:state(slider_min, F + ((T-F) * Min1000) div 1000),
    wf:state(slider_max, F + ((T-F) * Max1000) div 1000),
    wf:update(product_list, product_list_paged(1));


%event({newtour_slider}) -> % all gifts hack
%    Min1000 = list_to_integer(wf:q(newtour_slider_values_min)),
%    Max1000 = list_to_integer(wf:q(newtour_slider_values_max)),
%    F = wf:state(slider_min_value),
%    T = wf:state(slider_max_value),
%    wf:state(slider_min, 0),
%    wf:state(slider_max, 50000),
%    wf:update(product_list, product_list_paged(1));

event({page, Page}) ->
    wf:update(product_list, product_list_paged(Page));

event({show_details, Description, ImageUrl, Id}) ->
    Body = [
        #panel{class=holder, body=[
            "<center>",
            #image{image=ImageUrl, style="margin:10px; width:300px; height:300px;"},
            "</center>",
            gifts:decode_html(Description),
            "<center>",
            #singlerow{cells=[
                #tablecell{
                    body=#cool_button{text=?_T("1-st prize"), postback={chose_1_prize, Id, ImageUrl}, style="display:block;"}
                },
                #tablecell{
                    body=#cool_button{text=?_T("2-nd prize"), postback={chose_2_prize, Id, ImageUrl}, style="display:block;"}
                },
                #tablecell{
                    body=#cool_button{text=?_T("3-rd prize"), postback={chose_3_prize, Id, ImageUrl}, style="display:block;"}
                }
            ]},
            "</center>",
            #grid_clear{}
        ]}
    ],    
    wf:update(simple_panel, webutils:lightbox_panel_template(gift_lightbox, Body, hide_details)),
    wf:wire(simple_lightbox, #show{});

event({chose_1_prize, Id, ImageUrl}) ->
    set_prize(1, Id, ImageUrl),
    reset_slider(),
    event(hide_details);

event({chose_2_prize, Id, ImageUrl}) ->
    set_prize(2, Id, ImageUrl),
    reset_slider(),
    event(hide_details);

event({chose_3_prize, Id, ImageUrl}) ->
    set_prize(3, Id, ImageUrl),
    reset_slider(),
    event(hide_details);


event(browse_pressed) ->
    wf:wire(#alert{text=?_T("Not ready yet.")});

event(create_pressed) ->
    TourName = wf:q(tournament_name),
    TourDesc = wf:q(tournament_desc),
    TourGame = case wf:q(tour_game) of
        "OKEY" -> game_okey;
        "TAVLA" -> game_tavla
    end,
    TourType = case wf:q(tour_type) of
        "Elemeli" -> elimination;
        _ -> unknown
    end,
    TourDateSource = wf:q(tour_date),
    TourDateChunks = lists:reverse([list_to_integer(C) || C <- ling:split(TourDateSource, ".")]),
    TourDate = list_to_tuple(TourDateChunks),

    TourTimeSource = wf:q(tour_time),
    TourTimeChunks = [list_to_integer(C) || C <- ling:split(TourTimeSource, ":")],
    TourTime = list_to_tuple(TourTimeChunks ++ [0]),

    TourPlayers = list_to_integer(wf:q(tour_players)),
    TourQuota = list_to_integer(wf:q(tour_quota)),
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
                    wf:wire(#event{postback={start_tournament, TourName, TourDesc, TourDate, TourTime, TourPlayers, TourQuota, Prize1, Prize2, Prize3, TourType, TourGame}})
            end
    end;

event({start_tournament, TourName, TourDesc, TourDate, TourTime, TourPlayers, TourQuota, Prize1, Prize2, Prize3, TourType, TourGame}) ->
    TID = nsm_tournaments:create(wf:user(), TourName, TourDesc, TourDate, TourTime, TourPlayers, TourQuota, [Prize1, Prize2, Prize3], TourType, TourGame),
    AllowedUsers = ["doxtop","demo1","maxim","sustel","ahmettez",
                    "kunthar","alice","kate","serg","imagia","willbe"],
    [case nsm_db:get(user,User) of
           {ok, _} -> nsm_tournaments:join(User,TID);
           {error, _} -> ?INFO("TOURNAMENT DEFAULT USERS SKIP: ~p",[User])
     end || User <- AllowedUsers],
    nsm_srv_tournament_lobby_sup:start_lobby(TID),
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
    STourGame = case TourGame of
        game_okey -> ?_T("OKEY");
        game_tavla -> ?_T("TAVLA");
        _ -> "?"
    end,
    STourType = case TourType of
        elimination -> ?_T("elimination");
        _ -> "?"
    end,
    Desc = lists:flatten( URL ++ "|" ++ wf:user() ++ "|" ++ TourName ++ "|" ++ STourDesc ++ "|" ++ STourDate ++ "|" ++ STourTime ++ "|" ++ STourPlayers ++ "|" ++ STourQuota ++ "|" ++ STourType ++ "|" ++ STourGame),
    webutils:post_user_system_message(Desc),
    wf:redirect(URL);

event(prize_fund_changed) ->
    case wf:q(tour_players) of
        "16" -> wf:replace(tour_quota, #dropdown {postback=prize_fund_changed, id=tour_quota, style="position:absolute; left:807px; top:138px; width:110px; height:32px; font-size:16px; padding-top:2px;", options=[
                #option { text="10" },
                #option { text="8" }
            ]});
        _ -> wf:replace(tour_quota, #dropdown {postback=prize_fund_changed, id=tour_quota, style="position:absolute; left:807px; top:138px; width:110px; height:32px; font-size:16px; padding-top:2px;", options=[
                #option { text="10" },
                #option { text="8" },
                #option { text="6" },
                #option { text="4" },
                #option { text="2" }
            ]})
    end,
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

