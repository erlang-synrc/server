%% -*- mode: nitrogen -*-
-module(new_tournament).
-compile(export_all).

-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("nsm_db/include/common.hrl").
-include("setup.hrl").
-include("common.hrl").
-include("elements/records.hrl").

-define(GIFTSPERTOURPAGE, 20).

main() ->
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
	    <script type='text/javascript' src='/nitrogen/datepicker/js/datepicker.js'></script>

        <script>
        window.onload = function(){
            $('#inputDate').DatePicker({
	            format:'d.m.Y',
	            date: $('#inputDate').val(),
	            current: $('#inputDate').val(),
	            starts: 1,
	            position: 'r',
	            onBeforeShow: function(){
		            $('#inputDate').DatePickerSetDate($('#inputDate').val(), true);
	            },
	            onChange: function(formated, dates){
		            $('#inputDate').val(formated);
		            $('#inputDate').DatePickerHide();
	            }
            });
        };
        new Image('../images/tournament/new_tournament/btn_big_orange_hover.png');
        new Image('../images/tournament/new_tournament/btn_big_orange_pressed.png');
        </script>
    "}),
    wf:state(slider_min, 0),
    wf:state(slider_max, 50000),
    #template { file=code:priv_dir(nsw_srv)++"/templates/bare.html" }.

   
title() -> webutils:title(?MODULE).

body() ->
    #template{file=code:priv_dir(nsw_srv)++"/templates/info_page.html"}.

content() ->
    {{Y, M, D}, _} = calendar:now_to_datetime(erlang:now()),
    SY = integer_to_list(Y),
    SM = integer_to_list(M),
    SD = integer_to_list(D),
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
            #label{style="position:absolute; left:575px; top:84px;", text="Turnuva Resmi:"},
            #textbox{style="position:absolute; left:692px; top:77px; width:110px; height:28px; font-size:16px;", class="newtour_textbox", id=tournament_official},
            #button{style="position:absolute; left:806px; top:77px; width:110px; height:32px; font-size:16px;", text="BROWSE", id=browse},

            #label{style="position:absolute; left:42px; top:145px;", text="Oyun Türü:"},
            #dropdown {style="position:absolute; left:126px; top:138px; width:110px; height:32px; font-size:16px; padding-top:2px;", options=[
                        #option { text="OKEY" },
                        #option { text="TAVLA" }
            ]},
            #label{style="position:absolute; left:264px; top:145px;", text="Oyun Tipi:"},
            #dropdown {style="position:absolute; left:340px; top:138px; width:110px; height:32px; font-size:16px; padding-top:2px;", options=[
                        #option { text="EŞLİ" },
                        #option { text="—" }
            ]},
            #label{style="position:absolute; left:480px; top:145px;", text="Oyun Sayısı:"},
            #dropdown {style="position:absolute; left:576px; top:138px; width:160px; height:32px; font-size:16px; padding-top:2px;", options=[
                        #option { text="1000–2000" },
                        #option { text="2000–5000" },
                        #option { text="5000–10 000" },
                        #option { text="10 000–20 000" }
            ]},
            #label{style="position:absolute; left:764px; top:145px;", text="Kota:"},
            #dropdown {style="position:absolute; left:807px; top:138px; width:110px; height:32px; font-size:16px; padding-top:2px;", options=[
                        #option { text="+10 000" },
                        #option { text="+20 000" },
                        #option { text="+50 000" },
                        #option { text="+100 000" }
            ]},
            #label{style="position:absolute; left:42px; top:197px;", text="Turnuva Türü:"},
            #dropdown {style="position:absolute; left:146px; top:190px; width:100px; height:32px; font-size:16px; padding-top:2px;", options=[
                        #option { text="Elemeli" },
                        #option { text="—" }
            ]},
            #label{style="position:absolute; left:265px; top:197px;", text="Tarih:"},
            "<input type='text' id='inputDate' class='newtour_textbox' 
                style='position:absolute; left:310px; top:191px; width:140px; height:28px; font-size:16px;
                       background:url(../images/tournament/new_tournament/calendar_icon.png) no-repeat 118px 2px;' 
                value='" ++ SD ++ "." ++ SM ++ "." ++ SY ++ "'/>",

            #panel{style="height:1px; background-color:#c2c2c2; width:960px; margin-left:-25px; position:absolute; top:282px;", body=[]},
            #panel{class="newtour_title", style="top:257px;", body=[
                    #label{class="newtour_title_label", body="ÖDÜLÜ BELİRLE"}
                ]
            },
            #label{style="position:absolute; left:42px; top:350px;", text="Hediye Aralığı:"},
            #label{style="position:absolute; left:160px; top:329px;", text="0"},
            #label{style="position:absolute; left:312px; top:329px;", text="50 000"},
            #panel{id=slider_panel, style="position:absolute; left:160px; top:352px; width:200px; height:20px;", body=[
                #slider{range = true, id=newtour_slider, max=50000,
                    postback={?MODULE, {newtour_slider}},
                    values=[{min,0}, {max,50000}]
                }
            ]},
            #label{style="position:absolute; left:550px; top:350px;", text="Ödüller:"},
            #panel{id=prize_1, style="position:absolute; left:620px; top:315px; border:1px solid #cdcdcd;", body=[
                #label{style="position:absolute; left:36px; top:-20px;", text="1"},
                #image{style="width:80px; height:80px;", image="http://www.enilginc.com/images/products/00/08/45/845_buyuk.jpg"}
            ]},
            #panel{id=prize_2, style="position:absolute; left:710px; top:315px; border:1px solid #cdcdcd;", body=[
                #label{style="position:absolute; left:36px; top:-20px;", text="2"},
                #image{style="width:80px; height:80px;", image="http://www.enilginc.com/images/products/00/02/12/212_buyuk.jpg"}
            ]},
            #panel{id=prize_3, style="position:absolute; left:800px; top:315px; border:1px solid #cdcdcd;", body=[
                #label{style="position:absolute; left:36px; top:-20px;", text="3"},
                #image{style="width:80px; height:80px;", image="http://www.enilginc.com/images/products/00/07/31/731_buyuk.jpg"}
            ]}
        ]},

%        #panel{id = blanc, style="height:800px;"},
        #panel{id=product_list, body=product_list_paged(1)},
        #br{},
        #panel{style="height:1px; background-color:#c2c2c2; width:860px; margin-left:25px;", body=[]},
        #br{},
        #br{},
        "<center>",
        #button{class="newtour_orange_button", text="YARAT", id=create},
        "</center>"
    ].

product_list_paged(Page) ->
    MinPrice = wf:state(slider_min),
    MaxPrice = wf:state(slider_max),
    AllGiftsData = nsm_gifts_db:get_all_gifts(),
    OnlyGiftsData = lists:sublist( 
        lists:sort(
            fun(A, B) -> 
                if
                    A#gift.kakush_point < B#gift.kakush_point -> true;
                    A#gift.kakush_point == B#gift.kakush_point -> A#gift.id =< B#gift.id;
                    true -> false
                end
            end,
            [Gift || {Gift, _Obj} <- AllGiftsData, Gift#gift.enabled_on_site]
        ), 
        (Page-1) * ?GIFTSPERTOURPAGE + 1, ?GIFTSPERTOURPAGE),
    FilteredGiftsData = lists:filter(fun(G) -> (G#gift.kakush_point >= MinPrice) and (G#gift.kakush_point =< MaxPrice) end, OnlyGiftsData),
    ?PRINT({MinPrice, MaxPrice, FilteredGiftsData}),
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
            || OneGift <- FilteredGiftsData
        ],
        "</ul>",
        Buttons
    ].

event({newtour_slider}) ->
    wf:state(slider_min, list_to_integer(wf:q(newtour_slider_values_min))),
    wf:state(slider_max, list_to_integer(wf:q(newtour_slider_values_max))),
    wf:update(product_list, product_list_paged(1));

event({page, Page}) ->
    wf:update(product_list, product_list_paged(Page));

event({show_details, Description, ImageUrl, Id}) ->
    Body = [
        #panel{class=holder, body=[
            "<center>",
            #image{image=ImageUrl, style="margin:10px; width:300px; height:300px;"},
            "</center>",
            gifts:decode_html(Description),
            #singlerow{cells=[
                #tablecell{
                    body="", style="width:172px;"
                },
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
            #grid_clear{}
        ]}
    ],    
    wf:update(simple_panel, webutils:lightbox_panel_template(gift_lightbox, Body, hide_details)),
    wf:wire(simple_lightbox, #show{});

event(hide_details) ->
    wf:wire(simple_lightbox, #hide{});

event(Any) ->
    webutils:event(Any).
