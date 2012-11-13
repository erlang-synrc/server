%% -*- mode: nitrogen -*-
-module(all_tournaments).
-compile(export_all).

-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("nsm_db/include/common.hrl").
-include_lib("nsm_db/include/tournaments.hrl").
-include("setup.hrl").
-include("common.hrl").
-include("elements/records.hrl").

-define(TOURSPERTOURPAGE, 12).

main() ->
    case wf:user() /= undefined of
        true  -> main_authorized();
        false -> wf:redirect_to_login(?_U("/login"))
    end.

main_authorized() ->
    webutils:add_to_head({raw,              % this goes to styles.css. Still here for convenience of editing
    "
        <style media='screen' type='text/css'>
            .alltour_title {
                width:250px; 
                height:43px; 
                background: url(/images/tournament/lobby/top_plask.png);
                position:absolute; 
                left:21px; 
                top:-7px; 
                text-align:center;
            }

            .alltour_second_title {
                width:92px; 
                height:43px; 
                background: url(/images/tournament/tournaments_page/orange_plask_short.png);
                position:absolute; 
                left:21px; 
                top:-7px; 
                text-align:center;
            }

            .alltour_title_label {
                font-size:18px; 
                color:#fff; 
                font-weight:bold; 
                line-height:42px;
            }

            .alltour_textbox {
                font-size:16px; 
                height:28px;
                border:1px solid #cdcdcd;
            }

            .alltour_bars {
                display:block; 
                font:15px 'Gotham Rounded Bold','Trebuchet MS'; color:#fff; text-shadow:0 1px 1px #353535;
                border:0px; line-height:46px; text-align:center;
            }

            .alltour_bar_1 {
                width:152px; height:60px; background: url(/images/tournament/tournaments_page/bar_1_normal.png);
            }

            .alltour_bar_1:hover {
                width:152px; height:60px; background: url(/images/tournament/tournaments_page/bar_1_hover.png);
                text-decoration:none;
            }

            .alltour_bar_1:active {
                width:152px; height:60px; background: url(/images/tournament/tournaments_page/bar_1_pressed.png);
                text-decoration:none; line-height:48px;
            }

            .alltour_bar_2 {
                width:209px; height:60px; background: url(/images/tournament/tournaments_page/bar_2_normal.png);
            }

            .alltour_bar_2:hover {
                width:209px; height:60px; background: url(/images/tournament/tournaments_page/bar_2_hover.png);
                text-decoration:none;
            }

            .alltour_bar_2:active {
                width:209px; height:60px; background: url(/images/tournament/tournaments_page/bar_2_pressed.png);
                text-decoration:none; line-height:48px;
            }

            .alltour_bar_3 {
                width:194px; height:60px; background: url(/images/tournament/tournaments_page/bar_3_normal.png);
            }

            .alltour_bar_3:hover {
                width:194px; height:60px; background: url(/images/tournament/tournaments_page/bar_3_hover.png);
                text-decoration:none;
            }

            .alltour_bar_3:active {
                width:194px; height:60px; background: url(/images/tournament/tournaments_page/bar_3_pressed.png);
                text-decoration:none; line-height:48px;
            }

            .alltour_bar_4 {
                width:226px; height:60px; background: url(/images/tournament/tournaments_page/bar_4_normal.png);
            }

            .alltour_bar_4:hover {
                width:226px; height:60px; background: url(/images/tournament/tournaments_page/bar_4_hover.png);
                text-decoration:none;
            }

            .alltour_bar_4:active {
                width:226px; height:60px; background: url(/images/tournament/tournaments_page/bar_4_pressed.png);
                text-decoration:none; line-height:48px;
            }

            .alltour_bar_5 {
                width:188px; height:60px; background: url(/images/tournament/tournaments_page/bar_5_normal.png);
            }

            .alltour_bar_5:hover {
                width:188px; height:60px; background: url(/images/tournament/tournaments_page/bar_5_hover.png);
                text-decoration:none;
            }

            .alltour_bar_5:active {
                width:188px; height:60px; background: url(/images/tournament/tournaments_page/bar_5_pressed.png);
                text-decoration:none; line-height:48px;
            }

            .alltour_arrow_left {
                display:block; 
                width:15px; height:22px; background: url(/images/tournament/tournaments_page/arrow_left.png);
            }

            .alltour_arrow_right {
                display:block; 
                width:15px; height:22px; background: url(/images/tournament/tournaments_page/arrow_right.png);
            }

            .alltour_btns_blue {
                display:block; 
                font-size:15px; color:#111;
                border:0px; text-align:center; line-height:52px;
                height:52px;
            }

            .alltour_btn_blue_1 {
                width:205px; background: url(/images/tournament/tournaments_page/btn_blue_1_normal.png);
            }

            .alltour_btn_blue_1:hover {
                width:205px; background: url(/images/tournament/tournaments_page/btn_blue_1_hover.png);
                text-decoration:none;
            }

            .alltour_btn_blue_1:active {
                width:205px; background: url(/images/tournament/tournaments_page/btn_blue_1_pressed.png);
                text-decoration:none; line-height:54px;
            }

            .alltour_btn_blue_2 {
                width:150px; background: url(/images/tournament/tournaments_page/btn_blue_2_normal.png);
            }

            .alltour_btn_blue_2:hover {
                width:150px; background: url(/images/tournament/tournaments_page/btn_blue_2_hover.png);
                text-decoration:none;
            }

            .alltour_btn_blue_2:active {
                width:150px; background: url(/images/tournament/tournaments_page/btn_blue_2_pressed.png);
                text-decoration:none; line-height:54px;
            }

            .alltour_btn_blue_3 {
                width:210px; background: url(/images/tournament/tournaments_page/btn_blue_3_normal.png);
            }

            .alltour_btn_blue_3:hover {
                width:210px; background: url(/images/tournament/tournaments_page/btn_blue_3_hover.png);
                text-decoration:none;
            }

            .alltour_btn_blue_3:active {
                width:210px; background: url(/images/tournament/tournaments_page/btn_blue_3_pressed.png);
                text-decoration:none; line-height:54px;
            }

            .alltour_btn_blue_4 {
                width:147px; background: url(/images/tournament/tournaments_page/btn_blue_4_normal.png);
            }

            .alltour_btn_blue_4:hover {
                width:147px; background: url(/images/tournament/tournaments_page/btn_blue_4_hover.png);
                text-decoration:none;
            }

            .alltour_btn_blue_4:active {
                width:147px; background: url(/images/tournament/tournaments_page/btn_blue_4_pressed.png);
                text-decoration:none; line-height:54px;
            }

            .alltour_btn_blue_5 {
                width:147px; background: url(/images/tournament/tournaments_page/btn_blue_5_normal.png);
            }

            .alltour_btn_blue_5:hover {
                width:147px; background: url(/images/tournament/tournaments_page/btn_blue_5_hover.png);
                text-decoration:none;
            }

            .alltour_btn_blue_5:active {
                width:147px; background: url(/images/tournament/tournaments_page/btn_blue_5_pressed.png);
                text-decoration:none; line-height:54px;
            }


            .alltour_big_buttons {
                display:block; width:147px; height:52px;
                font:18px 'Gotham Rounded Bold','Trebuchet MS'; color:#fff; text-shadow:0 1px 1px #353535;
                border:0px; line-height:56px;
                text-align:center;
            }

            .alltour_orange_button {
                background: url(/images/tournament/tournaments_page/btn_orange_normal.png);
            }

            .alltour_orange_button:hover {
                background: url(/images/tournament/tournaments_page/btn_orange_normal.png);
                text-decoration:none;
            }

            .alltour_orange_button:active {
                background: url(/images/tournament/tournaments_page/btn_orange_pressed.png);
                text-decoration:none;
                line-height:58px;
            }

            .alltour_gray_button {
                background: url(/images/tournament/tournaments_page/btn_gray_normal.png);
            }

            .alltour_gray_button:hover {
                background: url(/images/tournament/tournaments_page/btn_gray_normal.png);
                text-decoration:none;
            }

            .alltour_gray_button:active {
                background: url(/images/tournament/tournaments_page/btn_gray_pressed.png);
                text-decoration:none;
                line-height:58px;
            }
        </style>

        <link rel='stylesheet' href='/nitrogen/datepicker/css/datepicker.css' type='text/css' />
	    <script type='text/javascript' src='/nitrogen/datepicker/js/datepicker_tr.js'></script>

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
                    $('.wfid_tour_date_check').prop('checked', true);
	            }
            });
        };
        new Image('/images/tournament/tournaments_page/bar_1_pressed.png');
        new Image('/images/tournament/tournaments_page/bar_1_hover.png');
        new Image('/images/tournament/tournaments_page/bar_2_pressed.png');
        new Image('/images/tournament/tournaments_page/bar_2_hover.png');
        new Image('/images/tournament/tournaments_page/bar_3_pressed.png');
        new Image('/images/tournament/tournaments_page/bar_3_hover.png');
        new Image('/images/tournament/tournaments_page/bar_4_pressed.png');
        new Image('/images/tournament/tournaments_page/bar_4_hover.png');
        new Image('/images/tournament/tournaments_page/bar_5_pressed.png');
        new Image('/images/tournament/tournaments_page/bar_5_hover.png');
        new Image('/images/tournament/tournaments_page/btn_blue_1_pressed.png');
        new Image('/images/tournament/tournaments_page/btn_blue_1_hover.png');
        new Image('/images/tournament/tournaments_page/btn_blue_2_pressed.png');
        new Image('/images/tournament/tournaments_page/btn_blue_2_hover.png');
        new Image('/images/tournament/tournaments_page/btn_blue_3_pressed.png');
        new Image('/images/tournament/tournaments_page/btn_blue_3_hover.png');
        new Image('/images/tournament/tournaments_page/btn_blue_4_pressed.png');
        new Image('/images/tournament/tournaments_page/btn_blue_4_hover.png');
        new Image('/images/tournament/tournaments_page/btn_blue_5_pressed.png');
        new Image('/images/tournament/tournaments_page/btn_blue_5_hover.png');
        new Image('/images/tournament/tournaments_page/btn_orange_pressed.png');
        new Image('/images/tournament/tournaments_page/btn_orange_hover.png');
        new Image('/images/tournament/tournaments_page/btn_gray_pressed.png');
        new Image('/images/tournament/tournaments_page/btn_gray_hover.png');
        </script>
    "}),
    #template { file=code:priv_dir(nsw_srv)++"/templates/bare.html" }.

   
title() -> webutils:title(?MODULE).

body() ->
    #template{file=code:priv_dir(nsw_srv)++"/templates/info_page.html"}.

content() ->
    {{Y, M, D}, _} = calendar:now_to_datetime(erlang:now()),
    SY = integer_to_list(Y),
    SM = integer_to_list(M),
    SD = integer_to_list(D),
    wf:state(alltour_arrow_shift, 0),
    [
        #panel{class="alltour_title", body=[
                #label{class="alltour_title_label", body="TURNUVALAR SAYFASI"}
            ]
        },
        #panel{id=top_selectors, style="height:700px; font-size:16px; ", body=[
            #panel{style="background-color:#e4e8e9;height:360px; margin-top:80px; margin-left:-25px; margin-right:-25px; width:960;", body=[]},
            #link{style="position:absolute; top:52px; left:-1px;", class="alltour_bars alltour_bar_1", text="ÖNE ÇIKANLAR", postback=bar},
            #link{style="position:absolute; top:52px; left:149px;", class="alltour_bars alltour_bar_2", text="ZAMANI YAKLAŞANLAR", postback=bar},
            #link{style="position:absolute; top:52px; left:356px;", class="alltour_bars alltour_bar_3", text="DOLDU DOLACAKLAR", postback=bar},
            #link{style="position:absolute; top:52px; left:548px;", class="alltour_bars alltour_bar_4", text="KALABALIK TURNUVALAR", postback=bar},
            #link{style="position:absolute; top:52px; left:772px;", class="alltour_bars alltour_bar_5", text="KALLAVİ HEDIYELER", postback=bar},
            #image{image="/images/tournament/tournaments_page/bar_dividers.png", style="position:absolute; top:61px; left:148px;"},
            #image{image="/images/tournament/tournaments_page/bar_dividers.png", style="position:absolute; top:61px; left:355px;"},
            #image{image="/images/tournament/tournaments_page/bar_dividers.png", style="position:absolute; top:61px; left:547px;"},
            #image{image="/images/tournament/tournaments_page/bar_dividers.png", style="position:absolute; top:61px; left:771px;"},
            
            #panel{id=featured_tours, style="position:absolute; top:112px; left:43px; width:880px;", body=[
                featured_tours()
            ]},

            #panel{style="height:1px; background-color:#c2c2c2; width:960px; margin-left:-25px; position:absolute; top:482px;", body=[]},
            #panel{class="alltour_second_title", style="top:472px;", body=[
                    #label{class="alltour_title_label", body="FILTRE"}
                ]
            },

            %filters
            #label{style="position:absolute; left:42px; top:540px;", text="Oyun Türü:"},
            #dropdown {style="position:absolute; left:126px; top:533px; width:110px; height:32px; font-size:16px; padding-top:2px;", options=[
                        #option { text="—" },
                        #option { text="OKEY" },
                        #option { text="TAVLA" }
            ]},
            #label{style="position:absolute; left:480px; top:540px;", text="Oyun Sayısı:"},
            #dropdown {style="position:absolute; left:576px; top:533px; width:160px; height:32px; font-size:16px; padding-top:2px;", options=[
                        #option { text="—" },
                        #option { text="16" },
                        #option { text="32" },
                        #option { text="64" },
                        #option { text="128" },
                        #option { text="256" },
                        #option { text="512" },
                        #option { text="1024" }
            ]},
            #label{style="position:absolute; left:764px; top:540px;", text="Kota:"},
            #dropdown {style="position:absolute; left:807px; top:533px; width:110px; height:32px; font-size:16px; padding-top:2px;", options=[
                        #option { text="—" },
                        #option { text="2" },
                        #option { text="4" },
                        #option { text="6" },
                        #option { text="8" },
                        #option { text="10" }
            ]},
            #label{style="position:absolute; left:285px; top:540px;", text="Tarih:"},
            #checkbox{id=tour_date_check, style="position:absolute; left:262px; top:536px; width:20px; height:20px;", checked=false},
            #textbox{id=tour_date, class="alltour_textbox",
                style="position:absolute; left:330px; top:532px; width:120px; height:28px; font-size:16px;
                       background:url(../images/tournament/new_tournament/calendar_icon.png) no-repeat 98px 2px;",
                text= (SD ++ "." ++ SM ++ "." ++ SY)},

            #link{style="position:absolute; top:590px; left:36px;", class="alltour_btns_blue alltour_btn_blue_1", text="ARKADAŞLARIMA GÖRE", postback=btn_blue_1},
            #link{style="position:absolute; top:590px; left:245px;", class="alltour_btns_blue alltour_btn_blue_2", text="HEDİYEYE GÖRE", postback=btn_blue_2},
            #link{style="position:absolute; top:590px; left:399px;", class="alltour_btns_blue alltour_btn_blue_3", text="KATILIM ORANINA GÖRE", postback=btn_blue_3},
            #link{style="position:absolute; top:590px; left:613px;", class="alltour_btns_blue alltour_btn_blue_4", text="PUANIMA GÖRE", postback=btn_blue_4},
            #link{style="position:absolute; top:590px; left:765px;", class="alltour_btns_blue alltour_btn_blue_5", text="KAKUŞUMA GÖRE", postback=btn_blue_5},

            #label{style="position:absolute; left:243px; top:674px;", text="Sıralama Şekli:"},
            #dropdown {style="position:absolute; left:357px; top:667px; width:110px; height:32px; font-size:16px; padding-top:2px;", options=[
                        #option { text="AZALAN" },
                        #option { text="ARTAN" }
            ]},
            #label{style="position:absolute; left:490px; top:674px;", text="Görünüm:"},
            #dropdown {style="position:absolute; left:567px; top:667px; width:110px; height:32px; font-size:16px; padding-top:2px;", options=[
                        #option { text="12 ADET" },
                        #option { text="24 ADET" },
                        #option { text="24 HEPSİ" }
            ]},

            #link{style="position:absolute; top:726px; left:326px;", class="alltour_big_buttons alltour_orange_button", text="FILTRELE", postback=btn_orange},
            #link{style="position:absolute; top:726px; left:492px;", class="alltour_big_buttons alltour_gray_button", text="SIFIRLA", postback=btn_gray},

            #button{style="position:absolute; top:740px; left:650px;", text=?_T("New"), postback=new_pressed},

            #link{style="position:absolute; top:268px; left:20px;", class="alltour_arrow_left", postback=arrow_left},
            #link{style="position:absolute; top:268px; left:925px;", class="alltour_arrow_right", postback=arrow_right},
            #link{} % this is WTF fix. Something with the Nitrogen I suppose. Delete it and the last link will appear twise on a page.
        ]},
        #panel{id=alltour_container, style="margin-top:5px; margin-left:-14px; width:960px;", body=[
            all_tours(1)
        ]}
    ].

featured_tours() ->
    AllTours = nsm_db:all(tournament),
    SortedTours = lists:sort(fun(#tournament{start_date=SD1, start_time=ST1, id=Id}, #tournament{start_date=SD2, start_time=ST2}) ->
        {SD1, ST1, Id} > {SD2, ST2, Id} end, AllTours),
    TourIds = [TId || #tournament{id=TId} <- SortedTours],
    ShiftedTours = lists:sublist(TourIds, 1+wf:state(alltour_arrow_shift), 4),
    [#panel{style="margin:8px; float:left", body=tourblock(Id)} || Id <- ShiftedTours].

all_tours(Page) ->
    AllTours = nsm_db:all(tournament),
    SortedTours = lists:sort(fun(#tournament{start_date=SD1, start_time=ST1}, #tournament{start_date=SD2, start_time=ST2}) ->
        {SD1, ST1} > {SD2, ST2} end, AllTours),
    TourIds = [TId || #tournament{id=TId} <- SortedTours],
    PageTourIds = lists:sublist(TourIds, (Page-1)*?TOURSPERTOURPAGE+1, ?TOURSPERTOURPAGE),
    [
        "<center>",
        #panel{style="height:1px; background-color:#c2c2c2; width:700px;", body=[]},
        [#panel{style="margin:16px; float:left", body=tourblock(TId)} || TId <- PageTourIds],
        #panel{style="display:block; height:100px;", body=[]},
        buttons(Page, length(AllTours)),
        "</center>"
    ].

test_tourblock() ->
    tourblock(0,"OKEY TURNUVASI", "TAVLA", "7.11.2012", 64, 5000, 
        "/images/tournament/tournaments_page/tournament_default_avatar.png",
        ["http://www.enilginc.com/images/products/00/08/45/845_buyuk.jpg", 
         "http://www.enilginc.com/images/products/00/02/12/212_buyuk.jpg",
         "http://www.enilginc.com/images/products/00/07/31/731_buyuk.jpg"]).

tourblock(Id) ->
    {ok, T} = nsm_db:get(tournament, Id),
    Title = T#tournament.name,
    Game = case T#tournament.game_type of
        game_okey -> "OKEY";
        game_tavla -> "TAVLA";
        game_batak -> "BATAK";
        _ -> "WTF"
    end,
    Date = integer_to_list(element(3, T#tournament.start_date)) ++ "." ++ 
           integer_to_list(element(2, T#tournament.start_date)) ++ "." ++ 
           integer_to_list(element(1, T#tournament.start_date)),
    NPlayers = T#tournament.players_count,
    Quota = T#tournament.quota,
    Avatar = "/images/tournament/tournaments_page/tournament_default_avatar.png",
    Prizes = case is_list(T#tournament.awards) of
        true ->
            GOs = [nsm_gifts_db:get_gift(A) || A <- T#tournament.awards],
            [case GO of
                {error, notfound} -> "/images/tournament/new_tournament/question.png";
                {ok, {Gift, _}} -> Gift#gift.image_small_url
            end || GO <- GOs];
        false ->
            ["/images/tournament/new_tournament/question.png","/images/tournament/new_tournament/question.png","/images/tournament/new_tournament/question.png"]
    end,
    tourblock(Id, Title, Game, Date, NPlayers, Quota, Avatar, Prizes).

tourblock(Id, Title, Game, Date, NGames, Quota, Avatar, Prizes) ->
    #link{url=?_T("tournament/lobby/id/")++integer_to_list(Id), body=[
        #panel{style="width:200px; height:308px; border:1px solid #adb1b0; background-color:#f6f9ff; position:relative;", body=[
            #panel{style="width:200px; height:28; position:absolute; left:-1px; top:-1px; 
                    font:14px 'Gotham Rounded Bold','Trebuchet MS'; color:#fff; text-shadow:0 1px 1px #353535; text-align:center; padding-top:6px;
                    background-color:#595959; border:1px solid #adb1b0;", body=[Title]},
            #image{image=Avatar, style="position:absolute; left:7px; top:34px;
                    -moz-border-radius:2px;
                    -webkit-border-radius:2px;
                    border-radius:2px;
                    border:1px solid #7e7f83;"},
            #panel{style="width:200px; height:1px; position:absolute; left:0px; top:134px; background-color:#9c9da2;", body=[]},
            #panel{style="width:200px; height:1px; position:absolute; left:0px; top:198px; background-color:#9c9da2;", body=[]},
            #label{style="position:absolute; left:9px; top:113px; color:#f67436; font-size:14px; font-weight:bold;", 
                body="Oyun Türü: <span style='color:#222; font-weight:normal;'>" ++ Game ++ "</span>" },

            #label{style="position:absolute; left:9px; top:143px; color:#f67436; font-size:12px; font-weight:bold;", 
                body="Başlangiç Tarihi: <span style='color:#222; font-weight:normal;'>" ++ Date ++ "</span>" },
            #label{style="position:absolute; left:9px; top:160px; color:#f67436; font-size:12px; font-weight:bold;", 
                body="Oyuncu Sayısı: <span style='color:#222; font-weight:normal;'>" ++ integer_to_list(NGames) ++ "</span>" },
            #label{style="position:absolute; left:9px; top:177px; color:#f67436; font-size:12px; font-weight:bold;", 
                body="Kota: <span style='color:#222; font-weight:normal;'>" ++ integer_to_list(Quota) ++ "</span>" },

            #label{style="position:absolute; left:9px; top:203px; color:#f67436; font-size:14px; font-weight:bold;", text="Ödüler: "},

            #label{style="position:absolute; left:34px; top:224px; color:#222; font-size:14px;", text="1"},
            #panel{style="position:absolute; left:9px; top:240px; background-color:#9c9da2; width:55px; height:1px;", body=[]},
            #image{style="position:absolute; left:9px; top:246px; width:55px; border:1px solid #ccd0d3;", image=lists:nth(1, Prizes)},

            #label{style="position:absolute; left:97px; top:224px; color:#222; font-size:14px;", text="2"},
            #panel{style="position:absolute; left:72px; top:240px; background-color:#9c9da2; width:55px; height:1px;", body=[]},
            #image{style="position:absolute; left:72px; top:246px; width:55px; border:1px solid #ccd0d3;", image=lists:nth(2, Prizes)},

            #label{style="position:absolute; left:159px; top:224px; color:#222; font-size:14px;", text="3"},
            #panel{style="position:absolute; left:135px; top:240px; background-color:#9c9da2; width:55px; height:1px;", body=[]},
            #image{style="position:absolute; left:135px; top:246px; width:55px; border:1px solid #ccd0d3;", image=lists:nth(3, Prizes)}
        ]}
    ]}.

buttons(Page, AllN) ->
    case AllN =< ?TOURSPERTOURPAGE of
        true -> "";
        false ->
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
                        || N <- lists:seq(1, AllN div ?TOURSPERTOURPAGE + 1)],
                        case Page * ?TOURSPERTOURPAGE >= AllN of                 
                            true -> #listitem{body=#link{text=">", postback={nothing}, class="inactive"}};
                            false -> #listitem{body=#link{text=">", postback={page, Page + 1}}}
                        end
                   ]}
               ]}
            ]}
    end.

event({page, Page}) ->
    wf:update(alltour_container, all_tours(Page));

event(arrow_left) ->
    Shift = wf:state(alltour_arrow_shift),
    case Shift of
        0 -> ok;
        _ -> 
            wf:state(alltour_arrow_shift, Shift-1),
            wf:update(featured_tours, featured_tours())
    end;

event(arrow_right) ->
    Shift = wf:state(alltour_arrow_shift),
    wf:state(alltour_arrow_shift, Shift+1),
    wf:update(featured_tours, featured_tours());

event(bar) ->
    wf:wire(#alert{text=?_T("Sorry, these filters are not yet implemented.")});

event(new_pressed) ->
    wf:redirect(?_U("/new-tournament"));

event(Any) ->
    webutils:event(Any).
