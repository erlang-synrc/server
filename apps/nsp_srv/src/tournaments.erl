-module(tournaments).
-compile(export_all).

-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("nsm_db/include/common.hrl").
-include_lib("nsm_db/include/tournaments.hrl").
-include_lib("nsm_db/include/user.hrl").
-include("setup.hrl").
-include("common.hrl").
-include("elements/records.hrl").

-define(BAR_PRIZE_FUND, 500).
-define(BAR_SOON_SECONDS, 24*60*60).
-define(BAR_FILL_PERCENT, 80).

game_type_image(T,Prefix) ->
   case T of
       game_okey -> lists:concat([Prefix,"/slider_okey.png"]);
       game_tavla -> lists:concat([Prefix,"/slider_tavla.png"]);
       game_batak -> lists:concat([Prefix,"/slider_batak.png"]);
       game_king -> lists:concat([Prefix,"/slider_king.png"]);
       _ -> "/tournaments_page/tournament_default_avatar.png"
   end.

main() ->
  webutils:add_to_head({raw,
  "<script>
   new Image('/images/tournament/tournaments_page/bar_1_pressed.png');
   new Image('/images/tournament/tournaments_page/bar_1_hover.png');
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
  </script>"}),
  #template { file=code:priv_dir(nsp_srv)++"/templates/bare.html" }.

title() -> webutils:title(?MODULE).

body() ->
    #template{file=code:priv_dir(nsp_srv)++"/templates/info_page.html"}.

content() ->
  AllTours = nsm_db:all(tournament),
  wf:state(all_fetch,AllTours),
%  {{Y, M, D}, _} = calendar:now_to_datetime(erlang:now()),
%  SY = integer_to_list(Y),
%  SM = integer_to_list(M),
%  SD = integer_to_list(D),
  wf:state(alltour_arrow_shift, 0),
  wf:state(sort_order, descend),
  wf:state(per_page, 12),
  [
    #panel{class="tournaments_title", body=?_T("TOURNAMENTS PAGE")},
    #panel{class="tournaments_header", body=[
      #link{text=?_T("Tournament Setup!"), class="matchmaker_game_rules tournament_setup", postback=show_page_2},
      #link{text=?_T("Turnament Rules!"), class="matchmaker_game_rules tournament_rules",  postback=show_page_1}
    ]},
    #panel{id=explaination_holder, style="font-size:14px;", body=[]},

    #list{class="tournaments_top_selector", body=[
      #listitem{body=#link{text=?_T("FINISHED IN PAST"), postback={bar, past}}},
      #listitem{body=#link{text=?_T("NOW PLAYING"), postback={bar, present}}},
      #listitem{body=#link{text=?_T("FUTURE TOURNAMENTS"), postback={bar, future}}}
    ]},

    #panel{class="tournaments_featured", body=[
        #panel{class="tournaments_featured_arrow", body=[
          #link{postback=arrow_left,
            body=#image{image="/images/tournament/tournaments_page/arrow_left.png"}}
        ]},
        #panel{id=featured_tours, class="tournaments_featured_tours", body=featured_tours(AllTours)},
        #panel{class="tournaments_featured_arrow", body=[
          #link{postback=arrow_right,
            body=#image{image="/images/tournament/tournaments_page/arrow_right.png"}}
        ]}
    ]},
    case nsm_accounts:user_paid(wf:user()) of
      true ->
        #panel{class="tournaments_filter_block", body=[
          #link{style="", class="alltour_big_buttons alltour_orange_button", text=?_T("NEW"), postback=new_pressed}
        ]};
      false -> []
    end,
    #hr{class="tournaments_hr"},
    #panel{class="tournaments_second_title", body=?_T("FILTER")},

    prototype_doxtop_panel(),

%    #panel{class="tournaments_filter_block", body=[
%      #label{text=?_T("Game Type:")},
%      #dropdown {id=tour_game, options=[#option{text=T} || T <- ["-", "OKEY", "TAVLA"]]},

%      #label{text=?_T("Players Count:")},
%      #dropdown {id=tour_players, options=[#option { text=T } || T <- ["-", "16", "32", "64", "128", "256", "512", "1024", "2048"]]},

%      #label{text=?_T("Quota:")},
%      #dropdown {id=tour_quota, options=[#option{text=T} || T <- ["-", "2","4","6","8", "10"]]},

%      #label{text=?_T("Date:")},
%      #checkbox{id=tour_date_check, style="width:20px; height:20px;", checked=false},
%      #textbox{id=tour_date, class="alltour_textbox",
%        style="width:120px; height:28px; font-size:16px;
%               background:url(../images/tournament/new_tournament/calendar_icon.png) no-repeat 98px 2px;",
%        text= (SD ++ "." ++ SM ++ "." ++ SY)}
%    ]},

%    #panel{class="tournaments_filter_block", body=[
%      #label{text=?_T("Sort by type:")},
%      #dropdown {postback=sort_order_set, id=sort_order, options=[
%        #option { text=?_T("DESC") },
%        #option { text=?_T("ASC") }
%      ]},
%      #label{text=?_T("View:")},
%      #dropdown {postback=per_page_set, id=per_page,     options=[
%        #option { text="12 " ++ ?_T("PCS") },
%        #option { text="24 " ++ ?_T("PCS") },
%        #option { text="24 " ++ ?_T("ALL") }
%      ]}
%    ]},

%    #panel{class="tournaments_filter_block", body=[
%      #link{class="alltour_btns_blue alltour_btn_blue_1", text=?_T("ACCORDING TO FRIENDS"), postback={sort_by, friends}},
%      #link{class="alltour_btns_blue alltour_btn_blue_2", text=?_T("BY GIFTS"), postback={sort_by, gifts}},
%      #link{class="alltour_btns_blue alltour_btn_blue_3", text=?_T("PARTICIPATION PERCENTAGE"), postback={sort_by, participation}}
%    ]},

%    #panel{class="tournaments_filter_block", body=[
%      case nsm_accounts:user_paid(wf:user()) of
%        true ->
%          [
%            #link{style="", class="alltour_big_buttons alltour_gray_button", text=?_T("FILTER"), postback=filter_pressed},
%            #link{style="", class="alltour_big_buttons alltour_gray_button", text=?_T("RESET"), postback=clean_filter_pressed},
%            #link{style="", class="alltour_big_buttons alltour_orange_button", text=?_T("NEW"), postback=new_pressed}
%          ];
%        false ->
%          [
%            #link{style="", class="alltour_big_buttons alltour_gray_button", text=?_T("FILTER"), postback=filter_pressed},
%            #link{style="", class="alltour_big_buttons alltour_gray_button", text=?_T("RESET"), postback=clean_filter_pressed}
%          ]
%      end
%    ]},
%    #hr{},
    #panel{class="tournaments_all_block", id=alltour_container, body=all_tours(AllTours,1)}
  ].

prototype_doxtop_panel() ->
  #panel{class="tournaments_filter_block", body=[
    game_type_filter(),
    players_filter(),
    %tournament_type_filter(),
    %date_filter(SD, SM, SY),
    quota_filter(),
    sort_filter(),
    view_filter(),
    by_filter(),
    #hr{}
  ]}.

players_filter()->
  #panel{class="create-block", body =[
      #panel{class=article1, body=[
        #h3{text=?_T("Players Count:")},
        #list{class="list1_green size1", body=[
          #listitem{body=#link{text=T, id=site_utils:simple_pickle({players, list_to_atom(T)}),
            postback={filter,{players, list_to_atom(T)}} } } || T <- ["16", "32", "64", "128", "256", "512", "1024", "2048"]
          ]}
        ]}
  ]}.

quota_filter()->
  #panel{class="create-block", body =[
      #panel{class=article1, body=[
        #h3{text=?_T("Quota:")},
        #list{class="list1_green size1", body=[
          #listitem{body=#link{text=T, id=site_utils:simple_pickle({quota, list_to_atom(T)}),
            postback={filter, {quota, list_to_atom(T)}} } } || T <- ["2","4","6","8","10"]
        ]}
      ]}
  ]}.

by_filter()->
  #panel{class="create-block", body =[
      #panel{class=article1, body=[
        #h3{text=?_T("Select by:")},
        #list{class="list1_blue  size1", body=[
          #listitem{body=#link{id=site_utils:simple_pickle({sort_by1, gifts}),
              text=?_T("BY GIFTS"), postback={filter, {sort_by1, gifts}}}},
          #listitem{body=#link{id=site_utils:simple_pickle({sort_by1, friends}),
              text=?_T("ACCORDING TO FRIENDS"), postback={filter, {sort_by1, friends}}}},
          #listitem{body=#link{id=site_utils:simple_pickle({sort_by1, participation}),
              text=?_T("PARTICIPATION PERCENTAGE"), postback={filter, {sort_by1, participation}}}}
        ]}
      ]}
  ]}.

view_filter()->
  #panel{class="create-block", body=[
    #panel{class=article1, body=[
      #h3{text=?_T("View:")},
      #list{class="list1_green size1", body=[
        #listitem{body=#link{text=T, id=site_utils:simple_pickle({per_page1, list_to_atom(T)}),
          postback={filter, {per_page1, list_to_atom(T)}} } }
        || T <- ["12 " ++ ?_T("PCS"), "24 " ++ ?_T("PCS")]
      ]}
    ]}
  ]}.

sort_filter()->
  #panel{class="create-block", body=[
    #panel{class=article1, body=[
      #h3{text=?_T("Sort by type:")},
      #list{class="list1_green size1", body=[
        #listitem{body=#link{text=T, id=site_utils:simple_pickle({sort_order1, list_to_atom(T)}),
          postback={filter, {sort_order1, list_to_atom(T)}} } } || T <- [?_T("DESC"),?_T("ASC")]
        ]}
    ]}
  ]}.

game_type_filter()->
  #panel{class="create-block", body =[
      #panel{class=article1, body=[
        #h3{text=?_T("Game Type")++":"},
        #list{class="list1_green size1", body=[
          #listitem{body=X} || X <- [
            #link{text=?_T("OKEY"), id = site_utils:simple_pickle({game, okey}),  postback={filter,{game, okey}}},
            #link{text=?_T("TAVLA"), id = site_utils:simple_pickle({game, tavla}), postback={filter, {game, tavla}}}
          ]
        ]}
      ]}
  ]}.

tournament_type_filter()->
  #panel{class="create-block", body =[
      #panel{class=article1, body=[
        #h3{text=?_T("Tournament Type")++":"},
        #list{class="list1_green size1", body=[
          #listitem{body=X} || X <- [
            #link{text=?_T("Normal"), id = site_utils:simple_pickle({tournament, normal}),  postback={filter,{tournament, normal}}},
            #link{text=?_T("other"), id = site_utils:simple_pickle({tournament, other}), postback={filter, {tournamet, other}}}
          ]
        ]}
      ]}
  ]}.

date_filter(SD, SM, SY)->
  #panel{class="create-block", body =[
      #panel{class=article1, body=[
        #h3{text=?_T("Date:")},
        #checkbox{id=tour_date_check1, style="width:20px;height:20px;", checked=false, postback={filter, {date, ok}}}, 
        #textbox{id=tour_date1, class="alltour_textbox1",
          actions=#event{type=change, postback={filter,{date, ok}}},
          text= (SD ++ "." ++ SM ++ "." ++ SY)}
      ]}
  ]}.

featured_tours(AllTours) ->
    Online = [ T || T=#tournament{id=Id,status=St} <- AllTours, St == activated],
    FilteredTours = case wf:state(bar) of
        past ->     [ T || T=#tournament{id=Id,winners=Winners} <- AllTours, Winners =/= undefined];
        future ->   [ T || T=#tournament{status=created} <- AllTours];
        present ->  Online;
        featured -> 
            PreFiltered = [{T, new_tournament:get_prizes_total(A)} || T=#tournament{awards=A} <- AllTours],
            [T || {T, G} <- PreFiltered, G >= ?BAR_PRIZE_FUND];
        soon ->
            PreFiltered = [{T, calendar:datetime_to_gregorian_seconds({SD, ST}) - calendar:datetime_to_gregorian_seconds(calendar:now_to_datetime(now()))} 
            || T=#tournament{start_date=SD, start_time=ST} <- AllTours],
            [T || {T, DT} <- PreFiltered, DT > 0, DT =< ?BAR_SOON_SECONDS];
        filled ->
            PreFiltered = [{T, 100 * length(nsm_tournaments:joined_users(Id)) / PC} || T=#tournament{id=Id, players_count=PC} <- AllTours],
            [T || {T, F} <- PreFiltered, F >= ?BAR_FILL_PERCENT];
        _ ->        AllTours
    end,
    SortedTours = lists:sort(fun(#tournament{start_date=SD1, start_time=ST1, id=Id1}, #tournament{start_date=SD2, start_time=ST2, id=Id2}) ->
        {SD1, ST1, Id1} > {SD2, ST2, Id2} end, FilteredTours),
    TourIds = [TId || #tournament{id=TId} <- SortedTours],
    wf:state(last_bar_tours, length(TourIds)),
    ShiftedTours = lists:sublist(SortedTours, 1+wf:state(alltour_arrow_shift), 4),
    [tourblock(Id) || Id <- ShiftedTours].

all_tours(AllTours,Page) ->
  ?INFO("Tours per page: ~p~n", [wf:state(per_page)]),
    ToursPerPage = wf:state(per_page),
    PreSortedTours = case wf:state(sort_by) of
        friends ->
            Friends = [UId || {_, _, UId} <- nsm_users:list_subscr(wf:user())],
            PreSortedToursFriends = [
                begin
                    U = [UId || #play_record{who=UId} <- nsm_tournaments:joined_users(Id)],
                    F = lists:sum([1 || F <- Friends, lists:member(F, U)]),
                    {T, F, Id}
                end
            || T=#tournament{id=Id} <- AllTours],
            [T || {T, _, _} <- lists:sort(fun({_, F1, Id1}, {_, F2, Id2}) ->
                {F1, Id1} > {F2, Id2}
            end, PreSortedToursFriends)];
        gifts ->
            PreSortedToursGifts = [
                begin
                    G = new_tournament:get_prizes_total(A),
                    {T, G, Id}
                end
            || T=#tournament{awards=A, id=Id} <- AllTours],
            [T || {T, _, _} <- lists:sort(fun({_, G1, Id1}, {_, G2, Id2}) ->
                {G1, Id1} > {G2, Id2}
            end, PreSortedToursGifts)];
        participation ->
            PreSortedToursPart = [
                begin
                    JUC = length(nsm_tournaments:joined_users(Id)),
                    P = JUC/PC,
                    {T, P, Id}
                end
            || T=#tournament{id=Id, players_count=PC} <- AllTours],
            [T || {T, _, _} <- lists:sort(fun({_, P1, Id1}, {_, P2, Id2}) ->
                {P1, Id1} > {P2, Id2}
            end, PreSortedToursPart)];
        _ ->
            lists:sort(fun(#tournament{start_date=SD1, start_time=ST1, id=Id1}, #tournament{start_date=SD2, start_time=ST2, id=Id2}) ->
                {SD1, ST1, Id1} > {SD2, ST2, Id2} 
            end, AllTours)
    end,

    SortedTours = case wf:state(sort_order) of
        descend -> PreSortedTours;
        _ -> lists:reverse(PreSortedTours)
    end,

    FilteredTours1 = case wf:state(game_filter) of
        undefined -> SortedTours;
        Game -> [T || T = #tournament{game_type=G} <- SortedTours, G==Game]
    end,

    FilteredTours2 = case wf:state(players_filter) of
        undefined -> FilteredTours1;
        Players -> [T || T = #tournament{players_count=P} <- FilteredTours1, P==Players]
    end,

    FilteredTours3 = case wf:state(quota_filter) of
        undefined -> FilteredTours2;
        Quota -> [T || T = #tournament{quota=Q} <- FilteredTours2, Q==Quota]
    end,

    FilteredTours4 = case wf:state(date_filter) of
        undefined -> FilteredTours3;
        Date -> [T || T = #tournament{start_date=D} <- FilteredTours3, D==Date]
    end,

    TourIds = [T || T=#tournament{id=TId} <- FilteredTours4],
    PageTourIds = lists:sublist(TourIds, (Page-1)*ToursPerPage+1, ToursPerPage),
    [
      [tourblock(TId) || TId <- PageTourIds],
      buttons(Page, length(AllTours))
    ].

test_tourblock() ->
    tourblock(0,"OKEY TURNUVASI", "TAVLA", "7.11.2012 00:00:00", 64, 5000, 
        "/images/tournament/tournaments_page/tournament_default_avatar.png",
        ["http://www.enilginc.com/images/products/00/08/45/845_buyuk.jpg", 
         "http://www.enilginc.com/images/products/00/02/12/212_buyuk.jpg",
         "http://www.enilginc.com/images/products/00/07/31/731_buyuk.jpg"],16,[]).

convert_date(Date) ->
    integer_to_list(element(3, Date)) ++ "." ++ 
    integer_to_list(element(2, Date)) ++ "." ++ 
    integer_to_list(element(1, Date)).

convert_time(Time) ->
    integer_to_list(element(1, Time)) ++ ":" ++ 
    integer_to_list(element(2, Time)) ++
           case element(2, Time) of 
                0 -> "0";
                _ -> ""
           end.

tourblock(T) ->
    Id = T#tournament.id,
    Title = T#tournament.name,
    Game = case T#tournament.game_type of
        game_okey -> "OKEY";
        game_tavla -> "TAVLA";
        game_batak -> "BATAK";
        _ -> "WTF"
    end,

    Date = convert_date(T#tournament.start_date),
    Time = convert_time(T#tournament.start_time),

    DateTime = Date ++ " " ++ Time,
    NPlayers = T#tournament.players_count,
    Quota = T#tournament.quota,
    Winners = T#tournament.winners,
    Avatar = game_type_image(T#tournament.game_type,"/images/tournament"),
             %"/images/tournament/tournaments_page/tournament_default_avatar.png",
    Prizes = case is_list(T#tournament.awards) of
        true ->
            GOs = [nsm_db:get(gifts,A) || A <- T#tournament.awards],
            [case GO of
                {error, notfound} -> "/images/tournament/nothing.png";
                {ok, Gift} -> Gift#gift.image_small_url
            end || GO <- GOs];
        false ->
            ["/images/tournament/new_tournament/question.png",
             "/images/tournament/new_tournament/question.png",
             "/images/tournament/new_tournament/question.png"]
    end,
    tourblock(Id, Title, Game, DateTime, NPlayers, Quota, Avatar, Prizes, NPlayers, Winners).

tourblock(Id, Title, Game, DateTime, NGames, Quota, Avatar, Prizes,PlayersCount, Winners) ->

    Color = case PlayersCount of 
         16   -> "#fff000";
         40   -> "#ffe000";
         60   -> "#ffe700";
         64   -> "#ffd000";
         100  -> "#ffb000";
         128  -> "#ffb000";
         200  -> "#ff7F00";
         256  -> "#ff7F00";
         300  -> "#ff6000";
         400  -> "#ff5000";
         500  -> "#ff5000";
         512  -> "#ff4000";
         1024 -> "#ff3000";
         1020 -> "#ff3000";
         2048 -> "#ff1000";
         _ -> "#ff1000"
    end,

    Places = case Winners of
                  undefined -> [integer_to_list(X)||X<-lists:seq(1, length(Prizes))];
                  L -> [Name||{Name,Pos,Gift}<-L] ++ [[]||X<-lists:seq(1,3-length(L))] end,

  #panel{class="tts_tournament", body=[
  #link{url=?_U("tournament/lobby/id/")++integer_to_list(Id), body=[
    #panel{style="background-color:"++Color++";", class="tts_title", body=[Title]},
    #image{image=Avatar, class="tts_avatar"},

    #hr{},
    #label{body=[?_T("Game Type:"), #span{body=Game}]},
    #label{body=[?_T("Starting") ++ ": ", #span{body=DateTime}]},
    #label{body=[?_T("Players total:"), #span{body=integer_to_list(NGames)}]},
    #label{body=[?_T("Quota:"), #span{body=integer_to_list(Quota)}]},

    #hr{},
    #label{text=?_T("Prizes:")},
    #panel{class="tts_prizes", body=
    [begin
      #panel{class="tts_prize", body =[
        #span{body=I},
        #hr{},
        "<center>",
        #panel{class="tts_prize_icon", body=#image{image=P}},
        "</center>"
      ]}
     end || {P, I} <- lists:zip(Prizes, Places) ]
    }
  ]}
  ]}.

buttons(Page, AllN) ->
    ToursPerPage = wf:state(per_page),
    case AllN =< ToursPerPage of
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
                        || N <- lists:seq(1, AllN div ToursPerPage + 1)],
                        case Page * ToursPerPage >= AllN of                 
                            true -> #listitem{body=#link{text=">", postback={nothing}, class="inactive"}};
                            false -> #listitem{body=#link{text=">", postback={page, Page + 1}}}
                        end
                   ]}
               ]}
            ]}
    end.

event({page, Page}) ->
    AllTours = wf:state(all_fetch),
    wf:update(alltour_container, all_tours(AllTours,Page));

event(arrow_left) ->
    AllTours = wf:state(all_fetch),
    Shift = wf:state(alltour_arrow_shift),
    case Shift of
        0 -> ok;
        _ -> 
            wf:state(alltour_arrow_shift, Shift-1),
            wf:update(featured_tours, featured_tours(AllTours))
    end;

event(arrow_right) ->
    AllTours = wf:state(all_fetch),
    Shift = wf:state(alltour_arrow_shift),    
    case wf:state(last_bar_tours) > (4+Shift) of
        true ->
            wf:state(alltour_arrow_shift, Shift+1),
            wf:update(featured_tours, featured_tours(AllTours));
        false ->
            ok
    end;

event(bar) ->
    wf:wire(#alert{text=?_T("Sorry, these filters are not yet implemented.")});

event(new_pressed) ->
    wf:redirect(?_U("/new-tournament"));

event(filter_pressed) ->
    Game = case wf:q(tour_game) of
        "—" -> undefined;
        "OKEY" -> game_okey;
        "TAVLA" -> game_tavla
    end,
    Players = case wf:q(tour_players) of
        "—" -> undefined;
        SN -> list_to_integer(SN)
    end,
    Quota = case wf:q(tour_quota) of
        "—" -> undefined;
        SQ -> list_to_integer(SQ)
    end,
    Date = case wf:q(tour_date_check) of
        undefined -> undefined;
        _ -> 
            SDate = wf:q(tour_date),
            list_to_tuple([list_to_integer(N) || N <- lists:reverse(ling:split(SDate, "."))])
    end,
    wf:state(game_filter, Game),
    wf:state(players_filter, Players),
    wf:state(quota_filter, Quota),
    wf:state(date_filter, Date),
    event({page, 1});

%event(clean_filter_pressed) ->
%    wf:set(tour_game, "—"),
%    wf:set(tour_players, "—"),
%    wf:set(tour_quota, "—"),
%    wf:replace(tour_date_check, #checkbox{id=tour_date_check, style="width:20px; height:20px;", checked=false}),
%    wf:state(game_filter, undefined),
%    wf:state(players_filter, undefined),
%    wf:state(quota_filter, undefined),
%    wf:state(date_filter, undefined),
%    wf:state(sort_by, undefined),
%    event({page, 1});

%event(sort_order_set) ->
%    wf:state(sort_order, case wf:q(sort_order) of
%        "AZALAN" -> descend;
%        _ -> ascend
%    end),
%    event({page, 1});

%event(per_page_set) ->
%    wf:state(per_page, case wf:q(per_page) of
%        "12 ADET" -> 12;
%        "24 ADET" -> 24;
%        _ -> 480    % all can hang up a machine while rendering and 480 is dohuya enough
%    end),
%    event({page, 1});

%event({sort_by, C}) ->
%    wf:state(sort_by, C),
%    event({page, 1});

event({bar, B}) ->
    AllTours = wf:state(all_fetch),
    wf:state(bar, B),
    wf:state(alltour_arrow_shift, 0),
    wf:update(featured_tours, featured_tours(AllTours));

event(show_page_1) ->
    wf:update(explaination_holder, [
        #br{},
        #br{},
        #link{text=?_T("Hide"), class="matchmaker_game_rules", postback=hide_explaination},
        #template{file=code:priv_dir(nsp_srv)++"/templates/tournament_exp_1.html"},
        #link{text=?_T("Hide"), class="matchmaker_game_rules", postback=hide_explaination},
        #br{},
        #br{}
    ]);

event(show_page_2) ->
    wf:update(explaination_holder, [
        #br{},
        #br{},
        #link{text=?_T("Hide"), class="matchmaker_game_rules", postback=hide_explaination},
        #template{file=code:priv_dir(nsp_srv)++"/templates/tournament_exp_2.html"},
        #link{text=?_T("Hide"), class="matchmaker_game_rules", postback=hide_explaination},
        #br{},
        #br{}
    ]);

event(hide_explaination) ->
    wf:update(explaination_holder, []);

event({filter, {Key, Value}})->
  case Key of
%    date ->
%      Date = case wf:q(tour_date_check1) of
%        undefined -> undefined;
%        _ ->
%          SDate = wf:q(tour_date1),
%          list_to_tuple([list_to_integer(N) || N <- lists:reverse(ling:split(SDate, "."))])
%      end,
%      wf:state(date_filter, Date);
    _ ->
      case wf:state(Key) of
        Value ->
          ui_deselect({Key, Value}),
          wf:state(Key, undefined);
        undefined ->
          ui_select({Key, Value}),
          wf:state(Key, Value);
        Rest ->
          ui_deselect({Key, Rest}),
          ui_select({Key, Value}),
          wf:state(Key, Value)
        end
  end,

  wf:state(game_filter, convert_state(game, wf:state(game))),
  wf:state(players_filter, convert_state(players, wf:state(players))),
  wf:state(quota_filter, convert_state(quota, wf:state(quota))),
  wf:state(sort_by, convert_state(sort_by1, wf:state(sort_by1))),
  wf:state(per_page, convert_state(per_page1, wf:state(per_page1))),
  wf:state(sort_order, convert_state(sort_order1, wf:state(sort_order1))),
  event({page, 1});

event(Any) ->
    webutils:event(Any).

convert_state(game, State) ->
  case State of
    okey -> game_okey;
    tavla -> game_tavla;
    _ -> undefined
  end;
convert_state(players, State) ->
  case State of
    undefined -> undefined;
    P -> list_to_integer(atom_to_list(P))
  end;
convert_state(quota, State) ->
  case State of
    undefined -> undefined;
    Q -> list_to_integer(atom_to_list(Q))
  end;
convert_state(sort_by1, State) -> State;
convert_state(per_page1, State) -> 
  PCS12 = list_to_atom("12 " ++ ?_T("PCS")),
  PCS24 = list_to_atom("24 " ++ ?_T("PCS")),
  case State of
    PCS12 -> ?INFO("12 PER PAGE"), 12;
    PCS24 -> ?INFO("24 PER PAGE"), 24;
    _ -> 480
  end;
convert_state(sort_order1, State)->
  case State of
    undefined -> undefined;
    A ->
      L = atom_to_list(A),
      ASC = ?_T("ASC"),
      DESC = ?_T("DESC"),
      case L of
        ASC -> ascend;
        DESC -> descend;
        _-> undefined
      end
  end;
convert_state(_, _) -> undefined.

api_event(Name, Tag, Data) ->
  webutils:api_event(Name, Tag, Data).

ui_select({Key, Value}) ->
  Id = site_utils:simple_pickle({Key, Value}),
  JSId = wf:js_escape(wf:to_list(Id)),
  wf:wire("objs('"++JSId++"').parent('li').addClass('active');").

ui_deselect({Key, Value})->
  Id = site_utils:simple_pickle({Key, Value}),
  wf:remove("for_"++Id),
  JSId = wf:js_escape(wf:to_list(Id)),
  wf:wire("objs('"++JSId++"').parent('li').removeClass('active');").

send_email(TID) ->
    Users = [begin {ok,U}=nsm_db:get(user,X#play_record.who), U end||X<-nsm_tournaments:joined_users(TID)],
    {ok,T} = nsm_db:get(tournament,TID),
    Tournament = T#tournament.description,
    Date = convert_date(T#tournament.start_date),
    Time = convert_time(T#tournament.start_time),
    Gifts = [case nsm_gifts_db:get_gift(A) of
                {error,_} -> no;
                {ok,{Gift,_}} -> [G]=Gift#gift.gift_name, G end || A <- T#tournament.awards],
    MainGift = lists:nth(1,Gifts),
    ?INFO("Main Gift: ~p",[MainGift]),
     [ begin
            Mail = User#user.email,
            case Mail of
                 undefined -> skip;
                 _ ->
            Username = User#user.username,
       {Subject, PlainText} = mail_construction:tournament(Username, Mail, Date, Time, MainGift, Tournament),
            ?INFO("Mail ~p",[{User#user.email,Subject,PlainText}]),
            nsx_msg:notify_email(Subject, PlainText, Mail) end
     end || User <- Users].

send_excuse_email(TID) ->
    Users = nsm_db:all(user),
     [ begin
            Mail = User#user.email,
            Username = User#user.username,
            {Subject, PlainText} = mail_construction:invite_10_jan(Username),
            ?INFO("Mail ~p",[{User#user.email,Subject,PlainText}]),
            nsx_msg:notify_email(Subject, PlainText, Mail)
     end || User <- Users, User#user.email /= undefined, User#user.status == ok].
