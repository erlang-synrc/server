-module (tournament_lobby_public).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("nsm_db/include/common.hrl").
-include_lib("nsm_db/include/tournaments.hrl").
-include_lib("nsm_db/include/config.hrl").
-include("setup.hrl").
-include("common.hrl").

title() -> webutils:title(?MODULE).

main() ->
  Id = wf:q(id),
  case wf:user() of
    undefined -> public;
    _ -> wf:redirect(?_U(["/tournament_lobby/id/", Id]))
  end,
  wf:state(tournament_int_id, list_to_integer(Id)),
  T = nsm_tournaments:get(Id),

  webutils:add_to_head({raw,
  ["<meta property=\"fb:app_id\" content=\"", ?FB_APP_ID, "\" />",
  "<meta property=\"og:title\" content=\"Tournament ", T#tournament.name, "\"/>",
    "<meta property=\"og:image\" content=\"", ?HTTP_ADDRESS, "/images/turnuvalar.png\" />",
    "<meta property=\"og:url\" content=\"", ?HTTP_ADDRESS, "/tournament/lobby/public/id/", Id, "\" />",
    "<meta property=\"og:description\" content=\"", T#tournament.description, "\" />",
    "<meta property=\"og:type\" content=\"kakaranet:tournament\" />"]
  }),
  #template { file=code:priv_dir(nsw_srv)++"/templates/bare_no_uservoice.html" }.

body() -> #template{file=code:priv_dir(nsw_srv)++"/templates/info_page.html"}.

content() ->
  Id = list_to_integer(wf:q(id)),
  {ok, T} = nsm_db:get(tournament, Id),
  Game = case T#tournament.game_type of
    game_okey -> "OKEY";
    game_tavla -> "TAVLA";
    game_batak -> "BATAK";
    _ -> "WTF"
  end,

  Date = integer_to_list(element(3, T#tournament.start_date)) ++ "." ++ 
         integer_to_list(element(2, T#tournament.start_date)) ++ "." ++ 
         integer_to_list(element(1, T#tournament.start_date)),

  wf:state(tour_start_time, T#tournament.start_time),
  wf:state(tour_start_date, T#tournament.start_date),
  Timer = case date() == T#tournament.start_date of
    false ->
      DDays = calendar:date_to_gregorian_days(T#tournament.start_date) - calendar:date_to_gregorian_days(date()),
      case DDays of
        1 -> "1 " ++ ?_T("day");
        N ->
          case N>0 of
            true -> integer_to_list(N) ++ " " ++ ?_T("days");
            false -> get_timer_for_now()
          end
      end;
    true ->
      wf:wire(#event{type=timer, delay=1000, postback=change_timer}),
      get_timer_for_now()
  end,
  Time = integer_to_list(element(1, T#tournament.start_time)) ++ ":" ++ 
         integer_to_list(element(2, T#tournament.start_time)) ++
         case element(2, T#tournament.start_time) of 
           0 -> "0";
           _ -> ""
         end,
  Prizes = case is_list(T#tournament.awards) of
    true ->
      GOs = [nsm_gifts_db:get_gift(A) || A <- T#tournament.awards],
      [case GO of
        {error, notfound} -> {"", "/images/tournament/nothing.png"};
        {ok, {Gift, _}} -> {site_utils:decode_letters(Gift#gift.gift_name), Gift#gift.image_small_url}
      end || GO <- GOs];
    false -> [
      {"?", "/images/tournament/new_tournament/question.png"},
      {"?", "/images/tournament/new_tournament/question.png"},
      {"?", "/images/tournament/new_tournament/question.png"}]
  end,
  {PN1, PI1} = hd(Prizes),
  {PN2, PI2} = hd(tl(Prizes)),
  {PN3, PI3} = hd(tl(tl(Prizes))),

  [
    #panel{class="tourlobby_title", body=[
      #label{class="tourlobby_title_label", body=?_U("TURNUVA LOBY")}
    ]},

    % left top block
    #panel{class="tourlobby_left_top_block", body=[
      "<center>",
        #label{class="tourlobby_left_top_block_label", body=T#tournament.name},
        #br{},
        #image{image="/images/tournament/lobby/tour_avatar.png"},
      "</center>"
    ]},

    %left bottom block
    #panel{class="tourlobby_left_bottom_block", body=[
      #br{},
      #label{class="tourlobby_left_bottom_block_title", body=?_U("Turnuva Bilgileri")},
      #br{},
      #label{class="tourlobby_left_bottom_block_label", body=?_U("Oyun Türü: ") ++ Game},
      #br{},
      #label{class="tourlobby_left_bottom_block_label", body=?_U("Kota: ") ++ integer_to_list(T#tournament.quota)}
    ]},

    %center - three panels with numbers
    #panel{class="tourlobby_orange_plask", body=[
      #label{class="tourlobby_every_plask_title", body=?_U("KATILIMCI SAYISI")},
      #br{},
      #label{class="tourlobby_every_plask_label", body=integer_to_list(T#tournament.players_count)}
    ]},

    #panel{class="tourlobby_sky_plask", body=[
      #label{class="tourlobby_every_plask_title", body=?_U("BAŞLAMA TARİHİ")},
      #br{},
      #link{body=#label{class="tourlobby_every_plask_label", body=Date}, style="text-decoration:none", title=Time}
    ]},

    #panel{class="tourlobby_blue_plask", body=[
      #label{class="tourlobby_every_plask_title", body=?_U("KALAN ZAMAN")},
      #br{},
      #label{id=lobby_timer, class="tourlobby_every_plask_label", body=Timer}
    ]},

    %prizes
    #panel{class="tourlobby_prizes", body=[
      #panel{class="tourlobby_prize_1",body=[
        "<center>",
          #panel{style="background-color:888; height:135px; display:table-cell; vertical-align:middle;",
            body=#image{style="max-width:130px; max-height:130px;", image=PI1}
          },
          #label{style="font-size:12px; color:#000;", body=PN1},
        "</center>",
        #panel{class="tourlobby_prize_star tourlobby_prize_star_1", body=
          #label{class="tourlobby_prize_star_text", body="1"}
        }]
      },
      #panel{class="tourlobby_prize_2", body=[
        "<center>",
          #panel{style="background-color:888; height:135px; display:table-cell; vertical-align:middle;",
            body=#image{style="max-width:130px; max-height:130px;", image=PI2}
          },
          #label{style="font-size:12px; color:#000;", body=PN2},
        "</center>",
        #panel{class="tourlobby_prize_star tourlobby_prize_star_2", body=
          #label{class="tourlobby_prize_star_text", body="2"}
        }
      ]},
      #panel{class="tourlobby_prize_3", body=[
        "<center>",
          #panel{style="background-color:888; height:135px; display:table-cell; vertical-align:middle;",
            body=#image{style="max-width:130px; max-height:130px;", image=PI3}
          },
          #label{style="font-size:12px; color:#000;", body=PN3},
        "</center>",
        #panel{class="tourlobby_prize_star tourlobby_prize_star_3", body=
          #label{class="tourlobby_prize_star_text", body="3"}
        }
      ]}
    ]},
    %players table
    #panel{id=players_table, class="tourlobby_table_panel", body=[""]}
  ].

api_event(_, _, _)-> ok.

event(change_timer) ->
  wf:update(lobby_timer, get_timer_for_now()),
  wf:wire(#event{type=timer, delay=1000, postback=change_timer});
event(_)-> ok.

str_plus_0(N) ->
  case N<10 of
    true -> "0" ++ integer_to_list(N);
    false -> integer_to_list(N)
  end.

get_timer_for_now() ->
  Id = list_to_integer(wf:q("id")),
  {ok, T} = nsm_db:get(tournament, Id),
  case T#tournament.status of
    canceled -> ?_T("CANCELED");
    _ ->
      TourTime = wf:state(tour_start_time),
      TourDate = wf:state(tour_start_date),
      DTime = case date() == TourDate of
        true -> 
          case wf:state(tour_long_id) of 
            [] -> calendar:time_to_seconds(TourTime) - calendar:time_to_seconds(time());
            _ -> 0  % started tournament is always either NOW or FINISHED
          end;
        false -> 0
      end,
      case DTime =< 0 of
        true -> 
          TId = wf:state(tournament_int_id),
          case rpc:call(?GAMESRVR_NODE, game_manager,get_tournament,[TId]) of
            [] -> 
              case DTime < -60 of 
                true -> ?_T("FINISHED");
                false -> ?_T("...") % right before starting on timer
              end;
            _ -> ?_T("NOW")
          end;
        false ->
          S = DTime rem 60,
          M = (DTime div 60) rem 60,
          H = (DTime div 3600),
          integer_to_list(H) ++ ":" ++ str_plus_0(M) ++ ":" ++ str_plus_0(S)
      end
  end.

