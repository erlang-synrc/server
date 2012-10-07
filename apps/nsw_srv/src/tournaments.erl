-module (tournaments).
-compile(export_all).

-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("nsm_db/include/tournaments.hrl").

-include("common.hrl").
-include("setup.hrl").

main() ->
    add_scripts(),
    webutils:add_script("/nitrogen/blockui.js"),    %PUBLIC BETA this is for blocking undone content
    webutils:add_raw("
        <script type=\"text/javascript\">
        $(document).ready(function(){
            $('div.for_blocking').block({ 
                message: '<h1>" ++ ?_T("This feature is not yet available in beta.") ++ "</h1><h2>" ++ ?_T("You will see this part very soon.") ++ "</h2>', 
                css: { border: '3px solid #a00' } 
            });
        });
        </script>
    "),
    #template { file=code:priv_dir(nsw_srv)++"/templates/bare.html" }.

title() -> webutils:title(?MODULE).

body() ->
    #template{file=code:priv_dir(nsw_srv)++"/templates/inner_page4.html"}.

game_type_image(T,Prefix) ->
   case T of
       game_okey -> lists:concat([Prefix,"okey.png"]);
       game_tavla -> lists:concat([Prefix,"tavla.png"]);
       game_batak -> lists:concat([Prefix,"batak.png"]);
       game_king -> lists:concat([Prefix,"king.png"]);
       _ -> "/images/tour_unknown.png"
   end.

make_bottom_panel(List) ->
    [ begin
                  {Y,M,D} = date(),
                  FormattedDate = io_lib:format("~p/~p/~p",[M,D,Y]),
                                MaxPlayer = io_lib:format("~p",[T#tournament.players_count]),
                  #panel{class=row, body=[
                    #panel{class=image,body=[#image{ image = game_type_image(T#tournament.game_type,"/images/tour_")}]},
                    #list{body=[
                       #link{class="txt",
                          url=lists:concat([?_U("/view-tournament"),"/id/",T#tournament.id]),
                          text=lists:concat([T#tournament.name," (",MaxPlayer,")"])},#br{},#br{},
                    #panel{class=text,body=[T#tournament.description]},
                    #panel{class=text,body=[FormattedDate]}
                    ]}
                ]}
                end

    || T <- List ].


all() ->
    Tournaments = nsm_tournaments:all(),
    {Left,Right} = lists:split(length(Tournaments) div 2, Tournaments),
    #panel{class="turnuvalar",body=[
        #panel{class="col-l",body=[
            #panel{class="cell",body=[
                #h3{text=?_T("Tournaments")},
                make_bottom_panel(Left)
            ]}
        ]},
        #panel{class="col-l",body=[
            #panel{class="cell",body=[
                #h3{text=?_T("Tournaments")},
                make_bottom_panel(Right)
            ]}
        ]}
    ]}.

slider() ->
    #panel{class="tournament-slider slider-container", body=[
        #panel{class="prev", body=[]},
        #panel{class="slider-content", body= tournament_slides()},
        #panel{class="next", body=[]}
        ]
    }.

event(Any) ->
    webutils:event(Any).

tournament_slides() ->
    Tours = nsm_tournaments:all(),
    [T1|Tail1] = Tours,
    [T2|Tail2] = Tail1,
    [T3|_] = Tail2,
    [#panel{class=slider, body=[slide_item(T1), slide_item(T2), slide_item(T3)]}].

slide_item(T)->
  {Y,M,D} = T#tournament.start_date,
  StartDate = io_lib:format("~p/~p/~p",[M,D,Y]),
  GameType = case T#tournament.game_type of
                                           game_okey -> "OKEY";
                                           game_batak -> "BATAK";
                                           game_king -> "KING";
                                           game_tavla -> "TAVLA";
                                           _ -> "Unknown"
                                           end,
                                MaxPlayer = io_lib:format("~p",[T#tournament.players_count]),
                                Quota = io_lib:format("~p",[T#tournament.quota]),

        #list{class="tour-item", body=[
            #listitem{body=[
              #link{
                   url=lists:concat([?_U("/view-tournament"),"/id/",T#tournament.id]),
                   body=[#image{image=game_type_image(T#tournament.game_type,"/images/tournament/slider_")}]}
            ]},
            #listitem{class=name, body=[

               #link{class=txt,text=case A = T#tournament.description of undefined -> "No Desc"; _ -> A
                                    end
                               ,url=lists:concat([?_U("/view-tournament"),"/id/",T#tournament.id])}
                                        ]    },
            [
              #listitem{body=wf:f("<span>~s:</span>~s",[Key, Val])}
              ||
              {Key, Val} <- [
                            {?_T("Game type"), ?_T(GameType)},
                            {?_T("Starting date"), ?_T(StartDate)},
                            {?_T("Num of players"), ?_T(MaxPlayer)},
                            {?_T("Quota"), ?_T(Quota)}
                            ]
            ],
           #listitem{class=line},
            #listitem{class=award, body=[#image{image="/images/img-45.jpg"}]}
        ]}.

add_scripts() ->
    webutils:add_script("/nitrogen/js/jquery.scrollTo-1.4.2-min.js"),
    webutils:add_script("/nitrogen/js/jquery.serialScroll-1.2.2-min.js"),

    webutils:add_raw("<script type=\"text/javascript\">
        function tournament_slider(){
            $('.tournament-slider').serialScroll({
                  target:'.slider-container',
		  cycle:true,
		  items:'div.slider',
		  start:0,
                  auto:2000,
		  duration:500,
		  force:true,
		  stop:true,
		  lock:false,
		  event:'click',
                  prev:'.prev-link',
                  next:'.next-link'
	      });
        }
        $(document).ready(function(){
            tournament_slider();
        });
	</script>").

