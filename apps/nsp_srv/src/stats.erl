-module (stats).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("elements/records.hrl").
-include("gettext.hrl").
-include("setup.hrl").

main() -> #template { file=code:priv_dir(nsp_srv)++"/templates/base.html" }.

body() ->
  #panel{class="page-content page-canvas", style="margin-top:20px;", body=[
    #h1{class="section-title", style="width:120px;margin-top:-30px;", text="Statistics"},
    #panel{class="top20-panel", body=#panel{class="top20-row", body=[

      #panel{class="top20-cell", body=[
        #h1{text="Top Active"},
        #hr{},
        #list{numbered=true, body=[
          #listitem{body=#panel{body=[
            #image{image=avatar:get_avatar_by_username(string:to_lower("doxtop"), tiny), alt="doxtop"},
            "doxtop"
          ]}} || I <- lists:seq(1,20)
        ]}
      ]},

      #panel{class="top20-cell", body=[
        #h1{text="Top Gamers"},
        #hr{},
        #list{numbered=true, body=[
          #listitem{body=#panel{body=[
            #image{image=avatar:get_avatar_by_username(string:to_lower("doxtop"), tiny), alt="doxtop"},
            "doxtop"
          ]}} || I <- lists:seq(1,20)
        ]}
      ]},

      #panel{class="top20-cell", body=[
        #h1{text="Top Seeded"},
        #hr{},
        #list{numbered=true, body=[
          #listitem{body=#panel{body=[
            #image{image=avatar:get_avatar_by_username(string:to_lower("doxtop"), tiny), alt="doxtop"},
            "doxtop"
          ]}} || I <- lists:seq(1,20)
        ]}
      ]}
    ]}},

    #hr{},
    #h1{text="activiti", class="section-second-title", style="width:120px;margin-left:20px;"},
    #panel{class="range-hist", body=[
      #google_candlestick{class="candle"}
    ]},

    #hr{},
    #h1{text="distribution",  class="section-second-title", style="width:120px;margin-left:20px;"},
    #panel{class="dist-chart", body=[
      #google_pie_chart{class="pie"}
    ]},

    #hr{},
    #h1{text="online",  class="section-second-title", style="width:120px;margin-left:20px;"},
    #panel{class="online-list", body=[
      #panel{class="friends", body=[]},
      #panel{class="groups", body=[]}
    ]}
  ]}.

google_pie_chart_event() ->
  Data = [{"aaaaa", "bbbbbb"}],
  List = [{array, [
    list_to_binary(L)
  ]} || {L,T} <- Data],
  mochijson2:encode({array, List}).

api_event(Name, Tag, Args)-> webutils:api_event(Name, Tag, Args).
event(Any)-> webutils:event(Any).

