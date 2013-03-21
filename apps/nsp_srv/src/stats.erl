-module (stats).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("elements/records.hrl").
-include("common.hrl").
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
          begin
            Usr = nsm_auth:ima_gio(I),
            #listitem{body=#panel{body=[
              #image{image=avatar:get_avatar_by_username(Usr, tiny), alt=Usr},
              Usr
            ]}} 
          end || I <- lists:seq(1,20)
        ]}
      ]},

      #panel{class="top20-cell", body=[
        #h1{text="Top Gamers"},
        #hr{},
        #list{numbered=true, body=[
          begin
            Usr = nsm_auth:ima_gio(I),
            #listitem{body=#panel{body=[
              #image{image=avatar:get_avatar_by_username(Usr, tiny), alt=Usr},
              Usr
            ]}}
          end || I <- lists:seq(1,20)
        ]}
      ]},

      #panel{class="top20-cell", body=[
        #h1{text="Top Seeded"},
        #hr{},
        #list{numbered=true, body=[
          begin
            Usr = nsm_auth:ima_gio(I),
            #listitem{body=#panel{body=[
              #image{image=avatar:get_avatar_by_username(Usr, tiny), alt=Usr},
              Usr
            ]}}
          end || I <- lists:seq(1,20)
        ]}
      ]}
    ]}},

    #hr{},
    #h1{text="activiti", class="section-second-title", style="width:120px;margin-left:20px;"},
    #google_chart2{title="Games started", type='CandlestickChart', tag=rangelist, legend='none', class="range-hist"},

    #hr{},
    #h1{text="distribution",  class="section-second-title", style="width:120px;margin-left:20px;"},
    #google_chart2{title="Players by Age", type='PieChart', tag=agedist, is3D=true, class="dist-chart", legend='left'},

    #hr{},
    #h1{text="online",  class="section-second-title", style="width:120px;margin-left:20px;"},
    #panel{class="online-list", body=[
      #panel{class="friends", body=[]},
      #panel{class="groups", body=[]}
    ]}
  ]}.

google_chart_data(agedist) ->
  Data= {struct,[
    {<<"cols">>,[
      {struct,[{<<"id">>,<<"task">>},{<<"label">>,<<"">>},{<<"type">>,<<"string">>}]},
      {struct,[{<<"id">>,<<"hours">>},{<<"label">>,<<"">>},{<<"type">>,<<"number">>}]}
    ]},
    {<<"rows">>,[
      {struct,[{<<"c">>,[{struct,[{<<"v">>,<<"18-20 Y.0.">>}]},{struct,[{<<"v">>,11}]}]}]},
      {struct,[{<<"c">>,[{struct,[{<<"v">>,<<"20-30 Y.O.">>}]},{struct,[{<<"v">>,2}]}]}]},
      {struct,[{<<"c">>,[{struct,[{<<"v">>,<<"30-45 Y.O.">>}]},{struct,[{<<"v">>,2}]}]}]},
      {struct,[{<<"c">>,[{struct,[{<<"v">>,<<"45-60 Y.O.">>}]},{struct,[{<<"v">>,2}]}]}]},
      {struct,[{<<"c">>,[{struct,[{<<"v">>,<<"60+ Y.O.">>}]},{struct,[{<<"v">>,7},{<<"f">>,<<"7.000">>}]}]}]}
    ]}
  ]},
  mochijson2:encode(Data);
google_chart_data(rangelist)->
  Data = {struct, [
    {<<"cols">>, [
      {struct, [{<<"id">>, <<"Col0">>}, {<<"type">>, <<"string">>}]},
      {struct, [{<<"id">>, <<"Col1">>}, {<<"type">>, <<"timeofday">>}]},
      {struct, [{<<"id">>, <<"Col2">>}, {<<"type">>, <<"timeofday">>}]},
      {struct, [{<<"id">>, <<"Col3">>}, {<<"type">>, <<"timeofday">>}]},
      {struct, [{<<"id">>, <<"Col4">>}, {<<"type">>, <<"timeofday">>}]}
    ]},
    {<<"rows">>, [
      {struct, [{<<"c">>, [{struct,[{<<"v">>, <<"Mon">>}]}, {struct, [{<<"v">>, [8,15,0]}]}, {struct, [{<<"v">>, [9,9,9]}]}, {struct, [{<<"v">>, [12,10,10]}]}, {struct, [{<<"v">>, [12,12,0]}]} ]}]},
      {struct, [{<<"c">>, [{struct,[{<<"v">>, <<"Tue">>}]}, {struct, [{<<"v">>, [0,15,0]}]}, {struct, [{<<"v">>, [2,9,9]}]}, {struct, [{<<"v">>, [4,10,10]}]}, {struct, [{<<"v">>, [5,12,0]}]} ]}]},
      {struct, [{<<"c">>, [{struct,[{<<"v">>, <<"Wed">>}]}, {struct, [{<<"v">>, [5,15,0]}]}, {struct, [{<<"v">>, [5,9,9]}]}, {struct, [{<<"v">>, [6,10,10]}]}, {struct, [{<<"v">>, [8,12,0]}]} ]}]},
      {struct, [{<<"c">>, [{struct,[{<<"v">>, <<"Thu">>}]}, {struct, [{<<"v">>, [10,15,0]}]}, {struct, [{<<"v">>, [12,9,9]}]}, {struct, [{<<"v">>, [13,10,10]}]}, {struct, [{<<"v">>, [14,12,0]}]} ]}]},
      {struct, [{<<"c">>, [{struct,[{<<"v">>, <<"Fri">>}]}, {struct, [{<<"v">>, [10,15,0]}]}, {struct, [{<<"v">>, [22,9,9]}]}, {struct, [{<<"v">>, [22,10,10]}]}, {struct, [{<<"v">>, [22,12,0]}]} ]}]}
    ]}
  ]},
  mochijson2:encode(Data).

api_event(Name, Tag, Args)-> webutils:api_event(Name, Tag, Args).
event(Any)-> webutils:event(Any).

