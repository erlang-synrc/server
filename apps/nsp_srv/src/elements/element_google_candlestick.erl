-module (element_google_candlestick).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

reflect() -> record_info(fields, google_candlestick).

render_element(R = #google_candlestick{}) ->
  Id = wf:temp_id(),
  Anchor = R#google_candlestick.anchor,
  Dlgt = R#google_candlestick.delegate,
  Postback = wf_event:serialize_event_context({google_candlestick_event, Dlgt}, Anchor, undefined, ?MODULE),
  U = "https://www.google.com/jsapi?autoload=" ++
    wf:url_encode("{\"modules\" : [ {\"name\": \"visualization\", \"version\": \"1.0\", \"packages\": [\"corechart\"], \"callback\": \"drawChart1\" }] }"),

  S = wf:f("$(function(){"++
    "window[\"drawChart1\"] = function(){"++
      "var data = new google.visualization.arrayToDataTable(["++
        "['Mon', [8,15,0], [9,9,9], [12,10,10], [12,12,0]]," ++
        "['Tue', [0,31,0], [2,38,2], [4,55,0], [5,2,2]]," ++
        "['Wed', [5,50,5], [5,55,5], [6,2,2], [8,2,0]]," ++
        "['Thu', [10,10,10], [12,3,0], [13,33,9], [14,50,0]]," ++
        "['Fri', [10,21,0], [22,10,0], [22,2,2], [22,32,0]]" ++
      "], true);"++

      "var options = {'legend':'none', 'title': 'Games started'};"++
      "var chart = new google.visualization.CandlestickChart(document.getElementById('~s'));"++
      "chart.draw(data, options);"++
    "};"++

    "var ref = document.createElement('script');" ++
    "ref.type = 'text/javascript';" ++
    "ref.src = '" ++ U ++ "';" ++
    "document.getElementsByTagName('head')[0].appendChild(ref);" ++
  "});", [Id]),
  wf:wire(#script{script=S}),

  wf_tags:emit_tag('div', [
    {class, [candlestick, R#google_candlestick.class]},
    {style, [R#google_candlestick.style, ";"]},
    {id, Id}
  ]).

event({google_candlestick_event, Delegate})->
  wf_context:type(first_request),
  wf:content_type("application/json"),
  Module = wf:coalesce([Delegate, wf:page_module()]),
  wf_context:data([Module:google_candlestick_event()]).
