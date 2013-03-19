-module (element_google_pie_chart).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

reflect() -> record_info(fields, google_pie_chart).

render_element(R = #google_pie_chart{}) ->
  Id = wf:temp_id(),
  Anchor = R#google_pie_chart.anchor,
  Dlgt = R#google_pie_chart.delegate,
  Postback = wf_event:serialize_event_context({google_pie_chart_event, Dlgt}, Anchor, undefined, ?MODULE),
  U = "https://www.google.com/jsapi?autoload=" ++
    wf:url_encode("{\"modules\" : [ {\"name\": \"visualization\", \"version\": \"1.0\", \"packages\": [\"corechart\"], \"callback\": \"drawChart\" }] }"),

  S = wf:f("$(function(){"++
    "window[\"drawChart\"] = function(){"++
      "var data = new google.visualization.DataTable();"++
      "data.addColumn('string', 'Topping');"++
      "data.addColumn('number', 'Slices');"++
      "data.addRows(["++
          "['18-20 Y.O.', 3],"++
          "['20-30 Y.O.', 1],"++
          "['35-45 Y.O.', 1],"++
          "['45-60 Y.O.', 1],"++
          "['60+ Y.O.', 2]"++
        "]);"++

      "var options = {'title':'Players by Age', 'width':500, 'height':400};"++
      "var chart = new google.visualization.PieChart(document.getElementById('~s'));"++
      "chart.draw(data, options);"++
    "};"++

    "var ref = document.createElement('script');" ++
    "ref.type = 'text/javascript';" ++
    "ref.src = '" ++ U ++ "';" ++
    "document.getElementsByTagName('head')[0].appendChild(ref);" ++
  "});", [Id]),
  wf:wire(#script{script=S}),

  wf_tags:emit_tag('div', [
    {class, [piechart, R#google_pie_chart.class]},
    {style, [R#google_pie_chart.style, ";"]},
    {id, Id}
  ]).

event({google_pie_chart_event, Delegate})->
  wf_context:type(first_request),
  wf:content_type("application/json"),
  Module = wf:coalesce([Delegate, wf:page_module()]),
  wf_context:data([Module:google_pie_chart_event()]).
