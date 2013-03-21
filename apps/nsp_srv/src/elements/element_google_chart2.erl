-module (element_google_chart2).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

-define(JsapiUrl(CallbackName), "https://www.google.com/jsapi?autoload="++
  wf:url_encode("{\"modules\" : [ {\"name\": \"visualization\", \"version\": \"1.0\", \"packages\": [\"corechart\"], \"callback\": \"" ++ CallbackName ++"\" }] }")).

reflect() -> record_info(fields, google_chart2).

render_element(R = #google_chart2{}) ->
  Id = wf:temp_id(),
  Postback = wf_event:serialize_event_context(
    {google_chart_data, R#google_chart2.delegate, R#google_chart2.tag}, R#google_chart2.anchor, undefined, ?MODULE),
  Options = "{'title':'"++ R#google_chart2.title ++
    "', 'width':" ++ integer_to_list(R#google_chart2.width) ++
    ", 'height':" ++ integer_to_list(R#google_chart2.height) ++
    ", 'is3D': '"   ++ atom_to_list(R#google_chart2.is3D) ++"'" ++
    ", legend: '" ++ atom_to_list(R#google_chart2.legend) ++"'"++
  "}",

  error_logger:info_msg("Options: ~p", [Options]),
  error_logger:info_msg("Type: ~p", [R#google_chart2.type]),

  CallbackName = "drawChart"++Id,
  S = wf:f("$(function(){"++
    "window[\"~s\"] = function(){" ++
      "console.log('callback started');"++
      "Nitrogen.$queue_event(null, '~s', '', {" ++
        "dataType: 'json'," ++
        "success: function(r){" ++
          "console.log(r);" ++
          "var data = new google.visualization.DataTable(r);" ++
          "var options = ~s;" ++
          "var chart = new google.visualization.~s(document.getElementById('~s'));" ++
          "console.log(chart);"++
          "chart.draw(data, options);" ++
        "}," ++
        "error: function(e){console.log(e);}"++
      "});" ++
    "};"++
    "var ref = document.createElement('script');" ++
    "ref.type = 'text/javascript';" ++
    "ref.src = '~s';" ++
    "document.getElementsByTagName('head')[0].appendChild(ref);" ++
  "});", [CallbackName, Postback, Options, R#google_chart2.type, Id, ?JsapiUrl(CallbackName)]),
  wf:wire(#script{script=S}),

  wf_tags:emit_tag('div', "", [
    {class, R#google_chart2.class},
    {style, R#google_chart2.style},
    {id, Id}
  ]).

event({google_chart_data, Delegate, Tag})->
  wf_context:type(first_request),
  wf:content_type("application/json"),
  Module = wf:coalesce([Delegate, wf:page_module()]),
  Data = Module:google_chart_data(Tag),
  wf_context:data([Data]).
