-module (element_textboxlist).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

reflect() -> record_info(fields, textboxlist).

render_element(R = #textboxlist{}) ->
  Id = wf:temp_id(),
  Anchor = R#textboxlist.anchor,
  Dlgt = R#textboxlist.delegate,
  Postback = wf_event:serialize_event_context({textboxlist_event, Dlgt}, Anchor, undefined, ?MODULE),
  Value = R#textboxlist.value,

  S = wf:f("$(function(){"++
    "var t = new $.TextboxList('~s',"++
      "{unique: true,"++
        "startEditableBit:false,"++
        "inBetweenEditableBits:false,"++
        "plugins: {"++
          "autocomplete:{"++
            "minLenght:2,"++
            "onlyFromValues: true,"++
            "queryRemote: true,"++
            "remote: {"++
              "postback: '~s'"++
            "}"++
          "}"++
        "}"++
    "});"++
    "var item = '~s'.split(',');"++
    "if(item!=''){"++
      "t.add(null, item[0],  item[1], item[3]);"++
    "}"++
    "var input = $('~s');" ++
    "var pad = $(input).outerWidth(true)-$(input).outerWidth();"++
    "var w = $(input).parent().width() - $(input).prev().outerWidth(true);" ++
    "$(input).next().width(w-pad);" ++
    "$(input).next().children('.textboxlist-autocomplete').width(w-pad);"++
  "});", ["#"++Id, Postback, string:join(Value, ","), R#textboxlist.id]),
  wf:wire(#script{script=S}),

  wf_tags:emit_tag(input, [
    {type, text},
    {class, [textboxlist, R#textboxlist.class]},
    {style, [R#textboxlist.style, "display:none;"]},
    {id, Id}
  ]).

event({textboxlist_event, Delegate})->
  wf_context:type(first_request),
  wf:content_type("application/json"),
  Module = wf:coalesce([Delegate, wf:page_module()]),
  wf_context:data([
      Module:textboxlist_event(wf:q("search"))
  ]).
