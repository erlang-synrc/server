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
  S = wf:f("
    $(function(){var t = new $.TextboxList('~s',
      {unique: true,
        startEditableBit:false,
        inBetweenEditableBits:false,
        plugins: {
          autocomplete:{
            minLenght:2,
            onlyFromValues: true,
            queryRemote: true,
            remote: {
              postback: '~s'
            }
          }
        }
      });
    });", ["#"++Id, Postback]),
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
