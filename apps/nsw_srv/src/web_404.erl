-module(web_404).
-compile(export_all).

-include_lib("nitrogen_core/include/wf.hrl").
-include("gettext.hrl").
-include("loger.hrl").

main() ->
    main_authorized().

main_authorized() -> #template { file=code:priv_dir(nsw_srv)++"/templates/bare.html" }.

title() -> webutils:title(?MODULE).

body() ->
    PathInfo = wf:path_info(),
    wf:status_code(404),
    ?WARNING("Not found: ~p", [PathInfo]),
    #section{class="white-block", body=[
	#h1{class="", text=wf:f(?_T("~p not found~n"), [PathInfo])}
	]}.

api_event(Name, Tag, Args)->
  webutils:api_event(Name, Tag, Args).

event(Other) ->
    webutils:event(Other).
