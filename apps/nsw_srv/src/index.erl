-module (index).
-compile(export_all).

-include_lib("nitrogen_core/include/wf.hrl").

-include("gettext.hrl").
-include("elements/records.hrl").

main() ->
    webutils:add_raw("<script type=\"text/javascript\">
    	$(document).ready(function() {
    		$('.slideshow').cycle({
    			fx:		'fade',
    			prev:	'.pager .prev',
    			next:	'.pager .next',
    			pager:	'.switcher ul',
    			timeout:	5000,
    			pagerAnchorBuilder: function(idx, slide) {
    				return '.switcher ul li:eq(' + idx + ') a';
    			}
    		});
    	});
    </script>"),
    #template { file=code:priv_dir(nsw_srv)++"/templates/bare_no_uservoice.html"}.

title() -> "Main Page".

%% template specific for main
body() ->
    case wf:depickle(wf:q(x)) of
	Url when is_list(Url) ->
	    case wf:user() of
		undefined ->
		    wf:redirect(?_U("/login"));
		_User ->
		    Dashboard = ?_U("/dashboard"),
		    case Url of
			"" -> wf:redirect(Dashboard);
			_  -> wf:redirect_from_login(Dashboard)
		    end
	    end;
	_ -> no_need_to_login
    end,
    case wf:q(message) of
	undefined ->
            ok;
        Message ->
            show_message(Message)
    end,
    case wf:q(facebook) of
    "true" ->
	case wf:q(code) /= undefined of
        true ->
	    fb_utils:login();
        false ->
            main_notauthorized()
        end;
    _ ->
	wf:info("No FB tag"),
        main_notauthorized()
    end.

main_notauthorized() ->
    S = site_utils:postback_to_js_string(?MODULE, show_register),
    wf:info("Postback js: ~s~n", [S]),
    case wf:user() of   %PUBLIC BETA this is unused right now, but should be useful later. It determines the logic of main page LET'S PLAY button
	undefined ->
	    wf:wire("$('.btn-light').click(function(){"++S++";return false;});");
	_User -> ""
    end,
    #template { file=code:priv_dir(nsw_srv)++"/templates/"++site_utils:detect_language()++"/main.html"}.

event(show_register) -> 
    wf:redirect(?_U("/login/register"));

event(Other) ->
    webutils:event(Other).

api_event(Name, Tag, Args)->
    fb_utils:api_event(Name, Tag, Args).

show_message(Message) ->
    Decoded = site_utils:base64_decode_from_url(Message),
    Element = webutils:lightbox_panel_template(simple_lightbox, [
        #h1{class="head", text=?_T("System message")},
        #panel{class=holder, body=[
            #panel{body=Decoded}, #br{},
            #cool_button{text=?_T("OK"), delegate=login, postback=hide_simpe_lightbox},
            #grid_clear{}
        ]}
    ]),
    wf:update(simple_panel, Element),
    wf:wire(simple_lightbox, #show{}).
