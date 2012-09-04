%% -*- mode: nitrogen -*-
-module (gifts).
-compile(export_all).

-include_lib("nitrogen_core/include/wf.hrl").

-include("gettext.hrl").

main() ->
    webutils:add_script("/nitrogen/gifts.js"),
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
    #template{file=code:priv_dir(nsw_srv)++"/templates/inner_page3.html"}.

html_tooltip() ->
    ["<div class=\"tooltip-2\" style=\"display:none\">
	<a style=\"text-decoration: none !important;\">"++?_T("Please become our member. You will start getting those gifts as soon as being our member.")++"</a><br/>",
	#link{url=?_U("price-table"), text=?_T("Buy Now")},
	"<img src=\"/images/ico-19.png\" alt=\"\" class=\"corner png\">
    </div>"].


event(Any) ->
    webutils:event(Any).
