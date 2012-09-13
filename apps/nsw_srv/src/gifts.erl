%% -*- mode: nitrogen -*-
-module (gifts).
-compile(export_all).

-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("nsm_srv/include/common.hrl").
-include("setup.hrl").
-include("common.hrl").


main() ->
%    webutils:add_script("/nitrogen/gifts.js"),
    webutils:add_script("/nitrogen/blockui.js"),    %PUBLIC BETA this is for blocking undone content
%    webutils:add_raw("
%        <script type=\"text/javascript\">
%        $(document).ready(function(){
%            $('div.for_blocking').block({ 
%                message: '<h1>" ++ ?_T("This feature is not yet available in beta.") ++ "</h1><h2>" ++ ?_T("You will see this part very soon.") ++ "</h2>', 
%                css: { border: '3px solid #a00' } 
%            });
%        });
%        </script>
%    "),

    #template { file=code:priv_dir(nsw_srv)++"/templates/bare.html" }.

ensafe(In) ->
    get_file:replace(
    get_file:replace(

    get_file:replace(
    get_file:replace(
    get_file:replace(
    get_file:replace(

    get_file:replace(
    get_file:replace(
    get_file:replace(
    get_file:replace(In,[304], "İ"),
                        [350], "Ş"),
                        [253], "ı"),
                        [221], "İ"),

                        [254], "ş"),
                        [222], "Ş"),
                        [240], "ğ"),
                        [208], "Ğ"),

                        [231], "ç"),
                        [199], "Ç").


title() -> webutils:title(?MODULE).

body() ->
%    #template{file=code:priv_dir(nsw_srv)++"/templates/inner_page3.html"}.
    AllGiftsData = rpc:call(?APPSERVER_NODE, nsm_gifts_db, get_all_gifts, []),
    OnlyGiftsData = lists:sublist( [Gift || {Gift, _Obj} <- AllGiftsData], 288),
%    ?INFO("Gifts: ~p", [[OneGift#gift.gift_name || OneGift <- OnlyGiftsData]]),
    [   "<section id='main'>
			<div class='top-space top-space-2'>
				<h1>Hedİyeler</h1>
			</div>",
        [
            begin
                case string:str(ensafe(OneGift#gift.gift_name), "ikolata Kap") of
                    0 -> ok;
                    _ -> ?INFO("Letter is: ~p", [hd(OneGift#gift.gift_name)])
                end,
                [
                    #label{text=ensafe(OneGift#gift.gift_name)},
                    #br{}
                ]
            end
            || OneGift <- OnlyGiftsData
        ],
        "</section>"
    ].

html_tooltip() ->
    ["<div class=\"tooltip-2\" style=\"display:none\">
	<a style=\"text-decoration: none !important;\">"++?_T("Please become our member. You will start getting those gifts as soon as being our member.")++"</a><br/>",
	#link{url=?_U("price-table"), text=?_T("Buy Now")},
	"<img src=\"/images/ico-19.png\" alt=\"\" class=\"corner png\">
    </div>"].


event(Any) ->
    webutils:event(Any).
