%% -*- mode: nitrogen -*-
-module (gifts).
-compile(export_all).

-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("nsm_db/include/common.hrl").
-include("setup.hrl").
-include("common.hrl").
-include("elements/records.hrl").

-define(GIFTSPERPAGE, 20).

main() ->
    webutils:add_script("/nitrogen/blockui.js"),
    #template { file=code:priv_dir(nsw_srv)++"/templates/bare.html" }.

   
decode_letters(In) ->
    case is_list(hd(In)) of
        true ->
            decode_letters(hd(In));
        _ ->
            ling:replace_a_lot(In, [ 
                {[286], "Ğ"},    % 'unicode'
                {[287], "ğ"},
                {[304], "İ"},
                {[305], "ı"},
                {[350], "Ş"},
                {[351], "ş"},

                {[246], "ö"},    % both latin-5 and 'unicode'
                {[214], "Ö"},
                {[252], "ü"},
                {[220], "Ü"},

                {[231], "ç"},    % latin-5
                {[199], "Ç"},
                {[240], "ğ"},
                {[208], "Ğ"},

                {[253], "ı"},
                {[221], "İ"},
                {[254], "ş"},
                {[222], "Ş"}
            ])
    end.

decode_amp(In) ->
    ling:replace(In, "&amp;", "&").

decode_entities(In) ->
    ling:replace_a_lot(In, [
        {"&lt;", "<"},
        {"&gt;", ">"},
        {"&quot;", "'"},

        {": medium", ": small"} % this is a dirty hack for making description fit into a page. 
                                % It should be eradicated with decent design.
    ]).

assume_eq(In) ->
    ling:replace_a_lot(In, [
        {"style'", "style='"},
        {"align'", "align='"},
        {"src'", "src='"}
    ]).

decode_html(In) ->
    decode_letters(assume_eq(decode_entities(decode_amp(decode_amp(In))))).

title() -> webutils:title(?MODULE).

body() ->
    [
    "<section id='main'>
		<div class='top-space top-space-2'>
			<h1>Hedİyeler</h1>
		</div>",
        #panel{id=product_list, body=product_list_paged(1)},
        "</section>"
    ].

product_list_paged(Page) ->
    AllGiftsData = nsm_gifts_db:get_all_gifts(),
    OnlyGiftsData = lists:sublist( 
        lists:sort(
            fun(A, B) -> 
                if
                    A#gift.kakush_point < B#gift.kakush_point -> true;
                    A#gift.kakush_point == B#gift.kakush_point -> A#gift.id =< B#gift.id;
                    true -> false
                end
            end,
            [Gift || {Gift, _Obj} <- AllGiftsData, Gift#gift.enabled_on_site]
        ), 
    (Page-1) * ?GIFTSPERPAGE + 1, ?GIFTSPERPAGE),
    Buttons = case length(AllGiftsData) > ?GIFTSPERPAGE of
        true ->
            #panel{class="paging-2", style="padding: 10px 0px 0px 0px;", body=[
                #panel{class="center", body=[
                    #list{body=[
                       case Page of
                           1 -> #listitem{body=#link{text="<", postback={nothing}, class="inactive"}};
                           _ -> #listitem{body=#link{text="<", postback={page, Page - 1}}}
                        end,
                        [case N of
                            Page -> #listitem{body=#link{text=integer_to_list(N), postback={nothing}, class="inactive", 
                                style="color:#444444; font-weight:bold;"}};
                            _ -> #listitem{body=#link{text=integer_to_list(N), postback={page, N}}}
                        end
                        || N <- lists:seq(1, (length(AllGiftsData) - 1) div ?GIFTSPERPAGE + 1)],
                        case Page * ?GIFTSPERPAGE >= length(AllGiftsData) of                 
                            true -> #listitem{body=#link{text=">", postback={nothing}, class="inactive"}};
                            false -> #listitem{body=#link{text=">", postback={page, Page + 1}}}
                        end
                   ]}
               ]}
            ]};
       false ->
            []
    end,
    [   
        "<ul class='prod-list'>",
        [
			["<li>
				<div class='box'>",
                    #singlerow { style="width:100%;", cells=[
                        #tablecell{body=
        					"<h2 class='head'>"++ ?_T("Price") ++ ":&nbsp;" ++ integer_to_list(OneGift#gift.kakush_currency) ++ "<br>" 
                            ++ ?_T("Kakuş") ++ ":&nbsp;" ++ integer_to_list(OneGift#gift.kakush_point) ++ "</h2>"
                        },
                        #tablecell{
                            style="text-align:right; background:#9d9d9d; color:#fff; 
                                   font-size:18px; padding-right:5px",
                            body=[
                                affiliates:kurus_to_string(OneGift#gift.our_price),
                                #image{image="images/tl_white.svg", style="width:12px; height:16px; padding-left:3px;"}
                            ]
                        }
                    ]},
					"<div class='img'>",
                    #link{body=#image{image=OneGift#gift.image_small_url, style="width:144px; height:118px;"}, 
                        postback={show_details, OneGift#gift.description_long, OneGift#gift.image_big_url, OneGift#gift.id}},
                    "</div>
				    <strong class='prod-name' style='padding-bottom:15px; margin-top:-15px;'>",
                    #link{text=decode_letters(OneGift#gift.gift_name),  
                        postback={show_details, OneGift#gift.description_long, OneGift#gift.image_big_url, OneGift#gift.id}},
                    "</strong>",
				"</div>",
            "</li>"]
            || OneGift <- OnlyGiftsData
        ],
        "</ul>",
        Buttons
    ].

html_tooltip() ->
    ["<div class=\"tooltip-2\" style=\"display:none\">
	<a style=\"text-decoration: none !important;\">"++?_T("Please become our member. You will start getting those gifts as soon as being our member.")++"</a><br/>",
	#link{url=?_U("price-table"), text=?_T("Buy Now")},
	"<img src=\"/images/ico-19.png\" alt=\"\" class=\"corner png\">
    </div>"].

event({show_details, Description, ImageUrl, Id}) ->
    Body = [
        #panel{class=holder, body=[
            "<center>",
            #image{image=ImageUrl, style="margin:10px; width:300px; height:300px;"},
            "</center>",
            decode_html(Description),
            #singlerow{cells=[
                #tablecell{
                    body="", style="width:272px;"
                },
                #tablecell{
                    body=#cool_button{text="Hediyeyi Al", postback={buy_gift, Id}, style="display:block;"}
                }
            ]},
            #grid_clear{}
        ]}
    ],    
    wf:update(simple_panel, webutils:lightbox_panel_template(gift_lightbox, Body, hide_details)),
    wf:wire(simple_lightbox, #show{});

event(hide_details) ->
    wf:wire(simple_lightbox, #hide{});

event({buy_gift, Id}) ->
    case wf:user() of
        undefined -> wf:redirect_to_login("/");
        _ ->
            case nsm_users:can_buy_gift(wf:user(), Id) of
                true ->
                    nsm_users:buy_gift(wf:user(), Id),
                    wf:wire(#alert{text=?_T("Check it in your profile!")}),
                    wf:redirect("/gifts");
                false ->
                    wf:wire(#alert{text=?_T("Sorry, you don't have enough kakush to buy it yet.")})
            end
    end;

event({page, Page}) ->
    wf:update(product_list, product_list_paged(Page));

event(Any) ->
    webutils:event(Any).
