-module(buy).
-author('Vladimir Baranov <baranoff.vladimir@gmail.com>').
-copyright('Paynet Internet ve Bilisim Hizmetleri A.S.').
-compile(export_all).

-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("nsm_db/include/membership_packages.hrl").
-include_lib("nsm_db/include/accounts.hrl").
-include_lib("nsm_db/include/user.hrl").

-include("elements/records.hrl").
-include("setup.hrl").
-include("common.hrl").

-define(CURRENT_PACKAGE_STATE_KEY, current_package).
-define(DEFAULT_REQ_TIMEOUT, 10000).

main() ->
    webutils:redirect_to_ssl("buy"),
    ?INFO("Buy page: ~p",[wf:user()]),
    User = wf:user(),
    if
        User /= undefined ->
            main_authorized();

        true->
            wf:redirect_to_login(?_U("/login"))
    end.

main_authorized() ->
    #template { file=code:priv_dir(nsp_srv)++"/templates/bare.html" }.

%% FIXME: add title
title() -> webutils:title(?MODULE).

%% this body funciton is common for all payment types
shared_body() ->
    try
        case wf:q(package_id) of
            %% for case when process errors in same page
            undefined ->
                Package = package(),
                Package == undefined andalso throw(package_undefined);
            PId ->
                {ok, Package} = nsm_membership_packages:get_package(PId),
                %% save package in state
                package(Package)
        end,
        #template{file=code:priv_dir(nsp_srv)++"/templates/inner_page_buy.html"}
    catch
        _:E->
            %% FIXME: if some error occurred when get package info - redirect to price
            ?PRINT(E),
            wf:redirect(?_U("/price_table"))
    end.

unregistered_popup() ->
    Msg = ?_T("Dear visitor, in case you want to be a member of us,"
        " we need to register you to our system."),
    Element = webutils:lightbox_panel_template(simple_lightbox, [
        #h1{class="head", text=?_T("Please, register")},
        #panel{class=holder, body=[
            #panel{body=Msg}, #br{},
            #cool_button{text=?_T("OK"), delegate=?MODULE, postback=ok_simple_lightbox},
            #cool_button{text=?_T("Close"), delegate=?MODULE, postback=hide_simple_lightbox},
            #grid_clear{}
        ]}
    ]),
    wf:update(simple_panel, Element),
    wf:wire(simple_lightbox, #show{}).

over_limit_popup(_Limit) ->
    Msg = ?_T("Dear visitor, we respect your enthusiasm, but we can't sell that much packages a month to a single person. Sorry."),
    Element = webutils:lightbox_panel_template(simple_lightbox, [
        #h1{class="head", text=?_T("Please, try next month")},
        #panel{class=holder, body=[
            #panel{body=Msg}, #br{},
            #cool_button{text=?_T("OK"), delegate=?MODULE, postback=hide_simple_lightbox},
            #grid_clear{}
        ]}
    ]),
    wf:update(simple_panel, Element),
    wf:wire(simple_lightbox, #show{}).

package_name()->
    Package = package(),
    [#span{text=?_T("Package")},
     io_lib:format("<strong>~p</strong>", [Package#membership_package.no])].

package_info()->
    Package = package(),
    %% FIXME: add dl to nitrogen elements
    io_lib:format("<dl>
        <dt>~s</dt>
        <dd>: ~p TL</dd>
        <dt>~s</dt>
        <dd>: ~p TL</dd>
        <dt>~s</dt>
        <dd>: ~p</dd>
        <dt>~s</dt>
        <dd>: ~p TL</dd>
        </dl>",
        [
         ?_T("Gift points (kakush) charge"), Package#membership_package.deducted_for_gifts,
         ?_T("Net membership fee"),Package#membership_package.net_membership,
         ?_T("Game quota"),Package#membership_package.quota,
         ?_T("Package fee"),Package#membership_package.amount]).


package_price()->
    Package = package(),
    ["<div class=\"price\">",strong(Package#membership_package.amount),
        #span{text=?_T("TL")},
        "</div>",
        #span{class="sub", text=?_T("VAT Included")}].


%% redirect other to webutils:event/1
event(hide_simple_lightbox) ->
    wf:wire(simple_lightbox, #hide{});
event(ok_simple_lightbox) ->
    wf:redirect_to_login(?_U("/login")),
    event(hide_simple_lightbox);
event(Any) ->
    webutils:event(Any).


%% put/get package to/from state
package(Package)->
    wf:session(?CURRENT_PACKAGE_STATE_KEY, Package).

package()->
    wf:session(?CURRENT_PACKAGE_STATE_KEY).

package_description(No)->
    io_lib:format(?_T("Membership package ~p"), [No]).

%% @doc Decode, encode info to add to purchase form
encode_info(Term) ->
    base64:encode_to_string(term_to_binary(Term)).

decode_info(String) ->
    Decoded = base64:decode(String),
    binary_to_term(Decoded).

%% Get all parameters (GET and POST) from request
req_params() ->
    %% Get query params and post params from the request bridge
    RequestBridge = wf_context:request_bridge(),
    QueryParams = RequestBridge:query_params(),
    PostParams = RequestBridge:post_params(),

    %% filter empty path
    FilterEmpty = fun(Parameters) ->
        [{Path, Value} || {Path, Value} <- Parameters,
        Path /= undefined, Path /= []]
    end,

    {FilterEmpty(QueryParams), FilterEmpty(PostParams)}.


post(Url, DataToSend) ->
    post(Url, DataToSend, ?DEFAULT_REQ_TIMEOUT).



post(Url, DataToSend, Timeout) ->
    http_request(post, "text/xml; charset=utf8", [], Url, DataToSend, Timeout).



http_request(Method, ContentType, Headers, Url, DataToSend, Timeout) ->
	%% http vesion
	V = "HTTP/1.1",

	UrlHeader = case Method of
                        get ->
                            {Url, ContentType};
                        post ->
                            {Url, Headers, ContentType, DataToSend}
                    end,

	?INFO("BUY: send request"),
	case httpc:request(Method, UrlHeader, [{timeout, Timeout}], []) of

		{ok, {{V, 200, _HMes}, _H, Result}} ->
			?INFO("http_request:<~p>:~p success", [Method, Url]),
			{ok, Result};

		{ok, {{V, Code, _HMes}, _, Mes}} ->
			?INFO("http_request:<~p>:~p ubnormal response code: Code:~p  Response: ~p", [Method, Url, Code, Mes]),
			{error, {ecode, Code}};

		{error, Reason} ->
			?INFO("http_request:<~p>:~p: unexpected error: ~p",  [Method, Url, Reason]),
			{error, Reason}
	end.

submit_form(Id) ->
    Script = lists:concat(["document.forms[\"",Id,"\"].submit();"]),
    wf:wire(#script{script=Script}).


convert_to_post(Params) ->
    PreParams = [begin
                     lists:concat([Key, "=", Value])
                 end || {Key, Value} <- Params],
    lists:flatten(string:join(PreParams, "&")).

iolist_to_string(IoList) ->
    Bin = erlang:iolist_to_binary(IoList),
    binary_to_list(Bin).

list_to_number(List) ->
    case lists:member($., List) of
        true ->
            list_to_float(List);
        false ->
            list_to_integer(List)
    end.


%% 'strong' tag implementation
strong(Val)->
    io_lib:format("<strong>~p</strong>", [Val]).

