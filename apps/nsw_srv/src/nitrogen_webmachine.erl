%% @author author <author@example.com>
%% @copyright YYYY author.
%% @doc Example webmachine_resource.

-module(nitrogen_webmachine).
-export([init/1,
         to_html/2,
         allowed_methods/2,
         post_is_create/2,
         process_post/2,
         encodings_provided/2
        ]).

-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("webmachine/include/webmachine.hrl").
-include("common.hrl").


%% Resource Functions %%

-record(state, {page_module}).

init(PageModule) ->
    State = #state { page_module=PageModule },
    {ok, State}.

allowed_methods(ReqData, State) ->
    {['HEAD', 'GET', 'POST'], ReqData, State}.

post_is_create(ReqData, State) ->
    {false, ReqData, State}.

readlog() ->
    timer:sleep(500),   % for log needs time to be written
    {ok, Files} = file:list_dir("./log"),
    case lists:member("error.log", Files) of
        true ->
            {ok, Log} = file:read_file("./log/crash.log"),
            binary_to_list(Log);
        false ->
            "No log provided"
    end.

to_html(ReqData, State) ->
    PageModule = State#state.page_module,
    {ok, Data, ReqData1} = do_nitrogen(PageModule, ReqData),
    case Data of
        "Internal"++_ ->
            ErrorMessage = "
<html xmlns='http://www.w3.org/1999/xhtml' xml:lang='en' lang='en'>
<head>
    <meta http-equiv='cache-control' content='max-age=0' />
    <meta http-equiv='cache-control' content='no-cache' />
    <meta http-equiv='expires' content='0' />
    <meta http-equiv='expires' content='Tue, 01 Jan 1980 1:00:00 GMT' />
    <meta http-equiv='pragma' content='no-cache' />
	<meta http-equiv='Content-Type' content='text/html; charset=utf-8' />
	<title>Error!</title>
</head>

<body style='background:url(/images/bg-inner-page.gif) repeat-x 0 0px;'>
    <table border=0 height='100%' width='100%'><tr><td valign='center'>
    <div style='

            width:464;
            font:0.875em Arial, Helvetica, sans-serif; 
            margin-left:auto; margin-right:auto;
            padding:15px 15px 15px 15px;
            border:1px solid #ccc;
            -moz-box-shadow:0 0 3px 2px #f5f5f5;
            -webkit-box-shadow:0 0 3px 2px #f5f5f5;
            box-shadow:0 0 3px 2px #f5f5f5;
            background:#fff;'>

        <img src='/images/ise.png'>
        <h1>"++?_T("Internal Server Error!")++"</h1>
        <p>"++?_T("Something strange happend, we are really sorry.")++?_T(" From this point you can:")++"</p>
        <ul>
            <li>"++?_T("go to")++" <a href='http://kakaranet.uservoice.com/'>"++?_T("support site")++"</a>"++?_T(" for help;")++"</li>
            <li>"++?_T("write us a")++" <a href='mailto:gokhan@kakaranet.com'>"++?_T("hate letter")++"</a>; </li>
            <li>"++?_T("or go")++" <a href='javascript:' onclick='history.go(-1);'>"++?_T("back")++"</a>.</li>
        </ul>
        <p>"++?_T("Once again, we are sorry. If you have some spare time, please tell us what caused this error, and we will make it right.")++"</p>
    </div>
    </td></tr></table>
</body>
<!--" ++
readlog()
++"-->
</html>
            ",
            {ErrorMessage, ReqData1, State};
%           SecondError = lists:suffix("internal_error", wrq:path(ReqData)),
%            case SecondError of
%                %% got error second time, show internal server error this time
%                true ->
%                    {Data, ReqData1, State};
%                _ ->
%
%                    Path = case wf:user() of
%                        undefined ->
%                            ?_U("/index");
%                        _ ->
%                            ?_U("/dashboard")
%                    end,
%                    Location = lists:concat([Path, ?_U("/internal_error")]),
%
%                    %% return moved temporary status code, and redirect to dashboard
%                    {{halt, 302}, wrq:set_resp_header("Location", Location, ReqData), State}
%            end;
%
        _ ->
            {Data, ReqData1, State}
    end.


process_post(ReqData, State) ->
    PageModule = State#state.page_module,
    {ok, Data, ReqData1} = do_nitrogen(PageModule, ReqData),
    ReqData2 = wrq:set_resp_body(Data, ReqData1),
    {true, ReqData2, State}.

do_nitrogen(PageModule, Req) ->
    % Make request and response bridges...
    RequestBridge = simple_bridge:make_request(webmachine_request_bridge, Req),
    ResponseBridge = simple_bridge:make_response(webmachine_response_bridge, Req),
    nitrogen:init_request(RequestBridge, ResponseBridge),

    case PageModule of
        dynamic_route_handler ->
            nitrogen:handler(dynamic_route_handler, []);
        i18n_route_handler ->
            nitrogen:handler(i18n_route_handler, []);
        _Other ->
            nitrogen:handler(static_route_handler, PageModule)
    end,

    nitrogen:handler(path_query_handler, []),
    nitrogen:handler(nsw_srv_to_nitrogen_config_handler, []),

    nitrogen:run().


encodings_provided(Req, State) ->
    {[{"gzip", fun(X) -> zlib:gzip(X) end}], Req, State}.
