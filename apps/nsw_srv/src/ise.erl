-module(ise).
-export([ise/0]).

-include_lib("nitrogen_core/include/wf.hrl").
-include("common.hrl").

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

ise() -> 
"<html xmlns='http://www.w3.org/1999/xhtml' xml:lang='en' lang='en'>
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
</html>".
