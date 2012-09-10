-module(buy_mobile_success).

-compile(export_all).


-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("nsm_srv/include/membership_packages.hrl").
-include_lib("nsm_srv/include/user.hrl").

-include("elements/records.hrl").
-include("setup.hrl").
-include("common.hrl").

% https://kakaranet.com/buy/mobile/basarili?mpy=20120910_151040_74&pid=3390185&
%          order=10859ea7-e5b3-44fc-80d1-b6a9eb89556c&op=1

main() ->
    User = wf:user(),
    case User of
        undefined ->
            wf:redirect_to_login(?_U("/login"));
         _->
            main_authorized()
    end.

main_authorized() ->
    #template { file=code:priv_dir(nsw_srv)++"/templates/bare.html" }.

title() -> ?_T("Buy mobile success").


body() ->
    ["Buy mobile success",
    "<!-- Google Code for order Conversion Page -->
    <script type='text/javascript'>
    /* <![CDATA[ */
    var google_conversion_id = 1008605414;
    var google_conversion_language = 'tr';
    var google_conversion_format = '2';
    var google_conversion_color = 'ffffff';
    var google_conversion_label = 'zEe1CPKo1AMQ5rH44AM';
    var google_conversion_value = 0;
    /* ]]> */
    </script>
    <script type='text/javascript' src='http://www.googleadservices.com/pagead/conversion.js'>
    </script>
    <noscript>
    <div style='display:inline;'>
    <img height='1' width='1' style='border-style:none;' alt='' src='http://www.googleadservices.com/pagead/conversion/1008605414/?value=0&amp;label=zEe1CPKo1AMQ5rH44AM&amp;guid=ON&amp;script=0'/>
    </div>
    </noscript>
    "].
