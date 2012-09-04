-module(buy_mobile_fail).

-compile(export_all).


-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("nsm_srv/include/membership_packages.hrl").
-include_lib("nsm_srv/include/user.hrl").

-include("elements/records.hrl").
-include("setup.hrl").
-include("common.hrl").

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

title() -> ?_T("Buy mobile fail").


body() ->
    "Buy mobile fail".
