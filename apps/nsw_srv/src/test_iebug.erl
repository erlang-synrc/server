%% -*- mode: nitrogen -*-
-module(test_iebug).
-compile(export_all).

-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("nsm_db/include/common.hrl").
-include_lib("nsm_db/include/tournaments.hrl").
-include("setup.hrl").
-include("common.hrl").
-include("elements/records.hrl").


main() ->
    #template { file=code:priv_dir(nsw_srv)++"/templates/bare.html" }.

body() ->
    "Test".

