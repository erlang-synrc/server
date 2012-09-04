-module(access_denied).

-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").

-include("gettext.hrl").

main() -> #template{file=code:priv_dir(nsw_srv)++"/templates/bare.html"}.

title() -> ?_T("Access denied!").

body() -> [?_T("Access denied!")].
