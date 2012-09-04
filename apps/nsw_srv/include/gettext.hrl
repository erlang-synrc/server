-include_lib("gettext/include/gettext.hrl").

-define(_T(S), gettext:key2str(S, site_utils:detect_language())).
-define(_TS(S, A), gettext_format:stxt(gettext:key2str(S, site_utils:detect_language()),A)).
-define(_U(U), uri_translator:translate(U, "en", site_utils:detect_language())).