-module (contact).

-include("gettext.hrl").

-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").

main() ->
    #template { file=code:priv_dir(nsp_srv)++"/templates/base.html" }.

title() -> ?_T("Address").

body() ->
  Email = fun(A) -> #link{url="mailto:"++A, text=A} end,
  [
  #panel{class="page-content", body=webutils:quick_nav()},
  #panel{class="page-content page-canvas notable", body=[
    #panel{text=?_T("ADDRESS"), class="page-content-title"},
    #p{body="PAYNET İNTERNET ve BİLİŞİM HİZMETLERİ A.Ş."},
    #p{body="Kısıklı Caddesi No: 79"},
    #p{body="Üsküdar - İSTANBUL 34662"},
    #p{body="AVCILAR v.d. 723 040 46 02"},
    #p{body=[?_T("Legal Contact"), ": Sinan Üstel ", Email("sustel@kakaranet.com")]},
    #p{body=[?_T("Technical Contact"), ": Sinan Üstel ", Email("sustel@kakaranet.com")]},
    #p{body="Tel: 0216 401 20 84"},
    #p{body="Faks: 0216 401 20 82"},
    #p{body=["Email: ", Email("info@kakaranet.com")]},
    #p{body=[?_T("Support desk link"), ": ",
      #link{url="http://kakaranet.uservoice.com/", text="http://kakaranet.uservoice.com/"}]}
   ]}
  ].

api_event(Name, Tag, Args)->
  webutils:api_event(Name, Tag, Args).

event(Other) ->
    webutils:event(Other).
