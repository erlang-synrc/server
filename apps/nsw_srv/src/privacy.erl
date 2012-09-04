-module(privacy).
-compile(export_all).

-include_lib("nitrogen_core/include/wf.hrl").
-include("gettext.hrl").

main() ->
    #template { file=code:priv_dir(nsw_srv)++"/templates/bare.html" }.


body() ->
    #container_12{class="container",
		  body=#grid_12{body=[#h1{text=?_T("Privacy Policy")},
				      #br{},
				      #grid_11{class="privacy", body=privacy_body(site_utils:detect_language())}
				     ]}}.

privacy_body("tr") ->
    #list{body=[
		#listitem{body="PAYNET, kullanıcıların www.kakaranet.com sitesi üzerinden ilettikleri kişisel bilgilerini, Gizlilik Politikası ile belirlenen amaçlar ve kapsam dışında, üçüncü kişilere açıklamayacaktır."},
		#listitem{body="Kişisel bilgiler, ad soyad, adres, telefon numarası, e-posta adresi gibi kullanıcıyı tanımlamaya yönelik her türlü diğer bilgiyi içermekte olup kısaca Gizli Bilgiler olarak anılacaktır."},
		#listitem{body="PAYNET, işbu Gizlilik Politikası ve Kullanıcı Sözleşmesinde tanımlı olan haller haricinde kişisel bilgileri herhangi bir şirket veya üçüncü kişilere açıklamayacaktır."},
		#listitem{body="PAYNET, kişisel bilgileri kendi bünyesinde, müşteri profili belirlemek ve istatistiksel çalışmalar yapmak amacıyla kullanabilecektir."},
		#listitem{body="PAYNET, kişisel bilgileri kesinlikle özel ve gizli tutmayı, bunu bir sır saklama yükümlülüğü olarak addetmeyi, gizliliğin sağlanması ve sürdürülmesi, gizli bilginin tamamının veya herhangi bir kısmının kamu alanına girmesini veya yetkisiz kullanımını veya üçüncü bir kişiye ifşasını önlemek için gerekli tedbirleri almayı ve gerekli özeni göstermeyi taahhüt etmektedir. PAYNET'in gerekli bilgi güvenliği önlemlerini almasına karşın, www.kakaranet.com sitesine ve sisteme yapılan saldırılar sonucunda gizli bilgilerin zarar görmesi veya üçüncü kişilerin eline geçmesi durumunda, PAYNET'in herhangi bir sorumluluğu olmayacaktır."},
		#listitem{body="PAYNET, kullanıcılara ve kullanıcıların www.kakaranet.com sitesinin kullanımına dair bilgileri, teknik bir iletişim dosyasını (Kurabiye-Cookie) kullanarak elde edebilir. Ancak, kullanıcılar dilerlerse teknik iletişim dosyasının gelmemesi veya teknik iletişim dosyası gönderildiğinde ikaz verilmesini sağlayacak biçimde tarayıcı ayarlarını değiştirebilirler."},
		#listitem{body="PAYNET, işbu Gizlilik Politikası hükümlerini, dilediği zaman www.kakaranet.com sitesinde yayınlamak suretiyle tek taraflı olarak değiştirebilir. PAYNET'in değişiklik yaptığı Gizlilik Politikası hükümleri, www.kakaranet.com sitesinde yayınlandığı tarihte yürürlülüğe girer."},
		""]};
privacy_body(_) ->
    %% TODO: add english translation
    privacy_body("tr").

event(Event) ->
    webutils:event(Event).
