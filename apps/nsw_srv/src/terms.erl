-module(terms).
-compile(export_all).

-include_lib("nitrogen_core/include/wf.hrl").
-include("gettext.hrl").

main() ->
    #template { file=code:priv_dir(nsw_srv)++"/templates/bare.html" }.


body() ->
    #container_12{class="container",
		  body=#grid_12{body=[#h1{text=?_T("Terms of use")},
				      #br{},
				      #grid_11{class="terms-of-use", body=terms_body(site_utils:detect_language())}
				     ]}}.

terms_body("tr") ->
    #list{body=[
		#listitem{body=["1. Taraflar",
				#p{body="İşbu sözleşme ve sözleşmeyle atıfta bulunulan ve sözleşmenin ayrılmaz birer parçası olan belgelerden ve eklerden (EK-1  Gizlilik Bildirimi, ) oluşan işbu  Üyelik ve Kullanım Sözleşmesi (bundan böyle 'Sözleşme' olarak anılacaktır), PAYNET İNTERNET ve BİLİŞİM HİZMETLERİ A.Ş ile Site'yi kullanan kullanıcılar arasında , Kullanıcının Site'ye ÜYE  olması amacıyla ve sözleşmenin, ilgili Site'nin bulunduğu elektronik ortamda, Kullanıcı  tarafından onaylanması anında düzenlenmiştir."},
				#p{body="Kullanıcı, Site'ye üye olarak, işbu  Üyelik ve Kullanım Sözleşmesi'nin tamamını okuduğunu, içeriğini bütünü ile anladığını ve tüm hükümlerini onayladığını kabul, beyan ve taahhüt eder."},
				""]},
		#listitem{body=["2. Tanımlar",
				#p{body="PAYNET:  PAYNET İNTERNET ve BİLİŞİM HİZMETLERİ A.Ş. Üniversite Mahallesi E-5 Yan Yol Gürer İş Merkezi No:6 Kat:3 Avcılar İstanbul adresinde mukim. "},
				#p{body="ÜYE: Site'ye  üye olan ve Site'de sunulan Hizmet'lerden, işbu sözleşmede belirtilen koşullar dahilinde yararlanan gerçek veya tüzel kişi "},
				#p{body="SİTE: www.KAKARANET.com isimli alan adından ve bu alan adına bağlı alt alan adlarından oluşan web sitesi "},
				#p{body="PAYNET Hizmetleri (Kısaca \"Hizmet\"): Kullanıcıların site içerisinde kullanabilecekleri her türlü sosyal ve sanal uygulamalar."},
				""]},
		#listitem{body=["3. Sözleşmenin Konusu ve Kapsamı",
				#p{body="Sözleşme'nin konusu, Site'de sunulan Hizmet'lerin, bu Hizmet'lerden yararlanma şartlarının ve tarafların hak ve yükümlülüklerinin tespitidir."},
				#p{body="Sözleşme'nin kapsamı, işbu sözleşme ve ekleri ile Site içerisinde yer alan, kullanıma, üyeliğe ve Hizmet'lere ilişkin olarak PAYNET tarafından yapılmış olan bilcümle uyarı, yazı ve açıklama gibi beyanlardır. ÜYE, ÜYE Sözleşmesi'nin hükümlerini kabul etmekle, Site içinde yer alan, kullanıma, üyeliğe ve Hizmet'lere ilişkin olarak PAYNET tarafından açıklanan her türlü beyanı da kabul etmiş olmaktadır. ÜYE, bahsi geçen beyanlarda belirtilen her türlü hususa uygun olarak davranacağını kabul, beyan ve taahhüt eder."},
				""]},
		#listitem{body=["4. Üyelik ve Hizmet Kullanımı Şartları",
				#list{body=[
					    #listitem{body="Üyelik, Site'nin ilgili bölümünden, ÜYE olmak isteyen kişi tarafından Site'ye üye olmak için gerekli kimlik bilgilerinin gönderilmesi suretiyle kayıt işleminin yaptırılması ve PAYNET tarafından kayıt işleminin onaylanması ile tamamlanır. Üyelik işlemi tamamlanmadan, işbu sözleşmede tanımlanan ÜYE olma hak ve yetkisine sahip olunamaz."},
					    #listitem{body="Site'ye üye olabilmek için, reşit olmak ve işbu sözleşmenin 5.2.e. maddesi uyarınca, PAYNET tarafından, geçici olarak üyelikten uzaklaştırılmış veya üyelikten süresiz yasaklanmış olmamak gerekmektedir. Reşit olmayan veya yukarıda belirtildiği gibi, işbu sözleşmenin 5.2.e. maddesi uyarınca, PAYNET tarafından geçici olarak üyelikten uzaklaştırılmış veya üyelikten süresiz yasaklanmış olan kişilerin Site kayıt işlemlerini tamamlamış olmaları, Site üyesi olmaları sonucunu doğurmayacaktır."}
					   ]}]},
		#listitem{body=["5.1. ÜYE'nın Hak ve Yükümlülükleri",
				#list{body=[
					    #listitem{body="ÜYE, üyelik prosedürlerini yerine getirirken, Site'nin Hizmet'lerinden faydalanırken ve Site'deki Hizmet'lerle ilgili herhangi bir işlemi yerine getirirken, ÜYE Sözleşmesi'nde yer alan tüm şartlara, Site'nin ilgili yerlerinde belirtilen kurallara ve yürürlükteki tüm mevzuata uygun hareket edeceğini, yukarıda belirtilen tüm şart ve kuralları anladığını ve onayladığını kabul, beyan ve taahhüt eder."},
					    #listitem{body="ÜYE,yürürlükteki emredici mevzuat hükümleri gereğince veya diğer ÜYE’ler ile üçüncü şahısların haklarının ihlal edildiğinin iddia edilmesi durumlarında, PAYNET'in kendisine ait gizli/özel/ticari bilgileri gerek resmi makamlara ve gerekse hak sahibi kişilere açıklamaya yetkili olacağını ve bu sebeple PAYNET'ten her ne nam altında olursa olsun tazminat talep edilemeyeceğini kabul, beyan ve taahhüt eder."},
					    #listitem{body="ÜYE'lerin PAYNET tarafından sunulan Hizmet'lerden yararlanabilmek amacıyla kullandıkları sisteme erişim araçlarının (ÜYE ismi, şifre v.b.) güvenliği, saklanması, üçüncü kişilerin bilgisinden uzak tutulması ve kullanılması durumlarıyla ilgili hususlar tamamen ÜYE'lerin sorumluluğundadır. ÜYE'ler, sisteme giriş araçlarının güvenliği, saklanması, üçüncü kişilerin bilgisinden uzak tutulması, kullanılması gibi hususlardaki tüm ihmal ve kusurlarından dolayı ÜYE'lerin ve/veya üçüncü kişilerin uğradığı veya uğrayabileceği zararlara istinaden PAYNET'i, doğrudan veya dolaylı, herhangi bir şekilde sorumlu tutamazlar."},
					    #listitem{body="ÜYE’ler, siteye kayıt esnasında ve veya daha sonra herhangi bir neden ile siteye vermiş oldukları kendilerine ait ve gizli tutulması gereken ve veya gerekmeyen her türlü ( mobil, sabit telefonlar dahil olmak üzere ) iletişim imkanları üzerinden, PAYNET in kendi faaliyetleri hakkında yapacağı her türlü tanıtım ve bilgilendirme duyuruları için bu iletişim kanalları üzerinden kendilerine ulaşılmasına onay verdiklerini ve bu neden ile herhangi bir zaman ve herhangi bir şekilde bu bildirimler sebebi ile hak ve gizlilik ihlali iddiasında ve bununla ilgili bir şikayet ve ihlal tazmini talebinde bulunmayacağını açık bir şekilde kabul eder. Ancak sözleşmenin iptal edilmesi durumunda sözleşme ile belirtilen gizlilik ilkesi hakları bakidir."},
					    #listitem{body="ÜYE'ler, Site dahilinde kendileri tarafından sağlanan bilgi ve içeriklerin doğru ve hukuka uygun olduğunu kabul, beyan ve taahhüt ederler. PAYNET, ÜYE'ler tarafından PAYNET'e iletilen veya Site üzerinden kendileri tarafından yüklenen, değiştirilen veya sağlanan bilgi ve içeriklerin doğruluğunu araştırma, bu bilgi ve içeriklerin güvenli, doğru ve hukuka uygun olduğunu taahhüt ve garanti etmekle yükümlü ve sorumlu olmadığı gibi, söz konusu bilgi ve içeriklerin yanlış veya hatalı olmasından dolayı ortaya çıkacak hiçbir zarardan da sorumlu tutulamaz."},
					    #listitem{body="ÜYE'ler, PAYNET'in yazılı onayı olmadan, ÜYE Sözleşmesi kapsamındaki hak ve yükümlülüklerini, kısmen veya tamamen, herhangi bir üçüncü kişiye devredemezler."},
					    #listitem{body="PAYNET'in sunduğu Hizmet'lerden yararlananlar ve Site'yi kullananlar, yalnızca hukuka uygun amaçlarla Site üzerinde işlem yapabilirler. ÜYE'lerin, Site dahilinde yaptığı her işlem ve eylemdeki hukuki ve cezai sorumluluk kendilerine aittir. Her ÜYE, PAYNET ve/veya başka bir üçüncü şahsın ayni veya şahsi haklarına veya malvarlığına tecavüz teşkil edecek şekilde, Site dahilinde bulunan resimleri, metinleri, görsel ve işitsel imgeleri, video kliplerini, dosyaları, veritabanlarını, katalogları ve listeleri çoğaltmayacağını, kopyalamayacağını, dağıtmayacağını, işlemeyeceğini, gerek bu eylemleri ile gerekse de başka yollarla PAYNET ile doğrudan ve/veya dolaylı olarak rekabete girmeyeceğini kabul, beyan ve taahhüt eder. PAYNET, ÜYE'lerin ÜYE Sözleşmesi hükümlerine ve/veya hukuka aykırı olarak Site üzerinde gerçekleştirdikleri faaliyetler nedeniyle üçüncü kişilerin uğradıkları veya uğrayabilecekleri zararlardan doğrudan ve/veya dolaylı olarak, hiçbir şekilde sorumlu tutulamaz."},
					    #listitem{body="ÜYE'ler de dahil olmak üzere üçüncü kişiler tarafından Site'de sağlanan hizmetlerden ve yayınlanan içeriklerden dolayı PAYNET'in, PAYNET çalışanlarının veya yöneticilerinin sorumluluğu bulunmamaktadır. Herhangi bir üçüncü kişi tarafından sağlanan ve yayınlanan bilgilerin, içeriklerin, görsel ve işitsel imgelerin doğruluğu ve hukuka uygunluğunun taahhüdü, bütünüyle bu eylemleri gerçekleştiren kişilerin sorumluluğundadır. PAYNET, ÜYE'ler de dahil olmak üzere üçüncü kişiler tarafından sağlanan hizmetlerin ve içeriklerin güvenliğini, doğruluğunu ve hukuka uygunluğunu taahhüt ve garanti etmemektedir."},
					    #listitem{body="ÜYE’ler Site üzerinde yer alan kendi üyelikleri veya tanıdıklarına ait üyelikler arasında para transferini sağlayacak şekilde işlem yapmayacaklarını ve Site'nin işleyişini manipüle edecek davranışlarda bulunmayacaklarını, aksi halde PAYNET'in uğrayacağı her türlü zararı tazmin edeceklerini kabul, beyan ve taahhüt eder."},
					    ""]}]},
		#listitem{body=["5.2. PAYNET'in Hak ve Yükümlülükleri",
				#list{body=[
					    #listitem{body="PAYNET, Site'de sunulan Hizmet'leri ve içerikleri her zaman değiştirebilme; ÜYE'lerin sisteme yükledikleri bilgileri ve içerikleri, ÜYE'ler de dahil olmak üzere, üçüncü kişilerin erişimine kapatabilme ve silme hakkını saklı tutmaktadır. PAYNET, bu hakkını, hiçbir bildirimde bulunmadan ve ihbar etmeden kullanabilir. ÜYE'ler, PAYNET'in talep ettiği değişiklik ve/veya düzeltmeleri ivedi olarak yerine getirmek zorundadırlar. PAYNET tarafından talep edilen değişiklik ve/veya düzeltme istekleri, gerekli görüldüğü takdirde, PAYNET tarafından yapılabilir. PAYNET tarafından talep edilen değişiklik ve/veya düzeltme taleplerinin ÜYE'ler tarafından zamanında yerine getirilmemesi sebebiyle doğan veya doğabilecek zararlar, hukuki ve cezai sorumluluklar tamamen ÜYE'lere aittir."},
					    #listitem{body="PAYNET,Site üzerinden, PAYNET'in kendi kontrolünde olmayan üçüncü kişi hizmet sağlayıcılar ve başkaca üçüncü kişilerin sahip olduğu ve işlettiği başka web sitelerine ve/veya portallara, dosyalara veya içeriklere 'link' verebilir. Bu 'link'ler, ÜYE'ler tarafından veya sadece referans kolaylığı nedeniyle PAYNET tarafından sağlanmış olabilir ve web sitesini veya siteyi işleten kişiyi desteklemek amacı veya web sitesi veya içerdiği bilgilere yönelik herhangi bir türde bir beyan veya garanti niteliği taşımamaktadır. Site üzerindeki 'link'ler vasıtasıyla erişilen portallar, web siteleri, dosyalar ve içerikler, bu 'link'ler vasıtasıyla erişilen portallar veya web sitelerinden sunulan hizmetler veya ürünler veya bunların içeriği hakkında PAYNET'in herhangi bir sorumluluğu yoktur."},
					    #listitem{body="PAYNET, Site'de yer alan üyeliğe ilişkin ÜYE bilgilerini, ÜYE güvenliği, kendi yükümlülüğünü ifa ve Site'nin ana sayfasında adı geçen grup şirketleri tarafından oluşturulan internet siteleri ve kendi bünyesindeki Site(www.kakaranet.com) ile ilgili pazarlama, tanıtım ve iletişim yapmak ve bazı istatistiki değerlendirmeler için dilediği biçimde kullanabilir. Bunları bir veritabanı üzerinde tasnif edip muhafaza edebilir.  PAYNET tarafından sitenin iyileştirilmesi, geliştirilmesine yönelik olarak ve/veya yasal mevzuat çerçevesinde siteye erişmek için kullanılan Internet servis sağlayıcısının adı ve Internet Protokol (IP) adresi, Siteye erişilen tarih ve saat, sitede bulunulan sırada erişilen sayfalar ve siteye doğrudan bağlanılmasını sağlayan Web sitesinin Internet adresi gibi birtakım bilgiler toplanabilir."},
					    #listitem{body="PAYNET, Site'de sağlanan Hizmet'ler kapsamında ÜYE'ler arasında ortaya çıkan uyuşmazlıklarda, arabulucu veya hakem sıfatlarıyla görev almaz."},
					    #listitem{body="PAYNET, ÜYE’ler arasında site üzerinden gerçekleşen ve Site'nin işleyişine ve/veya ÜYE Sözleşmesine ve/veya Site'nin genel kurallarına ve/veya genel ahlak kurallarına aykırı ve PAYNET tarafından kabul edilmesi mümkün olmayan mesajların ve/veya içeriklerin tespit edilmesi amacıyla gerekli içerik ve/veya mesaj taraması yapabilir ve tespit ettiği mesaj ve/veya içerikleri istediği zaman ve şekilde erişimden kaldırabilir; PAYNET, bu mesaj ve/veya içeriği oluşturan ÜYE'yı yazılı uyarabilir ve/veya ÜYE'nın üyeliğine, herhangi bir ihbar yapmadan, geçici veya kalıcı olarak, son verebilir."},
					    #listitem{body="ÜYE'ler ve PAYNET hukuken bağımsız taraflardır. Aralarında ortaklık, temsilcilik veya işçi-işveren ilişkisi yoktur. ÜYE Sözleşmesi'nin onaylanması ve uygulanması sonucunda, ortaklık, temsilcilik veya işçi-işveren ilişkisi doğmaz."},
					    #listitem{body="ÜYE'lerin, Site'ye üye olurken sisteme yükledikleri \"ÜYE isimleri\"veya \"kullanıcı\" isimleri de işbu ÜYE Sözleşmesi içinde yer alan hükümlere tabi olup, ÜYE'lerin \"ÜYE ismi\" veya \"kullanıcı\" ismi belirlerken üçüncü şahısların telif hakkı, marka, ticari unvan gibi yasal haklarını ihlal etmemesi gerekmektedir. ÜYE'lerin işbu madde hükmüne aykırı davranması durumunda, PAYNET ÜYE Sözleşmesi'ne aykırı bu durumun düzeltilmesini ÜYE'dan talep edebileceği gibi dilerse ÜYE'ya önceden haber vermeksizin ÜYE'nın üyeliğini geçici veya kalıcı olarak iptal edebilir."},
					    #listitem{body="Site içerisinde üyelerin kullanımına sunulan tüm uygulamalar üyelerin eğlenmelerini sağlamak amacı ile oluşturulmuş olup, burada kullanılan hiçbir terim, deyiş, görsel ve benzeri iletişim unsurları din, dil, ırk, etnik köken, cinsiyet ayrımcılığı amacı ile kullanılmadığı gibi bu ve benzer değerleri istismar etme amacını taşımaz, ayrıca tüm uygulamalarda üyelerin benzer değer yargılarını rencide edecek tutum ve davranışlara prensip olarak izin verilmez, bu gibi durumlar ile karşılaşılaşan üyelerin yazılı müracaatı ile sebebiyet veren yorumların düzeltilmesi için gereken çaba gösterilir."},
					    #listitem{body="Taraflar, PAYNET'e ait tüm bilgisayar kayıtlarının tek ve gerçek münhasır delil olarak, HUMK madde 287'ye uygun şekilde esas alınacağını ve söz konusu kayıtların bir delil sözleşmesi teşkil ettiği hususunu kabul ve beyan eder."},
					    ""]}]},
		#listitem{body=["6. Hizmetler",
				#p{body="PAYNET tarafından verilen Hizmet'lerin temelinde, ÜYE'lerin birbirleriyle Site        aracılığıyla iletişim ve eğlence amaçlı sosyal paylaşım uygulamalarını  sağlamak bulunmaktadır."},
				#list{body=[
					    #listitem{body="ÜYE profili oluşturulurken ve ÜYE profiline yeni yorumlar eklenirken, ÜYE'lerin yaptıkları yorumlarla ilgili tüm hukuki ve cezai sorumluluk, yorumu ekleyen ÜYE'ye aittir. PAYNET, ÜYE profillerinde bulunan yorumlardan dolayı hiçbir hukuki sorumluluk kabul etmeyecektir."},
					    #listitem{body="ÜYE'ler, kendileri için oluşturulmuş ÜYE profillerini, hiçbir şart ve koşulda, başka bir ÜYE'ye devredemez veya hiçbir şekilde kullanımına izin veremezler. Kendisi için oluşturulmuş ÜYE profilini başkasına devreden veya kullanıma açan ÜYE, PAYNET'in tek taraflı olarak ve ihbarda bulunmaksızın, ÜYE Sözleşmesi'ni feshetme ve ÜYE'nın üyeliğine son verme hakkı bulunduğunu kabul, beyan ve taahhüt eder."},
					    #listitem{body="ÜYE'ler, PAYNET tarafından Site'de belirtilen kuralları veya ÜYE Sözleşmesi'nin hükümlerini ihlal ettikleri takdirde, PAYNET tarafından ÜYE ‘liklerinin iptal edileceğini ve bununla ilgili hiçbir şekilde herhangi bir hak ve tazminat talebinde bulunmayacaklarını  kabul, beyan ve taahhüt ederler."},
					    ""]}]},
		#listitem{body=["7. Ücretlendirme",
				#p{body="PAYNET, üyelik ve kullanım  ücretlerini, Site'nin ilgili sayfalarında ilan edecektir. Hizmet ücretlerine ilişkin değişiklikler, değişikliğin ilan edildiği andan itibaren geçerlilik kazanacak ve ücretle ilgili bir kampanyanın bulunması halinde, kampanyanın sona erme tarihine kadar geçerli olacaktır. Site'de aksi belirtilmediği takdirde, Site'deki Hizmet'ler karşılığı alınacak bütün ücretler, Türk Lirası (TL) olarak hesaplanacak ve tahsil edilecektir."},
				""]},
		#listitem{body=["8. Gizlilik Politikası",
				#p{body="PAYNET, ÜYE'lere ilişkin bilgileri, işbu sözleşmenin EK-1 Gizlilik Politikası bölümündeki düzenlemelere uygun olarak kullanabilir. PAYNET, ÜYE'lere ait gizli bilgileri, ÜYE Sözleşmesi'nde aksine müsade edilen durumlar haricinde, üçüncü kişi ve kurumlara kullandırmaz."},
				""]},
		#listitem{body=["9. Diğer Hükümler",
				""]},
		#listitem{body=["9.1. Fikri Mülkiyet Hakları",
				#list{body=[
					    #listitem{body="Site'nin (tasarım, metin, imge, html kodu ve diğer kodlar da dahil ve fakat bunlarla sınırlı olmamak kaydıyla) tüm elemanları (PAYNET'in telif haklarına tabi çalışmalar) PAYNET'e ait olarak ve/veya PAYNET tarafından üçüncü bir kişiden alınan lisans hakkı altında kullanılmaktadır. ÜYE'ler, PAYNET Hizmet'lerini, PAYNET bilgilerini ve PAYNET'in telif haklarına tabi çalışmalarını yeniden satamaz, paylaşamaz, dağıtamaz, sergileyemez, çoğaltamaz, bunlardan türemiş çalışmalar yapamaz veya hazırlayamaz, veya başkasının PAYNET'in Hizmet'lerine erişmesine veya kullanmasına izin veremez; aksi takdirde, lisans verenler de dahil ancak bunlarla sınırlı olmaksızın, üçüncü kişilerin uğradıkları zararlardan dolayı PAYNET'ten talep edilen tazminat miktarını ve mahkeme masrafları ve avukatlık ücreti de dahil ancak bununla sınırlı olmamak üzere diğer her türlü yükümlülükleri karşılamakla sorumlu olacaklardır."},
					    #listitem{body="PAYNET'in, PAYNET Hizmet'leri, PAYNET bilgileri, PAYNET telif haklarına tabi çalışmalar, PAYNET ticari markaları, PAYNET ticari görünümü veya Site vasıtasıyla sahip olduğu her tür maddi ve fikri mülkiyet hakları da dahil tüm malvarlığı, ayni ve şahsi hakları, ticari bilgi ve know-how'a yönelik tüm hakları saklıdır."},
					    ""]}]},
		#listitem{body=["9.2. Sözleşme Değişiklikleri",
				#p{body="PAYNET, tamamen kendi takdirine bağlı ve tek taraflı olarak, işbu ÜYE Sözleşmesi'ni, uygun göreceği herhangi bir zamanda, Site'de ilan etmek suretiyle değiştirebilir. İşbu ÜYE Sözleşmesi'nin değişen hükümleri, ilan edildikleri tarihte geçerlilik kazanacak; geri kalan hükümler, aynen yürürlükte kalarak hüküm ve sonuçlarını doğurmaya devam edecektir. İşbu ÜYE Sözleşmesi, ÜYE'nın tek taraflı beyanları ile değiştirilemez."},
				""]},
		#listitem{body=["9.3. Mücbir Sebepler",
				#p{body="Hukuken 'mücbir sebep' sayılan tüm hallerde, PAYNET, işbu ÜYE Sözleşmesi ile belirlenen yükümlülüklerinden herhangi birini geç veya eksik ifa etme veya ifa etmeme nedeniyle yükümlü değildir. Bu ve bunun gibi durumlar, PAYNET için, gecikme, eksik ifa etme veya ifa etmeme veya temerrüt addedilmeyecek veya bu durumlar için PAYNET'ten herhangi bir nam altında tazminat talep edilemeyecektir. \"Mücbir sebep\" terimi, doğal afet, isyan, savaş, grev, iletişim sorunları, altyapı ve internet arızaları, elektrik kesintisi ve kötü hava koşulları da dahil ve fakat bunlarla sınırlı olmamak kaydıyla, ilgili tarafın makul kontrolü haricinde ve gerekli özeni göstermesine rağmen önleyemediği, kaçınılamayacak olaylar olarak yorumlanacaktır."},
				""]},
		#listitem{body=["9.4. Uygulanacak Hukuk ve Yetki",
				#p{body="İşbu ÜYE Sözleşmesi'nin uygulanmasında, yorumlanmasında ve hükümleri dahilinde doğan hukuki ilişkilerin yönetiminde Türk Hukuku uygulanacaktır. İşbu ÜYE Sözleşmesi'nden doğan veya doğabilecek her türlü ihtilafın hallinde, İstanbul Mahkemeleri ve İcra Daireleri yetkilidir."},
				""]},
		#listitem{body=["9.5. Sözleşmenin Feshi",
				#p{body="İşbu ÜYE Sözleşmesi, ÜYE Site'ye üye olduğu sürece yürürlükte kalacak ve taraflar arası hüküm ve sonuçlarını doğurmaya devam edecek; ÜYE'nın üyelik süresinin dolması veya geçici veya kalıcı olarak üyeliğinin durdurulması hallerinde sona ermiş sayılacaktır."},
				#p{body="PAYNET, ÜYE'lerin işbu ÜYE Sözleşmesi'ni ve/veya, Site içinde yer alan kullanıma, üyeliğe ve Hizmet'lere ilişkin benzeri kuralları ihlal etmeleri durumunda ve özellikle aşağıda sayılan hallerde, sözleşmeyi tek taraflı olarak feshedebilecek ve ÜYE'ler, fesih sebebiyle, PAYNET'in uğradığı tüm zararları tazmin etmekle yükümlü olacaktır:"},
				#list{body=[
					    #listitem{body="ÜYE'nın, herhangi bir yöntem kullanarak, Site'nin işleyişini manipüle edecek davranışlarda bulunması,"},
					    #listitem{body="ÜYE'nın kendisi için oluşturulmuş ÜYE profilini başkasına devretmesi veya kullanıma açması,"},
					    #listitem{body="ÜYE'nın üçüncü kişilerin haklarına tecavüz eden ve/veya etme tehlikesi bulunan fillerde bulunması."},
					    ""]}]},
	       ""]};
terms_body(_) ->
    %% TODO: add english translation
    terms_body("tr").

event(Event) ->
    webutils:event(Event).
