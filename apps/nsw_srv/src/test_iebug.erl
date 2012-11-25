%% -*- mode: nitrogen -*-
-module(test_iebug).
-compile(export_all).

-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("nsm_db/include/common.hrl").
-include_lib("nsm_db/include/tournaments.hrl").
-include("setup.hrl").
-include("common.hrl").
-include("elements/records.hrl").


main() ->"
<!DOCTYPE html>
<html xmlns='http://www.w3.org/1999/xhtml' xml:lang='tr' lang='tr'>
<head>
    <meta http-equiv='cache-control' content='max-age=0' />
    <meta http-equiv='cache-control' content='no-cache' />
    <meta http-equiv='expires' content='0' />
    <meta http-equiv='expires' content='Tue, 01 Jan 1980 1:00:00 GMT' />
    <meta http-equiv='pragma' content='no-cache' />
	<meta http-equiv='Content-Type' content='text/html; charset=utf-8' />
	<title>Kakaranet</title>
	<link rel='stylesheet' type='text/css' href='/css/kakara.min.css' media='all' />
	<link rel='stylesheet' href='/css/tr.css' />
	<script src='/js/kakara.min.js' type='text/javascript' charset='utf-8'></script>
	<!--[if IE]>
		<link rel='stylesheet' type='text/css' href='/css/ie.css' media='all'/>
	<![endif]-->
	<!--[if lt IE 7]>
		<script src='/nitrogen/js/ie-hover.js' type='text/javascript'></script>
		<script src='/nitrogen/js/ie-png.js' type='text/javascript'></script>
	<![endif]-->
	<!--[if lt IE 9]>
		<link rel='stylesheet' type='text/css' href='/css/ie7.css' media='all'/>
	<![endif]-->
	<!--[if IE 9]>
		<link rel='stylesheet' type='text/css' href='/css/ie9.css' media='all' />
	<![endif]-->
	<!-- Most of scripts and css files were wiped out, you can see old state of this file by command
	git cat-file blob 4a15d19e2b760379d3776a95fcb84bac9b86fab3 -->
	
    <link href='/nitrogen/video-js/video-js.css' rel='stylesheet'>
    <script src='/nitrogen/video-js/video.js'></script>

    <link href='/nitrogen/guiders-js/guiders-1.2.8.css' rel='stylesheet'>
    <script src='/nitrogen/guiders-js/guiders-1.2.8.js'></script>

    <script src='/nitrogen/js/form.js' type='text/javascript'></script>
    <script src='/nitrogen/swfobject.js' type='text/javascript' charset='utf-8'></script>
    <script src='/nitrogen/audio-player/audio-player.js' type='text/javascript' charset='utf-8'></script>
    <script type='text/javascript'>

        AudioPlayer.setup('/nitrogen/audio-player/player.swf', {width: 290});

        function upd_scrollers(){
            $('.scroller').serialScroll({
                cycle:true,
                items:'img',
                start:0,
                duration:500,
                force:true,
                stop:true,
                lock:false,
                exclude:1,
                event:'click'
            });
            $('.scroller_container').each(function(i){
            $(this).find('.scroller_prev').bind('click',{mel:i},
                function(event){$('.scroller').each(function(j){
                    if(j==event.data.mel){$(this).trigger('prev')}})
                });
            $(this).find('.scroller_next').bind('click',{mel:i},
                function(event){$('.scroller').each(function(j){
                    if(j==event.data.mel){$(this).trigger('next')}})
                });
            });
        }

        function game_slider(){
            $('.gallery-game').serialScroll({
                target:'.slider-container',
                cycle:true,
                items:'div.slider',
                start:0,
                auto:2000,
                duration:500,
                force:true,
                stop:true,
                lock:false,
                event:'click',
                prev:'.prev-link',
                next:'.next-link'
            });
        }

        function clear_inner_textarea(){
            var VRR = $('.inner_textaera');
            VRR.val('');
        }

        function getOffset( el ) {
            var _x = 0;
            var _y = 0;
            while( el && !isNaN( el.offsetLeft ) && !isNaN( el.offsetTop ) ) {
                _x += el.offsetLeft - el.scrollLeft;
                _y += el.offsetTop - el.scrollTop;
                el = el.offsetParent;
            }
            return { top: _y, left: _x };
        }

        function clear_tauto(){
            clear_inner_textarea();
            var Ss = $('.wfid_to_kaka_user').size();

            var ACBY = getOffset( document.getElementById('atocompletetextbox') ).top - 225;
            var ACBX = getOffset( document.getElementById('atocompletetextbox') ).left - 35;

            $('.ui-autocomplete').css('left', ACBX + 'px');
            $('.ui-autocomplete').css('top', ACBY + 'px');

            if((Ss % 8)==0 && Ss > 1){
                $('.wfid_form001toRow').css('height', (parseInt(Ss/8)+1)*30 + 'px' );
                $('.row .inner-row').css('height', ((parseInt(Ss/8)+1)*32+8) + 'px' );
            }else if(Ss < 8){
                $('.wfid_form001toRow').css('height', '30px' );
                $('.row .inner-row').css('height', '30px' );
            }
            if(Ss == 0){
                $('.wfid_form001toRow').css('display', 'none');
            }
        }

        function remove_all_tos(){
            $('.wfid_form001toRow').css('display', 'none');
            $('.wfid_flashm').children().remove();
        }

        function set_focus_to_search(){
            $('.wfid_add_entry_textbox').focus();
        }

        function upd_parent_to_float(BoxId){
            $('.wfid_' + BoxId).css('float', 'left');
            $('.wfid_' + BoxId).css('clear', '');
        }

        var TestStOpt = new Array();
        var NitrogenDropDownPostBackOptions = {values_array:'TestStOpt', select_number:3};
        var MyFeedEvent = '';
        function add_myfeed_to(){

            var ACBY = getOffset( document.getElementById('atocompletetextbox') ).top - 225;
            var ACBX = getOffset( document.getElementById('atocompletetextbox') ).left - 35;

            var Ss = $('.wfid_to_kaka_user').size();
            if(Ss == 0){
                Nitrogen.$queue_event(null, MyFeedEvent, '');
            }
            $('.ui-autocomplete').css('left', ACBX + 'px');
            $('.ui-autocomplete').css('top', ACBY + 'px');
        }

        function delete_flash_to(Ele){
            var E1 = $(Nitrogen.$anchor_path);
            var E2 = $(Nitrogen.$anchor_path).parent();
            E2.remove();
        }

        function qtip_all_links(){
            //%PHASE1 This came from qTip demo page to make default tooltips look better
            $('#content a[href][title]').qtip({
                content: {
                    text: false // Use each elements title attribute
                },
                hide: {
	                event: 'click'
                },
                show: { delay: 1500 }
            });
        }

        $(document).ready(function(){
            upd_scrollers();
            game_slider();

            //%PHASE1
            qtip_all_links();

            objs('sendentry').qtip({    // share button

                // these properties are actually working
                content: 'Buraya tıklayarak yeni bir gönderi girebilirsiniz. Merak etmeyin, istediğiniz zaman silebilirsiniz.',
                show: { delay: 1500 },
                position: {
	                my: 'top right',
	                at: 'bottom left'
                }
            });

            objs('statuschanger').qtip({    // status combobox
                show: { delay: 1500 },
                content: 'Durum bilginizi değiştirebilirsiniz.'
            });

            objs('friendslink').qtip({content:{text:false}, show: { delay: 1500 }});
            objs('groupslink').qtip({content:{text:false}, show: { delay: 1500 }});

            objs('directmessageslink').qtip({content:{text:false}, show: { delay: 1500 }});
            objs('myfeedlink').qtip({content:{text:false}, show: { delay: 1500 }});

            objs('leavegrouplink').qtip({content:{text:false}, show: { delay: 1500 }});

            objs('add_entry_textbox').autosize();

        });
	</script>
	<script src='/nitrogen/js/jquery.serialScroll-1.2.2-min.js' type='text/javascript' charset='utf-8'></script>
	<script src='/nitrogen/js/jquery.scrollTo-1.4.2-min.js' type='text/javascript' charset='utf-8'></script>
	<script src='/nitrogen/js/jquery.autosize-min.js' type='text/javascript' charset='utf-8'></script>

</head>
<body class='info_gifts'>
<div id='fb-root'></div>
	<section class='wrapper'>
		<header>
	<div class='block'>
		<strong class='logo vcard'><a href='/' class='fn org url wfid_temp515209 link' target='_self'>KakaraNet - Public Beta</a></strong>
		<div class='top wfid_temp518830 panel'><div class='ar wfid_temp518867 panel'><div class='box wfid_temp518898 panel'><a href='/giris/facebook' class='al wfid_temp518927 link' target='_self' style='margin-top:-2px;'><img class='wfid_temp518962 image' src='/images/fb_connect_tr.png'/></a><ul class='user-menu wfid_temp518997 list'><li class='wfid_temp519028 listitem'><a href='/giris' class='login wfid_temp519061 link' target='_self'>Giriş</a></li><li class='wfid_temp519100 listitem'><a href='/giris/register' class='signup wfid_temp519128 link' target='_self'>Üye Ol</a></li></ul></div></div></div><input class='wfid_temp519169 wfid_temp515527' type='hidden' value=''/><nav><ul class='wfid_temp519207 list'><li class='wfid_temp519233 listitem'><a href='/' class='wfid_temp519400 wfid_mainmenumainpage link' target='_self' title='Burada oyun oynayabilirsiniz'>Ana Sayfa</a></li><li class='wfid_temp519451 listitem'><a href='/panel' class='wfid_temp519480 wfid_mainmenumypage link' target='_self' title='Diğerleriyle bilgi paylaşabilirsiniz'>Benim Sayfam</a></li><li class='wfid_temp519527 listitem'><a href='/hediyeler' class='wfid_temp519555 wfid_mainmenugifts link' target='_self' title='Buradan kazandığınız hediyelerinizi görebilirsiniz.'>Hediyeler</a></li><li class='wfid_temp519604 listitem'><a href='/turnuvalar' class='wfid_temp519632 wfid_mainmenutournaments link' target='_self' title='Turnuvalara katılarak yeteneğinizi konuşturabilirsiniz'>Turnuvalar</a></li><li class='wfid_temp519684 listitem'><a href='/gruplar' class='wfid_temp519712 wfid_mainmenugroups link' target='_self' title='Grup ayarlarınızı buradan yönetebilirsiniz'>Gruplar</a></li></ul></nav>
      <script>
      (function(){
          var C = {text:false};
          var P = {my:'top right', at:'bottom left'};
          var S = {delay: 1500};
          objs('mainmenumainpage').qtip({content: C, position: P, show: S} );
          objs('mainmenumypage').qtip({content: C, position: P, show: S});
          objs('mainmenugifts').qtip({content: C, position: P, show: S});
          objs('mainmenutournaments').qtip({content: C, position: P, show: S});
          objs('mainmenugroups').qtip({content: C, position: P, show: S});
      })();
      </script>
	</div>
</header>
<div class='wfid_temp519896 wfid_simple_lightbox lightbox panel' style='position: fixed; top: 0px; left: 0px; bottom: 0px; right: 0px; display: none;'><div class='lightbox_background wfid_temp520080 panel' style='position: fixed; top: 0px; left: 0px; bottom: 0px; right: 0px; z-index: 98; background-color: #000000;'></div><table border='0' cellpadding='0' cellspacing='0' class='wfid_temp520119 table' style='position: fixed; top: 0px; left: 0px; width: 100%; height: 100%; z-index: 99; overflow:auto;'><tbody><tr class='wfid_temp520153 tablerow'><td class='wfid_temp520180 tablecell' style='vertical-align: middle;' align='center' valign='middle' colspan='1' rowspan='1'><center><table><tr><td><div class='wfid_temp520215 wfid_simple_panel panel'></div></td></tr></table></center></td></tr></tbody></table></div>

		<div class='list-top-photo-h'>
        <ul class='hemen-nav list-top-photo wfid_temp531664 list'><li class='mkh_active wfid_temp531729 listitem'><a href='/oyun-kuran/okey' class='wfid_temp531760 link' target='_self'><div class='box wfid_temp531791 panel'><div class='img wfid_temp531815 panel'><img class='wfid_temp531835 image' style='width:160px;height:88px' src='/images/img-007.jpg'/></div><div class='img img-2 wfid_temp531859 panel'><img class='wfid_temp531881 image' style='width:160px;height:88px' src='/images/img-008.jpg'/></div></div><div class='stat wfid_temp531901 panel'><span class='wfid_temp531922'>Pek yakında...</span></div><em><img class='png' src='/images/text-okey.png' alt='' width='77' height='77' ></em></a></li><li class='mkh_active wfid_temp531945 listitem'><a href='/oyun-kuran/backgammon' class='wfid_temp531966 link' target='_self'><div class='box wfid_temp531987 panel'><div class='img wfid_temp532006 panel'><img class='wfid_temp532026 image' style='width:160px;height:88px' src='/images/img-005.jpg'/></div><div class='img img-2 wfid_temp532048 panel'><img class='wfid_temp532069 image' style='width:160px;height:88px' src='/images/img-006.jpg'/></div></div><div class='stat wfid_temp532094 panel'><span class='wfid_temp532117'>Pek yakında...</span></div><em><img class='png' src='/images/text-tavla.png' alt='' width='77' height='77' ></em></a></li><li class='wfid_temp532144 listitem'><a href='#' class='wfid_temp532167 link' target='_self'><div class='box wfid_temp532189 panel'><div class='img wfid_temp532208 panel'><img class='wfid_temp532227 image' style='width:160px;height:88px' src='/images/img-003.jpg'/></div><div class='img img-2 wfid_temp532249 panel'><img class='wfid_temp532271 image' style='width:160px;height:88px' src='/images/img-004.jpg'/></div></div><div class='stat wfid_temp532293 panel'><span class='wfid_temp532313'>Pek yakında...</span></div><em><img class='png' src='/images/text-king.png' alt='' width='77' height='77' ></em></a></li><li class='wfid_temp532335 listitem'><a href='#' class='wfid_temp532354 link' target='_self'><div class='box wfid_temp532375 panel'><div class='img wfid_temp532394 panel'><img class='wfid_temp532415 image' style='width:160px;height:88px' src='/images/img-001.jpg'/></div><div class='img img-2 wfid_temp532435 panel'><img class='wfid_temp532456 image' style='width:160px;height:88px' src='/images/img-002.jpg'/></div></div><div class='stat wfid_temp532478 panel'><span class='wfid_temp532499'>Pek yakında...</span></div><em><img class='png' src='/images/text-batak.png' alt='' width='77' height='77' ></em></a></li><li class='wfid_temp532519 listitem'><a href='#' class='wfid_temp532538 link' target='_self'><div class='box wfid_temp532559 panel'><div class='img wfid_temp532580 panel'><img class='wfid_temp532606 image' style='width:160px;height:88px' src='/images/img-009.jpg'/></div><div class='img img-2 wfid_temp532627 panel'><img class='wfid_temp532649 image' style='width:160px;height:88px' src='/images/img-010.jpg'/></div></div><div class='stat wfid_temp532914 panel'><span class='wfid_temp532995'>Pek yakında...</span></div><em><img class='png' src='/images/text-sorbi.png' alt='' width='77' height='77' ></em></a></li></ul>
</div>

<section id='main'>
        <img class='wfid_temp546549 image' style='margin-top:-15px; margin-left:-24px;' src='/images/more_info/top_plask_1.png'/><label class='info-page-menu-elements info-page-gifts-label wfid_temp546602 label'>HEDİYELER</label><a href='/turnuvalar-bilgi' class='info-page-menu-elements info-page-tournaments-link wfid_temp546656 link' target='_self'>TURNUVALAR</a><a href='/sosyal-bilgi' class='info-page-menu-elements info-page-social-link wfid_temp546719 link' target='_self'>SOSYALLEŞMEK</a><a href='/oyun-kuran-bilgi' class='info-page-menu-elements info-page-matchmaker-link wfid_temp546777 link' target='_self'>KENDİ DÜNYAN</a><a href='/neden-bilgi' class='info-page-menu-elements info-page-membership-link wfid_temp546837 link' target='_self'>ÜYELIK PAKETİ</a><a href='/giris/register' class='info-page-menu-elements info-page-join-link wfid_temp546896 link' target='_self'>ÜYE OL!</a><div class='info-page-header-panel wfid_temp546954 panel'><img class='info-page-bg wfid_temp546998 image' src='/images/more_info/info_gifts_bg.png'/><div class='wfid_temp547034 panel' style='top:90px; left:230px; font-size:50px;'>Üye Olduğunuz An</div><div class='wfid_temp547069 panel' style='top:145px; left:225px; font-size:70px;'>Hediyenizi Alın!</div><div class='wfid_temp547103 panel' style='top:225px; left:230px; font-size:30px; color:#FC6404;'>Artık karşılıksız verme devri bitti...</div></div><div class='info-page-content-panel wfid_temp547135 panel'><div class='wfid_temp547176 panel' style='top:385px; left:400px; font-size:18px;'>Bundan sonra oynamaya ayırdığınız</div><div class='wfid_temp547210 panel' style='top:410px; left:430px; font-size:25px; color:#FC6404;'>zaman ve yaptığınız harcamalar</div><div class='wfid_temp547245 panel' style='top:444px; left:460px; font-size:18px;'>size <b>hediye</b> olarak geri dönüyor. </div></div><div class='info-page-content-panel wfid_temp547279 panel'><div class='wfid_temp547318 panel' style='top:627px; left:331px; font-size:18px;'>Üyelerimiz, hiçbir şarta bağlı</div><div class='wfid_temp547351 panel' style='top:651px; left:237px; font-size:18px;'>olmadan hediye puanları kazanırlar.</div><div class='wfid_temp547384 panel' style='top:674px; left:51px; font-size:25px; color:#FC6404;'>Kazanmak yada kaybetmek fark etmez.</div><div class='wfid_temp547417 panel' style='top:705px; left:144px; font-size:18px;'>Oyun oynayan her oyuncu hediye puanı kazanır.</div><div class='wfid_temp547448 panel' style='top:730px; left:72px; font-size:18px;'>Kaybeden de kazanır, kazanan daha da çok kazanır.</div></div><div class='info-page-content-panel wfid_temp547486 panel'><div class='wfid_temp547787 panel' style='top:842px; left:420px; font-size:18px;'>Hesabında biriken puanlar ile</div><div class='wfid_temp547820 panel' style='top:867px; left:439px; font-size:20px;'>hediye kataloğundan <b>istediği</b> hediyeyi alabilir.</div></div><div class='info-page-content-panel wfid_temp547849 panel'><div class='wfid_temp547885 panel' style='top:1052px; left:209px; font-size:18px;'>İsterse, puanlı üyelik paketi satın alarak</div><div class='wfid_temp547914 panel' style='top:1078px; left:186px; font-size:20px;'>daha çok <b>puan</b> kazanır.</div></div><div class='info-page-content-panel wfid_temp547942 panel'><div class='wfid_temp547977 panel' style='top:1287px; left:444px; font-size:18px;'>Sanal alemden satın alabileceğiniz</div><div class='wfid_temp548006 panel' style='top:1311px; left:468px; font-size:20px;'><b>çok geniş bir ürün yelpazesi</b> sizi bekliyor…</div></div>
</section>

		<footer><ul class='navbar wfid_temp549678 list'><li class='wfid_temp549708 listitem'><a href='/hediyeler' class='wfid_temp549727 link' target='_self'>Hediyeler</a></li><li class='wfid_temp549748 listitem'><a href='/kullanim-sartlari' class='wfid_temp549764 link' target='_self'>Kullanım Şartları</a></li><li class='wfid_temp549785 listitem'><a href='/gizlilik' class='wfid_temp549801 link' target='_self'>Gizlilik Politikamız</a></li><li class='wfid_temp549821 listitem'><a href='http://kakaranet.uservoice.com/' target='_blank'>Yardım & Destek</a></li><li class='wfid_temp549840 listitem'><a href='/iletisim' class='wfid_temp549856 link' target='_self'>İletişim:</a></li><li class='wfid_temp549876 listitem'>2011 &copy; Kakaranet. Tüm hakları saklıdır.<br />Kakaranet, Paynet A.Ş. tescilli markasıdır.<br/>Public Beta - 17 Aug 2012</li><li class='wfid_temp549894 listitem'><input name='.wfid_temp549909' id='.wfid_temp549909' type='checkbox' class='wfid_temp549909 wfid_replay_guiders checkbox' value='on' not_checked='true'/><label for='.wfid_temp549909'>Açıklamaları Göster</label></li></ul><a href='javascript:' class='lang wfid_temp549945 link' target='_self'><img alt=' ' class='wfid_temp549966 image' style='width:14px;height:10px;' src='/images/flag-uk.png'/>English</a></footer>
	</section>
<script type='text/javascript'>
Nitrogen.$set_param('pageContext', 'Iw_0joNQAAAdIHic3Vlbj9vGFVaduq03bhOkF7hv040TUvWKq13bwVa2DHhrFzXQOIbtFg2CgBiRQ3IkkkPPDFcrL_RTivyGvvSpb00f-th_1HNIXXgZrdZrIwjKF2ku5zv3M3PI6Erc6XSuRFf9zvWMhsz1RKrZqZ50djRLsjt373z22R2_s8PTQLghD7Qa-52rnkiYRsL3kfCDiKZ-zOSS1u_8DP4FPHQXC37nVz4LaB5rt7lwLU9hiafMH5uh3o9FZfvPlzi12a0gNzIpPKaUK1nIlZazNe2vU1yDecPaVtyfetSL2Jrglys16_PbgV7lrMr5o4zqyG1MbkX5QIGOXKQVuyueZDFzWwvbJVKaapNqjfk1EMbDD6IrfufHypM80wonfogT70k6ndz47w6B537M0wmJJAuG1n7KtRQhS_dPuM9Eb6xWfxxPKYtIFg8tpWcxUxFj2npQQpT4RElvM4YzVtaD-_vl1gc75_AOc9gvFVIu_x44h87RJUTYCGUSpgKxu4YA0kDIBCh2iZ5lbLiL_tgf0xNaEuxWgM7BUdNAjMbM0-cgES-iUjE93M110Du6IDLNfS56WUxnTNYGb89pg8KlwfB5iOyeldwALs_sTYItRAIz7O6Rsyn3dTQgh7_rz7v31nBBnnoa8oLkme8CNxFDTCu7e7bagc9N23KWi1YX-EpO4xfFhF3fiY8382I20DJne601DjVVDSyehFZ7ERJL6kG_veDnkqKUg7t9wypEi7eJn9Ii27AUC28yCGisDGvs1Itznw0ODEsnLNUDy4u5N7Fqq2jY6rhqtaKsUCgSaD8GtdFeWt7mLWPriKuuAyXFt3fXCJlkJ7tdZ4TTC_Z7ZwmLB3xusMkSvRC3e9bwYF2CcbftRHx4YI-HwwLB8ammDnBDqFI-LXkYMmlbKJjVnc-7LZC2RcyqpRDv30vVULCLqVYdzw35FdIEDqEYS6Ipu0KK4s96uG17hkGehAyC0CkBe-vgapvrAtno85MF0pskJc21GBz2TRn53edrNSnbyxihYC386eEJaNiCnoYt-FNueSPvgompdHkKLnCxeFPJaNPNJ1SSvzx_ToaFwyt7maRWI5pgn3NCY9uytoUV018EARwENmEx6ZI2S_cUOPbvtedn7flpxGNWIH36KfkNV0_pUxw5omDxJxZoYGFaeikyWGmyxwfY3xo2MHo4LlMWx_faRLMaEaJXaGDYJgGZKxTPwAFpA3heG0k4O2VKzggGHDDcIzGIMkB55xfxtcbwb7rYHAZt279QiyiYBtx3tXAndELdXJWnK3_N7OoZvaR6-PvjL4Gu4nJfeHmCJQzmHscM_x7PnkANpVpAhwL3Xs1QjpE4tcA9ji4MeXh4ty0SgP_17cDj0rW37zZERz1z3kN7LclAS7ha2haSWHsl71vEyk6beXAOLehSkn65Jq3RQom3wdCfkKPucNjHsIXRA3JgOBFWrsCLZ79_oMVzMV0yihgPIxTTzvAG9yTVALt_1L110P3t7f6COem2QxJRpZiSMtd70gRpwjy8ddTdADtnUP5QM1DlPih2WVWs2_3LC20mnjeNj0EOFeZNZfS5wpsr8klFypoRMT83NyVLxAlz4TCFrDJeYy_Ld00bUxUlSAXF0ocyY3edkq29pVZDVgFnL1eY8QoqBVxUNklIfd-FhIPmd5ViTkG7jQne47Oi-iGXIBZU28fi9Im_iZMFoVZuKO1QkKAViuTcZIUmVVH5kGrDgYUl5iVT-oX-ArqcIRy3U_JQSjqr6oObni56mUdSZI_ENH0mlD6m3gTIQD2sm2dwMOZMuRTJB9YKFbgrFkO_56Z5MmJycHteh_589gfG_Md4VQAYy7rXth6aPZkFsM0tq_v_SxG-8KlTJTo3iZeOcm6-ylnO3OIKZqd5HO9VLd0IiTIsmiH1HR0Phrhced5nSF1mN_oejG24vz0-KEy41p2mXiSgN6M6Mpzzjw_P2e6Uado0-uPDC5aTV5pnRaXD62qr1u3vf_Lsjw9fgMAvoaUhHvQVJJAiIa9e8gy0TQTBt51w_SEJnTCyeLkFYxEDsILrtpiQEdOayabNPy7ekUEW0a_wVdLXX2muY_Y1mB5lMr0RKPcPDPdDfDCAB6S42oPc5M_wg10cXOeKgFekwCdUQ182yjVrN2Pt-3wE7Qzyu9ZcKFsFYmrgN0CpSEwBCkMECg45gKamEcMb2oOb9jJ98Yig_mzdlza81Xj7Uo-IWuvYyOuVm2uzzdBoEInRGDJEsdQvjpiV30pA0BcCk4ChtUh3WtaADTpi4KIMKjSTmjNFcD_1dA4sZ2Qq5ISn4eYgsI6hPZxRor_9-wQsSiWdkBlLORlxScJ__QPEkpyEXLIRj7lUPOWvHfI5w31MJ2zG0z3ClWY-__YbXCSvaULhfIXmpUJh6mVNnjS0i0Lxon01xU8ChFhdiCyvQ-0dFHUcCTBfUjQVhiBrRY_JP5rqXHkRTcOiQtd9VCyCTZORgNp_WU3XPnmUyzwBD8QhGo8DIdj239_ouhfaXbFB8EByCC1VdNFLsZeMzopcL1J9vmeWsvlmpQQNpcizt8I0gPqgnQcRpRSUwncscHmNeMcCwy3rhBWmeMfApvsmHqutZhTpr63eXeO3jR-V_yedXu0d_rj4dlN7k1V8CjjsJTx1xqpG2jWSLrp9ILtjJvvYRLYUe0Ew3vB550OO1Y_ryuelG8svPO2lrV-LroOslY9Cv1hC1ae3f3QC31Y_Ln3ED45StzG5FeVDxbxcmnVrL1XgwFjvjTqdq09Gnet_-8-o85PJP_8HPnZSxg');

Nitrogen.$anchor('.wfid_temp515527', 'page');obj('.wfid_temp515527').is_facebook = function() {var s = Nitrogen.$encode_arguments_object(arguments);Nitrogen.$anchor('.wfid_temp515527', 'page');Nitrogen.$queue_event('page', 'IHDiYYNQAAAA4XicdY1BCsIwEEWHtkptFTyJoFA8g6cISWdiQjQJdmK9jAdy6Y0MKqgLV5__eW_GFGaKsKAzeRZ98EwXRmhkzzZ4IaM1BcIsp3gipkUoc8ubHcSL-sEdLFejtiiYjrFbd91m66CKck_vyGbySNp6QoSKTylPbT6mZU8qBOeg-bgI9UgqsT0M3-L_L6ZUAJOdgvn1pqDW9wf60Ucl', s);};
Nitrogen.$anchor('page', 'page');obj('temp515527').is_facebook(top!=self);
Nitrogen.$anchor('.wfid_temp519061', '.wfid_temp519061');Nitrogen.$observe_event('.wfid_temp519061', '.wfid_temp519061', 'click', function anonymous(event) {Nitrogen.$anchor('.wfid_temp519061', '.wfid_temp519061');Nitrogen.$queue_event('.wfid_temp519061', 'QBQm6oNQAAAAbXicy2DKYEth4E0tS80riU_OzytJrShJYeDKzEvLj0_PTCspTmFgy8lPz8wzTGHgLM1LSU3LzEtNyWYQ0CtPy0yJL0nNLTA1tDQwM8QilMGcxMDA6pnEwDP3ZBIDR4YjAI1SIXc', undefined);});
Nitrogen.$anchor('page', 'page');Nitrogen.$observe_event('page', 'page', 'keydown', function anonymous(event) {if (Nitrogen.$is_key_code(event, 27, false)) { Nitrogen.$anchor('page', 'page');
Nitrogen.$anchor('page', '.wfid_splash_lightbox');objs('.wfid_splash_lightbox').hide(null);
Nitrogen.$anchor('page', '.wfid_simple_lightbox');objs('.wfid_simple_lightbox').hide(null);return false; }});
Nitrogen.$anchor('.wfid_temp549909', '.wfid_temp549909');Nitrogen.$observe_event('.wfid_temp549909', '.wfid_temp549909', 'change', function anonymous(event) {Nitrogen.$anchor('.wfid_temp549909', '.wfid_temp549909');Nitrogen.$queue_event('.wfid_replay_guiders', 'ibGBCINQAAAAgXicy2DKYEth4E0tS80riU_OzytJrShJYeDKzEvLj0_PTCspTmEQK0otyEmsjE8vzUxJLSqOT85IzEtPTUlh4CzNS0lNy8xLTclmENArT8tMiS9JzS0wNbG0NLDMZhCBCKHqzmBOYmBg9Uxi4Jl7MomBI_MAAEhvK8o', undefined);});
Nitrogen.$anchor('.wfid_temp549945', '.wfid_temp549945');Nitrogen.$observe_event('.wfid_temp549945', '.wfid_temp549945', 'click', function anonymous(event) {Nitrogen.$anchor('.wfid_temp549945', '.wfid_temp549945');Nitrogen.$queue_event('.wfid_temp549945', 'AsOz0oNQAAAAfXicy2DKYEth4E0tS80riU_OzytJrShJYeDKzEvLj0_PTCspzmBKYeBPzkjMS0-NzwGSpYnpqdkMTCVFKQycpXkpqWmZeakp2QwCeuVpmSnxJam5BaYmlpYmpliEMpiTGBhYPZMYeOaeTGLgyFoPABNYJ4g', undefined);});
</script>
<script type='text/javascript'>
  var uvOptions = {};
  (function() {
  var uv = document.createElement('script'); uv.type = 'text/javascript'; uv.async = true;
  uv.src = ('https:' == document.location.protocol ? 'https://' : 'http://') + 'widget.uservoice.com/2nRvnf6ACekkhJwy8PZlA.js';
  var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(uv, s);
  })();
</script>
<script type='text/javascript'>
  var _gaq = _gaq || [];
  _gaq.push(['_setAccount', 'UA-23071234-1']);
  _gaq.push(['_trackPageview']);
  (function() {
    var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
    ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
    var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
  })();
</script>
</body>
</html>

".
