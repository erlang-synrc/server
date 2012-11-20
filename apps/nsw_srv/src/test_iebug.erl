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
<html xmlns='http://www.w3.org/1999/xhtml' xml:lang='en' lang='en'>
<head>
    <meta http-equiv='cache-control' content='max-age=0' />
    <meta http-equiv='X-UA-Compatible' content='IE=8,IE=9,IE=10' />
    <meta http-equiv='cache-control' content='no-cache' />
    <meta http-equiv='expires' content='0' />
    <meta http-equiv='expires' content='Tue, 01 Jan 1980 1:00:00 GMT' />
    <meta http-equiv='pragma' content='no-cache' />

	<meta http-equiv='Content-Type' content='text/html; charset=utf-8' />
	<title></title>
    <link rel='stylesheet' type='text/css' href='/css/kakara.full.css' media='all' />
	<link rel='stylesheet' href='/css/en.css' />
	<script src='/js/kakara.full.js' type='text/javascript' charset='utf-8'></script>
	<!--[if IE]>
		<link rel='stylesheet' type='text/css' href='/css/ie.css' media='all'/>
	<![endif]-->
	<!--[if lt IE 7]>
		<script src='/nitrogen/js/ie-hover.js' type='text/javascript'></script>
		<script src='/nitrogen/js/ie-png.js' type='text/javascript'></script>
	<![endif]-->
	<!--[if lt IE 9]>
		<script src='/nitrogen/js/html5.js' type='text/javascript'></script>
		<link rel='stylesheet' type='text/css' href='/css/ie7.css' media='all'/>
	<![endif]-->
	<!--[if IE 9]>
		<link rel='stylesheet' type='text/css' href='/css/ie9.css' media='all' />
	<![endif]-->
	<!-- Most of scripts and css files were wiped out, you can see old state of this file by command
	git cat-file blob 4a15d19e2b760379d3776a95fcb84bac9b86fab3 -->

</head>
<body class='test_iebug'>
<div id=fb-root></div><script>window.fbAsyncInit = function() {FB.init({ appId: '154227314626053',channelUrl: 'https://srv5.kakaranet.com/channel.html',status: true,cookie: true,xfbml: true,oauth: true});if(page.fbLogout) FB.Event.subscribe('auth.logout', function(response){page.fbLogout(response);});FB.getLoginStatus(function(response) {if(page.setFbIframe){console.log('Set FB application flag: '+ (top!=self));page.setFbIframe(top!=self);}if (response.status === 'connected') {var uid = response.authResponse.userID;console.log('User is connected: ' + uid);if(page.fbCheckPermissions){
		FB.api('/me/permissions', function(response){
		    var perms = response.data[0];
		    console.log('Permissions: '+perms);
		    page.fbCheckPermissions(perms);
		});
	    }else{console.log('No fbCheckPermissions')}} else if (response.status === 'not_authorized') {console.log('Not authenticated the app');} else {console.log('User isn't logged in');}});};function fb_login(){FB.getLoginStatus(function(response){if(response.status == 'connected'){console.log('User connected to FB, check for registered account');if(page.fbLogin){FB.api('/me?fields=id,username,first_name,last_name,email,birthday',function(response){page.fbLogin(response);});}}else{FB.login(function(r){if(r.authResponse){if(page.fbLogin){FB.api('/me?fields=id,username,first_name,last_name,email,birthday',function(response){page.fbLogin(response);});}}},{scope: 'email,user_birthday'});}});}function add_fb_service(){FB.ui({
	method: 'permissions.request',
	perms: 'publish_stream',
	display: 'popup'},
	function(response){
	    if(response && response.perms){
		console.log('Permissions granted: '+response.perms);if(page.fbCheckPermissions){
		    page.fbCheckPermissions(response.perms);
		}}else if(!response.perms){
		console.log('User did't grant permission.');
	    }
	});};
    function del_fb_service(){
	console.log('Todo: revoke fb permission.');
	FB.api({
	    method: 'auth.revokeExtendedPermission',
	    perm: 'publish_stream'
	},
	function(response) {
	    if(page.fbRemovePermissions){
		page.fbRemovePermissions(response);
	    }
	    console.log('Response revoke:' + response);
	}); 
    };function fb_feed(Msg){FB.api('/me/feed', 'post', { message: Msg },function(response) {if (!response || response.error) {console.log('Error occured');} else {console.log('Post ID: ' + response.id);}});};(function(d){var js, id = 'facebook-jssdk', ref = d.getElementsByTagName('script')[0];if (d.getElementById(id)) {return;}js = d.createElement('script');js.id = id;js.async = true;js.src = '//connect.facebook.net/en_US/all.js';ref.parentNode.insertBefore(js, ref);}(document));</script>
<section class='wrapper'>



    <header>
    <div class='block'>
	<strong class='logo vcard'><a href='/' class='fn org url wfid_temp265279 link' target='_self'>KakaraNet - Public Beta</a></strong>

<div class='top wfid_temp265521 panel'>
    <div class='ar wfid_temp265542 panel'>
"++
%        <div class='box wfid_temp265558 panel'>
"            <ul class='user-menu wfid_temp265576 list'>

                <li class='wfid_temp265593 listitem'><a href='javascript:' class='fb_login_btn wfid_temp265627 link' target='_self'>Login</a></li>
                <li class='wfid_temp265650 listitem'><a href='https://srv5.kakaranet.com/login' class='login wfid_temp265665 link' target='_self'>Login</a></li>
                <li class='wfid_temp265706 listitem'><a href='javascript:' class='signup wfid_temp265721 link' target='_self'>Signup</a></li>

            </ul>
"++
%        </div>
"    </div>
</div>

<nav>
    <ul class='wfid_temp265743 list'>
        <li class='wfid_temp265779 listitem'><a href='/' class='wfid_temp265794 wfid_mainmenumainpage link' target='_self' title='You can play games here'>Home</a></li>
        <li class='wfid_temp265818 listitem'><a href='/dashboard' class='wfid_temp265857 wfid_mainmenumypage link' target='_self' title='You can share information with others'>My Page</a></li>
        <li class='wfid_temp265882 listitem'><a href='/rules-okey' class='wfid_temp265897 wfid_mainmenurules link' target='_self' title='Read the rules of our games'>Rules</a></li>
        <li class='wfid_temp265919 listitem'><a href='/gifts' class='wfid_temp265934 wfid_mainmenugifts link' target='_self' title='Have no idea, what it is about'>Gifts</a></li>
        <li class='wfid_temp265957 listitem'><a href='/tournaments' class='wfid_temp266015 wfid_mainmenutournaments link' target='_self' title='You can join tournaments and show them all'>Tournaments</a></li><li class='wfid_temp266046 listitem'><a href='/groups' class='wfid_temp266064 wfid_mainmenugroups link' target='_self' title='You can manage your groups settings here'>Groups</a></li>
    </ul>
</nav>
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

<div class='wfid_temp266183 wfid_simple_lightbox lightbox panel' style='position: fixed; top: 0px; left: 0px; bottom: 0px; right: 0px; display: none; position: relative;'><div class='lightbox_background wfid_temp266210 panel' style='position: fixed; top: 0px; left: 0px; bottom: 0px; right: 0px; z-index: 98; background-color: #000000;'></div><table border='0' cellpadding='0' cellspacing='0' class='wfid_temp266230 table' style='position: fixed; top: 0px; left: 0px; width: 100%; height: 100%; z-index: 99; overflow:auto;'><tbody><tr class='wfid_temp266306 tablerow'><td class='wfid_temp266325 tablecell' style='vertical-align: middle;' align='center' valign='middle' colspan='1' rowspan='1'><center><table><tr><td><div class='wfid_temp266346 wfid_simple_panel panel'></div></td></tr></table></center></td></tr></tbody></table></div>

    Test
    <footer><ul class='navbar wfid_temp266750 list'><li class='wfid_temp266768 listitem'><a href='/gifts' class='wfid_temp266783 link' target='_self'>Gifts</a></li><li class='wfid_temp266801 listitem'><a href='/terms' class='wfid_temp266815 link' target='_self'>Terms of Service</a></li><li class='wfid_temp266833 listitem'><a href='/privacy' class='wfid_temp266850 link' target='_self'>Privacy Policy</a></li><li class='wfid_temp266915 listitem'><a href='http://kakaranet.uservoice.com/' target='_blank'>Help & Support</a></li><li class='wfid_temp266932 listitem'><a href='/contact' class='wfid_temp266946 link' target='_self'>Contact</a></li><li class='wfid_temp266963 listitem'>2011 &copy; Kakaranet. All rights reserved.<br/>Kakaranet is registered trademark of Paynet Inc.<br/></li><li class='wfid_temp266978 listitem'><input name='.wfid_temp266992' id='.wfid_temp266992' type='checkbox' class='wfid_temp266992 wfid_replay_guiders checkbox' value='on' not_checked='true'/><label for='.wfid_temp266992'>Replay Guiders</label></li></ul><a href='javascript:' class='lang wfid_temp267022 link' target='_self'><img alt=' ' class='wfid_temp267041 image' style='width:14px;height:10px;' src='/images/flag-tr.png'/>Turkish</a></footer>
</section>
<script type='text/javascript'>Nitrogen.$set_param('pageContext', '63PTq4NQAAADinichZDRToMwFIZPppjopjObmnnhQ2xxiT6C1z5AU9ozWiwtaw-Jvr2QiOuApHfw__xf-6FmBgBmKpOwqHmBTDhL-E1fcENY1bvX9_1uL7uXQExj3hSlhEy4CqkbzrvhUnErDfp-K-GufTrogv0VEp4kHnhjiA2L68a2lbYoy2nU3Ljo83XPOUuTkE3tncAQmMdCB_I_p-2z7bo2n-iS3FvBhcLT4PFf8zxPg44Nxievak6KDcIkZRlaR-1s9N-DrmqDbFSkbxSI05TaIE-C7rVES5oikU3PGldJ3MI7Ex3_0KPO47Sed02ssdLbN8sGYdotoGj8tNu4inBlqS5ygOwjh6vjZw6XL9tfSWZCwg');

Nitrogen.$anchor('page', 'page');obj('page').setFbIframe = function() {var s = Nitrogen.$encode_arguments_object(arguments);Nitrogen.$anchor('page', 'page');Nitrogen.$queue_event('page', 'PG9gq4NQAAAAwnicVcxBC8IwDAXg0NWBDk_-J2FnDx5Ls6SmiN3cMvHnG9xh9PTyHh8RJy3BmT9cNAxjUf4qwSkOmscS4pTFERwtw59IR9BYsy0vYVMVf4Kf4oPrML0W4pQLE4HXebWpW1iv2Kc5vqy5hLXa7_qXNAhw6BHa9w3BX-4_4-08Yw', s);};
Nitrogen.$anchor('page', 'page');obj('page').fbLogin = function() {var s = Nitrogen.$encode_arguments_object(arguments);Nitrogen.$anchor('page', 'page');Nitrogen.$queue_event('page', 'sCWt34NQAAAAvnicVczBCsIwDAbg0NWJijdfSvDmA5R2SZcgtNNlsr29QQ_S05_8fAk77hHO9KaiYahFaVWEYxxUaglxEnYIB8vwJXxC6GyzTubwUw1_gJ_iSG2YXgpSlkKI4PW1WLXP6VZHsXOXUyv-c_uHuwSwuybon_cE_rJ9ANpeOvs', s);};
Nitrogen.$anchor('page', 'page');obj('page').fbLogout = function() {var s = Nitrogen.$encode_arguments_object(arguments);Nitrogen.$anchor('page', 'page');Nitrogen.$queue_event('page', '18rZDYNQAAAAv3icVc1NCgIxDAXg0KmDP7j0UII7D1CaSTopQjo6rXp8gy6kq5c8PhJxMhIc-claw1S08rsS7ONUc9EQlyyOYGcZvkQOBINt1uU1_FTHb-CXOHMfppsSp6xMBL4-mlXbhJcyl2bvXMKe_Of-kAwIsDkjjPcrgj-9PiO_O3w', s);};
Nitrogen.$anchor('page', 'page');obj('page').fbSignedRequest = function() {var s = Nitrogen.$encode_arguments_object(arguments);Nitrogen.$anchor('page', 'page');Nitrogen.$queue_event('page', 'A5l_H4NQAAAAxnicVY3RCsIwDEVDVwcq4pv_4if46j6gNEu6FiHbXCp-vkEfpE8393DIzS73BCd-sWgYZ1F-K8EhjlpmCXEp2RHsLcNXyUeCzpqxsoWf1egP8EucuA2zqxCnIkwEXp_V0DnhUCYjd14rb7bqErbm_27_5Q4BdjeEfh0Q_OX6AQIxPg8', s);};
Nitrogen.$anchor('page', 'page');obj('page').fbCheckPermissions = function() {var s = Nitrogen.$encode_arguments_object(arguments);Nitrogen.$anchor('page', 'page');Nitrogen.$queue_event('page', 'ESLJMYNQAAAAyXicVcxNCgIxDAXg0KkDKm7ceBl3rtwJHqC0TWrKYOenGfH4Bl1IVy95fAkb7hEO9KIiLo5F6C0IOx8lj8X5KbNB2Gq6L-E9Qqebdrm6n2r4AHbyD2pD9VqQUi6ECFaWVatjChemONxoeeZa9b4imBRa_J_bl9wFgM01QD_fA9jT-QPV6T9N', s);};
Nitrogen.$anchor('page', 'page');obj('page').fbRemovePermissions = function() {var s = Nitrogen.$encode_arguments_object(arguments);Nitrogen.$anchor('page', 'page');Nitrogen.$queue_event('page', 'VoTZOINQAAAAynicVczBDsIgEATQCcUmajx58Ze8Gf0AAt1FiCm0hTZ-vhs9GE6zO3mZoEJPOPHGqZohp8rvSjjYocacjJ1iUIS9pPmScCR08kkXi_mphr-gJ_vkNkSvidjHxETQdVmlOnt35zFvfONljKXIQCEo71r9v9vN0Dlgd3Xo54eDvuADKTU_ow', s);};
Nitrogen.$anchor('.wfid_temp265627', '.wfid_temp265627');Nitrogen.$observe_event('.wfid_temp265627', '.wfid_temp265627', 'click', function anonymous(event) {Nitrogen.$anchor('.wfid_temp265627', '.wfid_temp265627');
Nitrogen.$anchor('.wfid_temp265627', '.wfid_temp265627');fb_login()});
Nitrogen.$anchor('.wfid_temp265721', '.wfid_temp265721');Nitrogen.$observe_event('.wfid_temp265721', '.wfid_temp265721', 'click', function anonymous(event) {Nitrogen.$anchor('.wfid_temp265721', '.wfid_temp265721');Nitrogen.$queue_event('.wfid_temp265721', 'x7bNzYNQAAAAb3icy2DKYEth4E0tS80riU_OzytJrShJYeAqSS0uic9MTSpNT2HgKEpNzywuSS1KYeAszUtJTcvMS03JZhDQK0_LTIkvSc0tMDIzNTcyxCKUwZzEwMDqmcTAVhicxMAimQkA8zshxg', undefined);});
Nitrogen.$anchor('page', 'page');Nitrogen.$observe_event('page', 'page', 'keydown', function anonymous(event) {if (Nitrogen.$is_key_code(event, 27, false)) { Nitrogen.$anchor('page', 'page');
Nitrogen.$anchor('page', '.wfid_splash_lightbox');objs('.wfid_splash_lightbox').hide(null);
Nitrogen.$anchor('page', '.wfid_simple_lightbox');objs('.wfid_simple_lightbox').hide(null);return false; }});
Nitrogen.$anchor('.wfid_temp266992', '.wfid_temp266992');Nitrogen.$observe_event('.wfid_temp266992', '.wfid_temp266992', 'change', function anonymous(event) {Nitrogen.$anchor('.wfid_temp266992', '.wfid_temp266992');Nitrogen.$queue_event('.wfid_replay_guiders', 'Zt1WJ4NQAAAAgXicy2DKYEth4E0tS80riU_OzytJrShJYeAqSS0uic9MTSpNT2EQK0otyEmsjE8vzUxJLSqOT85IzEtPTUlh4CzNS0lNy8xLTclmENArT8tMiS9JzS0wMjOztDTKZhCBCKHqzmBOYmBg9UxiYCsMTmJgkfoCAEUkKwQ', undefined);});
Nitrogen.$anchor('.wfid_temp267022', '.wfid_temp267022');Nitrogen.$observe_event('.wfid_temp267022', '.wfid_temp267022', 'click', function anonymous(event) {Nitrogen.$anchor('.wfid_temp267022', '.wfid_temp267022');Nitrogen.$queue_event('.wfid_temp267022', '1ZjMA4NQAAAAfXicbchBCsIwEAXQT9SCBdEDeAaRLPQMrj1AaDq_SagdLU7U4-sB3LzFyy43gg1fVAv9XY0fE7TGp4XCWFN2gm2fO00Mt5-1SxzhqIJ1VeFQlDJid3gPRYJxevjT-ej9n8qLCKwuEc18jVju3RcGdyWv', undefined);});</script>
<script type='text/javascript'>
var base_sidebar_url = 'http://kakaranet.qhub.com/';
var sidebar_config = {
    location: 'right',
	position: 'middle',
	target: '_self',
	margin_top: '0',
	border: '#000',
	caption: '',
	caption_image_path: 'http://kakaranet.qhub.com/widgets/images/help_and_feedback.png',
	back_color: '#900',
	back_selected_color: '#060',
    };
    var popup_config = {
	url: 'http://kakaranet.qhub.com/widgets/psw_lightbox.php?user=45286',
	link: 'Open in a new window',
	width: 700,
	height: 530,
    };
    document.write(unescape('%3Cscript src='http://kakaranet.qhub.com/widgets/scripts/sb.js' type='text/javascript'%3E%3C/script%3E'));
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
