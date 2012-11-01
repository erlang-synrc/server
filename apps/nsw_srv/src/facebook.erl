%%% @author JLarky <jlarky@punklan.net>
%%% @copyright Paynet Internet ve Bilisim Hizmetleri A.S. All Rights Reserved.
%%% @doc
%%% facebook subpages
%%% @end
%%% Created :  06 Dec 2011 by JLarky <jlarky@punklan.net>

-module(facebook).

-compile(export_all).

%-include("common.hrl").
-include_lib("nitrogen_core/include/wf.hrl").
%-include_lib("zealot/include/config.hrl").
-include("setup.hrl").
-include("common.hrl").

main() ->
    #template { file=code:priv_dir(nsw_srv)++"/templates/facebook.html"}.


%% @doc Returns javascript as string. If it's first call that we have to
%% initialize FB js SDK first.
fb_script(Add) ->
    case wf:state(fb_initiated) of
	true ->
	    Add;
	_ ->
	    wf:state(fb_initiated, true),
"window.fbAsyncInit = function() {
    FB.init({
      appId      : '"++?FB_APP_ID++"', // App ID
      status     : true, // check login status
      cookie     : true, // enable cookies to allow the server to access the session
      oauth      : true, // enable OAuth 2.0
      xfbml      : true  // parse XFBML
    });

    // FB.Canvas.getPageInfo(
    //   function(info) {
    //     console.log('Width: ' + info.clientWidth + ' Height: ' + info.clientHeight, info, FB.getAuthResponse());
    //   }
    // );
    // Additional initialization code here
    "++Add++";
  };

  // Load the SDK Asynchronously
  (function(d){
     var js, id = 'facebook-jssdk'; if (d.getElementById(id)) {return;}
     js = d.createElement('script'); js.id = id; js.async = true;
     js.src = \"//connect.facebook.net/en_US/all.js\";
     d.getElementsByTagName('head')[0].appendChild(js);
   }(document));"
end.

body() -> [hemen_nav()].

hemen_nav() ->
    {_, _, C} = now(), Csid = "&amp;csid="++wf:to_list(C),
    #list{class="games-list", body=[
	#listitem{body=#panel{class="holder "++ActiveClass,
			      body=["<span class='title'>",
				    #image{image=TitleImg, style="width:77px;height:77px", alt="image"},
				    "</span>",
                                    #panel{class="stat", body=[#span{text=?_T("Very soon...")}]},
				    #panel{class="ico",
					   body=#link{url=Link, body=#image{image=GageImg,
						      style="width:160px;height:101px", alt="image"}}},
				    #link{postback=test,url=Link, class="play-link", text=?_T("Let's Play!")}

                                   ]}}
	|| {TitleImg, ActiveClass, Link, GageImg} <-
	[{"/images/facebook/ico-okey.png",  "active", ?_U("/matchmaker/okey/style/fb/"++Csid), "/images/facebook/img-004.png"},
	 {"/images/facebook/ico-tavla.png", "active", ?_U("/matchmaker/tavla/style/fb/"++Csid), "/images/facebook/img-003.png"},
	 {"/images/facebook/ico-king.png",  "", "", "/images/facebook/img-02.jpg"},
	 {"/images/facebook/ico-batak.png", "", "", "/images/facebook/img-01.jpg"},
	 {"/images/facebook/ico-sorbi.png", "", "", "/images/facebook/img-05.jpg"}]]}.

event(test) ->
    wf:wire("console.log(top.location.href, window.location, document.referrer);"),
    wf:wire("console.log(1, FB.getAuthResponse());"),
    wf:wire("console.log(1, FB);");
event(Other) ->
    webutils:event(Other).
