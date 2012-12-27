-module (index).
-compile(export_all).

-include_lib("nitrogen_core/include/wf.hrl").

-include("gettext.hrl").
-include("elements/records.hrl").
-include("setup.hrl").

main() ->
  webutils:add_raw("<script type=\"text/javascript\">
  var slides = [
    '<img src=\"/images/slides/en/slide4.png\">',
    '<img src=\"/images/slides/en/slide3.png\">',
    '<img src=\"/images/slides/en/slide2.png\">'
  ];
  function loadedSlide(){
    $('.slideshow').cycle('resume');
  };

  function beforeSlide(current, next, options, forward){
    // works only from second item
    var img = $(next).attr('src')[0];
    if (slides[0] && options.addSlide) {
      options.addSlide(slides.pop());};
    };

  function afterSlide(current, next, options, forward){
    var img = $('.slideshow').find('img')[options.nextSlide];
    if (img.complete !=true) {
      $('.slideshow').cycle('pause');
      loadingTimer = +new Date();
      $(img).bind(\"load\",loadedSlide);
    };
    if (slides[0] && options.addSlide) {
      options.addSlide(slides.pop());
    };
  };

  $(document).ready(function() {
  $('.slideshow').append(slides.pop()).cycle({
    fx:     'fade',
    pause: true,
    prev:   '.pager .prev',
    next:   '.pager .next',
    pager:  '.switcher',
    after:    afterSlide,
    before:   beforeSlide,
    autostop: false,
    autostopCount: slides.length+2,
    timeout: 200,
    pagerAnchorBuilder: function(idx, slide) {
      return '.switcher li:eq(' + idx + ') a';
    }
  });
  });
  </script>"),
  #template { file=code:priv_dir(nsw_srv)++"/templates/index.html"}.

title() -> "Kakaranet Okey".

body() ->
  case wf:depickle(wf:q(x)) of
    Url when is_list(Url) ->
      case wf:user() of
        undefined -> wf:redirect(?_U("/login"));
        _User ->
          Dashboard = ?_U("/dashboard"),
          case Url of
            "" -> wf:redirect(Dashboard);
            _  -> wf:redirect_from_login(Dashboard)
          end
      end;
    _ -> no_need_to_login
  end,
  case wf:q(message) of
    undefined -> ok;
    Message -> show_message(Message)
  end,
  Slides = case site_utils:detect_language() of
    "tr" -> [
%      {"BAŞLIYORUZ", "slide5_tr"},
%      {"HEMEN KATILIN", "slide6_tr"},
      {"Hedİyeler", "slide1"},
      {"Turnuvalar", "slide2"},
      {"Sosyalleşİn!", "slide3"},
      {"Nasıl İstİyorsanız Öyle", "slide4"}];
    _ -> [
      {"Gifts", "slide1"},
      {"Tournaments", "slide2"},
      {"Be Social!", "slide3" },
      {"Matchmaker", "slide4"}
    ]
  end,
  [
  #panel{class="page-content", body=[
    "<div class=\"slideshow\"><img src=\"/images/slides/en/slide1.png\"></div>"
%    #list{class=slideshow, body=[
%      #listitem{class="slide "++S, body=[
%        #panel{class=btns, body=[
%          #link{text=?_T("More Info"), class="btn-dark", url=?_U("/info-gifts")}, %Detaylı Bilgi
%          #link{text=?_T("LET'S PLAY!"), class="btn-yellow", url=?_U("/login/register")} %ÜYE OL!
%        ]}
%      ]} || {_,S} <- Slides]}
  ]},
  #panel{class="slideshow-control", body=[
    #panel{class="page-content", body=[
    #list{class=switcher, body=[#listitem{body=#link{text=?_T(L)}} || {L,_} <- Slides]},
    #list{class=pager, body=[
      #listitem{body=#link{class=prev, text="prev"}},
      #listitem{body=#link{class=next, text="next"}}
    ]}]}
  ]},
  #panel{class="page-content", body=webutils:quick_nav()}
  ].

%event(show_register) ->
%    wf:redirect(?_U("/login/register"));

event(Other) ->
  webutils:event(Other).

api_event(fbAutoLogin, Tag, Args)->
  case wf:q(message) of
    undefined -> fb_utils:api_event(fbAutoLogin, Tag, Args);
    _ -> skip
  end;
api_event(Name, Tag, Args)->
  webutils:api_event(Name, Tag, Args).

show_message(Message) ->
    Decoded = site_utils:base64_decode_from_url(Message),
    Element = webutils:lightbox_panel_template(simple_lightbox, [
        #h1{class="head", text=?_T("System message")},
        #panel{class=holder, body=[
            #panel{body=Decoded}, #br{},
            #cool_button{text=?_T("OK"), delegate=login, postback=hide_simpe_lightbox},
            #grid_clear{}
        ]}
    ]),
    wf:update(simple_panel, Element),
    wf:wire(simple_lightbox, #show{}).
