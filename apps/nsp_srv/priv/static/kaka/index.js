var slang = 'en';
var slides = new Object();
  slides.en = [
    '<img src="/images/slides/en/slide4.png">',
    '<img src="/images/slides/en/slide3.png">',
    '<img src="/images/slides/en/slide2.png">'];
  slides.tr = [
    '<img src="/images/slides/tr/slide4.png">',
    '<img src="/images/slides/tr/slide3.png">',
    '<img src="/images/slides/tr/slide2.png">'];

function loadedSlide(){
  $('.slideshow').cycle('resume');
};

function beforeSlide(current, next, options, forward){
  var img = $(next).attr('src')[0];
  if (slides[slang][0] && options.addSlide) {
    options.addSlide(slides[slang].pop());};
};

function afterSlide(current, next, options, forward){
  var img = $('.slideshow').find('img')[options.nextSlide];
  if (img.complete !=true) {
      $('.slideshow').cycle('pause');
      loadingTimer = +new Date();
      $(img).bind("load",loadedSlide);
  };
  if (slides[slang][0] && options.addSlide) {
      options.addSlide(slides[slang].pop());
  };
};

function slideShow(locale){
  slang = locale;
  $('.slideshow').append(slides[slang].pop()).cycle({
    fx: 'fade',
    pause:  true,
    prev:   '.pager .prev',
    next:   '.pager .next',
    pager:  '.switcher',
    after:   afterSlide,
    before:  beforeSlide,
    autostop: false,
    timeout: 5000,
    pagerAnchorBuilder: function(idx, slide) {
      return '.switcher li:eq(' + idx + ') a';
    }
  });
};
