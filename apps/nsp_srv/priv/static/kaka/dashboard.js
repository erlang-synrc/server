AudioPlayer.setup("/nitrogen/audio-player/player.swf", {width: 290});

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
    $(this).find(".scroller_prev").bind('click',{mel:i},
      function(event){$('.scroller').each(function(j){
        if(j==event.data.mel){$(this).trigger('prev')}})
      });

    $(this).find(".scroller_next").bind('click',{mel:i},
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

function upd_parent_to_float(BoxId){
  $('.wfid_' + BoxId).css('float', 'left');
  $('.wfid_' + BoxId).css('clear', '');
}

$(document).ready(function(){
  upd_scrollers();
  game_slider();
});
