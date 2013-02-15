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
  $('.inner_textaera').val('');
  $('ul.ui-autocomplete').offset({top:0, left:0});
}

function remove_all_tos(){
  $('.wfid_form001toRow').css('display', 'none');
  $('.wfid_flashm').children().remove();
  $('ul.ui-autocomplete').offset({top:0, left:0});
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
  var Ss = $('.wfid_to_kaka_user').size();
  if(Ss == 0){
    Nitrogen.$queue_event(null, MyFeedEvent, '');
  }
  $('ul.ui-autocomplete').offset({top:0, left:0});
}

function delete_flash_to(Ele){
  var E1 = $(Nitrogen.$anchor_path);
  var E2 = $(Nitrogen.$anchor_path).parent();
  E2.remove();
}

$(document).ready(function(){
  upd_scrollers();
  game_slider();
  objs('add_entry_textbox').autosize();

  $('.wfid_to_tauto').focus(function(){
    console.log('focused');
    $('ul.ui-autocomplete').offset({top:0, left:0});
    var pos = $('.wfid_to_tauto').offset();
    console.log('text field position offeset:' + pos.top + ',' + pos.left);
    $('ul.ui-autocomplete').offset(pos);
  });

});
