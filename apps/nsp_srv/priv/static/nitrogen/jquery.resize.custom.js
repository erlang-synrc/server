jQuery(document).ready(function($) {
    var $body = $("body");
    var resize_event = function() {
	var height = $(window).height();
	var array = {650:"small-screen", 530:"extra-small-screen"};
	for (hl in array) {
		if (height < hl) {
			$body.addClass(array[hl]);
		} else {
			$body.removeClass(array[hl]);
		}
	}
    }
    resize_event();
    $(window).resize(resize_event);
})
