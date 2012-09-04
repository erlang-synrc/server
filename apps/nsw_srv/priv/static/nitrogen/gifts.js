$(function() {
	$(".prod-list .btn").click(function(a,b) {
		event.preventDefault();
		var $this = $(this);
		$(".tooltip-2:first").clone()
			.css({display: "block", position: "absolute", width: "150px"})
			.insertAfter($this)
			.position({my: "left bottom", at: "left top", of:$this, offset: "15 -5", collision: "fit"});
	})
});
