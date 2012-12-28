$().ready(function(){
	var file_input = $('.file input[type=file]').addClass('file-input-area');
	file_input.parent().parent().children("input").attr('readonly', true);
})
