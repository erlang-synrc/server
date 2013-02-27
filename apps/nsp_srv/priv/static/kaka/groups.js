var formV = undefined;
var ReloadPostback = undefined;

$(document).ready(function() {
  $("#add_group_dialog").dialog({
    autoOpen: false,
    height: 300,
    width: 350,
    modal: true,
    buttons: {
      "Create a group": function() {
        if(formV.checkForm()){
          submit_function();
        }else{
          formV.showErrors();
        }
      },
      "Cancel": function() {
        $( this ).dialog( "close" );
      }
    },
    close: function() {}
  });

  formV = $('#add_new_group_form').validate();
});

function submit_function(){
  var gname=$('#group_name').val();
  var gdesc=$('#group_description').val();
  var gtype=$('#group_type').val();
  $.post('/groups/create/new', { gname: gname, gdesc: gdesc, gtype: gtype },
    function(data) {
      var obj=jQuery.parseJSON(data);
      if(obj.status == 'ok'){
        $( "#add_group_dialog" ).dialog( "close" );
        reload_current_content();
      }else{
        // print error error
        $('#gc_error_label').html('Error:'+obj.error_description)
      }
  });
}

function clear_form_values(){
  formV.resetForm();formV.reset();formV.clean();
  $('#group_name').val('');
  $('#group_description').val('');
}

function reload_current_content() {
  console.log('reload current content');
  if(ReloadPostback != undefined){
    Nitrogen.$queue_event(null, ReloadPostback, '');
  }
}
