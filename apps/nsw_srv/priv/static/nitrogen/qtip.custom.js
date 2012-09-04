/*
Author: JLarky <jlarky@gmail.com>
Copyright (c) 2011 Paynet Internet ve Bilisim Hizmetleri A.S. All Rights Reserved.
*/

var qtip_wizard = new function() {
    tooltip_wiz = this.tooltip_wiz = {},
    this.hide_event = function(event, api) {
	    if (event.originalEvent && event.originalEvent.type == "click") { // close
		tooltip_wiz[api.id].is_closed = true
	    }
	},
    this.update_tooltip = function() {
	// traverse through all tooltips
	var done = false
	for (var id in tooltip_wiz) {
	    var tooltip = tooltip_wiz[id]
	    var $tooltip = jQuery("#ui-tooltip-"+id)
	    if (done || tooltip.is_closed || $(tooltip.target).hasClass('ui-draggable-disabled')) {
		$tooltip.qtip('hide')
	    } else {
		done = $tooltip.qtip('show').is(':visible')
	    }
	}
    },
    this.add_new = function(id, target) {
	tooltip_wiz[id] = {is_closed:false,target:target};
    }
}
