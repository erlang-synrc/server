/**
 *
 * Ext.layout.VBoxLayout extension that adds SplitBars between items to make them resizeable
 * by the user.
 *
 * Use:
 * layout: "splitvbox",
 * layoutConfig:{
 *     split: true,            // show Splitbar, defualts to yes
 *     margin: 5,              // Height of the Splitbar, defaults to 5
 *     minHeight: 50           // minHeight of Components, defaults to 50
 * }
 *
 **/
Ext.ns('Ext.ux.layout');

Ext.ux.layout.SplitVBoxLayout = Ext.extend(Ext.layout.VBoxLayout, {
    split : true,
    margin : 5,
    minHeight : 50,
    monitorResize : true,

    // private
    onLayout : function(container, target) {

        // add margin between elements if margin was defined using layoutConfig

        var items = container.items.items;
        if (this.margin != 0) {
            for (var i = 1; i < items.length; i++) {
                container.items.items[i].margins = {
                    left : 0,
                    top : this.margin,
                    right : 0,
                    bottom : 0
                };
            }
        }
        // get e.g. calculate the boxes array from the VBoxLayout. getBoxes doesn't exist..
        Ext.ux.layout.SplitVBoxLayout.superclass.onLayout.call(this, container,
                target);
        if (!this.boxes)
            this.boxes = Ext.ux.layout.SplitVBoxLayout.superclass
                    .calculateChildBoxes(items, this.container.getHeight()).boxes;

        // update the boxes with the size of the contianed item. other values defined in boxes[i] are not changed
        for (var i = 0; i < items.length; i++) {
            items[i].flex = items[i].getHeight();
            this.boxes[i].dirtySize = false;
            this.boxes[i].top = items[i].y;
            this.boxes[i].left = items[i].x;
            this.boxes[i].width = items[i].getWidth();
            this.boxes[i].height = items[i].getHeight();
        }

        // we are done if split is false
        if (!this.split)
            return;

        // create Splitbars
        if (!this.splitBars)
            this.splitBars = [];

        for (var i = 0; i < (items.length - 1); i++) {
            if (this.splitBars[i]) {
                // SpliVboxLayout parent element or browser was resized -> move the Splitbars
                this.splitBars[i].el.setStyle("top", this.boxes[(i + 1)].top
                                - (this.margin / 2));
                this.splitBars[i].el.setStyle("width",
                        this.boxes[(i + 1)].width);
            } else {
                // create Splitboxes and add beforeapply listener to resize the items
                var currentItem = items[(i + 1)];
                currentItem.minSize = this.minHeight;
                currentItem.boxMinHeight = this.minHeight;
                this.splitBars[i] = new Ext.SplitBar(this.innerCt.createChild({
                                    cls : 'x-layout-split x-layout-split-west',
                                    style : {
                                        top : (currentItem.y - (this.margin / 2))
                                                + 'px',
                                        left : '0px',
                                        height : this.margin + 'px',
                                        width : currentItem.getWidth() + 'px'
                                    }
                                }), currentItem.el, Ext.SplitBar.VERTICAL);
                this.splitBars[i].index = i;
                this.splitBars[i].addListener('beforeapply', this.onBoxResize,
                        this);
            } // else
        } // for
    },

    // helper function that allows to update a box by passing an object
    // private
    updateOneBox : function(index, newBox) {
        this.boxes[index].dirtySize = true;
        this.boxes[index].width = newBox.width;
        this.boxes[index].height = newBox.height;
        this.boxes[index].top = newBox.top;
        this.boxes[index].left = newBox.left;
    },

    // calculate the new box and item sizes
    // private
    onBoxResize : function(sb, height) {
        if (sb.dragSpecs.startSize) {
            var delta = (height - sb.dragSpecs.startSize);

            var lowerBox = this.boxes[(sb.index + 1)];
            var upperBox = this.boxes[(sb.index)]

            var lowerBottom = lowerBox.top + lowerBox.height;
            var upperH = upperBox.height - delta;
            var lowerH = height;

            // lowerBox.height can't be < this.minHeight
            if (lowerH < this.minHeight) {
                var expandLower = this.minHeight - lowerH;
                upperH = upperH - expandLower;
                lowerH = this.minHeight;
            }

            // upperBox.height can't be < this.minHeight
            if (upperH < this.minHeight) {
                lowerH = (lowerH - (delta - upperBox.height + this.minHeight));
                upperH = this.minHeight;
            }

            // new upper y of the lowerBox is upperBox +height
            lowerBox.top = (upperBox.top + upperH + this.margin);

            // update the lowerBox (this.boxes)
            this.updateOneBox((sb.index + 1), {
                        dirtySize : true,
                        width : lowerBox.width,
                        height : lowerH,
                        top : lowerBox.top,
                        left : lowerBox.left
                    });

            // Panel.setSize() adds the panel header to the item. -> item size will be wrong and
            // absolutely **** if the panel has no header (title is not defined)
            if (this.boxes[(sb.index + 1)].component.header)
                this.boxes[(sb.index + 1)].component.body.setHeight(lowerH
                        - this.boxes[(sb.index + 1)].component.header
                                .getHeight());
            // update the upperBox (this.boxes)
            this.updateOneBox(sb.index, {
                        dirtySize : true,
                        width : upperBox.width,
                        height : upperH,
                        top : upperBox.top,
                        left : upperBox.left
                    });

            // update the flex values of the items -> VBoxLayout will keep the size ratio of our boxes on resize
            this.container.items.items[(sb.index + 1)].flex = lowerH;
            this.container.items.items[(sb.index)].flex = upperH;
            sb.el.setStyle('top', lowerBox.top - (this.margin / 2)); // move the SplitBar
            this.updateChildBoxes(this.boxes); // Let VBoxLayout update the boxes and items
        }
        return false;
    }
});
Ext.Container.LAYOUTS['splitvbox'] = Ext.ux.layout.SplitVBoxLayout;