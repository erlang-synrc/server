Ext.ns('UI.admin');

UI.admin.RowEditor = Ext.extend(Ext.ux.grid.RowEditor, {
	startEditing : function(rowIndex, doFocus) {
		if (this.editing && this.isDirty()) {
			this.showTooltip(this.commitChangesText);
			return;
		}
		if (Ext.isObject(rowIndex)) {
			rowIndex = this.grid.getStore().indexOf(rowIndex);
		}
		if (this.fireEvent('beforeedit', this, rowIndex) !== false) {
			this.editing = true;
			var g = this.grid, view = g.getView(), row = view.getRow(rowIndex), record = g.store
					.getAt(rowIndex);

			this.record = record;
			// copy data to new object to prevent change
			this.cdata = {};
			for (var p in record.data) {
				this.cdata[p] = record.data[p]
			};

			this.rowIndex = rowIndex;
			this.values = {};
			if (!this.rendered) {
				this.render(view.getEditorParent());
			}
			var w = Ext.fly(row).getWidth();
			this.setSize(w);
			if (!this.initialized) {
				this.initFields();
			}
			var cm = g.getColumnModel(), fields = this.items.items, f, val;
			for (var i = 0, len = cm.getColumnCount(); i < len; i++) {
				val = this.preEditValue(record, cm.getDataIndex(i));
				f = fields[i];
				f.setValue(val);
				this.values[f.id] = Ext.isEmpty(val) ? '' : val;
			}
			this.verifyLayout(true);
			if (!this.isVisible()) {
				this.setPagePosition(Ext.fly(row).getXY());
			} else {
				this.el.setXY(Ext.fly(row).getXY(), {
							duration : 0.15
						});
			}
			if (!this.isVisible()) {
				this.show().doLayout();
			}
			if (doFocus !== false) {
				this.doFocus.defer(this.focusDelay, this);
			}
		}
	},

	stopEditing : function(saveChanges) {

		this.editing = false;
		if (!this.isVisible()) {
			return;
		}

		if (saveChanges === false || !this.isValid()) {
			this.hide();
			this.fireEvent('canceledit', this, saveChanges === false);
			return;
		}

		var changes = {}, cd = this.cdata, r = this.record, hasChange = false, recHasChange = false, cm = this.grid.colModel, fields = this.items.items;

		for (var i = 0, len = cm.getColumnCount(); i < len; i++) {
			if (!cm.isHidden(i)) {
				var dindex = cm.getDataIndex(i);
				if (!Ext.isEmpty(dindex)) {
					var oldValue = cd[dindex], value = this.postEditValue(
							fields[i].getValue(), oldValue, r, dindex);

					if (!Ext.isPrimitive(oldValue)) {
						hasChange = (!oldValue)
								|| (String(oldValue.name) !== String(value.name));
					} else {
						if (!Ext.isPrimitive(value)) {
							hasChange = String(oldValue) !== String(value.name);
						} else {
							hasChange = String(oldValue) !== String(value);
						}
					}

					if (hasChange) {
						recHasChange = true;
						changes[dindex] = value;
					}
				}
			}
		}

		if (recHasChange
				&& this.fireEvent('validateedit', this, changes, r,
						this.rowIndex) !== false) {
			r.beginEdit();
			Ext.iterate(changes, function(name, value) {
						r.set(name, value);
					});
			r.endEdit();
			this.fireEvent('afteredit', this, changes, r, this.rowIndex);
		}
		this.hide();
	}
});

UI.admin.PackagesGrid = Ext.extend(Ext.grid.EditorGridPanel, {
			onPackageSave : function(data) {
			},
			initComponent : function() {
				var self = this;

				var ds = new Ext.data.ArrayStore({
							idIndex : 0,
							fields : ['id', 'name', 'payment', 'gifts_points',
									'net_membership', 'quota', {
										name : 'available',
										type : 'boolean'
									}, 'price'],
							data : this.data || []
						});

				this.rowEditor = new UI.admin.RowEditor({
							saveText : 'Save',
							errorSummary : false,
							clicksToEdit : 2,
							listeners : {
								beforeedit : function(ed, rowIdx) {
								},

								afteredit : function(roweditor, changes,
										record, rowIndex) {
									// if id undefined - we create new record
									if (!record.get("id")) {
										data = changes;
									} else {
										data = record.data;
									}
									self.onPackageSave(data);
									self.getStore().commitChanges();
								},
								canceledit : function(ed) {
									var r = ed.record, st = self.getStore();
									if (!r.get('id')) {
										st.remove(r);
									}
									st.rejectChanges();
								}
							}
						});

				paymentEditor = new Ext.form.ComboBox({
							typeAhead : true,
							triggerAction : 'all',
							lazyRender : true,
							mode : 'local',
							store : new Ext.data.ArrayStore({
										id : 0,
										fields : ['id'],
										data : [['credit_card'], ['mobile'],
												['paypal'], ['wire_transfer'],
												['facebook']]
									}),
							valueField : 'id',
							displayField : 'id'
						})

				Ext.apply(this, {
							sm : this.sm || new Ext.grid.RowSelectionModel({
										singleSelect : true
									}),
							store : ds,
							plugins : [this.rowEditor],
							view : new Ext.ux.grid.BufferView({
										cacheSize : 50
									}),
							colModel : new Ext.grid.ColumnModel({
										defaults : {
											sortable : true
										},
										columns : [{
													header : 'Name',
													id : 'name',
													dataIndex : 'name',
													editor : new Ext.form.TextField(
															{
																allowBlank : false
															})
												}, {
													header : 'Payment type',
													id : 'payment',
													dataIndex : 'payment',
													editor : paymentEditor
												}, {
													header : 'Deducted for gifts',
													id : 'gifts_points',
													dataIndex : 'gifts_points',
													editor : new Ext.form.NumberField(
															{
																allowBlank : false
															})
												}, {
													header : 'Net membership',
													id : 'net_membership',
													dataIndex : 'net_membership',
													editor : new Ext.form.NumberField(
															{
																allowBlank : false
															})
												}, {
													header : 'Quota',
													id : 'quota',
													dataIndex : 'quota',
													editor : new Ext.form.NumberField(
															{
																allowBlank : false
															})
												}, {
													header : 'Price',
													id : 'price',
													dataIndex : 'price',
													editor : new Ext.form.NumberField(
															{
																allowBlank : false
															})
												}, {
													header : 'Available for sale',
													id : 'available',
													dataIndex : 'available',
													editor : new Ext.form.Checkbox()
												}]
									}),

							tbar : [{
										iconCls : 'icon-new',
										text : 'Add',
										handler : function() {
											var defaultData = {
												id : -1
											};
											self.rowEditor.stopEditing();

											store = self.getStore();
											n = new store.recordType(defaultData);
											store.insert(0, n);
											self.getView().refresh();
											self.rowEditor.startEditing(0);
										}
									}]
						});

				UI.admin.PackagesGrid.superclass.initComponent.call(this);
			}
		});

UI.admin.PurchasesGrid = Ext.extend(Ext.grid.EditorGridPanel, {
  onSave : function(data) {},
  updateData : function(data) {
    data = Base64.decode(unescape(data)),
    data = Ext.util.JSON.decode(data), this.getStore().loadData(data);
  },
  updateItem : function(id, state){
    var record = this.getStore().getById(id);
    if(record){
      record.set('state', state);
      record.commit();
    }
  },
  initComponent : function() {
    var self = this;

    function renderAmount(pkg) {
      return pkg.amount;
    }

    function renderPaymentType(pkg) {
      return pkg.payment_type;
    }

    var ds = new Ext.data.JsonStore({
      idIndex : 0,
      fields : ['id', 'external_id', 'user_name', 'user_info', 'state', 'm_package', 'start_time', 'end_time', 'info', 'state_log'],
      data : []
    });

    function handleConfirm(){
      var selection = self.getView().grid.selModel.selections.items[0];
      if(selection){
        self.confirmPayment(selection.id);
      }
    }

    function handleDiscard(){
      var selection = self.getView().grid.selModel.selections.items[0];
      if(selection){
        self.discardPayment(selection.id);
      }
    }

    function handleReload(){
      // this function is defined in elemens module (element_purchases_grid.erl)
      // updateData() callback will be called to update data
      self.loadDataRequest()
    }

    Ext.apply(this, {
      sm : this.sm || new Ext.grid.RowSelectionModel({singleSelect : true}),
      store : ds,
      view : new Ext.ux.grid.BufferView({cacheSize : 50}),
      colModel : new Ext.grid.ColumnModel({
        defaults : {sortable : true},
        columns : [
          {header : 'Id', id : 'id', dataIndex : 'id'},
          {header : 'External Id', id : 'external_id',dataIndex : 'external_id'},
          {header : 'State', id : 'state', dataIndex : 'state'},
          {header : 'User', id : 'user_name', dataIndex : 'user_name'},
          {dataIndex : 'm_package', header : 'Amount', renderer : renderAmount },
          {dataIndex : 'm_package', header : 'Payment type', renderer : renderPaymentType},
          {header : 'Start', id : 'start_time', dataIndex : 'start_time' },
          {header : 'End', id : 'end_time', dataIndex : 'end_time' }
        ]
      }),
      tbar : [
        {iconCls : 'icon-confirm', text : 'Confirm purchase', handler : handleConfirm},
        {iconCls : 'icon-discard', text : 'Discard purchase', handler : handleDiscard},
        {iconCls : 'icon-reload-db', text : 'Reload', handler : handleReload}
      ]
    });

    // load data
    self.loadDataRequest();
    UI.admin.PurchasesGrid.superclass.initComponent.call(this);
  }
});
