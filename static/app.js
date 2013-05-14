(function(){
    'use strict';
    var db = new Database();
    var t = new Table();
    t.headers = ['headers','body'];
    t.createRecord(['From:Nate','hi']);
    t.createRecord(['From:Raghu','Hello']);
    t.createRecord(['From:Satvik','Yupppppp']);
    db.putTable('emails',t);
    $(function(){
	    var tableTemplate = _.template($('#tableTemplate').html()),
		tableNameTemplate = _.template($('#tableNameTemplate').html()),
		viewport = $('#viewport'),
		viewport2 = $('#viewport2'),
		renderTable = function(name) {
		var t = db.getTableByName(name);
                if (t !== null) {viewport.empty().append(tableTemplate({table : t}));}
		else {viewport.empty()};
	    },
		getTableName = function() {
		    viewport2.empty().append(tableNameTemplate({})).find('#formTableName').submit(function(e){
			    var tName = $('#tableName').val();
			    renderTable(tName);
			    return false;
			});
		};
		
		getTableName();
	});
}).call(this);

