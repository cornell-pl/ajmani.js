(function(){
    'use strict';
    var db = new Database('http://localhost:3000',false);
    $(function(){
	    var tableTemplate = _.template($('#tableTemplate').html()),
		tableNameTemplate = _.template($('#tableNameTemplate').html()),
		viewport = $('#viewport'),
		viewport2 = $('#viewport2'),
		renderTable = function(t) {
                if (t !== null) {viewport.empty().append(tableTemplate({table : t, database : db}));}
		else {viewport.empty()};
	    },
		getTableName = function() {
		    viewport2.empty().append(tableNameTemplate({})).find('#formTableName').submit(function(e){
			    var tName = $('#tableName').val();
			    var ver = $('#clientVersion').val();
			    db.version = ver ? ver : 0;
			    db.getTableByName(tName,renderTable);
			    return false;
			});
		};
		
		getTableName();
	});
}).call(this);

