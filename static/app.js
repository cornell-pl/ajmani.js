(function(){
    'use strict';
    var db = new Database();
    var t = new Table();
    t.headers = ['headers','body'];
    t.createRecord(['From:Nate','hi']);
    t.createRecord(['From:Raghu','Grrr']);
    t.createRecord(['From:Satvik','???']);
    db.putTable('emails',t);
    $(function(){
	    var tableTemplate = _.template($('#tableTemplate').html());
	    var viewport = $('#viewport'),
		renderTable = function(name) {
		viewport.empty().append(tableTemplate({table : db.getTableByName('emails')}));
	    };
	    renderTable('emails');
	});
}).call(this);

