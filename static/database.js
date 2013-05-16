// Database 
function Database(url, cache) {
    url = typeof url !== 'undefined' ? url : 'localhost:3000';
    cache = typeof cache !== 'undefined' ? cache : false;
    console.log(url);
    console.log(cache);
    this.tables = {};
    this.cache = cache;
    this.url = url;
    this.version = 0;
    this.serverVersion = 0;
}

Database.prototype.getTableNames = function(callback){
    var dbs = this;
    if (!dbs.cache) { $.ajax({
		url: dbs.url + "/tables",
		type: 'GET',
		data: {version: dbs.version},
		dataType: 'json',
		success: function(d) { 
		    dbs.serverVersion = d.version;
		    callback(d);
		}
	    });}
    //    return keys(this.tables);
};

// Returns null if table name is not found in the database
Database.prototype.getTableByName = function(name,callback){
    var dbs = this;
    if (!dbs.cache){
	console.log("Going to make get call");
	$.ajax({
		url: (dbs.url + "/table/" + name) ,
		type: 'GET',
		data: {version: dbs.version},
		dataType: 'json',
		success: function(d) { 
		    console.log(JSON.stringify(d));
		    dbs.serverVersion = d.version;
		    var table = new Table(dbs);
		    table.headers = d.body.headers;
		    table.records = d.body.records;
		    callback(table);
		}
	    });
    }
    //    return (this.tables[name] || null);
};

Database.prototype.putTable = function(name,table,callback){
    var dbs = this;
    if (!dbs.cache){  $.ajax({
		url: dbs.url + "/table/" + name,
		type: 'PUT',
		data: {version: dbs.version, table: JSON.stringify(table)},
		dataType: 'json',
		success: function(d) {
		    dbs.serverVersion = d.version;
		    callback(d);
		}
	    });}

    //    this.tables[name] = table;
};

Database.prototype.delTable = function(name,callback){
    var dbs = this;
    if (!dbs.cache){  $.ajax({
		url: dbs.url + "/table/" + name,
		type: 'DELETE',
		data: {version: dbs.version},
		dataType: 'json',
		success: function(d) { 
		    dbs.serverVersion = d.version;
		    callback(d);
		}
	    });}

    //    delete this.tables[name];
};

// Table
function Table(db){
    this.headers = [];
    this.records = {};
    this.db = db;
}

Table.prototype.getHeaders = function(callback){
    var dbs = this.db;    
    if (!dbs.cache){  $.ajax({
		url: dbs.url + "/table/" + name + '/headers',
		type: 'GET',
		data: {version: dbs.version},
		dataType: 'json',
		success: function(d) { 
		    dbs.serverVersion = d.version;
		    callback(d);
		}
	    });}

    //    return dbs.headers;
};

Table.prototype.getRecords = function(callback){
    var dbs = this.db;
    if (!dbs.cache){  $.ajax({
		url: dbs.url + "/table/" + name + '/records',
		type: 'GET',
		data: {version: dbs.version},
		dataType: 'json',
		success: function(d) { 
		    dbs.serverVersion = d.version;
		    callback(d);
		}
	    });}

    //    return dbs.records;
};

// Returns null if record id is not found in the database
Table.prototype.getRecordById = function(id,callback){
    var dbs = this.db;
    if (!dbs.cache){  $.ajax({
		url: dbs.url + "/table/" + name + '/record/' + id.toString(),
		type: 'GET',
		data: {version: dbs.version},
		dataType: 'json',
		success: function(d) { 
		    dbs.serverVersion = d.version;
		    callback(d);
		}
	    });}

    //    return (dbs.records[id] || null)
};

Table.prototype.putRecord = function(id,fields,callback){
    var dbs = this.db;
    if (!dbs.cache){  $.ajax({
		url: dbs.url + "/table/" + name + '/record/' + id.toString(),
		type: 'PUT',
		data: {version: dbs.version,fields: JSON.stringify(fields)},
		dataType: 'json',
		success: function(d) { 
		    dbs.serverVersion = d.version;
		    callback(d);
		}
	    });}

    //    dbs.records[id] = fields;
};

// Assuming id to start from 0
Table.prototype.createRecord = function(fields,callback){
    var dbs = this.db;
    if (!dbs.cache){  $.ajax({
		url: dbs.url + "/table/" + name + '/record',
		type: 'POST',
		data: {version: dbs.version, fields: JSON.stringify(fields)},
		dataType: 'json',
		success: function(d) { 
		    dbs.serverVersion = d.version;
		    callback(d);
		}
	    });}

    //    var mid = max(intKeys(dbs.records));
    //    var id = mid < 0 ? 0 : mid+1;
    //    dbs.records[id] = fields;
    //    return id;
};

Table.prototype.delRecord = function(id,callback) {
    var dbs = this.db;
    if (!dbs.cache){  $.ajax({
		url: dbs.url + "/table/" + name + '/record/' + id.toString(),
		type: 'DELETE',
		data: {version: dbs.version},
		dataType: 'json',
		success: function(d) { 
		    dbs.serverVersion = d.version;
		    callback(d);
		}
	    });}
    
    //    delete dbs.records[id];
};


// Removing dependency of Underscore.js

function keys(aArr){
    var result = [];
    for (var key in aArr) {
  result.push(key);
    }
    return result;
}

function intKeys(aArr){
    var result = [];
    for (var key in aArr) {
	result.push(parseInt(key));
    }
    return result;
}

function max(list){
    var currMax = -Infinity;
    for (var i in list){
	if (list[i] > currMax){
	    currMax = list[i];
	}
    }
    return currMax;
}

	


