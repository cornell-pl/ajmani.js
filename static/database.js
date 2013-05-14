// Database 
function Database(url, cache) {
    url = typeof url !== 'undefined' ? url : 'localhost:3000';
    cache = typeof cache !== 'undefined' ? cache : false;
    console.log(url);
    console.log(cache);
    this.tables = {};
    this.cache = cache;
    this.url = url;
    this.version = -1;
    this.serverVersion = -1;
}

Database.prototype.getTableNames = function(callback){
    if (!this.cache) { $.ajax({
		url: this.url + "/tables",
		type: 'GET',
		data: {version: this.version},
		dataType: 'json',
		success: function(d) { 
		    this.serverVersion = d.version;
		    callback(d);
		}
	    });}
    //    return keys(this.tables);
};

// Returns null if table name is not found in the database
Database.prototype.getTableByName = function(name,callback){
    if (!this.cache){  
	$.ajax({
		url: (this.url + "/table/" + name) ,
		type: 'GET',
		data: {version: this.version},
		dataType: 'json',
		success: function(d) { 
		    console.log(JSON.stringify(d));
		    this.serverVersion = d.version;
		    callback(d);
		}
	    });
    }
    //    return (this.tables[name] || null);
};

Database.prototype.putTable = function(name,table,callback){
    if (!this.cache){  $.ajax({
		url: this.url + "/table/" + name,
		type: 'PUT',
		data: {version: this.version, table: JSON.stringify(table)},
		dataType: 'json',
		success: function(d) {
		    this.serverVersion = d.version;
		    callback(d);
		}
	    });}

    //    this.tables[name] = table;
};

Database.prototype.delTable = function(name,callback){
    if (!this.cache){  $.ajax({
		url: this.url + "/table/" + name,
		type: 'DELETE',
		data: {version: this.version},
		dataType: 'json',
		success: function(d) { 
		    this.serverVersion = d.version;
		    callback(d);
		}
	    });}

    //    delete this.tables[name];
};

// Table
function Table(){
    this.headers = [];
    this.records = {};
}

Table.prototype.getHeaders = function(callback){
    if (!this.cache){  $.ajax({
		url: this.url + "/table/" + name + '/headers',
		type: 'GET',
		data: {version: this.version},
		dataType: 'json',
		success: function(d) { 
		    this.serverVersion = d.version;
		    callback(d);
		}
	    });}

    //    return this.headers;
};

Table.prototype.getRecords = function(callback){
    if (!this.cache){  $.ajax({
		url: this.url + "/table/" + name + '/records',
		type: 'GET',
		data: {version: this.version},
		dataType: 'json',
		success: function(d) { 
		    this.serverVersion = d.version;
		    callback(d);
		}
	    });}

    //    return this.records;
};

// Returns null if record id is not found in the database
Table.prototype.getRecordById = function(id,callback){
    if (!this.cache){  $.ajax({
		url: this.url + "/table/" + name + '/record/' + id.toString(),
		type: 'GET',
		data: {version: this.version},
		dataType: 'json',
		success: function(d) { 
		    this.serverVersion = d.version;
		    callback(d);
		}
	    });}

    //    return (this.records[id] || null)
};

Table.prototype.putRecord = function(id,fields,callback){
    if (!this.cache){  $.ajax({
		url: this.url + "/table/" + name + '/record/' + id.toString(),
		type: 'PUT',
		data: {version: this.version,fields: JSON.stringify(fields)},
		dataType: 'json',
		success: function(d) { 
		    this.serverVersion = d.version;
		    callback(d);
		}
	    });}

    //    this.records[id] = fields;
};

// Assuming id to start from 0
Table.prototype.createRecord = function(fields,callback){
    if (!this.cache){  $.ajax({
		url: this.url + "/table/" + name + '/record',
		type: 'POST',
		data: {version: this.version, fields: JSON.stringify(fields)},
		dataType: 'json',
		success: function(d) { 
		    this.serverVersion = d.version;
		    callback(d);
		}
	    });}

    //    var mid = max(intKeys(this.records));
    //    var id = mid < 0 ? 0 : mid+1;
    //    this.records[id] = fields;
    //    return id;
};

Table.prototype.delRecord = function(id,callback) {
    if (!this.cache){  $.ajax({
		url: this.url + "/table/" + name + '/record/' + id.toString(),
		type: 'DELETE',
		data: {version: this.version},
		dataType: 'json',
		success: function(d) { 
		    this.serverVersion = d.version;
		    callback(d);
		}
	    });}
    
    //    delete this.records[id];
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

	


