// Database 
function Database() {
    this.tables = {}
}

Database.prototype.getTableNames = function(){
    return keys(this.tables);
};

// Returns null if table name is not found in the database
Database.prototype.getTableByName = function(name){
    return (this.tables[name] || null);
};

Database.prototype.putTable = function(name,table){
    this.tables[name] = table;
};

Database.prototype.delTable = function(name){
    delete this.tables[name];
};

// Table
function Table(){
    this.headers = [];
    this.records = {};
}

Table.prototype.getHeaders = function(){
    return this.headers;
};

Table.prototype.getRecords = function(){
    return this.records;
};

// Returns null if record id is not found in the database
Table.prototype.getRecordsById = function(id){
    return (this.records[id] || null)
};

Table.prototype.putRecord = function(id,fields){
    this.records[id] = fields;
};

// Assuming id to start from 0
Table.prototype.createRecord = function(fields){
    var mid = max(intKeys(this.records));
    var id = mid < 0 ? 0 : mid+1;
    this.records[id] = fields;
    return id;
};

Table.prototype.delRecord = function(id) {
    delete this.records[id];
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

	


