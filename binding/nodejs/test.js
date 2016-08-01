/**
 * test program
 * */

var Ip2region = require('./ip2region');

// 1. create a ip2region object
var ipObj = Ip2region.create('../../data/ip2region.db');

// 2. use
//console.log(ipObj.binarySearchSync("120.24.78.68"));
console.log(ipObj.btreeSearchSync("120.24.78.68"));

// 3. destroy
Ip2region.destroy(ipObj);
