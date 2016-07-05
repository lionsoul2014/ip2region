/**
 * test program
 * */

var Ip2region = require('./ip2region');
var ip2region = new Ip2region('../../data/ip2region.db'); 

console.log(ip2region.binarySearch("114.114.114.114"));
