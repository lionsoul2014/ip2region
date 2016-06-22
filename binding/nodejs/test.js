/**
 * test program
 * */

var ip2r = require('./ip2region');

ip2r.setDbFile('./ip2region.db');

console.log(ip2r.getDbFile());
console.log(ip2r.binarySearch("12321231"));
