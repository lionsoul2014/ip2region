/**
 * ip2region client for nodejs
 *
 * project: https://github.com/lionsoul2016/ip2region
 *
 * @author dongyado<dongyado@gmail.com>
 * */

var fs   = require('fs');
var ip2r = {};
ip2r.db_file    = null;
ip2r.db_fd      = null;


exports.setDbFile = function(path)
{
    ip2r.db_file = path;
    ip2r.db_fd   = fs.open(ip2r.db_file);
}

exports.getDbFile = function(path)
{
    return ip2r.db_file;
}

exports.binarySearch = function(ip)
{
    return ip;
}

exports.btreeSearch = function(ip) 
{
    return ip;
}
