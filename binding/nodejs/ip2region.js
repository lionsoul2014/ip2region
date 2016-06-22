/**
 * ip2region client for nodejs
 *
 * project: https://github.com/lionsoul2016/ip2region
 *
 * @author dongyado<dongyado@gmail.com>
 * */

var ip2r = {};
ip2r.db_file = null;


exports.setDbFile = function(path)
{
    ip2r.db_file = path;
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
