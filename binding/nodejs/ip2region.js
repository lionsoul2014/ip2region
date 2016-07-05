/**
 * ip2region client for nodejs
 *
 * project: https://github.com/lionsoul2016/ip2region
 *
 * @author dongyado<dongyado@gmail.com>
 * */


function Ip2region(db_path)
{
    var fs   = require('fs');
    var ipbase = [16777216, 65536, 256, 1]; // for ip2long
    var ip2r = {};
    ip2r.db_file_path   = null;
    ip2r.db_fd          = null;

    // db file check
    if (typeof(db_path) == "undefined" || fs.exists(db_path) ) {
        throw("[ip2region] db file not exists : " + db_path);
    }
    
    ip2r.db_file_path = db_path; 

    try {
        ip2r.db_fd = fs.openSync(ip2r.db_file_path, 'r');
    } catch(e) {
        throw("[ip2region] Can not open ip2region.db file , path : " + ip2r.db_file_path);
    }


    this.binarySearch = function(ip)
    {
        var buffer = new Buffer(8);
        fs.readSync(ip2r.db_fd, buffer, 0, 8, 0);
        console.log(buffer[0]);



        return ip2long(ip);
    }

    this.btreeSearch = function(ip) 
    {
        return ip2long(ip);
    }

    function ip2long(ip)
    {
        var val = 0;
        ip.split('.').forEach(function(ele, i){
            val += ipbase[i] * ele;
        }); 

        return val;
    }

    function getLong(buffer) 
    {
        
    }
}

module.exports = Ip2region;
