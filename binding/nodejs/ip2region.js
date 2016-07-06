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
    var db_file_path   = null;
    var db_fd          = null;

    // db file check
    if (typeof(db_path) == "undefined" || fs.exists(db_path) ) {
        throw("[ip2region] db file not exists : " + db_path);
    }
    
    db_file_path = db_path; 

    try {
        db_fd = fs.openSync(db_file_path, 'r');
    } catch(e) {
        throw("[ip2region] Can not open ip2region.db file , path : " + db_file_path);
    }

    var totalBlocks     = 0;
    var firstIndexPtr   = 0;
    var lastIndexPtr    = 0;
    var superBlock      = new Buffer(8);

    this.binarySearch = function(ip)
    {
        if (typeof(ip) == 'string') ip = ip2long(ip);

        // init basic search environment
        if (totalBlocks == 0) {
            fs.readSync(ip2r.db_fd, superBlock, 0, 8, 0);
            firstIndexPtr = getLong(superBlock, 0);
            lastIndexPtr  = getLong(superBlock, 4);
        }

        // search

        return ip2long(ip);
    }


    this.btreeSearch = function(ip) 
    {
        return ip2long(ip);
    }


    /**
     * convert ip to long (xxx.xxx.xxx.xxx to a integer)
     * */
    function ip2long(ip)
    {
        var val = 0;
        ip.split('.').forEach(function(ele, i){
            val += ipbase[i] * ele;
        }); 

        return val;
    }


    /**
     * get long value from buffer with specified offset
     * */
    function getLong(buffer, offset) 
    {
        return  (   
            (buffer[offset] & 0x000000FF) | 
            ((buffer[offset + 1] <<  8) & 0x0000FF00) | 
            ((buffer[offset + 2] << 16) & 0x00FF0000) |
            ((buffer[offset + 3] << 24) & 0xFF000000)
        );
    }
}

module.exports = Ip2region;
