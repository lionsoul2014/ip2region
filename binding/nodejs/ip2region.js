/**
 * ip2region client for nodejs
 *
 * project: https://github.com/lionsoul2014/ip2region
 *
 * @author dongyado<dongyado@gmail.com>
 * */
var fs   = require('fs');

var ipbase = [16777216, 65536, 256, 1]; // for ip2long
var ip2region = {};
ip2region.db_file_path   = null;
ip2region.db_fd          = null;

var totalBlocks         = 0;
var firstIndexPtr       = 0;
var lastIndexPtr        = 0;
var superBlock          = new Buffer(8);

var indexBlockLength    = 12;
var totalHeaderLength   = 4096;


/**
 * binary search synchronized
 * */
ip2region.binarySearchSync = function(ip)
{
    var low     = 0;
    var mid     = 0;
    var high    = totalBlocks;
    var dataPos = 0;
    var pos     = 0;
    var sip     = 0;
    var eip     = 0;
    var indexBuffer = new Buffer(12);

    if( typeof(ip) == 'string' ) ip = ip2long(ip);

    // binary search
    while( low <= high ) {
        mid = ((low + high) >> 1);
        pos = firstIndexPtr + mid * indexBlockLength;
        fs.readSync(this.db_fd, indexBuffer, 0, indexBlockLength, pos);
        sip = getLong(indexBuffer, 0);

        //console.log( ' sip : ' + sip + ' eip : ' + eip );
        if ( ip < sip) {
            high = mid - 1;
        } else {
            eip = getLong(indexBuffer, 4);
            if ( ip > eip ) {
                low = mid + 1;
            } else {
                dataPos = getLong(indexBuffer, 8);
                break;
            }
        }
    }

    // read data
    if (dataPos == 0) return null;
    var dataLen = ((dataPos >> 24) & 0xFF);
    var dataPos = (dataPos & 0x00FFFFFF);
    var dataBuffer = new Buffer(dataLen);

    fs.readSync(this.db_fd, dataBuffer, 0, dataLen, dataPos);

    var city_id = getLong(dataBuffer, 0);
    var data    = dataBuffer.toString('utf8', 4, dataLen);

    //console.log(city_id);
    //console.log(data);

    return { city: city_id, region: data };
}


var headerSip = null;
var headerPtr = 0;
var headerLen = 0;


/**
 * btree  search synchronized
 * */
ip2region.btreeSearchSync = function(ip) 
{
    var indexBlockBuffer  = new Buffer(indexBlockLength);
    var headerIndexBuffer = new Buffer(totalHeaderLength);
    if( typeof(ip) == 'string' )  ip = ip2long(ip);

    var i = 0;
    // header index handler
    if (headerSip == null) {
        fs.readSync(this.db_fd, headerIndexBuffer, 0, totalHeaderLength, 8);
        headerSip = new Array();
        headerPtr = new Array();

        var startIp = 0;
        var dataPtr = 0;
        for ( i = 0; i < totalHeaderLength; i += 8) {
            startIp = getLong(headerIndexBuffer, i);
            dataPtr = getLong(headerIndexBuffer, i + 4);
            if ( dataPtr == 0) break;
            
            headerSip.push(startIp);
            headerPtr.push(dataPtr);
            headerLen++; // header index size count
        } 
    }
    
    // first search  (in header index)
    var low  = 0;
    var mid  = 0;
    var high = headerLen;
    var sptr = 0;
    var eptr = 0;
    
    while(low <= high) {
        mid = ((low + high) >> 1);
        
        if (ip == headerSip[mid]) {
            if ( m > 0) {
                sptr = headerPtr[mid - 1];
                eptr = headerPtr[mid];
            } else {
                sptr = headerPtr[mid];
                eptr = headerPtr[mid + 1];
            }
            break;
        }
        
        if ( ip < headerSip[mid]) {
            if (mid == 0) {
                sptr = headerPtr[mid];
                eptr = headerPtr[mid + 1];
                break;
            } else if ( ip > headerSip[mid - 1]) {
                sptr = headerPtr[mid - 1];
                eptr = headerPtr[mid];
                break;
            }
            high = mid - 1;
        } else {
            if ( mid == headerLen - 1) {
                sptr = headerPtr[mid - 1];
                eptr = headerPtr[mid];
                break;
            } else if ( ip <= headerSip[mid + 1]) {
                sptr = headerPtr[mid];
                eptr = headerPtr[mid + 1];
                break;
            }
            low = mid + 1;
        }
    }
    
    // match nothing 
    if (sptr == 0) return null;
    
    // second search (in index)
    var blockLen    = eptr - sptr;
    var blockBuffer = new Buffer(blockLen + indexBlockLength);
    fs.readSync(this.db_fd, blockBuffer, 0, blockLen + indexBlockLength, sptr);

    low = 0;
    high = blockLen / indexBlockLength;
    
    var p = 0;
    var sip = 0;
    var eip = 0;
    var dataPtr = 0;

    while(low <= high) {
        mid = ((low + high) >> 1);
        p = mid * indexBlockLength;
        sip = getLong(blockBuffer, p);

        if (ip < sip) {
            high = mid - 1;
        } else {
            eip = getLong(blockBuffer, p + 4);
            if (ip > eip) {
                low = mid + 1;
            } else {
                dataPtr =  getLong(blockBuffer, p + 8);
                break;
            }
        }
    }

    // read data
    if (dataPtr == 0) return null;
    var dataLen = ((dataPtr >> 24) & 0xFF);
    var dataPtr = (dataPtr & 0x00FFFFFF);
    var dataBuffer = new Buffer(dataLen);

    fs.readSync(this.db_fd, dataBuffer, 0, dataLen, dataPtr);

    var city_id = getLong(dataBuffer, 0);
    var data    = dataBuffer.toString('utf8', 4, dataLen);

    //console.log(city_id);
    //console.log(data);

    return { city: city_id, region: data };
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
    var val =  (   
        (buffer[offset] & 0x000000FF) | 
        ((buffer[offset + 1] <<  8) & 0x0000FF00) | 
        ((buffer[offset + 2] << 16) & 0x00FF0000) |
        ((buffer[offset + 3] << 24) & 0xFF000000)
    );

    // convert to unsigned int
    if (val < 0) {
        val = val >>> 0;
    }
    return val;
}


exports.create = function(db_path) 
{
    if (typeof(db_path) == "undefined" || fs.exists(db_path) ) {
        throw("[ip2region] db file not exists : " + db_path);
    }
    
    ip2region.db_file_path = db_path; 

    try {
        ip2region.db_fd = fs.openSync(ip2region.db_file_path, 'r');
    } catch(e) {
        throw("[ip2region] Can not open ip2region.db file , path : "
                + ip2region.db_file_path);
    }

    // init basic search environment
    if (totalBlocks == 0) {
        fs.readSync(ip2region.db_fd, superBlock, 0, 8, 0);
        firstIndexPtr = getLong(superBlock, 0);
        lastIndexPtr  = getLong(superBlock, 4);
        totalBlocks   = (lastIndexPtr - firstIndexPtr) 
                            / indexBlockLength + 1;
    }

    return ip2region;
}


exports.destroy = function(ip2rObj)
{
    ip2rObj.db_file_path = null;
    fs.closeSync(ip2rObj.db_fd);
}
