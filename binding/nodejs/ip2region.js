/**
 * ip2region client for nodejs
 *
 * project: https://github.com/lionsoul2014/ip2region
 *
 * @author dongyado<dongyado@gmail.com>
 * @author leeching<leeching.fx@gmail.com>
 */
const fs = require('fs');

const IP_BASE = [16777216, 65536, 256, 1];
const INDEX_BLOCK_LENGTH = 12;
const TOTAL_HEADER_LENGTH = 8192;

/**
 * Convert ip to long (xxx.xxx.xxx.xxx to a integer)
 *
 * @param {string} ip
 * @return {number} long value
 */
function ip2long(ip) {
    const arr = ip.split('.');
    if (arr.length !== 4) {
        throw new Error('invalid ip');
    }
    return arr.reduce((val, n, i) => {
        n = Number(n);
        if (!Number.isInteger(n) || n < 0 || n > 255) {
            throw new Error('invalid ip');
        }
        return val + IP_BASE[i] * n;
    }, 0);
}

/**
 * Get long value from buffer with specified offset
 *
 * @param {Buffer} buffer
 * @param {number} offset
 * @return {number} long value
 */
function getLong(buffer, offset) {
    const val =
        (buffer[offset] & 0x000000ff) |
        ((buffer[offset + 1] << 8) & 0x0000ff00) |
        ((buffer[offset + 2] << 16) & 0x00ff0000) |
        ((buffer[offset + 3] << 24) & 0xff000000);
    return val < 0 ? val >>> 0 : val;
}

/**
 * @typedef {Object} SearchResult
 * @property {number} city
 * @property {string} region
 */

class IP2Region {
    static create(dbPath) {
        const oldInstance = IP2Region._instances.get(dbPath);
        if (oldInstance) {
            return oldInstance;
        } else {
            const instance = new IP2Region({ dbPath });
            IP2Region._instances.set(dbPath, instance);
            return instance;
        }
    }

    /**
     * For backward compatibility
     */
    static destroy() {
        IP2Region._instances.forEach(([key, instance]) => {
            instance.destroy();
        });
    }

    constructor(options = {}) {
        const { dbPath } = options;
        if (!dbPath || !fs.existsSync(dbPath)) {
            throw new Error(`[ip2region] db file not exists : ${dbPath}`);
        }

        try {
            this.dbFd = fs.openSync(dbPath, 'r');
        } catch (e) {
            throw new Error(
                `[ip2region] Can not open ip2region.db file , path: ${dbPath}`
            );
        }

        IP2Region._instances.set((this.dbPath = dbPath), this);

        this.totalBlocks = this.firstIndexPtr = this.lastIndexPtr = 0;
        this.calcTotalBlocks();

        this.headerIndexBuffer = new Buffer(TOTAL_HEADER_LENGTH);
        this.headerSip = [];
        this.headerPtr = [];
        this.headerLen = 0;
        this.prepareHeader();
    }

    /**
     * @public
     */
    destroy() {
        fs.closeSync(ip2rObj.dbFd);
        IP2Region._instances.delete(this.dbPath);
    }

    /**
     * @public
     * @param {string} ip
     * @return {SearchResult}
     */
    binarySearchSync(ip) {
        ip = ip2long(ip);

        let low = 0;
        let mid = 0;
        let high = this.totalBlocks;
        let dataPos = 0;
        let pos = 0;
        let sip = 0;
        let eip = 0;
        const indexBuffer = new Buffer(12);

        // binary search
        while (low <= high) {
            mid = (low + high) >> 1;
            pos = this.firstIndexPtr + mid * INDEX_BLOCK_LENGTH;
            fs.readSync(this.dbFd, indexBuffer, 0, INDEX_BLOCK_LENGTH, pos);
            sip = getLong(indexBuffer, 0);

            if (ip < sip) {
                high = mid - 1;
            } else {
                eip = getLong(indexBuffer, 4);
                if (ip > eip) {
                    low = mid + 1;
                } else {
                    dataPos = getLong(indexBuffer, 8);
                    break;
                }
            }
        }
        return this.readData(dataPos);
    }

    /**
     * @public
     * @param {string} ip
     * @return {SearchResult}
     */
    btreeSearchSync(ip) {
        ip = ip2long(ip);

        // first search  (in header index)
        let low = 0;
        let mid = 0;
        let high = this.headerLen;
        let sptr = 0;
        let eptr = 0;

        while (low <= high) {
            mid = (low + high) >> 1;

            if (ip == this.headerSip[mid]) {
                if (mid > 0) {
                    sptr = this.headerPtr[mid - 1];
                    eptr = this.headerPtr[mid];
                } else {
                    sptr = this.headerPtr[mid];
                    eptr = this.headerPtr[mid + 1];
                }
                break;
            }

            if (ip < this.headerSip[mid]) {
                if (mid == 0) {
                    sptr = this.headerPtr[mid];
                    eptr = this.headerPtr[mid + 1];
                    break;
                } else if (ip > this.headerSip[mid - 1]) {
                    sptr = this.headerPtr[mid - 1];
                    eptr = this.headerPtr[mid];
                    break;
                }
                high = mid - 1;
            } else {
                if (mid == this.headerLen - 1) {
                    sptr = this.headerPtr[mid - 1];
                    eptr = this.headerPtr[mid];
                    break;
                } else if (ip <= this.headerSip[mid + 1]) {
                    sptr = this.headerPtr[mid];
                    eptr = this.headerPtr[mid + 1];
                    break;
                }
                low = mid + 1;
            }
        }

        // match nothing
        if (sptr == 0) return null;

        // second search (in index)
        const blockLen = eptr - sptr;
        const blockBuffer = new Buffer(blockLen + INDEX_BLOCK_LENGTH);
        fs.readSync(
            this.dbFd,
            blockBuffer,
            0,
            blockLen + INDEX_BLOCK_LENGTH,
            sptr
        );

        low = 0;
        high = blockLen / INDEX_BLOCK_LENGTH;

        let p = 0;
        let sip = 0;
        let eip = 0;
        let dataPtr = 0;

        while (low <= high) {
            mid = (low + high) >> 1;
            p = mid * INDEX_BLOCK_LENGTH;
            sip = getLong(blockBuffer, p);

            if (ip < sip) {
                high = mid - 1;
            } else {
                eip = getLong(blockBuffer, p + 4);
                if (ip > eip) {
                    low = mid + 1;
                } else {
                    dataPtr = getLong(blockBuffer, p + 8);
                    break;
                }
            }
        }
        return this.readData(dataPtr);
    }

    /**
     * @private
     */
    calcTotalBlocks() {
        const superBlock = new Buffer(8);
        fs.readSync(this.dbFd, superBlock, 0, 8, 0);
        this.firstIndexPtr = getLong(superBlock, 0);
        this.lastIndexPtr = getLong(superBlock, 4);
        this.totalBlocks =
            (this.lastIndexPtr - this.firstIndexPtr) / INDEX_BLOCK_LENGTH + 1;
    }

    /**
     * @private
     */
    prepareHeader() {
        fs.readSync(
            this.dbFd,
            this.headerIndexBuffer,
            0,
            TOTAL_HEADER_LENGTH,
            8
        );

        for (let i = 0; i < TOTAL_HEADER_LENGTH; i += 8) {
            const startIp = getLong(this.headerIndexBuffer, i);
            const dataPtr = getLong(this.headerIndexBuffer, i + 4);
            if (dataPtr == 0) break;

            this.headerSip.push(startIp);
            this.headerPtr.push(dataPtr);
            this.headerLen++; // header index size count
        }
    }

    /**
     * @private
     * @param {number} dataPos
     * @return {SearchResult}
     */
    readData(dataPos) {
        if (dataPos == 0) return null;
        const dataLen = (dataPos >> 24) & 0xff;
        dataPos = dataPos & 0x00ffffff;
        const dataBuffer = new Buffer(dataLen);

        fs.readSync(this.dbFd, dataBuffer, 0, dataLen, dataPos);

        const city = getLong(dataBuffer, 0);
        const region = dataBuffer.toString('utf8', 4, dataLen);

        return { city, region };
    }
}

IP2Region._instances = new Map();

module.exports = IP2Region;
