<?php
// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.
//
// @Author Lion <chenxin619315@gmail.com>
// @Date   2022/06/21

namespace ip2region\xdb;
use \Exception;

// global constants
const Structure_20     = 2;
const Structure_30     = 3;
const IPv4VersionNo    = 4;
const IPv6VersionNo    = 6;
const HeaderInfoLength = 256;
const VectorIndexRows  = 256;
const VectorIndexCols  = 256;
const VectorIndexSize  = 8;


// Util class
class Util {
    // parse the specified IP address and return its bytes.
    // returns: NULL for failed or the packed bytes
    public static function parseIP($ipString) {
        $flag = FILTER_FLAG_IPV4 | FILTER_FLAG_IPV6;
        if (!filter_var($ipString, FILTER_VALIDATE_IP, $flag)) {
            return null;
        }

        return inet_pton($ipString);
    }

    // IP bytes to string
    public static function ipToString($ipBytes) {
        $l = strlen($ipBytes);
        return ($l == 4 || $l == 16) ? inet_ntop($ipBytes) : '<invalid-ip-bytes>';
    }

    // compare two ip bytes (packed string return by parsedIP)
    // returns: -1 if ip1 < ip2, 0 if ip1 == ip2 or 1 if ip1 > ip2
    public static function ipSubCompare($ip1, $buff, $offset) {
        // $r = substr_compare($ip1, $buff, $offset, strlen($ip1));
        // @Note: substr_compare is not working, use the substr + strcmp instead
        $r = strcmp($ip1, substr($buff, $offset, strlen($ip1)));
        if ($r < 0) {
            return -1;
        } else if ($r > 0) {
            return 1;
        } else {
            return 0;
        }
    }

    // returns: -1 if ip1 < ip2, 0 if ip1 == ip2 or 1 if ip1 > ip2
    public static function ipCompare($ip1, $ip2) {
        $r = strcmp($ip1, $ip2);
        if ($r < 0) {
            return -1;
        } else if ($r > 0) {
            return 1;
        } else {
            return 0;
        }
    }

    // version parse
    public static function versionFromName($ver_name) {
        $name = strtoupper($ver_name);
        if ($name == "V4" || $name == "IPv4") {
            return IPv4::default();
        } else if ($name == "V6" || $name == "IPv6") {
            return IPv6::default();
        } else {
            throw new Exception("invalid verstion name `{$ver_name}`");
        }
    }

    // version parse from header
    public static function versionFromHeader($header) {
        // Old structure 2.0 with IPv4 supports ONLY
        if ($header['version'] == Structure_20) {
            return IPv4::default();
        }

        // structure 3.0 after IPv6 supporting
        if ($header['version'] != Structure_30) {
            throw new Exception("invalid xdb structure version `{$header['version']}`");
        }

        if ($header['ipVersion'] == IPv4VersionNo) {
            return IPv4::default();
        } else if ($header['ipVersion'] == IPv6VersionNo) {
            return IPv6::default();
        } else {
            throw new Exception("invalid ip version number `{$header['ipVersion']}`");
        }
    }

    // binary string chars implode with space
    public static function bytesToString($buff, $offset, $length) {
        $sb = [];
        for ($i = 0; $i < $length; $i++) {
            $sb[] = ord($buff[$offset+$i]) & 0xFF;
        }
        return '['.implode(' ', $sb).']';
    }

    // decode a 4bytes long with Little endian byte order from a byte buffer
    public static function le_getUint32($b, $idx) {
        $val = (ord($b[$idx])) | (ord($b[$idx+1]) << 8)
            | (ord($b[$idx+2]) << 16) | (ord($b[$idx+3]) << 24);

        // convert signed int to unsigned int if on 32 bit operating system
        if ($val < 0 && PHP_INT_SIZE == 4) {
            $val = sprintf("%u", $val);
        }

        return $val;
    }

    // read a 2bytes int with litten endian byte order from a byte buffer
    public static function le_getUint16($b, $idx) {
        return ((ord($b[$idx])) | (ord($b[$idx+1]) << 8));
    }

    // Verify if the current Searcher could be used to search the specified xdb file.
    // Why do we need this check ?
    // The future features of the xdb impl may cause the current searcher not able to work properly.
    //
    // @Note: You Just need to check this ONCE when the service starts
    // Or use another process (eg, A command) to check once Just to confirm the suitability.
    // returns: null for everything is ok or the error string.
    public static function verify($handle) {
        // load the header
        $header = self::loadHeader($handle);
        if ($header == null) {
            return 'failed to load the header';
        }

        // get the runtime ptr bytes
        $runtimePtrBytes = 0;
        if ($header['version'] == Structure_20) {
            $runtimePtrBytes = 4;
        } else if ($header['version'] == Structure_30) {
            $runtimePtrBytes = $header['runtimePtrBytes'];
        } else {
            return "invalid structure version `{$header['version']}`";
        }

        // 1, confirm the xdb file size
        // to ensure that the maximum file pointer does not overflow
        $stat = fstat($handle);
        if ($stat == false) {
            return 'failed to stat the xdb file';
        }

        $maxFilePtr = (1 << ($runtimePtrBytes * 8)) - 1;
        // print_r([$stat['size'], $maxFilePtr]);
        if ($stat['size'] > $maxFilePtr) {
             return "xdb file exceeds the maximum supported bytes: {$maxFilePtr}";
        }

        return null;
    }

    public static function verifyFromFile($dbFile) {
        $handle = fopen($dbFile, 'r');
        if ($handle === false) {
            return null;
        }

        $r = self::verify($handle);
        fclose($handle);
        return $r;
    }

    // load header info from a specified file handle
    public static function loadHeader($handle) {
        if (fseek($handle, 0) == -1) {
            return null;
        }

        $buff = fread($handle, HeaderInfoLength);
        if ($buff === false) {
            return null;
        }

        // read bytes length checking
        if (strlen($buff) != HeaderInfoLength) {
            return null;
        }

        // return the decoded header info
        return array(
            'version'         => self::le_getUint16($buff, 0),
            'indexPolicy'     => self::le_getUint16($buff, 2),
            'createdAt'       => self::le_getUint32($buff, 4),
            'startIndexPtr'   => self::le_getUint32($buff, 8),
            'endIndexPtr'     => self::le_getUint32($buff, 12),
            'ipVersion'       => self::le_getUint16($buff, 16),
            'runtimePtrBytes' => self::le_getUint16($buff, 18)
        );
    }

    // load header info from the specified xdb file path
    public static function loadHeaderFromFile($dbFile) {
        $handle = fopen($dbFile, 'r');
        if ($handle === false) {
            return null;
        }

        $header = self::loadHeader($handle);
        fclose($handle);
        return $header;
    }

    // load vector index from a file handle
    public static function loadVectorIndex($handle) {
        if (fseek($handle, HeaderInfoLength) == -1) {
            return null;
        }

        $rLen = VectorIndexRows * VectorIndexCols * VectorIndexSize;
        $buff = fread($handle, $rLen);
        if ($buff === false) {
            return null;
        }

        if (strlen($buff) != $rLen) {
            return null;
        }

        return $buff;
    }

    // load vector index from a specified xdb file path
    public static function loadVectorIndexFromFile($dbFile) {
        $handle = fopen($dbFile, 'r');
        if ($handle === false) {
            return null;
        }

        $vIndex = self::loadVectorIndex($handle);
        fclose($handle);
        return $vIndex;
    }

    // load the xdb content from a file handle
    public static function loadContent($handle) {
        if (fseek($handle, 0, SEEK_END) == -1) {
            return null;
        }

        $size = ftell($handle);
        if ($size === false) {
            return null;
        }

        // seek to the head for reading
        if (fseek($handle, 0) == -1) {
            return null;
        }

        $buff = fread($handle, $size);
        if ($buff === false) {
            return null;
        }

        // read length checking
        if (strlen($buff) != $size) {
            return null;
        }

        return $buff;
    }

    // load the xdb content from a file path
    public static function loadContentFromFile($dbFile) {
        $str = file_get_contents($dbFile, false);
        if ($str === false) {
            return null;
        } else {
            return $str;
        }
    }

    public static function now() {
        return (microtime(true) * 1000);
    }
}

// IPv4 version class
class IPv4 {
    public $id;
    public $name;
    public $bytes;
    public $segmentIndexSize;
    
    private static $C = null;
    public static function default() {
        if (self::$C == null) {
            // 14 = 4 + 4 + 2 + 4
            self::$C = new self(IPv4VersionNo, 'IPv4', 4, 14);
        }
        return self::$C;
    }

    public function __construct($id, $name, $bytes, $segmentIndexSize) {
        $this->id = $id;
        $this->name = $name;
        $this->bytes = $bytes;
        $this->segmentIndexSize = $segmentIndexSize;
    }

    // compare the two ip bytes with the current version
    public function ipSubCompare($ip1, $buff, $offset) {
        // ip1: Little endian byte order encoded long from searcher.
        // ip2: Little endian byte order read from xdb index.
        $len  = strlen($ip1);
        $eIdx = $offset + $len;
        for ($i = 0, $j = $eIdx - 1; $i < $len; $i++, $j--) {
            $i1 = ord($ip1[$i]) & 0xFF;
            $i2 = ord($buff[$j]) & 0xFF;
            // printf("i:%d, j:%d, i1:%d, i2:%d\n", $i, $j, $i1, $i2);
            if ($i1 > $i2) {
                return 1;
            } else if ($i1 < $i2) {
                return -1;
            }
        }

        return 0;
    }

    public function __toString() {
        return sprintf(
            "{id:%d, name:%s, bytes:%d, segmentIndexSize:%d}", 
            $this->id, $this->name, $this->bytes, $this->segmentIndexSize
        );
    }
}

class IPv6 {
    public $id;
    public $name;
    public $bytes;
    public $segmentIndexSize;

    private static $C = null;
    public static function default() {
        if (self::$C == null) {
            // 38 = 16 + 16 + 2 + 4
            self::$C = new self(IPv6VersionNo, 'IPv6', 16, 38);
        }

        return self::$C;
    }

    public function __construct($id, $name, $bytes, $segmentIndexSize) {
        $this->id = $id;
        $this->name = $name;
        $this->bytes = $bytes;
        $this->segmentIndexSize = $segmentIndexSize;
    }

    public function ipSubCompare($ip, $buff, $offset) {
        // return Util::ipCompare($ip, substr($buff, $offset, strlen($ip)));
        return Util::ipSubCompare($ip, $buff, $offset);
    }

    public function __toString() {
        return sprintf(
            "{id:%d, name:%s, bytes:%d, segmentIndexSize:%d}", 
            $this->id, $this->name, $this->bytes, $this->segmentIndexSize
        );
    }
}

// Xdb searcher implementation
class Searcher {
    // ip version
    private $version;

    // xdb file handle
    private $handle  = null;
    private $ioCount = 0;

    // vector index in binary string.
    // string decode will be faster than the map based Array.
    private $vectorIndex = null;

    // xdb content buffer
    private $contentBuff = null;

    // ---
    // static function to create searcher

    /**
     * @throws Exception
     */
    public static function newWithFileOnly($version, $dbFile) {
        return new self($version, $dbFile, null, null);
    }

    /**
     * @throws Exception
     */
    public static function newWithVectorIndex($version, $dbFile, $vIndex) {
        return new self($version, $dbFile, $vIndex, null);
    }

    /**
     * @throws Exception
     */
    public static function newWithBuffer($version, $cBuff) {
        return new self($version, null, null, $cBuff);
    }

    // --- End of static creator

    /**
     * initialize the xdb searcher
     * @throws Exception
     */
    function __construct($version, $dbFile, $vectorIndex=null, $cBuff=null) {
        $this->version = $version;
        // check the content buffer first
        if ($cBuff != null) {
            $this->vectorIndex = null;
            $this->contentBuff = $cBuff;
        } else {
            // open the xdb binary file
            $this->handle = fopen($dbFile, "r");
            if ($this->handle === false) {
                throw new Exception("failed to open xdb file '%s'", $dbFile);
            }

            $this->vectorIndex = $vectorIndex;
        }
    }

    public function close() {
        if ($this->handle != null) {
            fclose($this->handle);
        }
    }

    public function getIPVersion() {
        return $this->version;
    }

    public function getIOCount() {
        return $this->ioCount;
    }

    /**
     * find the region info for the specified ip address.
     * @Note: the ip address couldO ONLY be a human-readable IP address string,
     * DO not use the packed binary string returned by #parseIP
     * 
     * @throws Exception
     */
    public function search($ip) {
        $ipBytes = Util::parseIP($ip);
        if ($ipBytes == null) {
            throw new Exception("invalid ip address `{$ip}`");
        }

        return $this->searchByBytes($ipBytes);
    }

    /**
     * find the region info for the specified binary ip bytes returned by #parseIP.
     * 
     * @throws Exception
     */
    public function searchByBytes($ipBytes) {
        // ip version check
        if (strlen($ipBytes) != $this->version->bytes) {
            throw new Exception("invalid ip address ({$this->version->name} expected)");
        }

        // reset the global counter
        $this->ioCount = 0;

        // locate the segment index block based on the vector index
        $il0 = ord($ipBytes[0]) & 0xFF;
        $il1 = ord($ipBytes[1]) & 0xFF;
        $idx = $il0 * VectorIndexCols * VectorIndexSize + $il1 * VectorIndexSize;
        if ($this->vectorIndex != null) {
            $sPtr = Util::le_getUint32($this->vectorIndex, $idx);
            $ePtr = Util::le_getUint32($this->vectorIndex, $idx + 4);
        } else if ($this->contentBuff != null) {
            $sPtr = Util::le_getUint32($this->contentBuff, HeaderInfoLength + $idx);
            $ePtr = Util::le_getUint32($this->contentBuff, HeaderInfoLength + $idx + 4);
        } else {
            // read the vector index block
            $buff = $this->read(HeaderInfoLength + $idx, 8);
            $sPtr = Util::le_getUint32($buff, 0);
            $ePtr = Util::le_getUint32($buff, 4);
        }

        // printf("sPtr: %d, ePtr: %d\n", $sPtr, $ePtr);
        [$bytes, $dBytes] = [strlen($ipBytes), strlen($ipBytes) << 1];

        // binary search the segment index to get the region info
        $idxSize = $this->version->segmentIndexSize;
        [$dataLen, $dataPtr, $l, $h] = [0, 0, 0, ($ePtr - $sPtr) / $idxSize];
        while ($l <= $h) {
            $m = ($l + $h) >> 1;
            $p = $sPtr + $m * $idxSize;

            // read the segment index
            $buff = $this->read($p, $idxSize);

            // compare the segment index
            if ($this->version->ipSubCompare($ipBytes, $buff, 0) < 0) {
                $h = $m - 1;
            } else if ($this->version->ipSubCompare($ipBytes, $buff, $bytes) > 0) {
                $l = $m + 1;
            } else {
                $dataLen = Util::le_getUint16($buff, $dBytes);
                $dataPtr = Util::le_getUint32($buff, $dBytes + 2);
                break;
            }
        }

        // empty match interception.
        // printf("dataLen: %d, dataPtr: %d\n", $dataLen, $dataPtr);
        if ($dataLen == 0) {
            return "";
        }

        // load and return the region data
        return $this->read($dataPtr, $dataLen);
    }

    // read specified bytes from the specified index
    private function read($offset, $len) {
        // check the in-memory buffer first
        if ($this->contentBuff != null) {
            return substr($this->contentBuff, $offset, $len);
        }

        // read from the file
        $r = fseek($this->handle, $offset);
        if ($r == -1) {
            throw new Exception("failed to fseek to {$offset}");
        }

        $this->ioCount++;
        $buff = fread($this->handle, $len);
        if ($buff === false) {
            throw new Exception("failed to fread from {$len}");
        }

        if (strlen($buff) != $len) {
            throw new Exception("incomplete read: read bytes should be {$len}");
        }

        return $buff;
    }

}