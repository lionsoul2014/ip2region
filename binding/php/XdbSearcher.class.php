<?php
// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.
//
// @Author Lion <chenxin619315@gmail.com>
// @Date   2022/06/21

class XdbSearcher
{
    const HeaderInfoLength = 256;
    const VectorIndexRows  = 256;
    const VectorIndexCols  = 256;
    const VectorIndexSize  = 8;
    const SegmentIndexSize = 14;

    // xdb file handle
    private $handle = null;

    // header info
    private $header = null;
    private $ioCount = 0;

    // vector index in binary string.
    // string decode will be faster than the map based Array.
    private $vectorIndex = null;

    // xdb content buffer
    private $contentBuff = null;

    /**
     * initialize the xdb searcher
     * @throws Exception
     */
    function __construct($dbFile, $vectorIndex=null, $cBuff=null) {
        // check the content buffer first
        if ($cBuff != null) {
            // check and autoload the vector index
            if ($vectorIndex != null) {
                // load the vector index
                $this->vectorIndex = null;
            }

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

    function close() {
        if ($this->handle != null) {
            fclose($this->handle);
        }
    }

    function getIOCount() {
        return $this->ioCount;
    }

    /**
     * find the region info for the specified ip address
     * @throws Exception
     */
    function search($ip) {
        // check and convert the sting ip to a 4-bytes long
        if (is_string($ip)) {
            $t = self::ip2long($ip);
            if ($t === false) {
                throw new Exception("invalid ip address `$ip`");
            }
            $ip = $t;
        }

        // reset the global counter
        $this->ioCount = 0;

        // locate the segment index block based on the vector index
        $il0 = ($ip >> 24) & 0xFF;
        $il1 = ($ip >> 16) & 0xFF;
        $idx = $il0 * self::VectorIndexRows * self::VectorIndexSize + $il1 * self::VectorIndexSize;
        printf("il0: %d, il1: %d, idx: %d\n", $il0, $il1, $idx);
        if ($this->vectorIndex != null) {
            $sPtr = self::getLong($this->vectorIndex, $idx);
            $ePtr = self::getLong($this->vectorIndex, $idx + 4);
        } else {
            // read the vector index block
            $buff = $this->read(self::HeaderInfoLength + $idx, 8);
            if ($buff === null) {
                throw new Exception("failed to read vector index at ${idx}");
            }

            $sPtr = self::getLong($buff, 0);
            $ePtr = self::getLong($buff, 4);
        }

        printf("sPtr: %d, ePtr: %d\n", $sPtr, $ePtr);

        // binary search the segment index to get the region info
        $dataLen = 0;
        $dataPtr = null;
        $l = 0;
        $h = ($ePtr - $sPtr) / self::SegmentIndexSize;
        while ($l <= $h) {
            $m = ($l + $h) >> 1;
            $p = $sPtr + $m * self::SegmentIndexSize;

            // read the segment index
            $buff = $this->read($p, self::SegmentIndexSize);
            if ($buff == null) {
                throw new Exception("failed to read segment index at ${$p}");
            }

            $sip = self::getLong($buff, 0);
            if ($ip < $sip) {
                $h = $m - 1;
            } else {
                $eip = self::getLong($buff, 4);
                if ($ip > $eip) {
                    $l = $m + 1;
                } else {
                    $dataLen = self::getShort($buff, 8);
                    $dataPtr = self::getShort($buff, 10);
                    break;
                }
            }
        }

        // match nothing interception.
        // @TODO: could this even be a case ?
        printf("dataLen: %d, dataPtr: %d\n", $dataLen, $dataPtr);
        if ($dataPtr == null) {
            return null;
        }

        // load and return the region data
        $buff = $this->read($dataPtr, $dataLen);
        if ($buff == null) {
            return null;
        }

        $str = [];
        foreach (unpack("C*", $buff) as $chr) {
            $str[] = chr($chr);
        }

        return implode($str);
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
            return null;
        }

        $this->ioCount++;
        $buff = fread($this->handle, $len);
        if ($buff === false) {
            return null;
        }

        if (strlen($buff) != $len) {
            return null;
        }

        return $buff;
    }

    // convert a string ip to long
    public static function ip2long($ip)
    {
        $ip = ip2long($ip);
        if ($ip === false) {
            return false;
        }

        // convert signed int to unsigned int if on 32 bit operating system
        if ($ip < 0 && PHP_INT_SIZE == 4) {
            $ip = sprintf("%u", $ip);
        }

        return $ip;
    }

    // read a 4bytes long from a byte buffer
    public static function getLong($b, $idx)
    {
        $val = (ord($b[$idx])) | (ord($b[$idx+1]) << 8)
            | (ord($b[$idx+2]) << 16) | (ord($b[$idx+3]) << 24);

        // convert signed int to unsigned int if on 32 bit operating system
        if ($val < 0 && PHP_INT_SIZE == 4) {
            $val = sprintf("%u", $val);
        }

        return $val;
    }

    // read a 2bytes short from a byte buffer
    public static function getShort($b, $idx)
    {
        return ((ord($b[$idx])) | (ord($b[$idx+1]) << 8));
    }

}