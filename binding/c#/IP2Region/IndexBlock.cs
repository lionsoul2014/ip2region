//*******************************
// Create By Rocher Kong 
// Github https://github.com/RocherKong
// Date 2018.02.09
//*******************************
using System;

namespace IP2Region
{

    public class IndexBlock
    {
        private static int LENGTH = 12;

        /**
         * start ip address 
        */
        private long startIp;

        /**
         * end ip address 
        */
        private long endIp;

        /**
         * data ptr and data length 
        */
        private uint dataPtr;

        /**
         * data length 
        */
        private int dataLen;

        public IndexBlock(long startIp, long endIp, uint dataPtr, int dataLen)
        {
            this.startIp = startIp;
            this.endIp = endIp;
            this.dataPtr = dataPtr;
            this.dataLen = dataLen;
        }

        public long GetStartIp()
        {
            return startIp;
        }

        public IndexBlock SetStartIp(long startIp)
        {
            this.startIp = startIp;
            return this;
        }

        public long GetEndIp()
        {
            return endIp;
        }

        public IndexBlock SetEndIp(long endIp)
        {
            this.endIp = endIp;
            return this;
        }

        public uint GetDataPtr()
        {
            return dataPtr;
        }

        public IndexBlock SetDataPtr(uint dataPtr)
        {
            this.dataPtr = dataPtr;
            return this;
        }

        public int GetDataLen()
        {
            return dataLen;
        }

        public IndexBlock SetDataLen(int dataLen)
        {
            this.dataLen = dataLen;
            return this;
        }

        public static int GetIndexBlockLength()
        {
            return LENGTH;
        }

        /**
         * get the bytes for storage
         * 
         * @return    byte[]
        */
        public byte[] GetBytes()
        {
            /*
             * +------------+-----------+-----------+
             * | 4bytes     | 4bytes    | 4bytes    |
             * +------------+-----------+-----------+
             *  start ip      end ip      data ptr + len 
            */
            byte[] b = new byte[12];

            Util.writeIntLong(b, 0, startIp);    //start ip
            Util.writeIntLong(b, 4, endIp);        //end ip

            //write the data ptr and the length
            long mix = dataPtr | ((dataLen << 24) & 0xFF000000L);
            Util.writeIntLong(b, 8, mix);

            return b;
        }
    }
}