//*******************************
// Create By Rocher Kong 
// Github https://github.com/RocherKong
// Date 2018.02.09
//*******************************

using System;

namespace IP2Region
{

    public class HeaderBlock
    {
        /**
         * index block start ip address
        */
        private long indexStartIp;

        /**
         * ip address 
        */
        private int indexPtr;

        public HeaderBlock(long indexStartIp, int indexPtr)
        {
            this.indexStartIp = indexStartIp;
            this.indexPtr = indexPtr;
        }

        public long GetIndexStartIp()
        {
            return indexStartIp;
        }

        public HeaderBlock SetIndexStartIp(long indexStartIp)
        {
            this.indexStartIp = indexStartIp;
            return this;
        }

        public int GetIndexPtr()
        {
            return indexPtr;
        }

        public HeaderBlock SetIndexPtr(int indexPtr)
        {
            this.indexPtr = indexPtr;
            return this;
        }

        /**
         * get the bytes for db storage
         * 
         * @return    byte[]
        */
        public byte[] GetBytes()
        {
            /*
             * +------------+-----------+
             * | 4bytes     | 4bytes    |
             * +------------+-----------+
             *  start ip      index ptr
            */
            byte[] b = new byte[8];

            Util.writeIntLong(b, 0, indexStartIp);
            Util.writeIntLong(b, 4, indexPtr);

            return b;
        }
    }
}