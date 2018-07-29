//*******************************
// Created By Rocher Kong 
// Github https://github.com/RocherKong
// Date 2018.02.09
//*******************************

namespace IP2Region.Models
{
    internal class HeaderBlock
    {
        public long IndexStartIp
        {
            get;
            private set;
        }

        public int IndexPtr
        {
            get;
            private set;
        }

        public HeaderBlock(long indexStartIp, int indexPtr)
        {
            IndexStartIp = indexStartIp;
            IndexPtr = indexPtr;
        }

        /// <summary>
        /// Get the bytes for total storage
        /// </summary>
        /// <returns>
        /// Bytes gotten.
        /// </returns>
        public byte[] GetBytes()
        {
            /*
             * +------------+-----------+
             * | 4bytes     | 4bytes    |
             * +------------+-----------+
             *  start ip      index ptr
            */
            byte[] b = new byte[8];
            Utils.WriteIntLong(b, 0, IndexStartIp);
            Utils.WriteIntLong(b, 4, IndexPtr);
            return b;
        }
    }
}