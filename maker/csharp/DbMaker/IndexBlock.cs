namespace DbMaker
{
    /// <summary>
    ///     item index class
    /// </summary>
    public class IndexBlock
    {
        private static int LENGTH = 12;

        public IndexBlock(long startIp, long endIp, int dataPtr, int dataLen)
        {
            StartIp = startIp;
            EndIp = endIp;
            DataPtr = dataPtr;
            DataLen = dataLen;
        }

        /**
         * start ip address
         */
        public long StartIp { get; set; }

        /**
         * end ip address
         */
        public long EndIp { get; set; }

        /**
         * data ptr and data length
         */
        public int DataPtr { get; set; }

        /**
         * data length
         */
        public int DataLen { get; set; }

        public IndexBlock SetStartIp(long startIp)
        {
            StartIp = startIp;
            return this;
        }

        public IndexBlock SetEndIp(long endIp)
        {
            EndIp = endIp;
            return this;
        }

        public IndexBlock SetDataPtr(int dataPtr)
        {
            DataPtr = dataPtr;
            return this;
        }

        public IndexBlock SetDataLen(int dataLen)
        {
            DataLen = dataLen;
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
             * | 4bytes        | 4bytes    | 4bytes    |
             * +------------+-----------+-----------+
             *  start ip      end ip      data ptr + len 
            */
            var b = new byte[12];

            Util.writeIntLong(b, 0, StartIp); //start ip
            Util.writeIntLong(b, 4, EndIp); //end ip

            //write the data ptr and the length
            var mix = DataPtr | DataLen << 24 & 0xFF000000L;
            Util.writeIntLong(b, 8, mix);

            return b;
        }
    }
}