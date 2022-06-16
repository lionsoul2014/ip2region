//*******************************
// Created By Rocher Kong 
// Github https://github.com/RocherKong
// Date 2018.02.09
//*******************************

namespace IP2Region
{
    internal class IndexBlock
    {
        public const int LENGTH = 12;

        public long StartIP
        {
            get;
            private set;
        }

        public long EndIp
        {
            get;
            private set;
        }

        public uint DataPtr
        {
            get;
            private set;
        }

        public int DataLen
        {
            get;
            private set;
        }

        public IndexBlock(long startIp, long endIp, uint dataPtr, int dataLen)
        {
            StartIP = startIp;
            EndIp = endIp;
            DataPtr = dataPtr;
            DataLen = dataLen;
        }

        public byte[] GetBytes()
        {
            /*
             * +------------+-----------+-----------+
             * | 4bytes     | 4bytes    | 4bytes    |
             * +------------+-----------+-----------+
             *  start ip      end ip      data ptr + len 
            */
            byte[] b = new byte[12];

            Utils.WriteIntLong(b, 0, StartIP);    //start ip
            Utils.WriteIntLong(b, 4, EndIp);        //end ip

            //write the data ptr and the length
            long mix = DataPtr | ((DataLen << 24) & 0xFF000000L);
            Utils.WriteIntLong(b, 8, mix);

            return b;
        }
    }
}