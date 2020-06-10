using System;
using System.IO;
using System.Text;

namespace DbMaker
{
    /// <summary>
    ///     ip db searcher class (Not thread safe)
    /// </summary>
    public class DbSearcher : IDisposable
    {
        public const int BTREE_ALGORITHM = 1;
        public const int BINARY_ALGORITHM = 2;
        public const int MEMORY_ALGORITYM = 3;

        /**
         * for memory mode
         * the original db binary string
        */
        private byte[] dbBinStr;

        /**
         * super blocks info
         */
        private long firstIndexPtr;

        private int headerLength;
        private int[] HeaderPtr;

        /**
         * header blocks buffer
         */
        private long[] HeaderSip;

        private long lastIndexPtr;

        /**
         * db file access handler
        */
        private readonly Stream raf;

        private int totalIndexBlocks;

        /**
         * construct class
         * 
         * @param   bdConfig
         * @param   dbFile
         * @throws  FileNotFoundException
         */
        public DbSearcher(DbConfig dbConfig, string dbFile)
        {
            this.dbConfig = dbConfig;
            raf = File.OpenRead(dbFile); //new RandomAccessFile(dbFile, "r");
        }

        /**
         * construct method with self-define std ip2region bianry string support
         * Thanks to the issue from Wendal at https://gitee.com/lionsoul/ip2region/issues/IILFL
         *
         * @param   dbConfig
         * @param   dbBinStr
         */
        public DbSearcher(DbConfig dbConfig, byte[] dbBinStr)
        {
            this.dbConfig = dbConfig;
            this.dbBinStr = dbBinStr;

            firstIndexPtr = Util.getIntLong(dbBinStr, 0);
            lastIndexPtr = Util.getIntLong(dbBinStr, 4);
            totalIndexBlocks = (int) ((lastIndexPtr - firstIndexPtr) / IndexBlock.GetIndexBlockLength()) + 1;
        }

        /**
         * db config
        */
        public DbConfig dbConfig { get; set; }

        public void Dispose()
        {
            Close();
        }

        /**
         * get the region with a int ip address with memory binary search algorithm
         *
         * @param   ip
         * @throws  IOException
        */
        public DataBlock MemorySearch(long ip)
        {
            var blen = IndexBlock.GetIndexBlockLength();
            if (dbBinStr == null)
            {
                dbBinStr = new byte[(int) raf.Length];
                raf.Seek(0L, SeekOrigin.Begin);
                //raf.readFully(dbBinStr, 0, dbBinStr.length);
                raf.Read(dbBinStr, 0, dbBinStr.Length);
                //initialize the global vars
                firstIndexPtr = Util.getIntLong(dbBinStr, 0);
                lastIndexPtr = Util.getIntLong(dbBinStr, 4);
                totalIndexBlocks = (int) ((lastIndexPtr - firstIndexPtr) / blen) + 1;
            }

            //search the index blocks to define the data
            int l = 0, h = totalIndexBlocks;
            long sip, eip, dataptr = 0;
            while (l <= h)
            {
                var m = l + h >> 1;
                var p = (int) (firstIndexPtr + m * blen);

                sip = Util.getIntLong(dbBinStr, p);
                if (ip < sip)
                {
                    h = m - 1;
                }
                else
                {
                    eip = Util.getIntLong(dbBinStr, p + 4);
                    if (ip > eip)
                    {
                        l = m + 1;
                    }
                    else
                    {
                        dataptr = Util.getIntLong(dbBinStr, p + 8);
                        break;
                    }
                }
            }

            //not matched
            if (dataptr == 0)
            {
                return null;
            }

            //get the data
            var dataLen = (int) (dataptr >> 24 & 0xFF);
            var dataPtr = (int) (dataptr & 0x00FFFFFF);
            var city_id = (int) Util.getIntLong(dbBinStr, dataPtr);
            //String region = new String(dbBinStr, dataPtr + 4, dataLen - 4, "UTF-8");
            var region = Encoding.UTF8.GetString(dbBinStr, dataPtr + 4, dataLen - 4);
            return new DataBlock(city_id, region, dataPtr);
        }

        /**
         * get the region throught the ip address with memory binary search algorithm
         * 
         * @param   ip
         * @return  DataBlock
         * @throws  IOException
         */
        public DataBlock MemorySearch(string ip)
        {
            return MemorySearch(Util.ip2long(ip));
        }


        /**
         * get by index ptr
         * 
         * @param   indexPtr
         * @throws  IOException
         */
        public DataBlock GetByIndexPtr(long ptr)
        {
            raf.Seek(ptr, SeekOrigin.Begin);
            var
                buffer = new byte[12];
            //raf.readFully(buffer, 0, buffer.length);
            raf.Read(buffer, 0, buffer.Length);
            //long startIp = Util.getIntLong(buffer, 0);
            //long endIp = Util.getIntLong(buffer, 4);
            var extra = Util.getIntLong(buffer, 8);

            var dataLen = (int) (extra >> 24 & 0xFF);
            var dataPtr = (int) (extra & 0x00FFFFFF);

            raf.Seek(dataPtr, SeekOrigin.Begin);
            var data = new byte[dataLen];
            //raf.readFully(data, 0, data.length);
            raf.Read(data, 0, data.Length);
            var city_id = (int) Util.getIntLong(data, 0);
            //String region = new String(data, 4, data.length - 4, "UTF-8");
            var region = Encoding.UTF8.GetString(data, 4, data.Length - 4);
            return new DataBlock(city_id, region, dataPtr);
        }

        /**
         * get the region with a int ip address with b-tree algorithm
         * 
         * @param   ip
         * @throws  IOException
         */
        public DataBlock BTreeSearch(long ip)
        {
            //check and load the header
            if (HeaderSip == null)
            {
                //raf.seek(8L);    //pass the super block
                raf.Seek(8, SeekOrigin.Begin);
                var b = new byte[dbConfig.TotalHeaderSize];
                // byte[] b = new byte[4096];
                //raf.readFully(b, 0, b.length);
                raf.Read(b, 0, b.Length);

                //fill the header
                int len = b.Length >> 3, idx = 0; //b.lenght / 8
                HeaderSip = new long[len];
                HeaderPtr = new int[len];
                long startIp;
                long xDataPtr = 0;
                for (var i = 0; i < b.Length; i += 8)
                {
                    startIp = Util.getIntLong(b, i);
                    xDataPtr = Util.getIntLong(b, i + 4);
                    if (xDataPtr == 0)
                    {
                        break;
                    }

                    HeaderSip[idx] = startIp;
                    HeaderPtr[idx] = (int) xDataPtr;
                    idx++;
                }

                headerLength = idx;
            }

            //1. define the index block with the binary search
            if (ip == HeaderSip[0])
            {
                return GetByIndexPtr(HeaderPtr[0]);
            }

            if (ip == HeaderSip[headerLength - 1])
            {
                return GetByIndexPtr(HeaderPtr[headerLength - 1]);
            }

            int l = 0, h = headerLength, sptr = 0, eptr = 0;
            while (l <= h)
            {
                var m = l + h >> 1;

                //perfetc matched, just return it
                if (ip == HeaderSip[m])
                {
                    if (m > 0)
                    {
                        sptr = HeaderPtr[m - 1];
                        eptr = HeaderPtr[m];
                    }
                    else
                    {
                        sptr = HeaderPtr[m];
                        eptr = HeaderPtr[m + 1];
                    }

                    break;
                }

                //less then the middle value
                if (ip < HeaderSip[m])
                {
                    if (m == 0)
                    {
                        sptr = HeaderPtr[m];
                        eptr = HeaderPtr[m + 1];
                        break;
                    }

                    if (ip > HeaderSip[m - 1])
                    {
                        sptr = HeaderPtr[m - 1];
                        eptr = HeaderPtr[m];
                        break;
                    }

                    h = m - 1;
                }
                else
                {
                    if (m == headerLength - 1)
                    {
                        sptr = HeaderPtr[m - 1];
                        eptr = HeaderPtr[m];
                        break;
                    }

                    if (ip <= HeaderSip[m + 1])
                    {
                        sptr = HeaderPtr[m];
                        eptr = HeaderPtr[m + 1];
                        break;
                    }

                    l = m + 1;
                }
            }

            //match nothing just stop it
            if (sptr == 0)
            {
                return null;
            }

            //2. search the index blocks to define the data
            int blockLen = eptr - sptr, blen = IndexBlock.GetIndexBlockLength();
            var
                iBuffer = new byte[blockLen + blen]; //include the right border block
            //raf.seek(sptr);
            //raf.readFully(iBuffer, 0, iBuffer.length);
            raf.Seek(sptr, SeekOrigin.Begin);
            raf.Read(iBuffer, 0, iBuffer.Length);

            l = 0;
            h = blockLen / blen;
            long sip, eip, dataptr = 0;
            while (l <= h)
            {
                var m = l + h >> 1;
                var p = m * blen;
                sip = Util.getIntLong(iBuffer, p);
                if (ip < sip)
                {
                    h = m - 1;
                }
                else
                {
                    eip = Util.getIntLong(iBuffer, p + 4);
                    if (ip > eip)
                    {
                        l = m + 1;
                    }
                    else
                    {
                        dataptr = Util.getIntLong(iBuffer, p + 8);
                        break;
                    }
                }
            }

            //not matched
            if (dataptr == 0)
            {
                return null;
            }

            //3. get the data
            var dataLen = (int) (dataptr >> 24 & 0xFF);
            var dataPtr = (int) (dataptr & 0x00FFFFFF);

            //raf.seek(dataPtr);
            var data = new byte[dataLen];
            // raf.readFully(data, 0, data.length);
            raf.Seek(dataPtr, SeekOrigin.Begin);
            raf.Read(data, 0, data.Length);
            var city_id = (int) Util.getIntLong(data, 0);
            //String region = new String(data, 4, data.length - 4, "UTF-8");
            var region = Encoding.UTF8.GetString(data, 4, data.Length - 4);
            return new DataBlock(city_id, region, dataPtr);
        }

        /**
         * get the region throught the ip address with b-tree search algorithm
         * 
         * @param   ip
         * @return  DataBlock
         * @throws  IOException
         */
        public DataBlock BTreeSearch(string ip)
        {
            return BTreeSearch(Util.ip2long(ip));
        }

        /**
         * get the region with a int ip address with binary search algorithm
         * 
         * @param   ip
         * @throws  IOException
         */
        public DataBlock BinarySearch(long ip)
        {
            var blen = IndexBlock.GetIndexBlockLength();
            if (totalIndexBlocks == 0)
            {
                //raf.seek(0L);
                var superBytes = new byte[8];
                //raf.readFully(superBytes, 0, superBytes.length);
                raf.Seek(0, SeekOrigin.Begin);
                raf.Read(superBytes, 0, superBytes.Length);
                //initialize the global vars
                firstIndexPtr = Util.getIntLong(superBytes, 0);
                lastIndexPtr = Util.getIntLong(superBytes, 4);
                totalIndexBlocks = (int) ((lastIndexPtr - firstIndexPtr) / blen) + 1;
            }

            //search the index blocks to define the data
            int l = 0, h = totalIndexBlocks;
            var
                buffer = new byte[blen];
            long sip, eip, dataptr = 0;
            while (l <= h)
            {
                var m = l + h >> 1;
                //raf.seek(firstIndexPtr + m * blen);    //set the file pointer
                //raf.readFully(buffer, 0, buffer.length);
                raf.Seek(firstIndexPtr + m * blen, SeekOrigin.Begin);
                raf.Read(buffer, 0, buffer.Length);
                sip = Util.getIntLong(buffer, 0);
                if (ip < sip)
                {
                    h = m - 1;
                }
                else
                {
                    eip = Util.getIntLong(buffer, 4);
                    if (ip > eip)
                    {
                        l = m + 1;
                    }
                    else
                    {
                        dataptr = Util.getIntLong(buffer, 8);
                        break;
                    }
                }
            }

            //not matched
            if (dataptr == 0)
            {
                return null;
            }

            //get the data
            var dataLen = (int) (dataptr >> 24 & 0xFF);
            var dataPtr = (int) (dataptr & 0x00FFFFFF);

            //raf.seek(dataPtr);
            var data = new byte[dataLen];
            //raf.readFully(data, 0, data.length);
            raf.Seek(dataPtr, SeekOrigin.Begin);
            raf.Read(data, 0, data.Length);

            var city_id = (int) Util.getIntLong(data, 0);
            //String region = new String(data, 4, data.length - 4, "UTF-8");
            var region = Encoding.UTF8.GetString(data, 4, data.Length - 4);
            return new DataBlock(city_id, region, dataPtr);
        }

        /**
         * get the region throught the ip address with binary search algorithm
         * 
         * @param   ip
         * @return  DataBlock
         * @throws  IOException
         */
        public DataBlock BinarySearch(string ip)
        {
            return BinarySearch(Util.ip2long(ip));
        }


        /**
         * close the db 
         * 
         * @throws IOException
         */
        private void Close()
        {
            HeaderSip = null; //let gc do its work
            HeaderPtr = null;
            dbBinStr = null;
            raf?.Close();
        }
    }
}