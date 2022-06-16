//*******************************
// Created By Rocher Kong 
// Github https://github.com/RocherKong
// Date 2018.02.09
//*******************************

using IP2Region.Models;
using System;
using System.IO;
using System.Text;
using System.Threading.Tasks;

namespace IP2Region
{
    public class DbSearcher : IDisposable
    {
        const int BTREE_ALGORITHM = 1;
        const int BINARY_ALGORITHM = 2;
        const int MEMORY_ALGORITYM = 3;

        private DbConfig _dbConfig = null;

        /// <summary>
        /// db file access handler
        /// </summary>
        private Stream _raf = null;

        /// <summary>
        /// header blocks buffer
        /// </summary>
        private long[] _headerSip = null;
        private int[] _headerPtr = null;
        private int _headerLength;

        /// <summary>
        /// super blocks info 
        /// </summary>
        private long _firstIndexPtr = 0;
        private long _lastIndexPtr = 0;
        private int _totalIndexBlocks = 0;

        /// <summary>
        /// for memory mode
        /// the original db binary string
        /// </summary>
        private byte[] _dbBinStr = null;

        /// <summary>
        /// Get by index ptr.
        /// </summary>
        private DataBlock GetByIndexPtr(long ptr)
        {
            _raf.Seek(ptr, SeekOrigin.Begin);
            byte[] buffer = new byte[12];
            _raf.Read(buffer, 0, buffer.Length);
            long extra = Utils.GetIntLong(buffer, 8);
            int dataLen = (int)((extra >> 24) & 0xFF);
            int dataPtr = (int)((extra & 0x00FFFFFF));
            _raf.Seek(dataPtr, SeekOrigin.Begin);
            byte[] data = new byte[dataLen];
            _raf.Read(data, 0, data.Length);
            int city_id = (int)Utils.GetIntLong(data, 0);
            string region = Encoding.UTF8.GetString(data, 4, data.Length - 4);
            return new DataBlock(city_id, region, dataPtr);
        }

        public DbSearcher(DbConfig dbConfig, string dbFile)
        {
            if (_dbConfig == null)
            {
                _dbConfig = dbConfig;
            }
            _raf = new FileStream(dbFile, FileMode.Open, FileAccess.Read, FileShare.Read);
        }

        public DbSearcher(string dbFile) : this(null, dbFile) { }

        public DbSearcher(DbConfig dbConfig, Stream dbFileStream)
        {
            if (_dbConfig == null)
            {
                _dbConfig = dbConfig;
            }

            _raf = dbFileStream;
        }

        public DbSearcher(Stream dbFileStream) : this(null, dbFileStream) { }

        #region Sync Methods
        /// <summary>
        /// Get the region with a int ip address with memory binary search algorithm.
        /// </summary>
        private DataBlock MemorySearch(long ip)
        {
            int blen = IndexBlock.LENGTH;
            if (_dbBinStr == null)
            {
                _dbBinStr = new byte[(int)_raf.Length];
                _raf.Seek(0L, SeekOrigin.Begin);
                _raf.Read(_dbBinStr, 0, _dbBinStr.Length);

                //initialize the global vars
                _firstIndexPtr = Utils.GetIntLong(_dbBinStr, 0);
                _lastIndexPtr = Utils.GetIntLong(_dbBinStr, 4);
                _totalIndexBlocks = (int)((_lastIndexPtr - _firstIndexPtr) / blen) + 1;
            }

            //search the index blocks to define the data
            int l = 0, h = _totalIndexBlocks;
            long sip = 0;

            while (l <= h)
            {
                int m = (l + h) >> 1;
                int p = (int)(_firstIndexPtr + m * blen);

                sip = Utils.GetIntLong(_dbBinStr, p);

                if (ip < sip)
                {
                    h = m - 1;
                }
                else
                {
                    sip = Utils.GetIntLong(_dbBinStr, p + 4);
                    if (ip > sip)
                    {
                        l = m + 1;
                    }
                    else
                    {
                        sip = Utils.GetIntLong(_dbBinStr, p + 8);
                        break;
                    }
                }
            }

            //not matched
            if (sip == 0) return null;

            //get the data
            int dataLen = (int)((sip >> 24) & 0xFF);
            int dataPtr = (int)((sip & 0x00FFFFFF));
            int city_id = (int)Utils.GetIntLong(_dbBinStr, dataPtr);
            string region = Encoding.UTF8.GetString(_dbBinStr, dataPtr + 4, dataLen - 4);//new String(dbBinStr, dataPtr + 4, dataLen - 4, Encoding.UTF8);

            return new DataBlock(city_id, region, dataPtr);
        }

        /// <summary>
        /// Get the region throught the ip address with memory binary search algorithm.
        /// </summary>
        public DataBlock MemorySearch(string ip)
        {
            return MemorySearch(Utils.Ip2long(ip));
        }

        /// <summary>
        /// Get the region with a int ip address with b-tree algorithm.
        /// </summary>
        private DataBlock BtreeSearch(long ip)
        {
            //check and load the header
            if (_headerSip == null)
            {
                _raf.Seek(8L, SeekOrigin.Begin);    //pass the super block
                byte[] b = new byte[4096];
                _raf.Read(b, 0, b.Length);
                //fill the header
                int len = b.Length >> 3, idx = 0;  //b.lenght / 8
                _headerSip = new long[len];
                _headerPtr = new int[len];
                long startIp, dataPtrTemp;
                for (int i = 0; i < b.Length; i += 8)
                {
                    startIp = Utils.GetIntLong(b, i);
                    dataPtrTemp = Utils.GetIntLong(b, i + 4);
                    if (dataPtrTemp == 0) break;

                    _headerSip[idx] = startIp;
                    _headerPtr[idx] = (int)dataPtrTemp;
                    idx++;
                }
                _headerLength = idx;
            }

            //1. define the index block with the binary search
            if (ip == _headerSip[0])
            {
                return GetByIndexPtr(_headerPtr[0]);
            }
            else if (ip == _headerPtr[_headerLength - 1])
            {
                return GetByIndexPtr(_headerPtr[_headerLength - 1]);
            }
            int l = 0, h = _headerLength, sptr = 0, eptr = 0;
            int m = 0;
            while (l <= h)
            {
                m = (l + h) >> 1;
                //perfectly matched, just return it
                if (ip == _headerSip[m])
                {
                    if (m > 0)
                    {
                        sptr = _headerPtr[m - 1];
                        eptr = _headerPtr[m];
                    }
                    else
                    {
                        sptr = _headerPtr[m];
                        eptr = _headerPtr[m + 1];
                    }
                }
                //less then the middle value
                else if (ip < _headerSip[m])
                {
                    if (m == 0)
                    {
                        sptr = _headerPtr[m];
                        eptr = _headerPtr[m + 1];
                        break;
                    }
                    else if (ip > _headerSip[m - 1])
                    {
                        sptr = _headerPtr[m - 1];
                        eptr = _headerPtr[m];
                        break;
                    }
                    h = m - 1;
                }
                else
                {
                    if (m == _headerLength - 1)
                    {
                        sptr = _headerPtr[m - 1];
                        eptr = _headerPtr[m];
                        break;
                    }
                    else if (ip <= _headerSip[m + 1])
                    {
                        sptr = _headerPtr[m];
                        eptr = _headerPtr[m + 1];
                        break;
                    }
                    l = m + 1;
                }
            }
            //match nothing just stop it
            if (sptr == 0) return null;
            //2. search the index blocks to define the data
            int blockLen = eptr - sptr, blen = IndexBlock.LENGTH;
            byte[] iBuffer = new byte[blockLen + blen];    //include the right border block
            _raf.Seek(sptr, SeekOrigin.Begin);
            _raf.Read(iBuffer, 0, iBuffer.Length);
            l = 0; h = blockLen / blen;
            long sip = 0;
            int p = 0;
            while (l <= h)
            {
                m = (l + h) >> 1;
                p = m * blen;
                sip = Utils.GetIntLong(iBuffer, p);
                if (ip < sip)
                {
                    h = m - 1;
                }
                else
                {
                    sip = Utils.GetIntLong(iBuffer, p + 4);
                    if (ip > sip)
                    {
                        l = m + 1;
                    }
                    else
                    {
                        sip = Utils.GetIntLong(iBuffer, p + 8);
                        break;
                    }
                }
            }
            //not matched
            if (sip == 0) return null;
            //3. get the data
            int dataLen = (int)((sip >> 24) & 0xFF);
            int dataPtr = (int)((sip & 0x00FFFFFF));
            _raf.Seek(dataPtr, SeekOrigin.Begin);
            byte[] data = new byte[dataLen];
            _raf.Read(data, 0, data.Length);
            int city_id = (int)Utils.GetIntLong(data, 0);
            String region = Encoding.UTF8.GetString(data, 4, data.Length - 4);// new String(data, 4, data.Length - 4, "UTF-8");
            return new DataBlock(city_id, region, dataPtr);
        }

        /// <summary>
        /// Get the region throught the ip address with b-tree search algorithm.
        /// </summary>
        public DataBlock BtreeSearch(string ip)
        {
            return BtreeSearch(Utils.Ip2long(ip));
        }

        /// <summary>
        /// Get the region with a int ip address with binary search algorithm.
        /// </summary>
        private DataBlock BinarySearch(long ip)
        {
            int blen = IndexBlock.LENGTH;
            if (_totalIndexBlocks == 0)
            {
                _raf.Seek(0L, SeekOrigin.Begin);
                byte[] superBytes = new byte[8];
                _raf.Read(superBytes, 0, superBytes.Length);
                //initialize the global vars
                _firstIndexPtr = Utils.GetIntLong(superBytes, 0);
                _lastIndexPtr = Utils.GetIntLong(superBytes, 4);
                _totalIndexBlocks = (int)((_lastIndexPtr - _firstIndexPtr) / blen) + 1;
            }

            //search the index blocks to define the data
            int l = 0, h = _totalIndexBlocks;
            byte[] buffer = new byte[blen];
            long sip = 0;
            while (l <= h)
            {
                int m = (l + h) >> 1;
                _raf.Seek(_firstIndexPtr + m * blen, SeekOrigin.Begin);    //set the file pointer
                _raf.Read(buffer, 0, buffer.Length);
                sip = Utils.GetIntLong(buffer, 0);
                if (ip < sip)
                {
                    h = m - 1;
                }
                else
                {
                    sip = Utils.GetIntLong(buffer, 4);
                    if (ip > sip)
                    {
                        l = m + 1;
                    }
                    else
                    {
                        sip = Utils.GetIntLong(buffer, 8);
                        break;
                    }
                }
            }
            //not matched
            if (sip == 0) return null;
            //get the data
            int dataLen = (int)((sip >> 24) & 0xFF);
            int dataPtr = (int)((sip & 0x00FFFFFF));
            _raf.Seek(dataPtr, SeekOrigin.Begin);
            byte[] data = new byte[dataLen];
            _raf.Read(data, 0, data.Length);
            int city_id = (int)Utils.GetIntLong(data, 0);
            String region = Encoding.UTF8.GetString(data, 4, data.Length - 4);//new String(data, 4, data.Length - 4, "UTF-8");
            return new DataBlock(city_id, region, dataPtr);
        }

        /// <summary>
        /// Get the region throught the ip address with binary search algorithm.
        /// </summary>
        public DataBlock BinarySearch(String ip)
        {
            return BinarySearch(Utils.Ip2long(ip));
        }
        #endregion

        #region Async Methods
        /// <summary>
        /// Get the region throught the ip address with memory binary search algorithm.
        /// </summary>
        public Task<DataBlock> MemorySearchAsync(string ip)
        {
            return Task.FromResult(MemorySearch(ip));
        }
        /// <summary>
        /// Get the region throught the ip address with b-tree search algorithm.
        /// </summary>
        public Task<DataBlock> BtreeSearchAsync(string ip)
        {
            return Task.FromResult(BtreeSearch(ip));
        }
        /// <summary>
        /// Get the region throught the ip address with binary search algorithm.
        /// </summary>
        public Task<DataBlock> BinarySearchAsync(string ip)
        {
            return Task.FromResult(BinarySearch(ip));
        }
        #endregion

        /// <summary>
        /// Close the db.
        /// </summary>
        public void Close()
        {
            _headerSip = null;
            _headerPtr = null;
            _dbBinStr = null;
            _raf.Close();
        }

        public void Dispose()
        {
            Close();
        }
    }

}
