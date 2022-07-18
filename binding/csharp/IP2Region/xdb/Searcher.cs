using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace IP2Region.xdb
{
    public class Searcher
    {

        // constant defined copied from the xdb maker
        public static int HeaderInfoLength = 256;
        public static int VectorIndexRows = 256;
        public static int VectorIndexCols = 256;
        public static int VectorIndexSize = 8;
        public static int SegmentIndexSize = 14;

        // random access file handle for file based search
        private readonly Stream handle;

        private int ioCount = 0;

        public int IOCount => this.ioCount;


        // vector index.
        // use the byte[] instead of VectorIndex entry array to keep
        // the minimal memory allocation.
        private readonly byte[] vectorIndex;

        // xdb content buffer, used for in-memory search
        private readonly byte[] contentBuff;


        // --- static method to create searchers


        public static Searcher NewWithFileOnly(String dbPath)
        {
            return new Searcher(dbPath, null, null);
        }

        public static Searcher NewWithVectorIndex(String dbPath, byte[] vectorIndex)
        {
            return new Searcher(dbPath, vectorIndex, null);
        }

        public static Searcher NewWithBuffer(byte[] cBuff)
        {
            return new Searcher(null, null, cBuff);
        }

        // --- End of creator
        public Searcher(string dbFile, byte[] vectorIndex, byte[] cBuff)
        {
            if (cBuff != null)
            {
                this.handle = null;
                this.vectorIndex = null;
                this.contentBuff = cBuff;
            }
            else
            {
                this.handle = File.OpenRead(dbFile);
                this.vectorIndex = vectorIndex;
                this.contentBuff = null;
            }
        }

        public void Close()
        {
            if (this.handle != null) this.handle.Close();
        }

        public string Search(string ipStr)
        {
            var ip = checkIP(ipStr);
            return Search(ip);
        }

        public string Search(long ip)
        {
            // reset the global counter
            this.ioCount = 0;
            // locate the segment index block based on the vector index
            int sPtr = 0, ePtr = 0;
            int il0 = (int)((ip >> 24) & 0xFF);
            int il1 = (int)((ip >> 16) & 0xFF);
            int idx = il0 * VectorIndexCols * VectorIndexSize + il1 * VectorIndexSize;
            if (vectorIndex != null)
            {
                sPtr = GetInt(vectorIndex, idx);
                ePtr = GetInt(vectorIndex, idx + 4);
            }
            else if (contentBuff != null)
            {
                sPtr = GetInt(contentBuff, HeaderInfoLength + idx);
                ePtr = GetInt(contentBuff, HeaderInfoLength + idx + 4);
            }
            else
            {
                byte[] vectorBuff = new byte[VectorIndexSize];
                Read(HeaderInfoLength + idx, vectorBuff);
                sPtr = GetInt(vectorBuff, 0);
                ePtr = GetInt(vectorBuff, 4);
            }

            // binary search the segment index block to get the region info
            byte[] buff = new byte[SegmentIndexSize];
            int dataLen = -1, dataPtr = -1;
            int l = 0, h = (ePtr - sPtr) / SegmentIndexSize;
            while (l <= h)
            {
                int m = (l + h) >> 1;
                int p = sPtr + m * SegmentIndexSize;

                // read the segment index
                Read(p, buff);
                long sip = GetIntLong(buff, 0);
                if (ip < sip)
                {
                    h = m - 1;
                }
                else
                {
                    long eip = GetIntLong(buff, 4);
                    if (ip > eip)
                    {
                        l = m + 1;
                    }
                    else
                    {
                        dataLen = GetInt2(buff, 8);
                        dataPtr = GetInt(buff, 10);
                        break;
                    }
                }
            }

            // empty match interception
            // System.out.printf("dataLen: %d, dataPtr: %d\n", dataLen, dataPtr);
            if (dataPtr < 0) return null;

            // load and return the region data
            byte[] regionBuff = new byte[dataLen];
            Read(dataPtr, regionBuff);
            //return new String(regionBuff, "utf-8");
            return Encoding.UTF8.GetString(regionBuff);
        }

        protected virtual void Read(int offset, byte[] buffer)
        {
            // check the in-memory buffer first
            if (contentBuff != null)
            {
                // @TODO: reduce data copying, directly decode the data ?
                //System.arraycopy(contentBuff, offset, buffer, 0, buffer.length);
                Array.Copy(contentBuff, offset, buffer, 0, buffer.Length);
                return;
            }

            // read from the file handle
            if (handle == null) throw new ArgumentNullException(nameof(handle));
            handle.Seek(offset, SeekOrigin.Begin);

            this.ioCount++;
            var rLen = handle.Read(buffer, 0, buffer.Length);
            if (rLen != buffer.Length) throw new IOException("incomplete read: read bytes should be " + buffer.Length);
        }

        // --- static cache util function
        public static Header LoadHeader(Stream stream)
        {
            stream.Seek(0, SeekOrigin.Begin);
            var buffer = new byte[HeaderInfoLength];
            stream.Read(buffer, 0, HeaderInfoLength);
            return new Header(buffer);
        }
        public static Header LoadHeaderFromFile(string dbPath)
        {
            using (var fs = File.OpenRead(dbPath)) return LoadHeader(fs);
        }
        public static byte[] LoadVectorIndex(Stream stream)
        {
            stream.Seek(HeaderInfoLength, SeekOrigin.Begin);
            int len = VectorIndexRows * VectorIndexCols * SegmentIndexSize;
            var buff = new byte[len];
            var rLen = stream.Read(buff, 0, buff.Length);
            if (rLen != len) throw new IOException("incomplete read: read bytes should be " + len);
            return buff;
        }
        public static byte[] LoadVectorIndexFromFile(string dbPath)
        {
            using (var fs = File.OpenRead(dbPath)) return LoadVectorIndex(fs);
        }
        public static byte[] LoadContent(Stream stream)
        {
            stream.Seek(0, SeekOrigin.Begin);
            using (var ms = new MemoryStream())
            {
                stream.CopyTo(ms);
                return ms.ToArray();
            }
        }

        public static byte[] LoadContentFromFile(string dbPath)
        {
            using (var fs = File.OpenRead(dbPath)) return LoadContent(fs);
        }

        public static int GetInt2(byte[] b, int offset)
        {
            return (
                (b[offset++] & 0x000000FF) |
                (b[offset] & 0x0000FF00)
            );
        }
        public static int GetInt(byte[] b, int offset)
        {
            return (
                ((b[offset++]) & 0x000000FF) |
                ((b[offset++] << 8) & 0x0000FF00) |
                ((b[offset++] << 16) & 0x00FF0000) |
                (int)((b[offset] << 24) & 0xFF000000)
            );
        }
        public static long GetIntLong(byte[] b, int offset)
        {
            return (
                ((b[offset++] & 0x000000FFL)) |
                ((b[offset++] << 8) & 0x0000FF00L) |
                ((b[offset++] << 16) & 0x00FF0000L) |
                ((b[offset] << 24) & 0xFF000000L)
            );
        }

        /* long int to ip string */
        public static string Long2ip(long ip)
        {
            return string.Join(".", (ip >> 24) & 0xFF, (ip >> 16) & 0xFF, (ip >> 8) & 0xFF, (ip) & 0xFF);
        }

        public static byte[] shiftIndex = { 24, 16, 8, 0 };

        /* check the specified ip address */
        public static long checkIP(String ip)
        {
            String[]
            ps = ip.Split('.');
            if (ps.Length != 4) throw new Exception("invalid ip address `" + ip + "`");

            long ipDst = 0;
            for (int i = 0; i < ps.Length; i++)
            {
                int val = Convert.ToInt32(ps[i]);
                if (val > 255)
                {
                    throw new Exception("ip part `" + ps[i] + "` should be less then 256");
                }
                ipDst |= ((long)val << shiftIndex[i]);
            }

            return ipDst & 0xFFFFFFFFL;
        }
    }
}
