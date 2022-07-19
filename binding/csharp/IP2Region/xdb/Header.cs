using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace IP2Region.xdb
{
    public class Header
    {
        public int Version { get; }
        public int IndexPolicy { get; }
        public int CreatedAt { get; }
        public int StartIndexPtr { get; }
        public int EndIndexPtr { get; }
        public byte[] Buffer { get; }
        public Header(byte[] buff)
        {
            if (buff == null) throw new ArgumentNullException(nameof(buff));
            if (buff.Length < 16) throw new ArgumentOutOfRangeException(nameof(buff));
            Version = Searcher.GetInt2(buff, 0);
            IndexPolicy = Searcher.GetInt2(buff, 2);
            CreatedAt = Searcher.GetInt(buff, 4);
            StartIndexPtr = Searcher.GetInt(buff, 8);
            EndIndexPtr = Searcher.GetInt(buff, 12);
            Buffer = buff;

        }
        public override string ToString()
        {
            return "{" +
            "Version: " + Version + ',' +
            "IndexPolicy: " + IndexPolicy + ',' +
            "CreatedAt: " + CreatedAt + ',' +
            "StartIndexPtr: " + StartIndexPtr + ',' +
            "EndIndexPtr: " + EndIndexPtr +
        '}';
        }
    }
}
