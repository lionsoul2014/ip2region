using IP2Region.Net.XDB;
using System.Buffers;
using System.Buffers.Binary;
using System.Net;
using System.Text;
using Xunit;

namespace IP2Region.Net.Test;

public class SearcherTest
{
    private readonly string _xdbPath = Path.Combine(AppContext.BaseDirectory, "TestData", "ip2region_v6.xdb");

    [Fact]
    public void Test()
    {
        // 常量定义
        // 每个 vector 索引项的字节数
        var VectorIndexSize = 8;

        // vector 索引的列数
        var VectorIndexCols = 256;

        // vector 索引段整个的字节数
        var VectorIndexLength = 512;

        //var ipAddress = IPAddress.Parse("240e:3b7:3272:d8d0:db09:c067:8d59:539e");
        var ipAddress = IPAddress.Parse("2c0f:fda8:21::");
        // [36,14,3,183,50,114,216,208,219,9,192,103,141,89,83,158]
        //var ipAddress = IPAddress.Parse("58.251.27.201");
        byte[] ip_bytes = ipAddress.GetAddressBytes();
        //Array.Reverse(bytes);

        //var ip = MemoryMarshal.Read<uint>(bytes);

        var il0 = ip_bytes[0] & 0xFF;
        var il1 = ip_bytes[1] & 0xFF;

        var idx = il0 * VectorIndexCols * VectorIndexSize + il1 * VectorIndexSize;

        var data = Read(256 + idx, VectorIndexSize);
        var sPtr = BinaryPrimitives.ReadUInt32LittleEndian(data.Span);
        var ePtr = BinaryPrimitives.ReadUInt32LittleEndian(data.Span.Slice(4));

        var indexSize = 38;
        var l = 0;
        var h = (ePtr - sPtr) / indexSize;
        var dataLen = 0;
        var dataPtr = 0l;

        var bytes = ip_bytes.Length;
        var dBytes = ip_bytes.Length << 1;
        var buff = new byte[indexSize];

        while (l < h)
        {
            // 得到中间的索引项
            int m = (int)(l + h) >> 1;

            var p = (int)sPtr + m * indexSize;

            buff = Read(p, buff.Length).ToArray();

            var s1 = BinaryPrimitives.ReadUInt32BigEndian(ip_bytes);
            var s = BinaryPrimitives.ReadUInt32BigEndian(buff.AsSpan().Slice(0, ip_bytes.Length).ToArray());
            var e = BinaryPrimitives.ReadUInt32BigEndian(buff.AsSpan().Slice(ip_bytes.Length).ToArray());
            if (s1 < s)
            {
                h = m - 1;
            }
            else if (s1 > e)
            {
                l = m + 1;
            }
            else
            {
                dataLen = BinaryPrimitives.ReadUInt16LittleEndian(buff.AsSpan().Slice(dBytes, 2));
                dataPtr = BinaryPrimitives.ReadUInt32LittleEndian(buff.AsSpan().Slice(dBytes + 2));
                break;
            }
        }

        var regionBuff = Read((int)dataPtr, (int)dataLen);
        var address = Encoding.UTF8.GetString(regionBuff.Span.ToArray());
    }

    private ReadOnlyMemory<byte> Read(int offset, int length)
    {
        int BufferSize = 4096;

        var stream = new FileStream(_xdbPath, FileMode.Open, FileAccess.Read, FileShare.Read, BufferSize, useAsync: true);

        byte[] buffer = ArrayPool<byte>.Shared.Rent(length);
        int totalBytesRead = 0;
        try
        {
            stream.Seek(offset, SeekOrigin.Begin);

            int bytesRead;
            do
            {
                int bytesToRead = Math.Min(BufferSize, length - totalBytesRead);
                bytesRead = stream.Read(buffer, totalBytesRead, bytesToRead);
                totalBytesRead += bytesRead;

            } while (bytesRead > 0 && totalBytesRead < length);
        }
        finally
        {
            ArrayPool<byte>.Shared.Return(buffer);
        }

        return new ReadOnlyMemory<byte>(buffer, 0, totalBytesRead);
    }

    [Theory]
    [InlineData("114.114.114.114")]
    [InlineData("119.29.29.29")]
    [InlineData("223.5.5.5")]
    [InlineData("180.76.76.76")]
    [InlineData("8.8.8.8")]
    public void TestSearchCacheContent(string ip)
    {
        var contentSearcher = new Searcher(CachePolicy.Content, _xdbPath);
        var region = contentSearcher.Search(ip);
        Console.WriteLine(region);
    }

    [Theory]
    [InlineData("114.114.114.114")]
    [InlineData("119.29.29.29")]
    [InlineData("223.5.5.5")]
    [InlineData("180.76.76.76")]
    [InlineData("8.8.8.8")]
    public void TestSearchCacheVector(string ip)
    {
        var vectorSearcher = new Searcher(CachePolicy.VectorIndex, _xdbPath);
        var region = vectorSearcher.Search(ip);
        Console.WriteLine(region);
    }

    [Theory]
    [InlineData("114.114.114.114")]
    [InlineData("119.29.29.29")]
    [InlineData("223.5.5.5")]
    [InlineData("180.76.76.76")]
    [InlineData("8.8.8.8")]
    public void TestSearchCacheFile(string ip)
    {
        var fileSearcher = new Searcher(CachePolicy.File, _xdbPath);
        var region = fileSearcher.Search(ip);
        Console.WriteLine(region);
    }

    [Theory]
    [InlineData(CachePolicy.Content)]
    [InlineData(CachePolicy.VectorIndex)]
    [InlineData(CachePolicy.File)]
    public void TestBenchSearch(CachePolicy cachePolicy)
    {
        Searcher searcher = new Searcher(cachePolicy, _xdbPath);
        var srcPath = Path.Combine(AppContext.BaseDirectory, "TestData", "ipv4_source.txt");

        foreach (var line in File.ReadLines(srcPath))
        {
            var ps = line.Trim().Split("|", 3);

            if (ps.Length != 3)
            {
                throw new ArgumentException($"invalid ip segment line {line}", nameof(line));
            }

            var sip = Util.IpAddressToUInt32(ps[0]);
            var eip = Util.IpAddressToUInt32(ps[1]);
            var mip = Util.GetMidIp(sip, eip);

            uint[] temp = { sip, Util.GetMidIp(sip, mip), mip, Util.GetMidIp(mip, eip), eip };

            foreach (var ip in temp)
            {
                //var region = searcher.Search(ip);

                //if (region != ps[2])
                //{
                //    throw new Exception($"failed search {ip} with ({region}!={ps[2]})");
                //}
            }
        }
    }
}