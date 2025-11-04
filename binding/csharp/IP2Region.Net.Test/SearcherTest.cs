using IP2Region.Net.XDB;
using Xunit;

namespace IP2Region.Net.Test;

public class SearcherTest
{
    private readonly string _xdbPath = Path.Combine(AppContext.BaseDirectory, "TestData", "ip2region_v4.xdb");

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