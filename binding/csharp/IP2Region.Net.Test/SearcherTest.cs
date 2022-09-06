using IP2Region.Net.XDB;

namespace IP2Region.Net.Test;

[TestFixture]
public class SearcherTest
{
    public static IEnumerable<string> Ips()
    {
        yield return "114.114.114.114";
        yield return "119.29.29.29";
        yield return "223.5.5.5";
        yield return "180.76.76.76";
        yield return "8.8.8.8";
    }

    [TestCaseSource(nameof(Ips))]
    public void TestSearchCacheContent(string ip)
    {
        var contentSearcher = new Searcher(CachePolicy.Content);
        var region = contentSearcher.Search(ip);
        Console.WriteLine(region);
    }

    [TestCaseSource(nameof(Ips))]
    public void TestSearchCacheVector(string ip)
    {
        var vectorSearcher = new Searcher(CachePolicy.VectorIndex);
        var region = vectorSearcher.Search(ip);
        Console.WriteLine(region);
    }

    [TestCaseSource(nameof(Ips))]
    public void TestSearchCacheFile(string ip)
    {
        var fileSearcher = new Searcher(CachePolicy.File);
        var region = fileSearcher.Search(ip);
        Console.WriteLine(region);
    }

    [TestCase(CachePolicy.Content)]
    [TestCase(CachePolicy.VectorIndex)]
    [TestCase(CachePolicy.File)]
    public void TestBenchSearch(CachePolicy cachePolicy)
    {
        Searcher searcher = new Searcher(cachePolicy);
        var srcPath = Path.Combine(AppContext.BaseDirectory, "TestData", "ip.merge.txt");

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
                var region = searcher.Search(ip);

                if (region != ps[2])
                {
                    throw new Exception($"failed search {ip} with ({region}!={ps[2]})");
                }
            }
        }
    }
}