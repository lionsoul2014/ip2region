using BenchmarkDotNet.Attributes;
using BenchmarkDotNet.Running;
using IP2Region.Net.XDB;

BenchmarkRunner.Run(typeof(Program).Assembly);

public class CachePolicyCompare
{
    private static readonly string XdbPathV4 = Path.Combine(AppContext.BaseDirectory, "IP2Region", "ip2region_v4.xdb");
    private static readonly string XdbPathV6 = Path.Combine(AppContext.BaseDirectory, "IP2Region", "ip2region_v6.xdb");
    private readonly Searcher _contentV4Searcher = new Searcher(CachePolicy.Content, XdbPathV4);
    private readonly Searcher _vectorV4Searcher = new Searcher(CachePolicy.VectorIndex, XdbPathV4);
    private readonly Searcher _fileV4Searcher = new Searcher(CachePolicy.File, XdbPathV4);
    private readonly Searcher _contentV6Searcher = new Searcher(CachePolicy.Content, XdbPathV6);
    private readonly Searcher _vectorV6Searcher = new Searcher(CachePolicy.VectorIndex, XdbPathV6);
    private readonly Searcher _fileV6Searcher = new Searcher(CachePolicy.File, XdbPathV6);

    private readonly string _testIpV4Address = "114.114.114.114";
    private readonly string _testIpV6Address = "240e:3b7:3272:d8d0:db09:c067:8d59:539e";

    public CachePolicyCompare()
    {
        _contentV4Searcher.Search(_testIpV4Address);
        _vectorV4Searcher.Search(_testIpV4Address);
        _fileV4Searcher.Search(_testIpV4Address);
        _contentV6Searcher.Search(_testIpV6Address);
        _vectorV6Searcher.Search(_testIpV6Address);
        _fileV6Searcher.Search(_testIpV6Address);
    }

    [Benchmark]
    [BenchmarkCategory(nameof(CachePolicy.Content))]
    public void ContentIpV4() => _contentV4Searcher.Search(_testIpV4Address);

    [Benchmark]
    [BenchmarkCategory(nameof(CachePolicy.VectorIndex))]
    public void VectorIndexIpV4() => _vectorV4Searcher.Search(_testIpV4Address);


    [Benchmark]
    [BenchmarkCategory(nameof(CachePolicy.File))]
    public void FileIpV4() => _fileV4Searcher.Search(_testIpV4Address);

    [Benchmark]
    [BenchmarkCategory(nameof(CachePolicy.Content))]
    public void ContentIpV6() => _contentV6Searcher.Search(_testIpV6Address);

    [Benchmark]
    [BenchmarkCategory(nameof(CachePolicy.VectorIndex))]
    public void VectorIndexIpV6() => _vectorV6Searcher.Search(_testIpV6Address);


    [Benchmark]
    [BenchmarkCategory(nameof(CachePolicy.File))]
    public void FileIpV6() => _fileV6Searcher.Search(_testIpV6Address);
}
