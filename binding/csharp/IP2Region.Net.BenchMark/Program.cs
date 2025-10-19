using BenchmarkDotNet.Attributes;
using BenchmarkDotNet.Running;
using IP2Region.Net.Abstractions;
using IP2Region.Net.XDB;

BenchmarkRunner.Run(typeof(Program).Assembly);

public class CachePolicyCompare
{
    private static readonly string XdbPath = Path.Combine(AppContext.BaseDirectory, "IP2Region", "ip2region_v4.xdb");
    private readonly ISearcher _contentSearcher = new Searcher(CachePolicy.Content, XdbPath);
    private readonly ISearcher _vectorSearcher = new Searcher(CachePolicy.VectorIndex,XdbPath);
    private readonly ISearcher _fileSearcher = new Searcher(CachePolicy.File,XdbPath);

    private readonly string _testIpAddress = "114.114.114.114";

    [Benchmark]
    [BenchmarkCategory(nameof(CachePolicy.Content))]
    public void CachePolicy_Content() => _contentSearcher.Search(_testIpAddress);

    [Benchmark]
    [BenchmarkCategory(nameof(CachePolicy.VectorIndex))]
    public void CachePolicy_VectorIndex() => _vectorSearcher.Search(_testIpAddress);


    [Benchmark]
    [BenchmarkCategory(nameof(CachePolicy.File))]
    public void CachePolicy_File() => _fileSearcher.Search(_testIpAddress);
}