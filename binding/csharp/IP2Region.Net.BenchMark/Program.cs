using BenchmarkDotNet.Attributes;
using BenchmarkDotNet.Running;
using IP2Region.Net.XDB;

BenchmarkRunner.Run(typeof(Program).Assembly);

public class CachePolicyCompare
{
    private readonly Searcher _contentSearcher = new Searcher(CachePolicy.Content);
    private readonly Searcher _vectorSearcher = new Searcher(CachePolicy.VectorIndex);

    private readonly string _testIpAddress = "114.114.114.114";

    [Benchmark]
    [BenchmarkCategory(nameof(CachePolicy.Content))]
    public void CachePolicy_Content() => _contentSearcher.Search(_testIpAddress);

    [Benchmark]
    [BenchmarkCategory(nameof(CachePolicy.VectorIndex))]
    public void CachePolicy_VectorIndex() => _vectorSearcher.Search(_testIpAddress);
}