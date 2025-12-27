// Copyright 2025 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.
// @Author Alan <lzh.shap@gmail.com>
// @Date   2023/07/25
// Updated by Argo Zhang <argo@live.ca> at 2025/11/21

using BenchmarkDotNet.Attributes;
using IP2Region.Net.XDB;

namespace IP2Region.Net.BenchMark;

[MemoryDiagnoser]
public class Benchmarks
{
    private static readonly string XdbPathV4 = Path.Combine(AppContext.BaseDirectory, "IP2Region", "ip2region_v4.xdb");
    private static readonly string XdbPathV6 = Path.Combine(AppContext.BaseDirectory, "IP2Region", "ip2region_v6.xdb");
    private static readonly Searcher _contentV4Searcher = new(CachePolicy.Content, XdbPathV4);
    private static readonly Searcher _vectorV4Searcher = new(CachePolicy.VectorIndex, XdbPathV4);
    private static readonly Searcher _fileV4Searcher = new(CachePolicy.File, XdbPathV4);
    private static readonly Searcher _contentV6Searcher = new(CachePolicy.Content, XdbPathV6);
    private static readonly Searcher _vectorV6Searcher = new(CachePolicy.VectorIndex, XdbPathV6);
    private static readonly Searcher _fileV6Searcher = new(CachePolicy.File, XdbPathV6);

    private readonly string _testIPv4Address = "114.114.114.114";
    private readonly string _testIPv6Address = "240e:3b7:3272:d8d0:db09:c067:8d59:539e";

    public Benchmarks()
    {
        _contentV4Searcher.Search(_testIPv4Address);
        _vectorV4Searcher.Search(_testIPv4Address);
        _fileV4Searcher.Search(_testIPv4Address);

        _contentV6Searcher.Search(_testIPv6Address);
        _vectorV6Searcher.Search(_testIPv6Address);
        _fileV6Searcher.Search(_testIPv6Address);
    }

    [Benchmark]
    [BenchmarkCategory("IPv4")]
    public void ContentIPv4() => _contentV4Searcher.Search(_testIPv4Address);

    [Benchmark]
    [BenchmarkCategory("IPv4")]
    public void VectorIPv4() => _vectorV4Searcher.Search(_testIPv4Address);

    [Benchmark]
    [BenchmarkCategory("IPv4")]
    public void FileIPv4() => _fileV4Searcher.Search(_testIPv4Address);

    [Benchmark]
    [BenchmarkCategory("IPv6")]
    public void ContentIPv6() => _contentV6Searcher.Search(_testIPv6Address);

    [Benchmark]
    [BenchmarkCategory("IPv6")]
    public void VectorIPv6() => _vectorV6Searcher.Search(_testIPv6Address);

    [Benchmark]
    [BenchmarkCategory("IPv6")]
    public void FileIPv6() => _fileV6Searcher.Search(_testIPv6Address);
}
