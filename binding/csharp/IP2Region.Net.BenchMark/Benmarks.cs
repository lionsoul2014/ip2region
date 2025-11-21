// Copyright 2025 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.
// @Author Alan <lzh.shap@gmail.com>
// @Date   2023/07/25
// Updated by Argo Zhang <argo@live.ca> at 2025/11/21

using BenchmarkDotNet.Attributes;
using IP2Region.Net.XDB;

namespace IP2Region.Net.BenchMark;

public class Benchmarks
{
    private static readonly string XdbPathV4 = Path.Combine(AppContext.BaseDirectory, "IP2Region", "ip2region_v4.xdb");
    private static readonly string XdbPathV6 = Path.Combine(AppContext.BaseDirectory, "IP2Region", "ip2region_v6.xdb");
    private static readonly Searcher _contentV4Searcher = new Searcher(CachePolicy.Content, XdbPathV4);
    private static readonly Searcher _vectorV4Searcher = new Searcher(CachePolicy.VectorIndex, XdbPathV4);
    private static readonly Searcher _fileV4Searcher = new Searcher(CachePolicy.File, XdbPathV4);
    private static readonly Searcher _contentV6Searcher = new Searcher(CachePolicy.Content, XdbPathV6);
    private static readonly Searcher _vectorV6Searcher = new Searcher(CachePolicy.VectorIndex, XdbPathV6);
    private static readonly Searcher _fileV6Searcher = new Searcher(CachePolicy.File, XdbPathV6);

    private readonly string _testIpV4Address = "114.114.114.114";
    private readonly string _testIpV6Address = "240e:3b7:3272:d8d0:db09:c067:8d59:539e";

    [Benchmark]
    [BenchmarkCategory(nameof(CachePolicy.Content))]
    public void ContentIpV4() => _contentV4Searcher.Search(_testIpV4Address);

    //[Benchmark]
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
