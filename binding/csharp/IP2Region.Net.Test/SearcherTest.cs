using IP2Region.Net.Abstractions;
using IP2Region.Net.XDB;
using Microsoft.Extensions.DependencyInjection;
using System.Net;
using Xunit;

namespace IP2Region.Net.Test;

public class SearcherTest
{
    private readonly string _xdbPathV4 = Path.Combine(AppContext.BaseDirectory, "TestData", "ip2region_v4.xdb");
    private readonly string _xdbPathV6 = Path.Combine(AppContext.BaseDirectory, "TestData", "ip2region_v6.xdb");

    [Theory]
    [InlineData("58.251.27.201", "中国|广东省|深圳市|联通", "v4")]
    [InlineData("114.114.114.114", "中国|江苏省|南京市|0", "v4")]
    [InlineData("119.29.29.29", "中国|北京|北京市|腾讯", "v4")]
    [InlineData("223.5.5.5", "中国|浙江省|杭州市|阿里云", "v4")]
    [InlineData("180.76.76.76", "中国|北京|北京市|百度", "v4")]
    [InlineData("8.8.8.8", "美国|0|0|Level3", "v4")]
    [InlineData("240e:3b7:3272:d8d0:db09:c067:8d59:539e", "中国|广东省|深圳市|家庭宽带", "v6")]
    public void TestSearchCacheContent(string ip, string expected, string version)
    {
        var _xdbPath = version == "v4" ? _xdbPathV4 : _xdbPathV6;
        var contentSearcher = new Searcher(CachePolicy.Content, _xdbPath);
        var region = contentSearcher.Search(ip);
        Assert.Equal(expected, region);
    }

    [Theory]
    [InlineData("58.251.27.201", "中国|广东省|深圳市|联通", "v4")]
    [InlineData("114.114.114.114", "中国|江苏省|南京市|0", "v4")]
    [InlineData("119.29.29.29", "中国|北京|北京市|腾讯", "v4")]
    [InlineData("223.5.5.5", "中国|浙江省|杭州市|阿里云", "v4")]
    [InlineData("180.76.76.76", "中国|北京|北京市|百度", "v4")]
    [InlineData("8.8.8.8", "美国|0|0|Level3", "v4")]
    [InlineData("240e:3b7:3272:d8d0:db09:c067:8d59:539e", "中国|广东省|深圳市|家庭宽带", "v6")]
    public void TestSearchCacheVector(string ip, string expected, string version)
    {
        var _xdbPath = version == "v4" ? _xdbPathV4 : _xdbPathV6;
        var vectorSearcher = new Searcher(CachePolicy.VectorIndex, _xdbPath);
        var region = vectorSearcher.Search(ip);
        Assert.Equal(expected, region);
    }

    [Theory]
    [InlineData("58.251.0.0", "中国|广东省|深圳市|联通", "v4")]
    [InlineData("58.251.255.255", "中国|广东省|深圳市|联通", "v4")]
    [InlineData("58.251.27.201", "中国|广东省|深圳市|联通", "v4")]
    [InlineData("114.114.114.114", "中国|江苏省|南京市|0", "v4")]
    [InlineData("119.29.29.29", "中国|北京|北京市|腾讯", "v4")]
    [InlineData("223.5.5.5", "中国|浙江省|杭州市|阿里云", "v4")]
    [InlineData("180.76.76.76", "中国|北京|北京市|百度", "v4")]
    [InlineData("8.8.8.8", "美国|0|0|Level3", "v4")]
    [InlineData("240e:3b7:3272:d8d0:db09:c067:8d59:539e", "中国|广东省|深圳市|家庭宽带", "v6")]
    [InlineData("240e:044d:2d00:0000:0000:0000:0000:0000", "美国|加利福尼亚州|洛杉矶|移动网络", "v6")]
    public void TestSearchCacheFile(string ip, string expected, string version)
    {
        var _xdbPath = version == "v4" ? _xdbPathV4 : _xdbPathV6;
        var fileSearcher = new Searcher(CachePolicy.File, _xdbPath);
        var region = fileSearcher.Search(ip);
        Assert.Equal(expected, region);
    }

    [Fact]
    public void IoCount_File_Ok()
    {
        var searcher = new Searcher(CachePolicy.File, _xdbPathV4);
        searcher.Search("58.251.27.201");
        Assert.Equal(3, searcher.IoCount);

        searcher.Search("58.251.27.201");
        Assert.Equal(3, searcher.IoCount);
    }

    [Fact]
    public void IoCount_Content_Ok()
    {
        var searcher = new Searcher(CachePolicy.Content, _xdbPathV4);
        searcher.Search("58.251.27.201");
        Assert.Equal(169, searcher.IoCount);

        searcher.Search("58.251.27.201");
        Assert.Equal(0, searcher.IoCount);
    }

    [Fact]
    public void IoCount_Vector_Ok()
    {
        var searcher = new Searcher(CachePolicy.VectorIndex, _xdbPathV4);
        searcher.Search("58.251.27.201");
        Assert.Equal(10, searcher.IoCount);

        searcher.Search("58.251.27.201");
        Assert.Equal(2, searcher.IoCount);
    }

    [Theory]
    [InlineData("58.251.255.255", "中国|广东省|深圳市|联通")]
    public void Search_UintIp_Ok(string ipStr, string expected)
    {
        var fileSearcher = new Searcher(CachePolicy.File, _xdbPathV4);
        var ipAddress = IPAddress.Parse(ipStr);
        var ip = XDB.Util.IpAddressToUInt32(ipAddress);
        var region = fileSearcher.Search(ip);
        Assert.Equal(expected, region);
    }

    [Theory]
    [InlineData("58.251.255.255", "中国|广东省|深圳市|联通")]
    public void AddIP2RegionService_Ok(string ipStr, string expected)
    {
        var services = new ServiceCollection();
        services.AddIP2RegionService(_xdbPathV4, CachePolicy.File);

        var provider = services.BuildServiceProvider();
        var searcher = provider.GetRequiredService<ISearcher>();
        var region = searcher.Search(ipStr);
        Assert.Equal(expected, region);

        searcher = provider.GetRequiredKeyedService<ISearcher>("IP2Region.Net");
        region = searcher.Search(ipStr);
        Assert.Equal(expected, region);
    }

    [Theory]
    [InlineData(CachePolicy.Content, "v4")]
    [InlineData(CachePolicy.VectorIndex, "v4")]
    [InlineData(CachePolicy.File, "v4")]
    [InlineData(CachePolicy.Content, "v6")]
    [InlineData(CachePolicy.VectorIndex, "v6")]
    [InlineData(CachePolicy.File, "v6")]
    public void TestBenchSearch(CachePolicy cachePolicy, string version)
    {
        var _xdbPath = version == "v4" ? _xdbPathV4 : _xdbPathV6;
        var searcher = new Searcher(cachePolicy, _xdbPath);
        var srcPath = Path.Combine(AppContext.BaseDirectory, "TestData", $"ip{version}_source.txt");

        foreach (var line in File.ReadLines(srcPath))
        {
            var ps = line.Trim().Split("|", 3);
            var sip = ps[0];
            var eip = ps[1];

            var s1 = searcher.Search(sip);
            var s2 = searcher.Search(eip);
            Assert.Equal(s1, ps[2]);
            Assert.Equal(s2, ps[2]);
        }
    }
}
