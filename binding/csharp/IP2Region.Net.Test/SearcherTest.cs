using IP2Region.Net.XDB;
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
    [InlineData("8.8.8.8", "", "v4")]
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
    [InlineData("8.8.8.8", "", "v4")]
    [InlineData("240e:3b7:3272:d8d0:db09:c067:8d59:539e", "中国|广东省|深圳市|家庭宽带", "v6")]
    public void TestSearchCacheVector(string ip, string expected, string version)
    {
        var _xdbPath = version == "v4" ? _xdbPathV4 : _xdbPathV6;
        var vectorSearcher = new Searcher(CachePolicy.VectorIndex, _xdbPath);
        var region = vectorSearcher.Search(ip);
        Assert.Equal(expected, region);
    }

    [Theory]
    [InlineData("58.251.27.201", "中国|广东省|深圳市|联通", "v4")]
    [InlineData("114.114.114.114", "中国|江苏省|南京市|0", "v4")]
    [InlineData("119.29.29.29", "中国|北京|北京市|腾讯", "v4")]
    [InlineData("223.5.5.5", "中国|浙江省|杭州市|阿里云", "v4")]
    [InlineData("180.76.76.76", "中国|北京|北京市|百度", "v4")]
    [InlineData("8.8.8.8", "", "v4")]
    [InlineData("240e:3b7:3272:d8d0:db09:c067:8d59:539e", "中国|广东省|深圳市|家庭宽带", "v6")]
    public void TestSearchCacheFile(string ip, string expected, string version)
    {
        var _xdbPath = version == "v4" ? _xdbPathV4 : _xdbPathV6;
        var fileSearcher = new Searcher(CachePolicy.File, _xdbPath);
        var region = fileSearcher.Search(ip);
        Assert.Equal(expected, region);
    }

    [Fact]
    public void Test()
    {
        var ip = XDB.Util.IpAddressToUInt32("58.251.27.201");
        var searcher = new Searcher(CachePolicy.File, _xdbPathV4);
        var region = searcher.Search(ip);
        Assert.Equal("中国|广东省|深圳市|联通", region);
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