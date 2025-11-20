using Xunit;

namespace IP2Region.Net.Test;

public class XdbTest
{
    [Fact]
    public async Task VersionIPV4_Ok()
    {
        var db = Path.Combine(AppContext.BaseDirectory, "TestData", $"ip2region_v4.xdb");

        var version = await XDB.Util.GetVersionAsync(db);
        Assert.Equal(3, version.Ver);
        Assert.Equal(1, version.CachePolice);
        Assert.Equal("2025-09-06 02:24:16", version.CreatedTime.ToString("yyyy-MM-dd HH:mm:ss"));
        Assert.Equal((uint)955933, version.StartIndex);
        Assert.Equal((uint)11042415, version.EndIndex);
        Assert.Equal(4, version.IPVer);
        Assert.Equal(4, version.BytesCount);
    }

    [Fact]
    public async Task VersionIPV6_Ok()
    {
        var db = Path.Combine(AppContext.BaseDirectory, "TestData", $"ip2region_v6.xdb");
        var version = await XDB.Util.GetVersionAsync(db);
        Assert.Equal(3, version.Ver);
        Assert.Equal(1, version.CachePolice);
        Assert.Equal("2025-10-17 04:41:04", version.CreatedTime.ToString("yyyy-MM-dd HH:mm:ss"));
        Assert.Equal((uint)3094259, version.StartIndex);
        Assert.Equal((uint)36258303, version.EndIndex);
        Assert.Equal(6, version.IPVer);
        Assert.Equal(4, version.BytesCount);
    }

    [Fact]
    public async Task GetVersionAsync_Error()
    {
        await Assert.ThrowsAsync<ArgumentNullException>(async () => await XDB.Util.GetVersionAsync(null));
        await Assert.ThrowsAsync<FileNotFoundException>(async () => await XDB.Util.GetVersionAsync(Path.Combine(AppContext.BaseDirectory, "test.xdb")));
    }
}
