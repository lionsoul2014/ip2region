using IP2Region.Net.XDB;
using System.Buffers;
using System.Buffers.Binary;
using System.Net;
using Xunit;

namespace IP2Region.Net.Test;

public class XdbTest
{
    [Theory]
    [InlineData("v4")]
    [InlineData("v6")]
    public async Task Version_Ok(string version)
    {
        var db = Path.Combine(AppContext.BaseDirectory, "TestData", $"ip2region_{version}.xdb");
        using var reader = File.OpenRead(db);

        using var owner = MemoryPool<byte>.Shared.Rent(256);
        var length = await reader.ReadAsync(owner.Memory[0..256]);
        Assert.Equal(256, length);

        var ver = BinaryPrimitives.ReadUInt16LittleEndian(owner.Memory[..2].ToArray());
        var indexPolicy = BinaryPrimitives.ReadUInt16LittleEndian(owner.Memory.Slice(2, 2).ToArray());
        var createdAt = BinaryPrimitives.ReadUInt16LittleEndian(owner.Memory.Slice(4, 4).ToArray());
        var startIndexPtr = BinaryPrimitives.ReadUInt16LittleEndian(owner.Memory.Slice(8, 4).ToArray());
        var endIndexPtr = BinaryPrimitives.ReadUInt16LittleEndian(owner.Memory.Slice(12, 4).ToArray());

        // since IPv6 supporting
        var ipVersion = BinaryPrimitives.ReadUInt16LittleEndian(owner.Memory.Slice(16, 2).ToArray());
        var runtimePtrBytes = BinaryPrimitives.ReadUInt16LittleEndian(owner.Memory.Slice(18, 2).ToArray());
    }

    [Fact]
    public void ParseIp()
    {
        //var ip = IPAddress.Parse("2409:895a:8b4:2de4:20e7:5a15:fe6f:431c");
        var ip = IPAddress.Parse("183.160.236.53");
        var ipNum = Util.IpAddressToUInt32(ip);

        var searcher = new Searcher(CachePolicy.Content, Path.Combine(AppContext.BaseDirectory, "TestData", "ip2region_v4.xdb"));
        var result = searcher.Search(ip);
    }
}
