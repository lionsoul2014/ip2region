using System.Buffers;
using System.Buffers.Binary;
using Xunit;

namespace IP2Region.Net.Test;

public class XdbTest
{
    [Fact]
    public async Task VersionIPV4_Ok()
    {
        var db = Path.Combine(AppContext.BaseDirectory, "TestData", $"ip2region_v4.xdb");
        using var reader = File.OpenRead(db);

        using var owner = MemoryPool<byte>.Shared.Rent(256);
        var length = await reader.ReadAsync(owner.Memory[0..256]);
        Assert.Equal(256, length);

        var ver = BinaryPrimitives.ReadUInt16LittleEndian(owner.Memory.Span[..2]);
        Assert.Equal(3, ver);

        var indexPolicy = BinaryPrimitives.ReadUInt16LittleEndian(owner.Memory.Span.Slice(2, 2));
        Assert.Equal(1, indexPolicy);

        var createdAt = BinaryPrimitives.ReadUInt32LittleEndian(owner.Memory.Span.Slice(4, 4));
        var dtm = new DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc);
        dtm = dtm.AddSeconds(createdAt);
        Assert.Equal("2025-09-06 02:24:16", dtm.ToString("yyyy-MM-dd HH:mm:ss"));

        var startIndexPtr = BinaryPrimitives.ReadUInt32LittleEndian(owner.Memory.Span.Slice(8, 4));
        Assert.Equal((uint)955933, startIndexPtr);

        var endIndexPtr = BinaryPrimitives.ReadUInt32LittleEndian(owner.Memory.Span.Slice(12, 4));
        Assert.Equal((uint)11042415, endIndexPtr);

        // since IPv6 supporting
        var ipVersion = BinaryPrimitives.ReadUInt16LittleEndian(owner.Memory.Span.Slice(16, 2));
        Assert.Equal(4, ipVersion);

        var runtimePtrBytes = BinaryPrimitives.ReadUInt16LittleEndian(owner.Memory.Span.Slice(18, 2));
        Assert.Equal(4, runtimePtrBytes);
    }

    [Fact]
    public async Task VersionIPV6_Ok()
    {
        var db = Path.Combine(AppContext.BaseDirectory, "TestData", $"ip2region_v6.xdb");
        using var reader = File.OpenRead(db);

        using var owner = MemoryPool<byte>.Shared.Rent(256);
        var length = await reader.ReadAsync(owner.Memory[0..256]);
        Assert.Equal(256, length);

        var ver = BinaryPrimitives.ReadUInt16LittleEndian(owner.Memory.Span[..2]);
        Assert.Equal(3, ver);

        var indexPolicy = BinaryPrimitives.ReadUInt16LittleEndian(owner.Memory.Span.Slice(2, 2));
        var createdAt = BinaryPrimitives.ReadUInt32LittleEndian(owner.Memory.Span.Slice(4, 4));
        var dtm = new DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc);
        dtm = dtm.AddSeconds(createdAt);
        Assert.Equal("2025-10-17 04:41:04", dtm.ToString("yyyy-MM-dd HH:mm:ss"));

        var startIndexPtr = BinaryPrimitives.ReadUInt32LittleEndian(owner.Memory.Span.Slice(8, 4));
        Assert.Equal((uint)3094259, startIndexPtr);

        var endIndexPtr = BinaryPrimitives.ReadUInt32LittleEndian(owner.Memory.Span.Slice(12, 4));
        Assert.Equal((uint)36258303, endIndexPtr);

        // since IPv6 supporting
        var ipVersion = BinaryPrimitives.ReadUInt16LittleEndian(owner.Memory.Span.Slice(16, 2));
        Assert.Equal(6, ipVersion);

        var runtimePtrBytes = BinaryPrimitives.ReadUInt16LittleEndian(owner.Memory.Span.Slice(18, 2));
        Assert.Equal(4, runtimePtrBytes);
    }
}
