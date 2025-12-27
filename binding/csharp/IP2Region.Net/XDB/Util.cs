// Copyright 2025 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.
// @Author Alan <lzh.shap@gmail.com>
// @Date   2023/07/25
// Updated by Argo Zhang <argo@live.ca> at 2025/11/21

using System.Buffers;
using System.Buffers.Binary;
using System.Net;
using System.Runtime.InteropServices;

namespace IP2Region.Net.XDB;

/// <summary>
/// 工具类
/// </summary>
public static class Util
{
    public static uint IpAddressToUInt32(string ipAddress)
    {
        var address = IPAddress.Parse(ipAddress);
        return IpAddressToUInt32(address);
    }

    public static uint IpAddressToUInt32(IPAddress ipAddress)
    {
        byte[] bytes = ipAddress.GetAddressBytes();
        Array.Reverse(bytes);
        return MemoryMarshal.Read<uint>(bytes);
    }

    public static uint GetMidIp(uint x, uint y)
        => (x & y) + ((x ^ y) >> 1);

    public static async Task<XdbVersion> GetVersionAsync(string dbPath, CancellationToken token = default)
    {
        if (string.IsNullOrEmpty(dbPath))
        {
            throw new ArgumentNullException(nameof(dbPath));
        }

        if (!File.Exists(dbPath))
        {
            throw new FileNotFoundException("xdb file not found.", dbPath);
        }

        using var reader = File.OpenRead(dbPath);
        return await GetVersionAsync(reader, token);
    }

    internal static async Task<XdbVersion> GetVersionAsync(FileStream reader, CancellationToken token = default)
    {
        XdbVersion ret = default;
        var buffer = ArrayPool<byte>.Shared.Rent(256);

        try
        {
            var length = await reader.ReadAsync(buffer, 0, 256, token);
            if (length == 256)
            {
                ret = Parse(buffer);
            }
        }
        finally
        {
            ArrayPool<byte>.Shared.Return(buffer);
        }

        return ret;
    }

    private static XdbVersion Parse(ReadOnlySpan<byte> buffer)
    {
        var ret = new XdbVersion
        {
            Ver = BinaryPrimitives.ReadUInt16LittleEndian(buffer.Slice(0, 2)),
            CachePolice = BinaryPrimitives.ReadUInt16LittleEndian(buffer.Slice(2, 2)),
            StartIndex = BinaryPrimitives.ReadUInt32LittleEndian(buffer.Slice(8, 4)),
            EndIndex = BinaryPrimitives.ReadUInt32LittleEndian(buffer.Slice(12, 4)),
            IPVer = BinaryPrimitives.ReadUInt16LittleEndian(buffer.Slice(16, 2)),
            BytesCount = BinaryPrimitives.ReadUInt16LittleEndian(buffer.Slice(18, 2))
        };

        var createdAt = BinaryPrimitives.ReadUInt32LittleEndian(buffer.Slice(4, 4));
        var dtm = new DateTimeOffset(1970, 1, 1, 0, 0, 0, TimeSpan.FromHours(0));
        ret.CreatedTime = dtm.AddSeconds(createdAt);

        return ret;
    }
}
