// Copyright 2025 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.
// @Author Alan <lzh.shap@gmail.com>
// @Date   2023/07/25
// Updated by Argo Zhang <argo@live.ca> at 2025/11/21

using IP2Region.Net.Abstractions;
using IP2Region.Net.Internal;
using IP2Region.Net.Internal.Abstractions;
using System.Buffers.Binary;
using System.Net;
using System.Text;

namespace IP2Region.Net.XDB;

/// <summary>
/// <see cref="ISearcher"/> 实现类
/// </summary>
public class Searcher : ISearcher
{
    private readonly AbstractCacheStrategy _cacheStrategy;

    /// <summary>
    /// <inheritdoc/>
    /// </summary>
    public int IoCount => _cacheStrategy.IoCount;

    /// <summary>
    /// <inheritdoc/>
    /// </summary>
    public Searcher(CachePolicy cachePolicy, string dbPath)
    {
        var factory = new CacheStrategyFactory(dbPath);
        _cacheStrategy = factory.CreateCacheStrategy(cachePolicy);
    }

    /// <summary>
    /// <inheritdoc/>
    /// </summary>
    public string? Search(string ipStr)
    {
        var ipAddress = IPAddress.Parse(ipStr);
        return SearchCore(ipAddress.GetAddressBytes());
    }

    /// <summary>
    /// <inheritdoc/>
    /// </summary>
    public string? Search(IPAddress ipAddress)
    {
        return SearchCore(ipAddress.GetAddressBytes());
    }

    /// <summary>
    /// <inheritdoc/>
    /// </summary>
    public string? Search(uint ipAddress)
    {
        var bytes = BitConverter.GetBytes(ipAddress);
        Array.Reverse(bytes);
        return SearchCore(bytes);
    }

    string? SearchCore(byte[] ipBytes)
    {
        // 重置 IO 计数器
        _cacheStrategy.ResetIoCount();

        // 每个 vector 索引项的字节数
        var vectorIndexSize = 8;

        // vector 索引的列数
        var vectorIndexCols = 256;

        // 计算得到 vector 索引项的开始地址。
        var il0 = ipBytes[0];
        var il1 = ipBytes[1];
        var idx = il0 * vectorIndexCols * vectorIndexSize + il1 * vectorIndexSize;

        var vector = _cacheStrategy.GetVectorIndex(idx);
        var sPtr = BinaryPrimitives.ReadUInt32LittleEndian(vector.Span);
        var ePtr = BinaryPrimitives.ReadUInt32LittleEndian(vector.Span.Slice(4));

        var length = ipBytes.Length;
        var indexSize = length * 2 + 6;
        var l = 0;
        var h = (ePtr - sPtr) / indexSize;
        var dataLen = 0;
        long dataPtr = 0;

        while (l <= h)
        {
            int m = (int)(l + h) >> 1;

            var p = (int)sPtr + m * indexSize;
            var buff = _cacheStrategy.GetData(p, indexSize);

            var s = buff.Span.Slice(0, length);
            var e = buff.Span.Slice(length, length);
            if (ByteCompare(ipBytes, s) < 0)
            {
                h = m - 1;
            }
            else if (ByteCompare(ipBytes, e) > 0)
            {
                l = m + 1;
            }
            else
            {
                dataLen = BinaryPrimitives.ReadUInt16LittleEndian(buff.Span.Slice(length * 2, 2));
                dataPtr = BinaryPrimitives.ReadUInt32LittleEndian(buff.Span.Slice(length * 2 + 2, 4));
                break;
            }
        }

        var regionBuff = _cacheStrategy.GetData((int)dataPtr, dataLen);
        return Encoding.UTF8.GetString(regionBuff.Span.ToArray());
    }

    static int ByteCompare(byte[] ip1, ReadOnlySpan<byte> ip2) => ip1.Length == 4 ? IPv4Compare(ip1, ip2) : IPv6Compare(ip1, ip2);

    static int IPv4Compare(byte[] ip1, ReadOnlySpan<byte> ip2)
    {
        var ret = 0;
        for (int i = 0; i < ip1.Length; i++)
        {
            var ip2Index = ip1.Length - 1 - i;
            if (ip1[i] < ip2[ip2Index])
            {
                return -1;
            }
            else if (ip1[i] > ip2[ip2Index])
            {
                return 1;
            }
        }
        return ret;
    }

    static int IPv6Compare(byte[] ip1, ReadOnlySpan<byte> ip2)
    {
        var ret = 0;
        for (int i = 0; i < ip1.Length; i++)
        {
            if (ip1[i] < ip2[i])
            {
                return -1;
            }
            else if (ip1[i] > ip2[i])
            {
                return 1;
            }
        }
        return ret;
    }
}
