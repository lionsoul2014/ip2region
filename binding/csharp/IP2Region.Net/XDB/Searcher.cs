// Copyright 2023 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.
// @Author Alan Lee <lzh.shap@gmail.com>
// @Date   2023/07/23

using IP2Region.Net.Abstractions;
using IP2Region.Net.Internal;
using IP2Region.Net.Internal.Abstractions;
using System.Buffers.Binary;
using System.Net;
using System.Runtime.InteropServices;
using System.Text;

namespace IP2Region.Net.XDB;

public class Searcher : ISearcher
{
    private readonly AbstractCacheStrategy _cacheStrategy;
    public int IoCount => _cacheStrategy.IoCount;

    public Searcher(CachePolicy cachePolicy, string dbPath)
    {
        var factory = new CacheStrategyFactory(dbPath);
        _cacheStrategy = factory.CreateCacheStrategy(cachePolicy);
    }

    public string? Search(string ipStr)
    {
        var ip = Util.IpAddressToUInt32(ipStr);
        return Search(ip);
    }

    public string? Search(IPAddress ipAddress)
    {
        var ip = Util.IpAddressToUInt32(ipAddress);
        return Search(ip);
    }

    public string? Search(uint ip)
    {
        var version = new Version() { Length = 4, IndexSize = 14 };
        var index = _cacheStrategy.GetVectorIndex(ip);
        uint sPtr = MemoryMarshal.Read<uint>(index.Span);
        uint ePtr = MemoryMarshal.Read<uint>(index.Span.Slice(4));

        uint dataLen = 0;
        uint dataPtr = 0;
        uint l = 0;
        uint h = (ePtr - sPtr) / (uint)version.IndexSize;

        while (l <= h)
        {
            var mid = Util.GetMidIp(l, h);
            var pos = sPtr + mid * version.IndexSize;
            var buffer = _cacheStrategy.GetData((int)pos, version.IndexSize);

            if (ip < version.GetVectorIndexStartPos(buffer))
            {
                h = mid - 1;
            }
            else if (ip > version.GetVectorIndexEndPos(buffer))
            {
                l = mid + 1;
            }
            else
            {
                dataLen = BinaryPrimitives.ReadUInt16LittleEndian(buffer.Span.Slice(8));
                dataPtr = BinaryPrimitives.ReadUInt32LittleEndian(buffer.Span.Slice(10));
                break;
            }
        }

        if (dataLen == 0)
        {
            return default;
        }

        var regionBuff = _cacheStrategy.GetData((int)dataPtr, (int)dataLen);
        return Encoding.UTF8.GetString(regionBuff.Span.ToArray());
    }
}