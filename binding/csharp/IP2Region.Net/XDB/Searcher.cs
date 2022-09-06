// Copyright 2022 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.
// @Author Alan Lee <lzh.shap@gmail.com>
// @Date   2022/09/06

using System.Net;
using System.Text;

namespace IP2Region.Net.XDB;

public class Searcher : ISearcher
{
    const int HeaderInfoLength = 256;
    const int VectorIndexRows = 256;
    const int VectorIndexCols = 256;
    const int VectorIndexSize = 8;
    const int SegmentIndexSize = 14;

    private readonly byte[]? _vectorIndex;
    private readonly byte[]? _contentBuff;
    private readonly FileStream _contentStream;
    private readonly CachePolicy _cachePolicy;
    public int IoCount { get; private set; }

    public Searcher(CachePolicy cachePolicy = CachePolicy.Content, string? dbPath = null)
    {
        if (string.IsNullOrEmpty(dbPath))
        {
            dbPath = Path.Combine(AppContext.BaseDirectory, "Data", "ip2region.xdb");
        }

        _contentStream = File.OpenRead(dbPath);
        _cachePolicy = cachePolicy;

        switch (_cachePolicy)
        {
            case CachePolicy.Content:
                using (var stream = new MemoryStream())
                {
                    _contentStream.CopyTo(stream);
                    _contentBuff = stream.ToArray();
                }

                break;
            case CachePolicy.VectorIndex:
                var vectorLength = VectorIndexRows * VectorIndexCols * VectorIndexSize;
                _vectorIndex = new byte[vectorLength];
                Read(HeaderInfoLength, _vectorIndex);
                break;
        }
    }

    ~Searcher()
    {
        _contentStream.Close();
        _contentStream.Dispose();
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
        var il0 = ip >> 24 & 0xFF;
        var il1 = ip >> 16 & 0xFF;
        var idx = il0 * VectorIndexCols * VectorIndexSize + il1 * VectorIndexSize;

        uint sPtr = 0, ePtr = 0;

        switch (_cachePolicy)
        {
            case CachePolicy.VectorIndex:
                sPtr = BitConverter.ToUInt32(_vectorIndex.AsSpan()[(int)idx..]);
                ePtr = BitConverter.ToUInt32(_vectorIndex.AsSpan()[((int)idx + 4)..]);
                break;
            case CachePolicy.Content:
                sPtr = BitConverter.ToUInt32(_contentBuff.AsSpan()[(HeaderInfoLength + (int)idx)..]);
                ePtr = BitConverter.ToUInt32(_contentBuff.AsSpan()[(HeaderInfoLength + (int)idx + 4)..]);
                break;
            case CachePolicy.File:
                var buff = new byte[VectorIndexSize];
                Read((int)(idx + HeaderInfoLength), buff);
                sPtr = BitConverter.ToUInt32(buff);
                ePtr = BitConverter.ToUInt32(buff.AsSpan()[4..]);
                break;
        }


        var dataLen = 0;
        uint dataPtr = 0;
        var l = 0;
        var h = (int)((ePtr - sPtr) / SegmentIndexSize);
        var buffer = new byte[SegmentIndexSize];

        while (l <= h)
        {
            var mid = Util.GetMidIp(l, h);
            var pos = sPtr + mid * SegmentIndexSize;

            Read((int)pos, buffer);
            var sip = BitConverter.ToUInt32(buffer);

            if (ip < sip)
            {
                h = mid - 1;
            }
            else
            {
                var eip = BitConverter.ToUInt32(buffer.AsSpan()[4..]);
                if (ip > eip)
                {
                    l = mid + 1;
                }
                else
                {
                    dataLen = BitConverter.ToUInt16(buffer.AsSpan()[8..]);
                    dataPtr = BitConverter.ToUInt32(buffer.AsSpan()[10..]);
                    break;
                }
            }
        }

        if (dataLen == 0)
        {
            return default;
        }

        var regionBuff = new byte[dataLen];
        Read((int)dataPtr, regionBuff);
        return Encoding.UTF8.GetString(regionBuff);
    }

    private void Read(int offset, byte[] buff)
    {
        switch (_cachePolicy)
        {
            case CachePolicy.Content:
                _contentBuff.AsSpan()[offset..(offset + buff.Length)].CopyTo(buff);
                break;
            default:
                _contentStream.Seek(offset, SeekOrigin.Begin);
                IoCount++;

                var rLen = _contentStream.Read(buff);
                if (rLen != buff.Length)
                {
                    throw new IOException($"incomplete read: readed bytes should be {buff.Length}");
                }

                break;
        }
    }
}