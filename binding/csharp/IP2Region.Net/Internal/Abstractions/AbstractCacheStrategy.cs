// Copyright 2023 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.
// @Author Alan <lzh.shap@gmail.com>
// @Date   2023/07/25

using System.Buffers;

namespace IP2Region.Net.Internal.Abstractions;

internal abstract class AbstractCacheStrategy
{
    protected const int HeaderInfoLength = 256;
    protected const int VectorIndexRows = 256;
    protected const int VectorIndexCols = 256;
    protected const int VectorIndexSize = 8;

    protected readonly FileStream XdbFileStream;
    private const int BufferSize = 4096;

    internal int IoCount { get; private set; }

    protected AbstractCacheStrategy(string xdbPath)
    {
        XdbFileStream = new FileStream(xdbPath, FileMode.Open, FileAccess.Read, FileShare.Read, BufferSize,
            useAsync: true);
    }

    protected int GetVectorIndexStartPos(uint ip)
    {
        var il0 = ip >> 24 & 0xFF;
        var il1 = ip >> 16 & 0xFF;
        var idx = il0 * VectorIndexCols * VectorIndexSize + il1 * VectorIndexSize;
        return (int)idx;
    }

    internal abstract ReadOnlyMemory<byte> GetVectorIndex(uint ip);

    internal virtual ReadOnlyMemory<byte> GetData(int offset, int length)
    {
        byte[] buffer = ArrayPool<byte>.Shared.Rent(length);
        int totalBytesRead = 0;
        try
        {
            XdbFileStream.Seek(offset, SeekOrigin.Begin);

            int bytesRead;
            do
            {
                int bytesToRead = Math.Min(BufferSize, length - totalBytesRead);
                bytesRead = XdbFileStream.Read(buffer, totalBytesRead, bytesToRead);
                totalBytesRead += bytesRead;
            
                IoCount++;
            } while (bytesRead > 0 && totalBytesRead < length);
        }
        finally
        {
            ArrayPool<byte>.Shared.Return(buffer);
        }

        return new ReadOnlyMemory<byte>(buffer, 0, totalBytesRead);
    }
}