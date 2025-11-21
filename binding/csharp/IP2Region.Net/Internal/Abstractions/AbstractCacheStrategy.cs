// Copyright 2025 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.
// @Author Alan <lzh.shap@gmail.com>
// @Date   2023/07/25
// Updated by Argo Zhang <argo@live.ca> at 2025/11/21

using System.Buffers;

namespace IP2Region.Net.Internal.Abstractions;

internal abstract class AbstractCacheStrategy(string xdbPath)
{
    protected const int HeaderInfoLength = 256;
    protected const int VectorIndexSize = 8;

    private const int BufferSize = 64 * 1024;

    public int IoCount { get; private set; }

    protected FileStream XdbFileStream = new(xdbPath, FileMode.Open, FileAccess.Read, FileShare.Read, BufferSize, FileOptions.RandomAccess);

    public void ResetIoCount()
    {
        IoCount = 0;
    }

    public virtual ReadOnlyMemory<byte> GetVectorIndex(int offset) => GetData(HeaderInfoLength + offset, VectorIndexSize);

    public virtual ReadOnlyMemory<byte> GetData(long offset, int length)
    {
        byte[] buffer = ArrayPool<byte>.Shared.Rent(length);
        int totalBytesRead = 0;
        try
        {
            XdbFileStream.Seek(offset, SeekOrigin.Begin);

            int bytesRead;
            while (totalBytesRead < length)
            {
                bytesRead = XdbFileStream.Read(buffer, totalBytesRead, length);
                totalBytesRead += bytesRead;

                IoCount++;
            }
        }
        finally
        {
            ArrayPool<byte>.Shared.Return(buffer);
        }

        return new ReadOnlyMemory<byte>(buffer, 0, totalBytesRead);
    }
}
