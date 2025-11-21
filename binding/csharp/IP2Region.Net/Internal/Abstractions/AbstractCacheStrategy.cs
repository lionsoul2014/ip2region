// Copyright 2023 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.
// @Author Alan <lzh.shap@gmail.com>
// @Date   2023/07/25

using System.Buffers;

namespace IP2Region.Net.Internal.Abstractions;

internal abstract class AbstractCacheStrategy
{
    protected readonly FileStream XdbFileStream;
    private const int BufferSize = 4096;

    internal int IoCount { get; private set; }

    protected AbstractCacheStrategy(string xdbPath)
    {
        XdbFileStream = new FileStream(xdbPath, FileMode.Open, FileAccess.Read, FileShare.Read, BufferSize,
            useAsync: true);
    }

    internal virtual ReadOnlyMemory<byte> GetData(int offset, int length)
    {
        byte[] buffer = ArrayPool<byte>.Shared.Rent(length);
        int totalBytesRead = 0;
        try
        {
            XdbFileStream.Seek(offset, SeekOrigin.Begin);

            int bytesRead;
            while (totalBytesRead < length)
            {
                int bytesToRead = Math.Min(BufferSize, length - totalBytesRead);
                bytesRead = XdbFileStream.Read(buffer, totalBytesRead, bytesToRead);
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
