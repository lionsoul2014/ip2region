// Copyright 2023 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.
// @Author Alan <lzh.shap@gmail.com>
// @Date   2023/07/25

using System.Buffers;

namespace IP2Region.Net.Internal.Abstractions;

internal abstract class AbstractCacheStrategy(string xdbPath)
{
    private const int BufferSize = 64 * 1024;

    public int IoCount { get; private set; }

    public void ResetIoCount()
    {
        IoCount = 0;
    }

    public virtual ReadOnlyMemory<byte> GetVectorIndexStartPos(int offset)
    {
        return GetData(256 + offset, 8);
    }

    public virtual ReadOnlyMemory<byte> GetData(int offset, int length)
    {
        byte[] buffer = ArrayPool<byte>.Shared.Rent(length);
        int totalBytesRead = 0;
        try
        {
            var stream = GetXdbFileStream();
            stream.Seek(offset, SeekOrigin.Begin);

            int bytesRead;
            while (totalBytesRead < length)
            {
                int bytesToRead = Math.Min(BufferSize, length - totalBytesRead);
                bytesRead = stream.Read(buffer, totalBytesRead, bytesToRead);
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

    FileStream? _xdbFileStream;

    protected FileStream GetXdbFileStream()
    {
        if (_xdbFileStream == null)
        {
            _xdbFileStream = new FileStream(xdbPath, FileMode.Open, FileAccess.Read, FileShare.Read, BufferSize, FileOptions.RandomAccess);
        }
        return _xdbFileStream;
    }
}
