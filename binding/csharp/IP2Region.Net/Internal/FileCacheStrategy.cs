// Copyright 2025 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.
// @Author Alan <lzh.shap@gmail.com>
// @Date   2023/07/25
// Updated by Argo Zhang <argo@live.ca> at 2025/11/21

using System.Buffers;

namespace IP2Region.Net.Internal;

class FileCacheStrategy(string xdbPath) : ICacheStrategy
{
    protected const int HeaderInfoLength = 256;
    protected const int VectorIndexSize = 8;

    protected const int BufferSize = 64 * 1024;

    protected FileStream XdbFileStream = new(xdbPath, FileMode.Open, FileAccess.Read, FileShare.Read, BufferSize, FileOptions.RandomAccess);

    public int IoCount { get; set; }

    public void ResetIoCount()
    {
        IoCount = 0;
    }

    public virtual ReadOnlyMemory<byte> GetVectorIndex(int offset) => GetData(HeaderInfoLength + offset, VectorIndexSize);

    public virtual ReadOnlyMemory<byte> GetData(long offset, int length)
    {
        var buffer = ArrayPool<byte>.Shared.Rent(length);
        try
        {
            int totalBytesRead = 0;
            XdbFileStream.Seek(offset, SeekOrigin.Begin);

            int bytesRead;
            while (totalBytesRead < length)
            {
                bytesRead = XdbFileStream.Read(buffer, totalBytesRead, length - totalBytesRead);
                if (bytesRead == 0)
                {
                    break;
                }

                totalBytesRead += bytesRead;
                IoCount++;
            }

            var ret = new byte[totalBytesRead];
            if (totalBytesRead > 0)
            {
                Array.Copy(buffer, 0, ret, 0, totalBytesRead);
            }
            return ret;
        }
        finally
        {
            ArrayPool<byte>.Shared.Return(buffer);
        }
    }
}
