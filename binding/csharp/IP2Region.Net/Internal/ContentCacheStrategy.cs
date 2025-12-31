// Copyright 2025 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.
// @Author Alan <lzh.shap@gmail.com>
// @Date   2023/07/25
// Updated by Wong <vcd.hai@outlook.com> at 2025/12/31

namespace IP2Region.Net.Internal;

class ContentCacheStrategy(string xdbPath) : ICacheStrategy
{
    // TODO: these constants can be moved to the interface as defaults when using .NET 10
    private const int HeaderInfoLength = 256;
    private const int VectorIndexSize = 8;

    private readonly ReadOnlyMemory<byte> _cacheData = File.ReadAllBytes(xdbPath);

    public int IoCount => 0;

    public void ResetIoCount()
    {
        // Do nothing
    }

    public ReadOnlyMemory<byte> GetVectorIndex(int offset)
        => _cacheData.Slice(HeaderInfoLength + offset, VectorIndexSize);

    public ReadOnlyMemory<byte> GetData(long offset, int length) => _cacheData.Slice((int)offset, length);

    public void Dispose()
    {
        // Do nothing
    }
}
