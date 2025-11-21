// Copyright 2025 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.
// @Author Alan <lzh.shap@gmail.com>
// @Date   2023/07/25
// Updated by Argo Zhang <argo@live.ca> at 2025/11/21

using IP2Region.Net.Internal.Abstractions;

namespace IP2Region.Net.Internal;

class VectorIndexCacheStrategy : AbstractCacheStrategy
{
    private const int VectorIndexRows = 256;
    private const int VectorIndexCols = 256;

    private readonly ReadOnlyMemory<byte> _vectorCache;

    public VectorIndexCacheStrategy(string xdbPath) : base(xdbPath)
    {
        XdbFileStream.Seek(HeaderInfoLength, SeekOrigin.Begin);
        var buffer = new byte[VectorIndexRows * VectorIndexCols * VectorIndexSize];
        var length = XdbFileStream.Read(buffer, 0, buffer.Length);
        _vectorCache = new ReadOnlyMemory<byte>(buffer);
    }

    public override ReadOnlyMemory<byte> GetVectorIndex(int offset) => _vectorCache.Slice(offset, VectorIndexSize);
}
