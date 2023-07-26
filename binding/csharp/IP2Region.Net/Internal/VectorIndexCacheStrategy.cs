// Copyright 2023 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.
// @Author Alan <lzh.shap@gmail.com>
// @Date   2023/07/25

using IP2Region.Net.Internal.Abstractions;

namespace IP2Region.Net.Internal;

internal class VectorIndexCacheStrategy : AbstractCacheStrategy
{
    private readonly ReadOnlyMemory<byte> _vectorIndex;

    public VectorIndexCacheStrategy(string xdbPath) : base(xdbPath)
    {
        var vectorLength = VectorIndexRows * VectorIndexCols * VectorIndexSize;
        _vectorIndex = base.GetData(HeaderInfoLength, vectorLength);
    }

    internal override ReadOnlyMemory<byte> GetVectorIndex(uint ip)
    {
        var idx = GetVectorIndexStartPos(ip);
        return _vectorIndex.Slice(idx, VectorIndexSize);
    }
}