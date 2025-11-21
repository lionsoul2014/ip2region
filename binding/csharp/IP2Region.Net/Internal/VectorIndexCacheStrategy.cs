// Copyright 2025 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.
// @Author Alan <lzh.shap@gmail.com>
// @Date   2023/07/25
// Updated by Argo Zhang <argo@live.ca> at 2025/11/21

using IP2Region.Net.Internal.Abstractions;

namespace IP2Region.Net.Internal;

internal class VectorIndexCacheStrategy : AbstractCacheStrategy
{
    readonly ReadOnlyMemory<byte> _vectorCache = default;

    public VectorIndexCacheStrategy(string xdbPath) : base(xdbPath)
    {
        _vectorCache = base.GetData(256, 256 * 256 * 8);
    }

    public override ReadOnlyMemory<byte> GetVectorIndexStartPos(int offset) => _vectorCache.Slice(offset, 8);
}
