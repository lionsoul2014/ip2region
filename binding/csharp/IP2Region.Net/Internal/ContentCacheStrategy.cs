// Copyright 2025 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.
// @Author Alan <lzh.shap@gmail.com>
// @Date   2023/07/25
// Updated by Argo Zhang <argo@live.ca> at 2025/11/21

using IP2Region.Net.Internal.Abstractions;

namespace IP2Region.Net.Internal;

class ContentCacheStrategy : AbstractCacheStrategy
{
    readonly ReadOnlyMemory<byte> _cacheData = default;

    public ContentCacheStrategy(string xdbPath) : base(xdbPath)
    {
        using var reader = base.GetXdbFileStream();
        _cacheData = base.GetData(0, (int)reader.Length);
    }

    public override ReadOnlyMemory<byte> GetData(int offset, int length)
    {
        return _cacheData.Slice(offset, length);
    }
}
