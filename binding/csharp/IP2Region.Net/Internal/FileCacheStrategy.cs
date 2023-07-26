// Copyright 2023 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.
// @Author Alan <lzh.shap@gmail.com>
// @Date   2023/07/25

using IP2Region.Net.Internal.Abstractions;

namespace IP2Region.Net.Internal;

internal class FileCacheStrategy : AbstractCacheStrategy
{
    public FileCacheStrategy(string xdbPath) : base(xdbPath)
    {
    }

    internal override ReadOnlyMemory<byte> GetVectorIndex(uint ip)
    {
        var idx = GetVectorIndexStartPos(ip);
        return GetData(HeaderInfoLength + idx, VectorIndexSize);
    }
}