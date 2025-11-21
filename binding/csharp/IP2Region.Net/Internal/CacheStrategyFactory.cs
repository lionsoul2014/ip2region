// Copyright 2023 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.
// @Author Alan <lzh.shap@gmail.com>
// @Date   2023/07/25

using IP2Region.Net.Internal.Abstractions;
using IP2Region.Net.XDB;

namespace IP2Region.Net.Internal;

internal class CacheStrategyFactory(string xdbPath)
{
    public AbstractCacheStrategy CreateCacheStrategy(CachePolicy cachePolicy) => cachePolicy switch
    {
        CachePolicy.Content => new ContentCacheStrategy(xdbPath),
        CachePolicy.VectorIndex => new VectorIndexCacheStrategy(xdbPath),
        _ => new FileCacheStrategy(xdbPath),
    };
}
