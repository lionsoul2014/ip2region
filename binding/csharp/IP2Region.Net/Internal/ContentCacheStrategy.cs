// Copyright 2025 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.
// @Author Alan <lzh.shap@gmail.com>
// @Date   2023/07/25
// Updated by Argo Zhang <argo@live.ca> at 2025/11/21

namespace IP2Region.Net.Internal;

class ContentCacheStrategy : FileCacheStrategy
{
    private readonly ReadOnlyMemory<byte> _cacheData;

    public ContentCacheStrategy(string xdbPath) : base(xdbPath)
    {
        _cacheData = base.GetData(0, (int)XdbFileStream.Length);
        XdbFileStream.Close();
        XdbFileStream.Dispose();
    }

    public override ReadOnlyMemory<byte> GetData(long offset, int length) => _cacheData.Slice((int)offset, length);

    protected override void Dispose(bool disposing)
    {
        base.Dispose(false);
    }
}
