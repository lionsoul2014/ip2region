// Copyright 2025 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.
// @Author Alan <lzh.shap@gmail.com>
// @Date   2023/07/25
// Updated by Argo Zhang <argo@live.ca> at 2025/11/21

using Xunit;

namespace IP2Region.Net.Test;

public class UtilTest
{
    [Fact]
    public void IpAddressToUInt32_Ok()
    {
        var uintIp = XDB.Util.IpAddressToUInt32("114.114.114.114");
        Assert.Equal((uint)1920103026, uintIp);
    }

    [Fact]
    public void GetMidIp_Ok()
    {
        var uintIp = XDB.Util.GetMidIp(1, 10);
        Assert.Equal((uint)5, uintIp);
    }
}