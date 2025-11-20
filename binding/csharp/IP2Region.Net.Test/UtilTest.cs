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