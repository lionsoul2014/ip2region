using Xunit;

namespace IP2Region.Net.Test;

public class UtilTest
{
    [Theory]
    [InlineData("114.114.114.114")]
    public void TestIpAddressToUInt32(string value)
    {
        var uintIp = XDB.Util.IpAddressToUInt32(value);
        Console.WriteLine(uintIp);
    }
}