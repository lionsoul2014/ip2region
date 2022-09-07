using IP2Region.Net.XDB;

namespace IP2Region.Net.Test;

[TestFixture]
public class UtilTest
{
    [TestCase("114.114.114.114")]
    public void TestIpAddressToUInt32(string value)
    {
        var uintIp = XDB.Util.IpAddressToUInt32(value);
        Console.WriteLine(uintIp);
    }
}