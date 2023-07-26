using System.Net;
using System.Runtime.InteropServices;

namespace IP2Region.Net.XDB;

public static class Util
{
    public static uint IpAddressToUInt32(string ipAddress)
    {
        var address = IPAddress.Parse(ipAddress);
        return IpAddressToUInt32(address);
    }
    
    public static uint IpAddressToUInt32(IPAddress ipAddress)
    {
        byte[] bytes = ipAddress.GetAddressBytes();
        Array.Reverse(bytes);
        return MemoryMarshal.Read<uint>(bytes);
    }

    public static uint GetMidIp(uint x, uint y)
        => (x & y) + ((x ^ y) >> 1);
}