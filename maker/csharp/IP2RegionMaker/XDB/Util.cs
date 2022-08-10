using System;
using System.Collections.Generic;
using System.Linq;
using System.Net;
using System.Text;
using System.Threading.Tasks;

namespace IP2RegionMaker.XDB
{
    public static class Util
    {
        public static uint IpAddressToUInt32(string ipAddress)
        {
            var address = IPAddress.Parse(ipAddress);
            byte[] bytes = address.GetAddressBytes();
            Array.Reverse(bytes);
            return BitConverter.ToUInt32(bytes, 0);
        }

        public static string UInt32ToIpAddress(uint ipAddress)
        {
            byte[] bytes = BitConverter.GetBytes(ipAddress);
            Array.Reverse(bytes);
            return new IPAddress(bytes).ToString();
        }

        public static Segment GetSegment(string line)
        {
            var ps = line.Split("|", 3);

            if (ps.Length != 3)
            {
                throw new ArgumentException($"invalid ip segment line {line}");
            }

            var sip = Util.IpAddressToUInt32(ps[0]);
            var eip = Util.IpAddressToUInt32(ps[1]);

            if (sip > eip)
            {
                throw new ArgumentException($"start ip {ps[0]} should not be greater than end ip {ps[1]}");
            }

            if (string.IsNullOrEmpty(ps[2]))
            {
                throw new ArgumentException($"empty region info in segment line {line}");
            }

            return new Segment
            {
                StartIP = sip,
                EndIP = eip,
                Region = ps[2],
            };
        }

        public static void CheckSegments(List<Segment> segments)
        {
            Segment? last = null;

            foreach (var seg in segments)
            {
                if (seg.StartIP > seg.EndIP)
                {
                    throw new ArgumentException($"segment `{seg}`: start ip should not be greater than end ip");
                }

                if (last != null && last.EndIP + 1 != seg.StartIP)
                {
                    throw new ArgumentException($"discontinuous data segment: last.eip+1({seg.StartIP}) != seg.sip({seg.EndIP},#{seg})");
                }

                last = seg;
            }
        }
    }
}
