using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace IP2RegionMaker.Test
{
    [TestFixture]
    internal class UtilTest
    {
        [TestCase("114.114.114.114")]
        public void TestIpAddressToUInt32(string value)
        {
            Assert.DoesNotThrow(() => XDB.Util.IpAddressToUInt32(value));
        }

        [TestCase(1920103026)]
        public void TestUInt32ToIpAddress(int value)
        {
            Assert.DoesNotThrow(() => XDB.Util.UInt32ToIpAddress((uint)value));
        }

        [TestCase("28.201.224.0|29.34.191.255|美国|0|0|0|0")]
        public void TestSplitSegment(string value)
        {
            Assert.DoesNotThrow(() =>
            {
                var seg=XDB.Util.GetSegment(value);

                var segList= seg.Split();

                XDB.Util.CheckSegments(segList);

                foreach (var item in segList)
                { 
                    Console.WriteLine(item);
                }
            });
        }
    }
}
