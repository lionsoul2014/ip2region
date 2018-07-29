using IP2Region;
using IP2Region.Models;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace UnitTests
{
    /// <summary>
    /// This test class is mainly used to test your exception types.
    /// It should throw exceptions of differnt kinds of types on certain occations.
    /// </summary>
    [TestClass]
    public class ExceptionTest
    {
        /// <summary>
        /// Path error should throw "DbMakerConfigException"
        /// </summary>
        [TestMethod]
        [ExpectedException(typeof(DbMakerConfigException))]
        public void TestDbMakerConfigException()
        {
            using (var _dbSearcher = new DbSearcher(new DbConfig(255), "../../../../../data/ip2region.db")) { }
        }
        /// <summary>
        /// Invalid IP should throw "IPInValidException"
        /// </summary>
        [TestMethod]
        public void TestInvalidIP()
        {
            using (var _dbSearcher = new DbSearcher("../../../../../data/ip2region.db"))
            {
                var invalidIps = new string[] { "256.255.1.1", "-1.0.0.0", "192.168.4", "x.y.z" };
                var counter = 0;

                foreach (var item in invalidIps)
                {
                    try
                    {
                        _dbSearcher.MemorySearch(item);
                    }
                    catch (IPInValidException)
                    {
                        counter++;
                    }
                }

                Assert.AreEqual(counter, 4);
            }
        }
    }
}
