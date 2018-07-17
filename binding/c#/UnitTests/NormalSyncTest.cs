using Microsoft.VisualStudio.TestTools.UnitTesting;
using IP2Region;
namespace UnitTests
{
    /// <summary>
    /// This class is used to test with normal IPs (Chinese, Foreign and Special).
    /// Use sync functions for testing.
    /// </summary>
    [TestClass]
    public class NormalSyncTest
    {
        private static DbSearcher _dbSearcher = null;

        [ClassInitialize]
        public static void Init(TestContext context)
        {
            _dbSearcher = new DbSearcher("../../../../../data/ip2region.db");
        }

        [ClassCleanup]
        public static void ClearUp()
        {
            _dbSearcher.Dispose();
        }

        #region Normal Valid Chinese IP Test Cases
        [TestMethod]
        [TestCategory("Chinese Ip Test")]
        public void BinarySearchTestForChinese()
        {
            const int cityId = 2163;
            const string region = "中国|0|广东省|深圳市|鹏博士";

            var result = _dbSearcher.BinarySearch("101.105.35.57");
            Assert.AreEqual(result.CityID, cityId);
            Assert.AreEqual(result.Region, region);
        }
        [TestMethod]
        [TestCategory("Chinese Ip Test")]
        public void BtreeSearchTestForChinese()
        {
            const int cityId = 2163;
            const string region = "中国|0|广东省|深圳市|鹏博士";

            var result = _dbSearcher.BtreeSearch("101.105.35.57");
            Assert.AreEqual(result.CityID, cityId);
            Assert.AreEqual(result.Region, region);
        }
        [TestMethod]
        [TestCategory("Chinese Ip Test")]
        public void MemorySearchTestForChinese()
        {
            const int cityId = 2163;
            const string region = "中国|0|广东省|深圳市|鹏博士";

            var result = _dbSearcher.MemorySearch("101.105.35.57");
            Assert.AreEqual(result.CityID, cityId);
            Assert.AreEqual(result.Region, region);
        }
        #endregion

        #region Normal Valid Foreign IP Test Cases
        [TestMethod]
        [TestCategory("Foreign Ip Test")]
        public void BinarySearchTestForForeign()
        {
            const int cityId = 71;
            const string region = "荷兰|0|0|0|0";

            var result = _dbSearcher.BinarySearch("86.84.21.60");
            Assert.AreEqual(result.CityID, cityId);
            Assert.AreEqual(result.Region, region);
        }
        [TestMethod]
        [TestCategory("Foreign Ip Test")]
        public void BtreeSearchTestForForeign()
        {
            const int cityId = 71;
            const string region = "荷兰|0|0|0|0";

            var result = _dbSearcher.BtreeSearch("86.84.21.60");
            Assert.AreEqual(result.CityID, cityId);
            Assert.AreEqual(result.Region, region);
        }
        [TestMethod]
        [TestCategory("Foreign Ip Test")]
        public void MemorySearchTestForForeign()
        {
            const int cityId = 71;
            const string region = "荷兰|0|0|0|0";

            var result = _dbSearcher.MemorySearch("86.84.21.60");
            Assert.AreEqual(result.CityID, cityId);
            Assert.AreEqual(result.Region, region);
        }
        #endregion

        #region Normal Valid Special Ip Test Cases
        [TestMethod]
        [TestCategory("Special Ip Test")]
        public void BinarySearchTestForSpecial()
        {
            const int cityId = 0;
            const string region = "0|0|0|内网IP|内网IP";
            string[] specialIps = new string[] { "255.255.255.255", "0.0.0.0" };

            foreach (var item in specialIps)
            {
                var result = _dbSearcher.BinarySearch(item);
                Assert.AreEqual(result.CityID, cityId);
                Assert.AreEqual(result.Region, region);
            }
        }
        [TestMethod]
        [TestCategory("Special Ip Test")]
        public void BtreeSearchTestForSpecial()
        {
            const int cityId = 0;
            const string region = "0|0|0|内网IP|内网IP";
            string[] specialIps = new string[] { "255.255.255.255", "0.0.0.0" };

            foreach (var item in specialIps)
            {
                var result = _dbSearcher.BtreeSearch(item);
                Assert.AreEqual(result.CityID, cityId);
                Assert.AreEqual(result.Region, region);
            }
        }
        [TestMethod]
        [TestCategory("Special Ip Test")]
        public void MemorySearchTestForSpecial()
        {
            const int cityId = 0;
            const string region = "0|0|0|内网IP|内网IP";
            string[] specialIps = new string[] { "255.255.255.255", "0.0.0.0" };

            foreach (var item in specialIps)
            {
                var result = _dbSearcher.MemorySearch(item);
                Assert.AreEqual(result.CityID, cityId);
                Assert.AreEqual(result.Region, region);
            }
        }
        #endregion
    }
}
