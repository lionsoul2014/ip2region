using Microsoft.VisualStudio.TestTools.UnitTesting;
using IP2Region;
using System.Threading.Tasks;

namespace UnitTests
{
    /// <summary>
    /// This class is used to test with normal IPs (Chinese, Foreign and Special).
    /// Use async functions for testing.
    /// </summary>
    [TestClass]
    public class NormalAsyncTest
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
        [TestCategory("Async Chinese Ip Test")]
        public async Task AsyncBinarySearchTestForChinese()
        {
            const int cityId = 2163;
            const string region = "中国|0|广东省|深圳市|鹏博士";

            var result = await _dbSearcher.AsyncBinarySearch("101.105.35.57");
            Assert.AreEqual(result.CityID, cityId);
            Assert.AreEqual(result.Region, region);
        }
        [TestMethod]
        [TestCategory("Async Chinese Ip Test")]
        public async Task AsyncBtreeSearchTestForChinese()
        {
            const int cityId = 2163;
            const string region = "中国|0|广东省|深圳市|鹏博士";

            var result = await _dbSearcher.AsyncBtreeSearch("101.105.35.57");
            Assert.AreEqual(result.CityID, cityId);
            Assert.AreEqual(result.Region, region);
        }
        [TestMethod]
        [TestCategory("Async Chinese Ip Test")]
        public async Task AsyncMemorySearchTestForChinese()
        {
            const int cityId = 2163;
            const string region = "中国|0|广东省|深圳市|鹏博士";

            var result = await _dbSearcher.AsyncMemorySearch("101.105.35.57");
            Assert.AreEqual(result.CityID, cityId);
            Assert.AreEqual(result.Region, region);
        }
        #endregion

        #region Normal Valid Foreign IP Test Cases
        [TestMethod]
        [TestCategory("Async Foreign Ip Test")]
        public async Task AsyncBinarySearchTestForForeign()
        {
            const int cityId = 71;
            const string region = "荷兰|0|0|0|0";

            var result = await _dbSearcher.AsyncBinarySearch("86.84.21.60");
            Assert.AreEqual(result.CityID, cityId);
            Assert.AreEqual(result.Region, region);
        }
        [TestMethod]
        [TestCategory("Async Foreign Ip Test")]
        public async Task AsyncBtreeSearchTestForForeign()
        {
            const int cityId = 71;
            const string region = "荷兰|0|0|0|0";

            var result = await _dbSearcher.AsyncBtreeSearch("86.84.21.60");
            Assert.AreEqual(result.CityID, cityId);
            Assert.AreEqual(result.Region, region);
        }
        [TestMethod]
        [TestCategory("Async Foreign Ip Test")]
        public async Task AsyncMemorySearchTestForForeign()
        {
            const int cityId = 71;
            const string region = "荷兰|0|0|0|0";

            var result = await _dbSearcher.AsyncMemorySearch("86.84.21.60");
            Assert.AreEqual(result.CityID, cityId);
            Assert.AreEqual(result.Region, region);
        }
        #endregion

        #region Normal Valid Special Ip Test Cases
        [TestMethod]
        [TestCategory("Async Special Ip Test")]
        public async Task AsyncBinarySearchTestForSpecial()
        {
            const int cityId = 0;
            const string region = "0|0|0|内网IP|内网IP";
            string[] specialIps = new string[] { "255.255.255.255", "0.0.0.0" };

            foreach (var item in specialIps)
            {
                var result = await _dbSearcher.AsyncBinarySearch(item);
                Assert.AreEqual(result.CityID, cityId);
                Assert.AreEqual(result.Region, region);
            }
        }
        [TestMethod]
        [TestCategory("Async Special Ip Test")]
        public async Task AsyncBtreeSearchTestForSpecial()
        {
            const int cityId = 0;
            const string region = "0|0|0|内网IP|内网IP";
            string[] specialIps = new string[] { "255.255.255.255", "0.0.0.0" };

            foreach (var item in specialIps)
            {
                var result = await _dbSearcher.AsyncBtreeSearch(item);
                Assert.AreEqual(result.CityID, cityId);
                Assert.AreEqual(result.Region, region);
            }
        }
        [TestMethod]
        [TestCategory("Async Special Ip Test")]
        public async Task AsyncMemorySearchTestForSpecial()
        {
            const int cityId = 0;
            const string region = "0|0|0|内网IP|内网IP";
            string[] specialIps = new string[] { "255.255.255.255", "0.0.0.0" };

            foreach (var item in specialIps)
            {
                var result = await _dbSearcher.AsyncMemorySearch(item);
                Assert.AreEqual(result.CityID, cityId);
                Assert.AreEqual(result.Region, region);
            }
        }
        #endregion
    }
}
