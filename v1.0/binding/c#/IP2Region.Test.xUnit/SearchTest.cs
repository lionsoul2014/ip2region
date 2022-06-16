using System;
using System.Threading.Tasks;
using Xunit;

namespace IP2Region.Test.xUnit
{
    public class SearchTest : IDisposable
    {
        private readonly DbSearcher _search;

        public String GetRandomIP()
        {
            return new Random(Guid.NewGuid().GetHashCode()).Next(0, 255).ToString() + "."
                + new Random(Guid.NewGuid().GetHashCode()).Next(0, 255).ToString() + "."
                  + new Random(Guid.NewGuid().GetHashCode()).Next(0, 255).ToString() + "."
                    + new Random(Guid.NewGuid().GetHashCode()).Next(0, 255).ToString();
        }

        public SearchTest()
        {
            _search = new DbSearcher(Environment.CurrentDirectory + @"\DB\ip2region.db");
        }
        [Fact]
        public void Search_Test()
        {
            string memResult = _search.MemorySearch("223.104.246.20").Region;
            string binarySearchResult = _search.BinarySearch("223.104.246.20").Region;
            string binaryTreeSearchResult = _search.BtreeSearch("223.104.246.20").Region;

            Assert.NotNull(memResult);
            Assert.NotNull(binarySearchResult);
            Assert.NotNull(binaryTreeSearchResult);

            Assert.Equal(memResult, binarySearchResult);
            Assert.Equal(binaryTreeSearchResult, memResult);
        }

        [Fact]
        public void Search_Correct_Test()
        {
            for (int i = 0; i < 1000000; i++)
            {
                var newIp=GetRandomIP();
                string memResult = _search.MemorySearch(newIp).Region;
                string binarySearchResult = _search.BinarySearch(newIp).Region;
                string binaryTreeSearchResult = _search.BtreeSearch(newIp).Region;

                Assert.True(memResult == binarySearchResult && memResult == binaryTreeSearchResult);
            }

        }

        [Fact]
        public async Task SearchAsync_Test()
        {
            // We don't need the synchronizeContext, so just set to false
            var memResult = await _search.MemorySearchAsync("183.192.62.65").ConfigureAwait(false);
            var binarySearchResult = await _search.BinarySearchAsync("183.192.62.65").ConfigureAwait(false);
            var bTreeSearchResult = await _search.BtreeSearchAsync("183.192.62.65").ConfigureAwait(false);

            Assert.NotNull(memResult.Region);
            Assert.NotNull(binarySearchResult.Region);
            Assert.NotNull(bTreeSearchResult.Region);

            Assert.Equal(memResult.Region, binarySearchResult.Region);
            Assert.Equal(bTreeSearchResult.Region, memResult.Region);
        }

        public void Dispose()
        {
            _search.Dispose();
        }
    }
}
