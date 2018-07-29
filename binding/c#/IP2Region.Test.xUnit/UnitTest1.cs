using System;
using System.Threading.Tasks;
using Xunit;

namespace IP2Region.Test.xUnit
{
    public class UnitTest1
    {
        private readonly DbSearcher _search;
        public UnitTest1()
        {
            _search = new DbSearcher(Environment.CurrentDirectory + @"\DB\ip2region.db");
        }
        [Fact]
        public void Search_Test()
        {

            Assert.NotNull(_search.MemorySearch("183.192.62.65").Region);
            Assert.NotNull(_search.MemorySearchAsync("183.192.62.65").Result.Region);
            Assert.NotNull(_search.BinarySearch("183.192.62.65").Region);
            Assert.NotNull(_search.BinarySearchAsync("183.192.62.65").Result.Region);
            Assert.NotNull(_search.BtreeSearch("183.192.62.65").Region);
            Assert.NotNull(_search.BtreeSearchAsync("183.192.62.65").Result.Region);

        }

        [Fact]
        public async Task SearchAsync_Test()
        {
            var result = await _search.MemorySearchAsync("183.192.62.65");
            Assert.NotNull(result.Region);
        }


    }
}
