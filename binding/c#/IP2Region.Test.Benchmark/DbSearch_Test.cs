using BenchmarkDotNet.Attributes;
using IP2Region.Models;
using System.Threading.Tasks;

namespace IP2Region.Test.Benchmark
{
    [RankColumn]
    public class DbSearch_Test : TestBase
    {
        private string RandomIP = "";

        [Benchmark]
        public DataBlock MemorySearch()
        {
            RandomIP = GetRandomIP();
            return _search.MemorySearch(RandomIP);
        }

        [Benchmark]
        public async Task<DataBlock> MemorySearch_Async()
        {
            RandomIP = GetRandomIP();
            return await _search.MemorySearchAsync(RandomIP);
        }

        [Benchmark]
        public DataBlock BinarySearch()
        {
            RandomIP = GetRandomIP();
            return _search.BinarySearch(RandomIP);
        }

        [Benchmark]
        public async Task<DataBlock> BinarySearch_Async()
        {
            RandomIP = GetRandomIP();
            return await _search.BinarySearchAsync(RandomIP);
        }

        [Benchmark]
        public DataBlock BtreeSearch()
        {
            RandomIP = GetRandomIP();
            return _search.BtreeSearch(RandomIP);
        }

        [Benchmark]
        public async Task<DataBlock> BtreeSearch_Async()
        {
            RandomIP = GetRandomIP();
            return await _search.BtreeSearchAsync(RandomIP);
        }
    }
}
