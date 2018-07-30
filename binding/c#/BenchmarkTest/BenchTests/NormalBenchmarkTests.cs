using BenchmarkDotNet.Attributes;
using System.Threading.Tasks;

namespace BenchmarkTest.BenchTests
{
    public class NormalBenchmarkTests : BenchTestBase
    {
        [Params("0.0.0.0", "210.109.255.230", "192.168.0.1", "255.255.255.255", "183.196.233.159",
    "77.49.66.88", "210.248.255.231", "10.10.10.10", "197.84.60.202", "35.193.251.120",
     "20.108.91.101", "120.196.148.137", "249.255.250.200", "112.65.1.130")]
        public string validIp = null;

        [Benchmark]
        public void TestSyncSpeedForMemorySearch()
        {
            _dbSearcher.MemorySearch(validIp);
        }
        [Benchmark]
        public async Task TestAsyncSpeedForMemorySearch()
        {
            await _dbSearcher.MemorySearchAsync(validIp);
        }

        [Benchmark]
        public void TestSyncSpeedForBinarySearch()
        {
            _dbSearcher.BinarySearch(validIp);
        }
        [Benchmark]
        public async Task TestAsyncBinarySearch()
        {
            await _dbSearcher.BinarySearchAsync(validIp);
        }

        [Benchmark]
        public void TestSyncSpeedForBTreeSearch()
        {
            _dbSearcher.BtreeSearch(validIp);
        }
        [Benchmark]
        public async Task TestAsyncSpeedForBTreeSearch()
        {
            await _dbSearcher.BtreeSearchAsync(validIp);
        }
    }
}
