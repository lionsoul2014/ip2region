using BenchmarkDotNet.Attributes;
using IP2Region;

namespace BenchmarkTest.BenchTests
{
    public class BenchTestBase
    {
        protected DbSearcher _dbSearcher = null;

        [GlobalSetup]
        public void Init()
        {
            _dbSearcher = new DbSearcher("../../../../../data/ip2region.db");
        }
        [GlobalCleanup]
        public void Clearup()
        {
            _dbSearcher.Dispose();
        }
    }
}
