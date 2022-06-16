using BenchmarkDotNet.Attributes;
using System;

namespace IP2Region.Test.Benchmark
{
    public class TestBase
    {
        protected DbSearcher _search;

        private readonly string _dBFilePath = "";
        public TestBase()
        {

        }

        public TestBase(String DBFilePath)
        {
            _dBFilePath = DBFilePath;
        }

        [GlobalSetup]
        public void Init()
        {
            if (String.IsNullOrEmpty(_dBFilePath))
            {
                _search = new DbSearcher(AppContext.BaseDirectory + @"\DB\ip2region.db");
            }
            else
            {
                _search = new DbSearcher(_dBFilePath);
            }

        }

        [GlobalCleanup]
        public void Dispose()
        {
            _search.Dispose();
        }

        public String GetRandomIP()
        {
            return new Random(Guid.NewGuid().GetHashCode()).Next(0, 255).ToString() + "."
                + new Random(Guid.NewGuid().GetHashCode()).Next(0, 255).ToString() + "."
                  + new Random(Guid.NewGuid().GetHashCode()).Next(0, 255).ToString() + "."
                    + new Random(Guid.NewGuid().GetHashCode()).Next(0, 255).ToString();
        }
    }
}
