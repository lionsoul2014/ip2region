using BenchmarkDotNet.Running;
using BenchmarkTest.BenchTests;

namespace BenchmarkTest
{
    public class Program
    {
        public static void Main(string[] args)
        {
            BenchmarkRunner.Run<NormalBenchmarkTests>();
        }
    }
}
