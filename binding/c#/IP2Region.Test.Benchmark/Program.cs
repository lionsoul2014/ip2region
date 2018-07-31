using BenchmarkDotNet.Running;
using System;

namespace IP2Region.Test.Benchmark
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("Now starting benchmark test, please wait......");
            var summary = BenchmarkRunner.Run<DbSearch_Test>();
            Console.ReadLine();
        }
    }
}
