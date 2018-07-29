using BenchmarkDotNet.Running;
using System;

namespace IP2Region.Test.Benchmark
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("Welcome To IP2Regin!");
            var summary = BenchmarkRunner.Run<DbSearch_Test>();
            Console.ReadLine();
        }
    }
}
