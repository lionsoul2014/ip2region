using System;

namespace DbMaker
{
    class Program
    {
        static void Main(string[] args)
        {
            var fn = @"G:\src\ip2region\data\ip2region.db";
            using (var searcher = new DbSearcher(new DbConfig(), fn))
            {
                Console.WriteLine(searcher.BinarySearch("202.102.227.68"));
                Console.WriteLine(searcher.BTreeSearch("202.102.227.68"));
                Console.ReadLine();
            }
        }
    }
}
