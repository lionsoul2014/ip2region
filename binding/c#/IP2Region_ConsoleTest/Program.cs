using IP2Region;
using System;
using System.Diagnostics;

namespace IP2Region_ConsoleTest
{
    class Program
    {
        static void Main(string[] args)
        {
            DbSearcher dbSearcher = new DbSearcher(new DbConfig
            {

            }, AppContext.BaseDirectory + @"/DBFile/ip2region.db");
            string ipAddress = "";
            DataBlock result = null, result2 = null,result3=null;
            Console.WriteLine("请输入IP地址：");
            ipAddress = Console.ReadLine();
            Stopwatch sw = new Stopwatch();
            do
            {
                sw.Start();
                try
                {
                    result = dbSearcher.BtreeSearch(ipAddress);
                    sw.Stop();
                    Console.WriteLine("[btree]你的IP所属区域为：" + result.GetRegion() + ";耗时：" + sw.Elapsed.TotalMilliseconds + "ms");
                    sw.Start();
                    result2 = dbSearcher.BinarySearch(ipAddress);
                    sw.Stop();
                    Console.WriteLine("[binarySearch]你的IP所属区域为：" + result2.GetRegion() + ";耗时：" + sw.Elapsed.TotalMilliseconds + "ms");
                    sw.Start();
                    result3 = dbSearcher.MemorySearch(ipAddress);
                    sw.Stop();
                    Console.WriteLine("[MemorySearch]你的IP所属区域为：" + result3.GetRegion() + ";耗时：" + sw.Elapsed.TotalMilliseconds + "ms");
                    Console.WriteLine("请输入IP地址：");
                }
                catch (Exception ex)
                {

                    Console.WriteLine(ex.Message);
                    sw.Stop();
                }
              
                //Console.WriteLine("结束请输入bye");

            } while ((ipAddress = Console.ReadLine()) != "bye");
        }
    }
}
