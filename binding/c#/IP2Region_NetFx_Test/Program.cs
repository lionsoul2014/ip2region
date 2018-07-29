using IP2Region;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace IP2Region_NetFx_Test
{
    class Program
    {
        static void Main(string[] args)
        {
            DbSearcher _search = new DbSearcher(Environment.CurrentDirectory + @"\DB\ip2region.db");
            Console.WriteLine(_search.MemorySearch("183.192.62.65").Region);
            Console.Read();
        }
    }
}
