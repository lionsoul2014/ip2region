using IP2Region.xdb;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace IP2Region.SearchTest
{
    internal class Program
    {

        public static void PrintHelp(String[] args)
        {
            Console.WriteLine("ip2region xdb searcher");
            Console.WriteLine("SearchTest.exe [command] [command options]");
            Console.WriteLine("Command: ");
            Console.WriteLine("  search    search input test");
            Console.WriteLine("  bench     search bench test");
        }
        public static Searcher CreateSearcher(String dbPath, String cachePolicy)
        {
            if ("file" == cachePolicy)
            {
                return Searcher.NewWithFileOnly(dbPath);
            }
            else if ("vectorIndex" == cachePolicy)
            {
                byte[] vIndex = Searcher.LoadVectorIndexFromFile(dbPath);
                return Searcher.NewWithVectorIndex(dbPath, vIndex);
            }
            else if ("content" == cachePolicy)
            {
                byte[] cBuff = Searcher.LoadContentFromFile(dbPath);
                return Searcher.NewWithBuffer(cBuff);
            }
            else
            {
                throw new Exception("invalid cache policy `" + cachePolicy + "`, options: file/vectorIndex/content");
            }
        }

        public static void SearchTest(string[] args)
        {
            String dbPath = "", cachePolicy = "vectorIndex";
            foreach (string r in args)
            {
                if (r.Length < 5)
                {
                    continue;
                }

                if (r.IndexOf("--") != 0)
                {
                    continue;
                }

                int sIdx = r.IndexOf('=');
                if (sIdx < 0)
                {
                    Console.WriteLine("missing = for args pair `{0}`", r);
                    return;
                }

                String key = r.Substring(2, sIdx - 2);
                String val = r.Substring(sIdx + 1);
                // System.out.printf("key=%s, val=%s\n", key, val);
                if ("db" == key)
                {
                    dbPath = val;
                }
                else if ("cache-policy" == key)
                {
                    cachePolicy = val;
                }
                else
                {
                    Console.WriteLine("undefined option `{0}`", r);
                    return;
                }
            }

            if (dbPath.Length < 1)
            {
                Console.WriteLine("jSearchTest.exe search [command options]");
                Console.WriteLine("options:");
                Console.WriteLine(" --db string              ip2region binary xdb file path");
                Console.WriteLine(" --cache-policy string    cache policy: file/vectorIndex/content");
                return;
            }

            Searcher searcher = CreateSearcher(dbPath, cachePolicy);

            Console.WriteLine("ip2region xdb searcher test program, cachePolicy: %s\ntype 'quit' to exit", cachePolicy);
            while (true)
            {
                Console.Write("ip2region>> ");
                var line = Console.ReadLine().Trim();
                if (line.Length < 2) continue;
                if (line == "quit") break;
                try
                {
                    var st = new Stopwatch();
                    st.Start();
                    var region = searcher.Search(line);
                    st.Stop();
                    var cost = st.ElapsedMilliseconds;
                    Console.WriteLine("{{region: {0}, ioCount: {1}, took: {2} ms}}", region, searcher.IOCount, cost);
                }
                catch (Exception e)
                {
                    Console.WriteLine("{{err:{0}, ioCount: {1}}}", e, searcher.IOCount);
                }
            }
            Console.WriteLine("searcher test program exited, thanks for trying");

        }
        public static void BenchTest(String[] args)
        {

        }
        static void Main(string[] args)
        {
            if (args.Length < 1)
            {
                PrintHelp(args);
                return;
            }
            switch (args[0])
            {
                case "search":
                    try
                    {
                        SearchTest(args);
                    }
                    catch (Exception e)
                    {
                        Console.WriteLine("failed running search test: {0}", e);
                    }
                    break;
                case "bench":
                    try
                    {
                        BenchTest(args);
                    }
                    catch (Exception e)
                    {
                        Console.WriteLine("failed running bench test: {0}", e);
                    }
                    break;
                default: PrintHelp(args); break;
            }
        }
    }
}
