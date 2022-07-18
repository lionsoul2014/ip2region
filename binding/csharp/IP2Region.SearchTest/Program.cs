using IP2Region.xdb;
using System;
using System.Diagnostics;
using System.IO;

namespace IP2Region.SearchTest
{
    internal class Program
    {

        public static void PrintHelp(String[] args)
        {
            Console.WriteLine("ip2region xdb searcher");
            Console.WriteLine("IP2Region.SearchTest.exe [command] [command options]");
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
                // Console.WriteLinef("key=%s, val=%s\n", key, val);
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
                Console.WriteLine("IP2Region.SearchTest.exe search [command options]");
                Console.WriteLine("options:");
                Console.WriteLine(" --db string              ip2region binary xdb file path");
                Console.WriteLine(" --cache-policy string    cache policy: file/vectorIndex/content");
                return;
            }

            Searcher searcher = CreateSearcher(dbPath, cachePolicy);

            Console.WriteLine("ip2region xdb searcher test program, cachePolicy: {0}\ntype 'quit' to exit", cachePolicy);
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
            String dbPath = "", srcPath = "", cachePolicy = "vectorIndex";
            foreach (String r in args)
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
                if ("db" == key)
                {
                    dbPath = val;
                }
                else if ("src" == key)
                {
                    srcPath = val;
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

            if (dbPath.Length < 1 || srcPath.Length < 1)
            {
                Console.WriteLine("IP2Region.SearchTest.exe bench [command options]");
                Console.WriteLine("options:");
                Console.WriteLine(" --db string              ip2region binary xdb file path");
                Console.WriteLine(" --src string             source ip text file path");
                Console.WriteLine(" --cache-policy string    cache policy: file/vectorIndex/content");
                return;
            }

            Searcher searcher = CreateSearcher(dbPath, cachePolicy);
            long count = 0;
            var sw = new Stopwatch();
            var lines = File.ReadAllLines(srcPath);
            foreach (var line in lines)
            {
                String l = line.Trim();
                String[] ps = l.Split(new[] { '|' }, 3);
                if (ps.Length != 3)
                {
                    Console.WriteLine("invalid ip segment `{0}`", l);
                    return;
                }
                long sip;
                try
                {
                    sip = Searcher.checkIP(ps[0]);
                }
                catch (Exception e)
                {
                    Console.WriteLine("check start ip `{0}`: {1}", ps[0], e);
                    return;
                }
                long eip;
                try
                {
                    eip = Searcher.checkIP(ps[1]);
                }
                catch (Exception e)
                {
                    Console.WriteLine("check end ip `{0}`: {1}", ps[1], e);
                    return;
                }

                if (sip > eip)
                {
                    Console.WriteLine("start ip({0}) should not be greater than end ip({1})", ps[0], ps[1]);
                    return;
                }

                long mip = (sip + eip) >> 1;
                foreach (var ip in new long[] { sip, (sip + mip) >> 1, mip, (mip + eip) >> 1, eip })
                {
                    sw.Start();
                    String region = searcher.Search(ip);
                    sw.Stop();
                    // check the region info
                    if (ps[2] != (region))
                    {
                        Console.WriteLine("failed search({0}) with ({1} != {2})\n", Searcher.Long2ip(ip), region, ps[2]);
                        return;
                    }

                    count++;
                }
            }

            Console.WriteLine("Bench finished, {{cachePolicy: {0}, total: {1}, took: {2}, cost: {3} ms/op}}",
                    cachePolicy, count, sw.Elapsed,
                    count == 0 ? 0 : sw.ElapsedMilliseconds / count);
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
