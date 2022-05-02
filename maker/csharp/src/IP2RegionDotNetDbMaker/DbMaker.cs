using System;

namespace IP2RegionDotNetDbMaker
{


    /**
     * fast ip db maker
     * 
     * db struct:
     * 1. header part
     * 1): super part:
     * +------------+-----------+
     * | 4 bytes	| 4 bytes   |
     * +------------+-----------+
     *  start index ptr, end index ptr
     * 
     * 2): b-tree index part
     * +------------+-----------+-----------+-----------+
     * | 4bytes		| 4bytes	| 4bytes	| 4bytes	| ...
     * +------------+-----------+-----------+-----------+
     *  start ip ptr  index ptr
     *  
     * 2. data part:
     * +------------+-----------------------+
     * | 2bytes		| dynamic length		|
     * +------------+-----------------------+
     *  data length   city_id|Country|Province|Area|City|ISP
     *  
     * 3. index part: (ip range)
     * +------------+-----------+---------------+
     * | 4bytes		| 4bytes	| 4bytes		|
     * +------------+-----------+---------------+
     *  start ip 	  end ip	  3 byte data ptr & 1 byte data length
     * 
     * @author chenxin<chenxin619315@gmail.com>
*/
    public class DbMaker
    {
        /**
         * db config 
        */
        private DbConfig dbConfig;

        /**
         * ip source file path
        */
        private File ipSrcFile;

        /**
         * buffer 
        */
        private LinkedList<IndexBlock> indexPool;
        private LinkedList<HeaderBlock> headerPool;

        /**
         * global region Id mapping data 
        */
        private File globalRegionFile = null;
        private HashMap<String, int?> globalRegionMap = null;

        /**
         * region and data ptr mapping data 
        */
        private HashMap<String, DataBlock> regionPtrPool = null;

        /**
         * construct method
         * 
         * @param	config
         * @param	ipSrcFile tb source ip file
         * @param	globalRegionFile global_region.csv file offer by lion
         * @throws	DbMakerConfigException 
         * @throws	IOException 
        */
        public DbMaker(
                DbConfig config,
                String ipSrcFile,
                String globalRegionFile)
        {
            this.dbConfig = config;
            this.ipSrcFile = new File(ipSrcFile);
            this.globalRegionMap = new HashMap<String, int?>();
            this.regionPtrPool = new HashMap<String, DataBlock>();
            if (globalRegionFile != null)
            {
                this.globalRegionFile = new File(globalRegionFile);
            }

            if (this.ipSrcFile.exists() == false)
            {
                throw new Exception("Error: Invalid file path " + ipSrcFile);
            }
        }

        /**
         * initialize the db file 
         * 
         * @param	raf
         * @ 
        */
        private void initDbFile(RandomAccessFile raf)
        {
            //1. zero fill the header part
            raf.seek(0L);
            raf.write(new byte[8]);     //super block
            raf.write(new byte[dbConfig.getTotalHeaderSize()]);     //header block

            headerPool = new LinkedList<HeaderBlock>();
            indexPool = new LinkedList<IndexBlock>();
        }

        /**
         * make the Db file 
         * 
         * @param	dbFile target output file path
         * @ 
        */
        public void make(String dbFile)
        {
            //check and load the gloabl region
            if (globalRegionFile != null)
            {
                Console.WriteLine("+-Try to load the global region data ...");
                BufferedReader greader = new BufferedReader(new FileReader(globalRegionFile));
                String gline = null;
                while ((gline = greader.readLine()) != null)
                {
                    String[] p = gline.split(",");
                    if (p.Length != 5) continue;

                    //push the mapping
                    globalRegionMap.put(p[2], Int32.Parse(p[0]));
                }

                greader.close();
                Console.WriteLine("|--[Ok]");
            }

            //alloc the header size
            BufferedReader reader = new BufferedReader(new FileReader(this.ipSrcFile));
            RandomAccessFile raf = new RandomAccessFile(dbFile, "rw");

            //init the db file
            initDbFile(raf);
            Console.WriteLine("+-Db file initialized.");

            //analysis main loop
            Console.WriteLine("+-Try to write the data blocks ... ");
            String line = null;
            while ((line = reader.readLine()) != null)
            {
                line = line.trim();
                if (line.length() == 0) continue;
                if (line.charAt(0) == '#') continue;

                //1. get the start ip
                int sIdx = 0, eIdx = 0;
                if ((eIdx = line.indexOf('|', sIdx + 1)) == -1) continue;
                String startIp = line.substring(sIdx, eIdx);

                //2. get the end ip
                sIdx = eIdx + 1;
                if ((eIdx = line.indexOf('|', sIdx + 1)) == -1) continue;
                String endIp = line.substring(sIdx, eIdx);

                //3. get the region
                sIdx = eIdx + 1;
                String region = line.substring(sIdx);

                Console.WriteLine("+-Try to process item " + line);
                addDataBlock(raf, startIp, endIp, region);
                Console.WriteLine("|--[Ok]");
            }
            Console.WriteLine("|--Data block flushed!");
            Console.WriteLine("|--Data file pointer: " + raf.getFilePointer() + "\n");

            //write the index bytes
            Console.WriteLine("+-Try to write index blocks ... ");

            //record the start block
            IndexBlock indexBlock = null;
            HeaderBlock hb = null;
            indexBlock = indexPool.getFirst();
            long indexStartIp = indexBlock.getStartIp(),
                        indexStratPtr = raf.getFilePointer(), indexEndPtr;
            headerPool.add(new HeaderBlock(indexStartIp, (int)(indexStratPtr)));

            int blockLength = IndexBlock.getIndexBlockLength();
            int counter = 0, shotCounter = (dbConfig.getIndexBlockSize() / blockLength) - 1;
            //var indexIt = indexPool.iterator();
            //while (indexIt.hasNext())
            foreach (var indexIt in indexPool.iterator())
            {
                indexBlock = indexIt;
                if (++counter >= shotCounter)
                {
                    hb = new HeaderBlock(
                        indexBlock.getStartIp(),
                        (int)raf.getFilePointer()
                    );

                    headerPool.add(hb);
                    counter = 0;
                }

                //write the buffer
                raf.write(indexBlock.getBytes());
            }

            //record the end block
            if (counter > 0)
            {
                indexBlock = indexPool.getLast();
                hb = new HeaderBlock(
                    indexBlock.getStartIp(),
                    ((int)raf.getFilePointer()) - IndexBlock.getIndexBlockLength()
                );

                headerPool.add(hb);
            }

            indexEndPtr = raf.getFilePointer();
            Console.WriteLine("|--[Ok]");

            //write the super blocks
            Console.WriteLine("+-Try to write the super blocks ... ");
            raf.seek(0L);   //reset the file pointer
            byte[] superBuffer = new byte[8];
            Util.writeIntLong(superBuffer, 0, indexStratPtr);
            Util.writeIntLong(superBuffer, 4, indexEndPtr - blockLength);
            raf.write(superBuffer);
            Console.WriteLine("|--[Ok]");

            //write the header blocks
            Console.WriteLine("+-Try to write the header blocks ... ");
            //var headerIt = headerPool.iterator();
            //while (headerIt.hasNext())
            //{
            //    HeaderBlock headerBlock = headerIt.next();
            //    raf.write(headerBlock.getBytes());
            //}
            foreach (var headerBlock in headerPool.iterator())
            {
                raf.write(headerBlock.getBytes());
            }

            //write the copyright and the release timestamp info
            Console.WriteLine("+-Try to write the copyright and release date info ... ");
            raf.seek(raf.length());
            //Calendar cal = Calendar.getInstance();
            //SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy/MM/dd");
            String copyright = "Created by lionsoul at " + DateTime.Now.ToString("yyyy/MM/dd"); //dateFormat.format(cal.getTime());
            var timestamp = (DateTime.Now.ToUniversalTime().Ticks - 621355968000000000) / 10000000;
            raf.write((Int32)timestamp);   //the unix timestamp
            raf.write(copyright.getBytes());
            Console.WriteLine("|--[Ok]");

            reader.close();
            raf.close();
        }

        /**
         * internal method to add a new data block record
         * 
         * @param	raf
         * @param	startIp
         * @param	endIp
         * @param	region data
        */
        private void addDataBlock(
                RandomAccessFile raf,
                String startIp, String endIp, String region)
        {
            try
            {
                byte[] data = region.getBytes("UTF-8");
                int dataPtr = 0;

                /*byte[] city = new byte[4];
                int city_id = getCityId(region);
                Util.writeIntLong(city, 0, city_id);
                dataPtr = (int)raf.getFilePointer();
                raf.write(city);
                raf.write(data);*/

                //check region ptr pool first
                if (regionPtrPool.containsKey(region))
                {
                    DataBlock dataBlock = regionPtrPool.get(region);
                    dataPtr = dataBlock.getDataPtr();
                    Console.WriteLine("dataPtr: " + dataPtr + ", region: " + region);
                }
                else
                {
                    byte[] city = new byte[4];
                    int city_id = getCityId(region);
                    Util.writeIntLong(city, 0, city_id);
                    dataPtr = (int)raf.getFilePointer();
                    raf.write(city);
                    raf.write(data);

                    regionPtrPool.put(region, new DataBlock(city_id, region, dataPtr));
                }

                //add the data index blocks
                IndexBlock ib = new IndexBlock(
                    Util.ip2long(startIp),
                    Util.ip2long(endIp),
                    dataPtr,
                    data.Length + 4     //4 bytes for the city id
                );
                indexPool.add(ib);
            }
            catch (Exception e)
            {
                Console.WriteLine(e);
            }
        }

        /**
         * get the city id base on the global region data 
         * 
         * @param	region
         * @return	int
        */
        public int getCityId(String region)
        {
            String[] p = region.split("|");
            if (p.Length != 5) return 0;

            String key = null;
            Int32? intv = null;
            for (int i = 3; i >= 0; i--)
            {
                if (p[i].equals("0")) continue;
                if (i == 3
                    && p[i].indexOf("省直辖县级") > -1)
                {
                    key = p[2] + p[3];
                }
                else
                {
                    key = p[i];
                }

                intv = globalRegionMap.get(key);
                if (intv == null) return 0;
                return intv.Value;
            }

            return 0;
        }

        public DbConfig getDbConfig()
        {
            return dbConfig;
        }

        public DbMaker setDbConfig(DbConfig dbConfig)
        {
            this.dbConfig = dbConfig;
            return this;
        }

        public File getIpSrcFile()
        {
            return ipSrcFile;
        }

        public DbMaker setIpSrcFile(File ipSrcFile)
        {
            this.ipSrcFile = ipSrcFile;
            return this;
        }

        /**
         * make this directly a runnable application
         * interface to make the database file 
        */
        public static void Main(String[] args)
        {
            String dstDir = "./data/";
            String[] path = new String[] { null, null };
            for (int i = 0; i < args.Length; i++)
            {
                if (args[i].equals("-src"))
                {
                    path[0] = args[++i];
                }
                else if (args[i].equals("-region"))
                {
                    path[1] = args[++i];
                }
                else if (args[i].equals("-dst"))
                {
                    dstDir = args[++i];
                }
            }

            for (int i = 0; i < path.Length; i++)
            {
                if (path[i] == null)
                {
                    Console.WriteLine("Usage: dbMaker "
                            + "-src [source text file path] "
                            + "-region [global region file path]");
                    Console.WriteLine("eg: dbMaker "
                            + "-src ./data/ip.merge.txt -region ./data/origin/global_region.csv");
                    Environment.Exit(0);
                    return;
                }
            }

            //check and stdlize the destination directory
            if (!dstDir.endsWith("/"))
            {
                dstDir = dstDir + "/";
            }

            try
            {
                DbConfig config = new DbConfig();
                DbMaker dbMaker = new DbMaker(config, path[0], path[1]);
                dbMaker.make(dstDir + "ip2region.db");
            }
            catch (Exception e)
            {
                Console.WriteLine(e);
            }
        }
    }
}
