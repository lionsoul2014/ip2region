package org.lionsoul.ip2region;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.io.UnsupportedEncodingException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;

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
	private LinkedList<IndexBlock>  indexPool;
	private LinkedList<HeaderBlock> headerPool;
	
	/**
	 * global region Id mapping data 
	*/
	private File globalRegionFile = null;
	private HashMap<String, Integer> globalRegionMap = null;
	
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
			String globalRegionFile ) throws DbMakerConfigException, IOException
	{
		this.dbConfig = config;
		this.ipSrcFile = new File(ipSrcFile);
		this.globalRegionMap = new HashMap<String, Integer>();
		this.regionPtrPool = new HashMap<String, DataBlock>();
		if ( globalRegionFile != null ) {
			this.globalRegionFile = new File(globalRegionFile);
		}
		
		if ( this.ipSrcFile.exists() == false ) {
			throw new IOException("Error: Invalid file path " + ipSrcFile);
		}
	}
	
	/**
	 * initialize the db file 
	 * 
	 * @param	raf
	 * @throws IOException 
	*/
	private void initDbFile( RandomAccessFile raf ) throws IOException
	{
		//1. zero fill the header part
		raf.seek(0L);
		raf.write(new byte[8]);		//super block
		raf.write(new byte[dbConfig.getTotalHeaderSize()]);		//header block
		
		headerPool = new LinkedList<HeaderBlock>();
		indexPool  = new LinkedList<IndexBlock>();
	}
	
	/**
	 * make the Db file 
	 * 
	 * @param	dbFile target output file path
	 * @throws IOException 
	*/
	public void make(String dbFile) throws IOException
	{
		//check and load the gloabl region
		if ( globalRegionFile != null ) {
			System.out.println("+-Try to load the global region data ...");
			BufferedReader greader = new BufferedReader(new FileReader(globalRegionFile));
			String gline = null;
			while ( (gline = greader.readLine()) != null ) {
				String[] p = gline.split(",");
				if ( p.length != 5 ) continue;
				
				//push the mapping
				globalRegionMap.put(p[2], Integer.valueOf(p[0]));
			}
			
			greader.close();
			System.out.println("|--[Ok]");
		}
		
		//alloc the header size
		BufferedReader reader = new BufferedReader(new FileReader(this.ipSrcFile));
		RandomAccessFile raf  = new RandomAccessFile(dbFile, "rw");
		
		//init the db file
		initDbFile(raf);
		System.out.println("+-Db file initialized.");
		
		//analysis main loop
		System.out.println("+-Try to write the data blocks ... ");
		String line = null;
		while ( ( line = reader.readLine() ) != null ) {
			line = line.trim();
			if ( line.length() == 0 ) 	 continue;
			if ( line.charAt(0) == '#' ) continue;
			
			//1. get the start ip
			int sIdx = 0, eIdx = 0;
			if ( (eIdx = line.indexOf('|', sIdx + 1)) == -1 ) continue;
			String startIp = line.substring(sIdx, eIdx);
			
			//2. get the end ip
			sIdx = eIdx + 1;
			if ( (eIdx = line.indexOf('|', sIdx + 1)) == -1 ) continue;
			String endIp = line.substring(sIdx, eIdx);
			
			//3. get the region
			sIdx = eIdx + 1;
			String region = line.substring(sIdx);
			
			System.out.println("+-Try to process item " + line);
			addDataBlock(raf, startIp, endIp, region);
			System.out.println("|--[Ok]");
		}
		System.out.println("|--Data block flushed!");
		System.out.println("|--Data file pointer: "+raf.getFilePointer()+"\n");
		
		//write the index bytes
		System.out.println("+-Try to write index blocks ... ");
		
		//record the start block
		IndexBlock indexBlock = null;
		HeaderBlock hb = null;
		indexBlock = indexPool.getFirst();
		long indexStartIp = indexBlock.getStartIp(), 
					indexStratPtr = raf.getFilePointer(), indexEndPtr;
		headerPool.add(new HeaderBlock(indexStartIp, (int)(indexStratPtr)));
		
		int blockLength = IndexBlock.getIndexBlockLength();
		int counter = 0, shotCounter = (dbConfig.getIndexBlockSize()/blockLength) - 1;
		Iterator<IndexBlock> indexIt = indexPool.iterator();
		while ( indexIt.hasNext() ) {
			indexBlock = indexIt.next();
			if ( ++counter >= shotCounter ) {
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
		if ( counter > 0 ) {
			indexBlock = indexPool.getLast();
			hb = new HeaderBlock(
				indexBlock.getStartIp(),
				((int)raf.getFilePointer()) - IndexBlock.getIndexBlockLength()
			);
			
			headerPool.add(hb);
		}
		
		indexEndPtr = raf.getFilePointer();
		System.out.println("|--[Ok]");
		
		//write the super blocks
		System.out.println("+-Try to write the super blocks ... ");
		raf.seek(0L);	//reset the file pointer
		byte[] superBuffer = new byte[8];
		Util.writeIntLong(superBuffer, 0, indexStratPtr);
		Util.writeIntLong(superBuffer, 4, indexEndPtr - blockLength);
		raf.write(superBuffer);
		System.out.println("|--[Ok]");
		
		//write the header blocks
		System.out.println("+-Try to write the header blocks ... ");
		Iterator<HeaderBlock> headerIt = headerPool.iterator();
		while ( headerIt.hasNext() ) {
			HeaderBlock headerBlock = headerIt.next();
			raf.write(headerBlock.getBytes());
		}
		
		//write the copyright and the release timestamp info
		System.out.println("+-Try to write the copyright and release date info ... ");
		raf.seek(raf.length());
		Calendar cal = Calendar.getInstance();
		SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy/MM/dd");
		String copyright = "Created by lionsoul at " + dateFormat.format(cal.getTime());
		raf.write((int)(cal.getTime().getTime()/1000));	//the unix timestamp
		raf.write(copyright.getBytes());
		System.out.println("|--[Ok]");
		
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
		try {
		    byte[] data = region.getBytes("UTF-8");
		    int dataPtr = 0;
		    
		    /*byte[] city = new byte[4];
            int city_id = getCityId(region);
            Util.writeIntLong(city, 0, city_id);
            dataPtr = (int)raf.getFilePointer();
            raf.write(city);
            raf.write(data);*/
            
		    //check region ptr pool first
		    if ( regionPtrPool.containsKey(region) ) {
		        DataBlock dataBlock = regionPtrPool.get(region);
		        dataPtr = dataBlock.getDataPtr();
		        System.out.println("dataPtr: " + dataPtr + ", region: " + region);
		    } else {
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
				data.length + 4		//4 bytes for the city id
			);
			indexPool.add(ib);
		} catch (UnsupportedEncodingException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	/**
	 * get the city id base on the global region data 
	 * 
	 * @param	region
	 * @return	int
	*/
	public int getCityId( String region )
	{
		String[] p = region.split("\\|");
		if ( p.length != 5 ) return 0;
		
		String key = null;
		Integer intv = null;
		for ( int i = 3; i >= 0; i-- ) {
			if ( p[i].equals("0") ) continue;
			if ( i == 3 
				&& p[i].indexOf("省直辖县级") > -1 ) {
				key = p[2]+p[3];
			} else {
				key = p[i];
			}
			
			intv = globalRegionMap.get(key);
			if ( intv == null ) return 0;
			return intv.intValue();
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
	public static void main(String args[])
	{
	    String dstDir = "./data/";
	    String[] path = new String[]{null, null};
	    for ( int i = 0; i < args.length; i++ ) {
	        if ( args[i].equals("-src") ) {
	            path[0] = args[++i];
	        } else if ( args[i].equals("-region") ) {
	            path[1] = args[++i];
	        } else if ( args[i].equals("-dst") ) {
	            dstDir  = args[++i];
	        }
	    }
	    
	    for ( int i = 0; i < path.length; i++ ) {
	        if ( path[i] == null ) {
	            System.out.println("Usage: java -jar dbMaker.jar "
	                    + "-src [source text file path] "
	                    + "-region [global region file path]");
	            System.out.println("eg: java -jar dbMaker.jar "
	                    + "-src ./data/ip.merge.txt -region ./data/origin/global_region.csv");
	            System.exit(0);
	        }
	    }
	    
	    //check and stdlize the destination directory
	    if ( ! dstDir.endsWith("/") ) {
	        dstDir = dstDir + "/";
	    }
	    
	    try {
            DbConfig config = new DbConfig();
            DbMaker dbMaker = new DbMaker(config, path[0], path[1]);
            dbMaker.make(dstDir + "ip2region.db");
        } catch (DbMakerConfigException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
	}
}