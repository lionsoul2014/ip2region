package org.lionsoul.ip2region.test;

import java.io.IOException;

import org.lionsoul.ip2region.DbConfig;
import org.lionsoul.ip2region.DbMaker;
import org.lionsoul.ip2region.DbMakerConfigException;

/**
 * project test script
 * 
 * @author chenxin<chenxin619315@gmail.com>
*/
public class TestMaker 
{
	public static void main(String[] argv)
	{		
		try {
			DbConfig config = new DbConfig();
			DbMaker dbMaker = new DbMaker(
				config, 
				"/data0/code/java/ip2region-dev/data/ip.merge.txt",
				"/data0/code/java/ip2region-dev/data/origin/global_region.csv"
			);
			
			dbMaker.make("/data0/code/java/ip2region-dev/data/ip2region.db");
		} catch (DbMakerConfigException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
}
