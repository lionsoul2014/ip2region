package org.lionsoul.ip2region.test;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;

import org.lionsoul.ip2region.DataBlock;
import org.lionsoul.ip2region.DbConfig;
import org.lionsoul.ip2region.DbMakerConfigException;
import org.lionsoul.ip2region.DbSearcher;

/**
 * data check class
 * 
 * @author koma<komazhang@foxmail.com>
**/

public class TestUnit {

    public static void main(String[] args) {
        try {
            DbSearcher _searcher = new DbSearcher(new DbConfig(), "./data/ip2region.db");
            BufferedReader bfr   = new BufferedReader(new FileReader("./data/ip.merge.txt"));
            BufferedWriter bwr   = new BufferedWriter(new FileWriter("./data/error_log.txt", true));
            int errCount  = 0;
            int lineCount = 0;
            String str = null;
            
            while ( (str = bfr.readLine()) != null ) {
                StringBuffer line = new StringBuffer(str);
                //get first ip
                int first_idx   = line.indexOf("|");
                String first_ip = line.substring(0, first_idx);
                
                line = new StringBuffer( line.substring(first_idx + 1) );
                
                //get second ip
                int second_idx   = line.indexOf("|");
                String second_ip = line.substring(0, second_idx);
                
                //get addr
                String source_region = line.substring(second_idx + 1);
                
                //search from DbSearcher
                System.out.println("+---Start, start to search");
                System.out.println("+---[Info]: Source region = "+source_region);
                
                System.out.println("+---[Info]: Step1, search for first IP: "+first_ip);
                DataBlock fdata = _searcher.binarySearch(first_ip);
                if ( ! fdata.getRegion().equalsIgnoreCase( source_region ) ) {
                    System.out.println("[Error]: Search first IP failed, DB region = "+fdata.getRegion());
                    bwr.write("[Source]: Region: "+fdata.getRegion());
                    bwr.newLine();
                    bwr.write("[Source]: First Ip: "+first_ip);
                    bwr.newLine();
                    bwr.write("[DB]: Region: "+fdata.getRegion());
                    bwr.newLine();
                    bwr.flush();
                    errCount++;
                }
                
                System.out.println("+---[Info]: Step2, search for second IP: "+second_ip);
                DataBlock sdata = _searcher.btreeSearch(second_ip);
                if ( ! sdata.getRegion().equalsIgnoreCase( source_region ) ) {
                    System.out.println("[Error]: Search second IP failed, DB region = "+sdata.getRegion());
                    bwr.write("[Source]: Region: "+sdata.getRegion());
                    bwr.newLine();
                    bwr.write("[Source]: First Ip: "+second_ip);
                    bwr.newLine();
                    bwr.write("[DB]: Region: "+sdata.getRegion());
                    bwr.newLine();
                    bwr.flush();
                    errCount++;
                }
                
                lineCount++;
            }
            
            bwr.close();
            bfr.close();
            System.out.println("+---Done, search complished");
            System.out.println("+---Statistics, Error count = "+errCount
                                +", Total line = "+lineCount
                                +", Fail ratio = "+((float)(errCount/lineCount))*100+"%");
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        } catch (DbMakerConfigException e) {
            e.printStackTrace();
        } catch ( Exception e ) {
            e.printStackTrace();
        }
    }

}
