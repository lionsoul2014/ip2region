package org.lionsoul.ip2region.test;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import org.lionsoul.ip2region.DataBlock;
import org.lionsoul.ip2region.DbConfig;
import org.lionsoul.ip2region.DbMakerConfigException;
import org.lionsoul.ip2region.DbSearcher;
import org.lionsoul.ip2region.Util;

/**
 * project test script
 * 
 * @author chenxin<chenxin619315@gmail.com>
*/
public class TestSearcher 
{
    public static void main(String[] argv)
    {    
        if ( argv.length == 0 ) {
            System.out.println("| Usage: java -jar ip2region-{version}.jar [ip2region db file]");
            return;
        }
        
        File file = new File(argv[0]);
        if ( file.exists() == false ) {
            System.out.println("Error: Invalid ip2region.db file");
            return;
        }
        
        int algorithm = DbSearcher.BTREE_ALGORITHM;
        String algoName = "B-tree";
        if ( argv.length > 1 ) {
            if ( argv[1].equalsIgnoreCase("binary")) {
                algoName  = "Binary"; 
                algorithm = DbSearcher.BINARY_ALGORITHM;
            } else if ( argv[1].equalsIgnoreCase("memory") ) {
                algoName  = "Memory";
                algorithm = DbSearcher.MEMORY_ALGORITYM;
            }
        }
        
        try {
            System.out.println("initializing "+algoName+" ... ");
            DbConfig config = new DbConfig();
            DbSearcher searcher = new DbSearcher(config, argv[0]);
            BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
            
            //define the method
            Method method = null;
            switch ( algorithm ) 
            {
            case DbSearcher.BTREE_ALGORITHM:
                method = searcher.getClass().getMethod("btreeSearch", String.class);
                break;
            case DbSearcher.BINARY_ALGORITHM:
                method = searcher.getClass().getMethod("binarySearch", String.class);
                break;
            case DbSearcher.MEMORY_ALGORITYM:
                method = searcher.getClass().getMethod("memorySearch", String.class);
                break;
            }
            
            System.out.println("+----------------------------------+");
            System.out.println("| ip2region test shell             |");
            System.out.println("| Author: chenxin619315@gmail.com  |");
            System.out.println("| Type 'quit' to exit program      |");
            System.out.println("+----------------------------------+");
            
            double sTime = 0, cTime = 0;
            String line = null;
            DataBlock dataBlock = null;
            while ( true ) {
                System.out.print("ip2region>> ");
                line = reader.readLine().trim();
                if ( line.length() < 2 ) continue;
                if ( line.equalsIgnoreCase("quit") ) break;
                if ( Util.isIpAddress(line) == false ) {
                    System.out.println("Error: Invalid ip address");
                    continue;
                }
                
                sTime = System.nanoTime();
                dataBlock = (DataBlock) method.invoke(searcher, line);
                cTime = (System.nanoTime() - sTime) / 1000000;
                System.out.printf("%s in %.5f millseconds\n", dataBlock, cTime);
            }
            
            reader.close();
            searcher.close();
            System.out.println("+--Bye");
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (DbMakerConfigException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (NoSuchMethodException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (SecurityException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (IllegalAccessException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (IllegalArgumentException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (InvocationTargetException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }
}
