using System;


namespace IP2RegionDotNetDbMaker
{

    /**
     * util class 
     * 
     * @author chenxin<chenxin619315@gmail.com>
*/
    public class Util
    {
        /**
         * write specfield bytes to a byte array start from offset
         * 
         * @param    b
         * @param    offset
         * @param    v
         * @param    bytes
        */
        public static void write(byte[] b, int offset, long v, int bytes)
        {
            for (int i = 0; i < bytes; i++)
            {
                b[offset++] = (byte)((v >> (8 * i)) & 0xFF);
            }
        }

        /**
         * write a int to a byte array
         * 
         * @param    b
         * @param    offet
         * @param    v
        */
        public static void writeIntLong(byte[] b, int offset, long v)
        {
            b[offset++] = (byte)((v >> 0) & 0xFF);
            b[offset++] = (byte)((v >> 8) & 0xFF);
            b[offset++] = (byte)((v >> 16) & 0xFF);
            b[offset] = (byte)((v >> 24) & 0xFF);
        }

        /**
         * get a int from a byte array start from the specifiled offset
         * 
         * @param    b
         * @param    offset
        */
        public static long getIntLong(byte[] b, int offset)
        {
            return (
                ((b[offset++] & 0x000000FFL)) |
                ((b[offset++] << 8) & 0x0000FF00L) |
                ((b[offset++] << 16) & 0x00FF0000L) |
                ((b[offset] << 24) & 0xFF000000L)
            );
        }

        /**
         * get a int from a byte array start from the specifield offset
         * 
         * @param    b
         * @param    offset
        */
        public static int getInt3(byte[] b, int offset)
        {
            return (
                (b[offset++] & 0x000000FF) |
                (b[offset++] & 0x0000FF00) |
                (b[offset] & 0x00FF0000)
            );
        }

        public static int getInt2(byte[] b, int offset)
        {
            return (
                (b[offset++] & 0x000000FF) |
                (b[offset] & 0x0000FF00)
            );
        }

        public static int getInt1(byte[] b, int offset)
        {
            return (
                (b[offset] & 0x000000FF)
            );
        }

        /**
         * string ip to long ip
         * 
         * @param    ip
         * @return    long
        */
        public static long ip2long(String ip)
        {
            ip = ip.Trim();
            String[] ips = ip.Split('.');
            long ip1 = Int64.Parse(ips[0]);
            long ip2 = Int64.Parse(ips[1]);
            long ip3 = Int64.Parse(ips[2]);
            long ip4 = Int64.Parse(ips[3]);
            long ip2long = 1L * ip1 * 256 * 256 * 256 + ip2 * 256 * 256 + ip3 * 256 + ip4;
            return ip2long;
        }

        /**
         * int to ip string 
         * 
         * @param    ip
         * @return    string
        */
        public static String long2ip(long ip)
        {
            StringBuilder sb = new StringBuilder();

            sb
            .append((ip >> 24) & 0xFF).append('.')
            .append((ip >> 16) & 0xFF).append('.')
            .append((ip >> 8) & 0xFF).append('.')
            .append((ip >> 0) & 0xFF);

            return sb.toString();
        }

        /**
         * check the validate of the specifeld ip address
         * 
         * @param    ip
         * @return    boolean
        */
        public static bool isIpAddress(String ip)
        {
            String[] p = ip.split(".");
            if (p.Length != 4) return false;
            foreach (var pp in p)
            {
                if (pp.Length > 3) return false;
                var val = Int32.Parse(pp);
                if (val > 255) return false;
            }

            return true;
        }
    }
}
