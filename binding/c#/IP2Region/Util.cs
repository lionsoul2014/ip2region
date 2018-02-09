//*******************************
// Create By Rocher Kong 
// Github https://github.com/RocherKong
// Date 2018.02.09
//*******************************
using System;
using System.Globalization;
using System.Text;

namespace IP2Region
{

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
        public static void write(byte[] b, int offset, ulong v, int bytes)
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
        public static long ip2long(string ip)
        {
            string[] p = ip.Split('.');
            if (p.Length != 4) return 0;
            var bip1 = long.TryParse(p[0], out long ip1);
            var bip2 = long.TryParse(p[1], out long ip2);
            var bip3 = long.TryParse(p[2], out long ip3);
            var bip4 = long.TryParse(p[3], out long ip4);

            if (!bip1 || !bip2 || !bip3 || !bip4
                || ip4 > 255 || ip1 > 255 || ip2 > 255 || ip3 > 255
                || ip4 < 1 || ip1 < 1 || ip2 < 1 || ip3 < 1)
            {
                throw new Exception("IP Illegal.");
            }

            long p1 = ((ip1 << 24) & 0xFF000000);
            long p2 = ((ip2 << 16) & 0x00FF0000);
            long p3 = ((ip3 << 8) & 0x0000FF00);
            long p4 = ((ip4 << 0) & 0x000000FF);
            return ((p1 | p2 | p3 | p4) & 0xFFFFFFFFL);
        }

        /**
         * int to ip string 
         * 
         * @param    ip
         * @return    string
        */
        public static string long2ip(long ip)
        {
            StringBuilder sb = new StringBuilder();

            sb
            .Append((ip >> 24) & 0xFF).Append('.')
            .Append((ip >> 16) & 0xFF).Append('.')
            .Append((ip >> 8) & 0xFF).Append('.')
            .Append((ip >> 0) & 0xFF);

            return sb.ToString();
        }

        /**
         * check the validate of the specifeld ip address
         * 
         * @param    ip
         * @return    boolean
        */
        public static Boolean isIpAddress(string ip)
        {
            string[] p = ip.Split('.');
            if (p.Length != 4) return false;

            foreach (string pp in p)
            {
                if (pp.Length > 3) return false;
                int val = int.Parse(pp);
                if (val > 255) return false;
            }

            return true;
        }
    }

}