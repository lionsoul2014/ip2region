//*******************************
// Created By Rocher Kong 
// Github https://github.com/RocherKong
// Date 2018.02.09
//*******************************

using System;

namespace IP2Region
{
    public class IPInValidException : Exception
    {
        const string ERROR_MSG = "IP Illigel. Please input a valid IP.";
        public IPInValidException() : base(ERROR_MSG) { }
    }
    internal static class Utils
    {
        /// <summary>
        /// Write specfield bytes to a byte array start from offset.
        /// </summary>
        public static void Write(byte[] b, int offset, ulong v, int bytes)
        {
            for (int i = 0; i < bytes; i++)
            {
                b[offset++] = (byte)((v >> (8 * i)) & 0xFF);
            }
        }

        /// <summary>
        /// Write a int to a byte array.
        /// </summary>
        public static void WriteIntLong(byte[] b, int offset, long v)
        {
            b[offset++] = (byte)((v >> 0) & 0xFF);
            b[offset++] = (byte)((v >> 8) & 0xFF);
            b[offset++] = (byte)((v >> 16) & 0xFF);
            b[offset] = (byte)((v >> 24) & 0xFF);
        }

        /// <summary>
        /// Get a int from a byte array start from the specifiled offset.
        /// </summary>
        public static long GetIntLong(byte[] b, int offset)
        {
            return (
                ((b[offset++] & 0x000000FFL)) |
                ((b[offset++] << 8) & 0x0000FF00L) |
                ((b[offset++] << 16) & 0x00FF0000L) |
                ((b[offset] << 24) & 0xFF000000L)
            );
        }

        /// <summary>
        /// Get a int from a byte array start from the specifield offset.
        /// </summary>
        public static int GetInt3(byte[] b, int offset)
        {
            return (
                (b[offset++] & 0x000000FF) |
                (b[offset++] & 0x0000FF00) |
                (b[offset] & 0x00FF0000)
            );
        }

        public static int GetInt2(byte[] b, int offset)
        {
            return (
                (b[offset++] & 0x000000FF) |
                (b[offset] & 0x0000FF00)
            );
        }

        public static int GetInt1(byte[] b, int offset)
        {
            return (
                (b[offset] & 0x000000FF)
            );
        }
        /// <summary>
        /// String ip to long ip.
        /// </summary>
        public static long Ip2long(string ip)
        {
            string[] p = ip.Split('.');
            if (p.Length != 4) throw new IPInValidException();

            foreach (string pp in p)
            {
                if (pp.Length > 3) throw new IPInValidException();
                if (!int.TryParse(pp, out int value) || value > 255)
                {
                    throw new IPInValidException();
                }
            }
            var bip1 = long.TryParse(p[0], out long ip1);
            var bip2 = long.TryParse(p[1], out long ip2);
            var bip3 = long.TryParse(p[2], out long ip3);
            var bip4 = long.TryParse(p[3], out long ip4);

            if (!bip1 || !bip2 || !bip3 || !bip4
                || ip4 > 255 || ip1 > 255 || ip2 > 255 || ip3 > 255
                || ip4 < 0 || ip1 < 0 || ip2 < 0 || ip3 < 0)
            {
                throw new IPInValidException();
            }
            long p1 = ((ip1 << 24) & 0xFF000000);
            long p2 = ((ip2 << 16) & 0x00FF0000);
            long p3 = ((ip3 << 8) & 0x0000FF00);
            long p4 = ((ip4 << 0) & 0x000000FF);
            return ((p1 | p2 | p3 | p4) & 0xFFFFFFFFL);
        }

        /// <summary>
        /// Int to ip string.
        /// </summary>
        public static string Long2ip(long ip)
        {
            return $"{(ip >> 24) & 0xFF}.{(ip >> 16) & 0xFF}.{(ip >> 8) & 0xFF}.{ip & 0xFF}";
        }
    }

}