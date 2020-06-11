using System;
using System.Collections.Generic;
using System.Text;

namespace DbMaker
{
    public static class Extensions
    {
        public static byte[] getBytes(this string str, string encode)
        {
            return Encoding.GetEncoding(encode).GetBytes(str);
        }

        public static Boolean containsKey<TKey, TValue>(this IDictionary<TKey, TValue> dic, TKey key)
        {
            return dic.ContainsKey(key);
        }

        public static TValue get<TKey, TValue>(this IDictionary<TKey, TValue> dic, TKey key)
        {
            return dic[key];
        }

        public static void put<TKey, TValue>(this IDictionary<TKey, TValue> dic, TKey key, TValue value)
        {
            dic.Add(key,value);
        }

        public static void add<TItem>(this IList<TItem> list, TItem item)
        {
            list.Add(item);
        }

        public static void printStackTrace(this Exception e)
        {
            Console.WriteLine(e);
        }
    }
}