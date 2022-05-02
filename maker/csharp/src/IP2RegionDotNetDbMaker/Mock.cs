using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace IP2RegionDotNetDbMaker
{
    public static class Extensions
    {
        public static Int32 length(this string str)
        {
            if (String.IsNullOrWhiteSpace(str))
            {
                return 0;
            }
            return str.Length;
        }
        public static string trim(this string str)
        {
            return str.Trim();
        }
        public static char charAt(this string str, Int32 i)
        {
            return str[i];
        }
        public static int indexOf(this string str, string value)
        {
            return str.IndexOf(value);
        }
        public static int indexOf(this string str, char value, Int32 start)
        {
            return str.IndexOf(value, start);
        }
        public static string substring(this string str, Int32 startIndex)
        {
            return str.Substring(startIndex);
        }
        public static string substring(this string str, Int32 startIndex, Int32 endIndex)
        {
            return str.Substring(startIndex, endIndex - startIndex);
        }
        public static bool equals(this string str1, string str2)
        {
            return String.Equals(str1, str2, StringComparison.InvariantCultureIgnoreCase);
        }
        public static string[] split(this string str, string separator)
        {
            return str.Split(new[] { separator }, StringSplitOptions.None);
        }
        public static byte[] getBytes(this string str)
        {
            return Encoding.UTF8.GetBytes(str);
        }
        public static byte[] getBytes(this string str, string encoding)
        {
            return Encoding.GetEncoding(encoding).GetBytes(str);
        }
        public static bool endsWith(this string str, string value)
        {
            return str.EndsWith(value);
        }
    }
    public class StringBuilder
    {
        private readonly System.Text.StringBuilder _builder = new System.Text.StringBuilder();

        internal StringBuilder append(object value)
        {
            _builder.Append(value);
            return this;
        }

        internal string toString()
        {
            return _builder.ToString();
        }
    }
    public class File
    {
        public File(string ipSrcFile)
        {
            _fileInfo = new FileInfo(ipSrcFile);
        }
        private FileInfo _fileInfo;
        public FileInfo FileInfo => _fileInfo;
        internal bool exists()
        {
            return _fileInfo.Exists;
        }
    }
    public class LinkedList<T> : List<T>
    {
        internal T getFirst()
        {
            return this[0];
        }

        internal void add(T item)
        {
            this.Add(item);
        }

        internal T getLast()
        {
            return this[this.Count - 1];
        }

        internal IEnumerable<T> iterator()
        {
            return this;
        }
    }
    public class HashMap<K, V> : Dictionary<K, V>
    {
        internal void put(K k, V v)
        {
            this[k] = v;
        }

        internal bool containsKey(K key)
        {
            return this.ContainsKey(key);
        }

        internal V get(K k)
        {
            if (containsKey(k))
            {
                return this[k];
            }
            return default;
        }

    }
    public class FileReader
    {
        private File globalRegionFile;
        private Queue<String> _lines = new Queue<string>();
        public FileReader(File file)
        {
            this.globalRegionFile = file;
            using (var fs = file.FileInfo.OpenRead())
            {
                using (var sr = new StreamReader(fs))
                {
                    while (!sr.EndOfStream)
                    {
                        var line = sr.ReadLine();
                        _lines.Enqueue(line);
                    }
                }
            }
        }

        internal string readLine()
        {
            if (_lines.Count > 0)
            {
                return _lines.Dequeue();
            }
            return null;
        }

        internal void close()
        {
        }
    }
    public class BufferedReader
    {
        private FileReader fileReader;

        public BufferedReader(FileReader fileReader)
        {
            this.fileReader = fileReader;
        }

        internal string readLine()
        {
            return fileReader.readLine();
        }

        internal void close()
        {
            fileReader.close();
        }
    }
    public class RandomAccessFile
    {
        private string dbFile;
        private Stream stream;

        internal void seek(long v)
        {
            stream.Seek(v, SeekOrigin.Begin);
        }

        internal void write(byte[] vs)
        {
            stream.Write(vs, 0, vs.Length);
        }

        internal void readFully(byte[] dbBinStr, int v, int length)
        {
            stream.Read(dbBinStr, v, length);
        }

        private string v;

        public RandomAccessFile(string dbFile, string v)
        {
            this.dbFile = dbFile;
            this.v = v;
            this.stream = new FileStream(dbFile, FileMode.OpenOrCreate, FileAccess.ReadWrite);
        }
        public long length()
        {
            return this.stream.Length;
        }

        internal void close()
        {
            if (stream == null)
            {
                return;
            }
            this.stream.Flush();
            this.stream.Close();
            this.stream = null;
        }

        internal long getFilePointer()
        {
            return stream.Position;
        }

        internal void write(int v)
        {
            var bytes = BitConverter.GetBytes(v);
            stream.Write(bytes, 0, bytes.Length);
        }
    }
}
