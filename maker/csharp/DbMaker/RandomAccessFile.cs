using System;
using System.IO;

namespace DbMaker
{
    public class RandomAccessFile : IDisposable
    {
        private readonly Stream _stream;

        public RandomAccessFile(String file) : this(File.OpenRead(file))
        {

        }

        public RandomAccessFile(Stream stream)
        {
            if (stream == null)
            {
                throw new ArgumentNullException(nameof(stream));
            }
            _stream = stream;
        }

        public void Dispose()
        {
            _stream?.Dispose();
        }

        public long length()
        {
            return _stream.Length;
        }

        public void seek(long position)
        {
            _stream.Seek(position, SeekOrigin.Begin);
        }

        public void readFully(byte[] dbBinStr, int offset, int count)
        {
            _stream.Read(dbBinStr, offset, count);
        }

        public void close()
        {
            _stream.Dispose();
        }

        public void write(byte[] bytes)
        {
            _stream.Write(bytes);
        }

        public void write(int i)
        {
            
        }

        public long getFilePointer()
        {
            return _stream.Position;
        }
    }
}