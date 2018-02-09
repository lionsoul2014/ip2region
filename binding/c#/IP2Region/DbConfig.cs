//*******************************
// Create By Rocher Kong 
// Github https://github.com/RocherKong
// Date 2018.02.09
//*******************************
using System;

namespace IP2Region
{
    public class DbConfig
    {
        /**
         * total header data block size
        */
        private int totalHeaderSize;

        /**
         * max index data block size
         * u should always choice the fastest read block size 
        */
        private int indexBlockSize;

        /**
         * construct method
         * 
         * @param    totalHeaderSize
         * @param    dataBlockSize
         * @throws DbMakerConfigException 
        */
        public DbConfig(int totalHeaderSize)
        {
            if ((totalHeaderSize % 8) != 0)
            {
                throw new DbMakerConfigException("totalHeaderSize must be times of 8");
            }
            this.totalHeaderSize = totalHeaderSize;
            this.indexBlockSize = 8192; //4 * 2048
        }

        public DbConfig()
        {
            this.totalHeaderSize = 8 * 2048;
            this.indexBlockSize = 8192;
        }

        public int GetTotalHeaderSize()
        {
            return totalHeaderSize;
        }

        public DbConfig SetTotalHeaderSize(int totalHeaderSize)
        {
            this.totalHeaderSize = totalHeaderSize;
            return this;
        }

        public int GetIndexBlockSize()
        {
            return indexBlockSize;
        }

        public DbConfig SetIndexBlockSize(int dataBlockSize)
        {
            this.indexBlockSize = dataBlockSize;
            return this;
        }
    }

}