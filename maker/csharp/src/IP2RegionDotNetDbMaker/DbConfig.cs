using System;
namespace IP2RegionDotNetDbMaker
{

    /**
     * database configuration class
     * 
     * @author chenxin<chenxin619315@gmail.com>
*/
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
         * @param   totalHeaderSize
         * @ 
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

        public DbConfig() : this(8 * 2048)
        {
        }

        public int getTotalHeaderSize()
        {
            return totalHeaderSize;
        }

        public DbConfig setTotalHeaderSize(int totalHeaderSize)
        {
            this.totalHeaderSize = totalHeaderSize;
            return this;
        }

        public int getIndexBlockSize()
        {
            return indexBlockSize;
        }

        public DbConfig setIndexBlockSize(int dataBlockSize)
        {
            this.indexBlockSize = dataBlockSize;
            return this;
        }
    }
}
