using System;
using System.Collections.Generic;
using System.Text;

namespace DbMaker
{
    /// <summary>
    /// database configuration class
    /// </summary>
    public class DbConfig
    {
        public DbConfig(int totalHeaderSize)
        {
            if (totalHeaderSize % 8 != 0)
            {
                throw new DbMakerConfigException("totalHeaderSize must be times of 8");
            }

            TotalHeaderSize = totalHeaderSize;
            IndexBlockSize = 8192; //4*2048
        }

        public DbConfig() : this(8 * 2048)
        {

        }

        /// <summary>
        /// total header data block size
        /// </summary>
        public int TotalHeaderSize { get; set; }
        /// <summary>
        ///  max index data block size
        /// u should always choice the fastest read block size 
        /// </summary>
        public int IndexBlockSize { get; set; }
        
        public DbConfig SetTotalHeaderSize(int totalHeaderSize)
        {
            this.TotalHeaderSize = totalHeaderSize;
            return this;
        }
        public DbConfig SetIndexBlockSize(int dataBlockSize)
        {
            this.IndexBlockSize = dataBlockSize;
            return this;
        }
    }
}
