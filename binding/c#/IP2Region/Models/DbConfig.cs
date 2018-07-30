//*******************************
// Created By Rocher Kong 
// Github https://github.com/RocherKong
// Date 2018.02.09
//*******************************
using System;

namespace IP2Region.Models
{
    public class DbMakerConfigException : Exception
    {
        public string ErrMsg { get; private set; }
        public DbMakerConfigException(string errMsg)
        {
            ErrMsg = errMsg;
        }
    }

    public class DbConfig
    {
        public int TotalHeaderSize
        {
            get;
            private set;
        }

        public int indexBlockSize
        {
            get;
            private set;
        }

        public DbConfig(int totalHeaderSize)
        {
            if ((totalHeaderSize % 8) != 0)
            {
                throw new DbMakerConfigException("totalHeaderSize must be times of 8");
            }
            TotalHeaderSize = totalHeaderSize;
            //4 * 2048
            indexBlockSize = 8192; 
        }

        public DbConfig():this(8 * 2048)
        {
        }
    }

}