//*******************************
// Create By Rocher Kong 
// Github https://github.com/RocherKong
// Date 2018.02.09
//*******************************
using System;
using System.Text;

namespace IP2Region
{
    public class DataBlock
    {
        /**
         * city id 
        */
        private int city_id;

        /**
         * region address
        */
        private String region;

        /**
         * region ptr in the db file
        */
        private int dataPtr;

        /**
         * construct method
         * 
         * @param  city_id
         * @param  region  region string
         * @param  ptr data ptr
        */
        public DataBlock(int city_id, String region, int dataPtr)
        {
            this.city_id = city_id;
            this.region = region;
            this.dataPtr = dataPtr;
        }

        public DataBlock(int city_id, String region)
        {
            this.city_id = city_id;
            this.region = region;
            this.dataPtr = 0;
        }

        public int GetCityId()
        {
            return city_id;
        }

        public DataBlock SetCityId(int city_id)
        {
            this.city_id = city_id;
            return this;
        }

        public String GetRegion()
        {
            return region;
        }

        public DataBlock SetRegion(String region)
        {
            this.region = region;
            return this;
        }

        public int GetDataPtr()
        {
            return dataPtr;
        }

        public DataBlock SetDataPtr(int dataPtr)
        {
            this.dataPtr = dataPtr;
            return this;
        }

        public override String ToString()
        {
            StringBuilder sb = new StringBuilder();

            sb.Append(city_id).Append('|').Append(region).Append('|').Append(dataPtr);
            return sb.ToString();
        }

    }

}
