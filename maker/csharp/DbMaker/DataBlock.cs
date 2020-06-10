using System;

namespace DbMaker
{
    /// <summary>
    ///     data block class
    /// </summary>
    public class DataBlock
    {
        /// <summary>
        ///     construct method
        /// </summary>
        /// <param name="cityId"></param>
        /// <param name="region"></param>
        /// <param name="dataPtr"></param>
        public DataBlock(int cityId, string region, int dataPtr)
        {
            CityId = cityId;
            Region = region;
            DataPtr = dataPtr;
        }

        public DataBlock(int cityId, string region) : this(cityId, region, 0)
        {
        }

        /// <summary>
        ///     city id
        /// </summary>
        public int CityId { get; set; }

        /// <summary>
        ///     region address
        /// </summary>
        public string Region { get; set; }

        /// <summary>
        ///     region ptr in the db file
        /// </summary>
        public int DataPtr { get; set; }

        public override string ToString()
        {
            return String.Join("|", this.CityId, this.Region, this.DataPtr);
        }
    }
}