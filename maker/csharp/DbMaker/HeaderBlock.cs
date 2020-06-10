namespace DbMaker
{
    /// <summary>
    ///     header block class
    /// </summary>
    public class HeaderBlock
    {
        public HeaderBlock(long indexStartIp, int indexPtr)
        {
            IndexStartIp = indexStartIp;
            IndexPtr = indexPtr;
        }

        /// <summary>
        ///     index block start ip address
        /// </summary>
        public long IndexStartIp { get; set; }

        /// <summary>
        ///     ip address
        /// </summary>
        public int IndexPtr { get; set; }

        public HeaderBlock SetIndexStartIp(long indexStartIp)
        {
            IndexStartIp = indexStartIp;
            return this;
        }

        public HeaderBlock SetIndexPtr(int indexPtr)
        {
            IndexPtr = indexPtr;
            return this;
        }

        /// <summary>
        ///     get the bytes for db storage
        /// </summary>
        /// <returns></returns>
        public byte[] GetBytes()
        {
            /*
             * +------------+-----------+
             * | 4bytes        | 4bytes    |
             * +------------+-----------+
             *  start ip      index ptr
            */

            var b = new byte[8];

            Util.writeIntLong(b, 0, IndexStartIp);
            Util.writeIntLong(b, 4, IndexPtr);

            return b;
        }
    }
}