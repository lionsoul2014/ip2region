using System;
namespace IP2RegionDotNetDbMaker
{

    /**
     * configuration exception
     * 
     * @author chenxin<chenxin619315@gmail.com>
*/
    public class DbMakerConfigException : Exception
    {
        public DbMakerConfigException(string info) : base(info)
        {

        }
    }
}
