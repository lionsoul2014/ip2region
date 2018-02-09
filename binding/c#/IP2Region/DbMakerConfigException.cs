//*******************************
// Create By Rocher Kong 
// Github https://github.com/RocherKong
// Date 2018.02.09
//*******************************
namespace IP2Region
{
    public class DbMakerConfigException : System.Exception
    {
        public string ErrorCode { get; set; }
        public DbMakerConfigException(string ErrorCode)
        {
           this.ErrorCode=ErrorCode;
        }
    }
}