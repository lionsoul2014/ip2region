package org.lionsoul.ip2region;

/**
 * configuration exception
 * 
 * @author chenxin<chenxin619315@gmail.com>
*/
public class DbMakerConfigException extends Exception
{
    private static final long serialVersionUID = 4495714680349884838L;
    
    public DbMakerConfigException( String info )
    {
        super(info);
    }
    
    public DbMakerConfigException( Throwable res )
    {
        super(res);
    }
    
    public DbMakerConfigException( String info, Throwable res )
    {
        super(info, res);
    }
}
