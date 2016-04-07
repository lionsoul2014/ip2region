package org.lionsoul.ip2region;

/**
 * data block class
 * 
 * @author    chenxin<chenxin619315@gmail.com>
*/
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
    
    public DataBlock( int city_id, String region )
    {
        this.city_id = city_id;
        this.region = region;
    }

    public int getCityId()
    {
        return city_id;
    }

    public DataBlock setCityId(int city_id)
    {
        this.city_id = city_id;
        return this;
    }

    public String getRegion()
    {
        return region;
    }

    public DataBlock setRegion(String region)
    {
        this.region = region;
        return this;
    }
    
    @Override
    public String toString()
    {
        StringBuilder sb = new StringBuilder();
        
        sb.append(city_id).append('|').append(region);
        return sb.toString();
    }
}
