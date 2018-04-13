package org.lionsoul.ip2region;

/**
 * data block class
 * 
 * @author	chenxin<chenxin619315@gmail.com>
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
	public DataBlock( int city_id, String region, int dataPtr )
	{
		this.city_id = city_id;
		this.region  = region;
		this.dataPtr = dataPtr;
	}
	
	public DataBlock(int city_id, String region)
	{
	    this(city_id, region, 0);
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
	
	public int getDataPtr()
	{
	    return dataPtr;
	}
	
	public DataBlock setDataPtr(int dataPtr)
	{
	    this.dataPtr = dataPtr;
	    return this;
	}
	
	@Override
	public String toString()
	{
		StringBuilder sb = new StringBuilder();
		
		sb.append(city_id).append('|').append(region).append('|').append(dataPtr);
		return sb.toString();
	}

}
