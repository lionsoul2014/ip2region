class Ip2Region
  INDEX_BLOCK_LENGTH = 12
  TOTAL_HEADER_LENGTH = 8192
  attr_accessor :f, :headerSip, :headerPtr, :headerLen, :indexSPtr, :indexLPtr, :indexCount, :dbBinStr

  def initialize(dbfile = "../../data/ip2region.db")
    initDatabase(dbfile)
    true
  end

  def initDatabase(dbfile = "ip2region.db")
    @dbBinStr = File.open(dbfile, 'rb').read

    @indexSPtr = getLong(@dbBinStr, 0)
    @indexLPtr = getLong(@dbBinStr, 4)
    @indexCount = ((indexLPtr - indexSPtr) / INDEX_BLOCK_LENGTH).to_i + 1
    # puts "#{@indexSPtr} #{@indexLPtr} #{@indexCount}"
  end

  def memorySearch(ip)
    ip = ip2long(ip)

    l, h, dataPtr = 0, indexCount , 0
    # print(l,h,dataPtr)
    while l <= h
      m  = (l + h) >> 1
      p = m * INDEX_BLOCK_LENGTH + indexSPtr
      # puts "m = #{m}, p = #{p}"
      sip = getLong(@dbBinStr, p)
      # puts "sip =  #{sip}"
            
      if ip < sip
        h = m - 1
      else
        eip = getLong(@dbBinStr, p + 4)
        if ip > eip
          l = m + 1
        else
          dataPtr = getLong(@dbBinStr, p + 8)
          break
        end
      end
    end

    if dataPtr == 0
      return nil
    end

    returnData(dataPtr)
  end

  def returnData(dataPtr)
    dataLen = (dataPtr >> 24) & 0xFF
    dataPtr = dataPtr & 0x00FFFFFF
    data = @dbBinStr[dataPtr, dataLen]
    # @dbBinStr.seek(dataPtr)
    # data = @dbBinStr.read(dataLen)
    
    return {
      :city_id => getLong(data, 0),
      :region => data[4..-1].force_encoding("utf-8")
    }
  end

  def getLong(b, offset)
    if (b[offset..offset+3] || []).length == 4
      # puts "unpack = #{b[offset..offset+3].unpack('I')}"
      return b[offset..offset+3].unpack('I')[0].to_i
    end
    0
  end

  def ip2long(ip)
    ip.split('.').inject(0) {|s,x| s*256 + x.to_i}
  end
end