import ip2Region

if __name__ == '__main__':
    # 1. 预先加载整个 xdb
    dbPath = "./data/ip2region.xdb";
    # vi = ip2Region.Ip2Region.loadVectorIndexFromFile(dbfile="./data/ip2region.xdb")
    cb = ip2Region.Ip2Region.loadContentFromFile(dbfile=dbPath)
    
    # 2. 仅需要使用上面的全文件缓存创建查询对象, 不需要传源 xdb 文件
    searcher = ip2Region.Ip2Region(contentBuff=cb)
    
    # 3. 执行查询
    ip = "1.2.3.4"
    region_str = searcher.searchByIPStr(ip)
    print(region_str)
    
    # 4. 关闭searcher
    searcher.close()