#-*- coding:utf-8 -*-
"""
" ip2region python seacher client module
"
" Autho: koma<komazhang@foxmail.com>
" Date : 2015-11-06
"""
import struct, sys, os, time
from ip2Region import Ip2Region

def testSearch():
    """
    " ip2region test function
    """
    llen = len(sys.argv)

    if llen < 2:
        print "Usage: python ip2Region.py [ip2region db file] [alrogrithm]"
        print "Algorithm: binary or b-tree"
        return 0

    dbFile    = sys.argv[1]
    method    = 1
    algorithm = "b-tree"
    
    if (not os.path.isfile(dbFile)) or (not os.path.exists(dbFile)):
        print "[Error]: Specified db file is not exists."
        return 0

    if llen > 2:
        algorithm = sys.argv[2]
        if algorithm == "binary":
            method = 2
        elif algorithm == "memory":
            method = 3

    print "initializing %s..." % (algorithm)
    print "+----------------------------------+"
    print "| ip2region test program           |"
    print "| Author: chenxin619315@gmail.com. |"
    print "| Type 'quit' to exit program      |"
    print "+----------------------------------+"

    searcher = Ip2Region(dbFile);

    while True:
        line = raw_input("ip2region>> ")
        line = line.strip()

        if line == "":
            print "[Error]: Invalid ip address."
            continue

        if line == "quit":
            print "[Info]: Thanks for your use, Bye."
            break

        if not searcher.isip(line):
            print "[Error]: Invalid ip address."
            continue

        sTime = time.time() * 1000
        if method == 1:
            data = searcher.btreeSearch(line)
        elif method == 2:
            data = searcher.binarySearch(line)
        else:
            data = searcher.memorySearch(line)
        eTime = time.time() * 1000

        if isinstance(data, dict):
            print "%s|%s in %f millseconds" % (data["city_id"], data["region"], eTime-sTime)
        else:
            print "[Error]: ", data

    searcher.close()

if __name__ == "__main__":
    testSearch()
