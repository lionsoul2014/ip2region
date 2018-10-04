#-*- coding:utf-8 -*-
"""
" ip2region python seacher client module
"
" Autho: koma<komazhang@foxmail.com>
" Date : 2015-11-06
"""
import struct, sys, os, time
from platform import python_version

from ip2Region import Ip2Region

def testSearch():
    """
    " ip2region test function
    """
    argLen     = len(sys.argv)
    version    = python_version()
    algorithms = ["binary", "b-tree", "memory"]

    if argLen < 2:
        print("Usage: python testSearcher.py [ip2region db file] [alrogrithm]")
        print("Algorithm: %s" % ", ".join(algorithms))
        return 0

    dbFile = sys.argv[1]

    if (not os.path.isfile(dbFile)) or (not os.path.exists(dbFile)):
        print("[Error]: Specified db file is not exists.")
        return 0

    if argLen > 2:
        algorithm = sys.argv[2]
    try:
        algorithms.index(algorithm)
    except Exception as e:
        algorithm = "b-tree"

    print("initializing %s..." % (algorithm))
    print("+----------------------------------+")
    print("| ip2region test program           |")
    print("| Author: chenxin619315@gmail.com. |")
    print("| Type 'quit' to exit program      |")
    print("+----------------------------------+")

    searcher = Ip2Region(dbFile)

    while True:
        if version[:1] == "2":
            line = raw_input("ip2region>> ")
        else:
            line = input("ip2region>> ")
        line = line.strip()

        if line == "":
            print("[Error]: Invalid ip address.")
            continue

        if line == "quit":
            print("[Info]: Thanks for your use, Bye.")
            break

        if not searcher.isip(line):
            print("[Error]: Invalid ip address.")
            continue

        try:
            sTime = time.time()*1000
            if algorithm == "binary":
                data = searcher.binarySearch(line)
            elif algorithm == "memory":
                data = searcher.memorySearch(line)
            else:
                data = searcher.btreeSearch(line)
            eTime = time.time()*1000
            print("%s|%s in %5f millseconds" % (data["city_id"], data["region"].decode('utf-8'), eTime - sTime))
        except Exception as e:
            print("[Error]: %s" % e)

    searcher.close()

if __name__ == "__main__":
    testSearch()
