# Copyright 2022 The Ip2Region Authors. All rights reserved.
# Use of this source code is governed by a Apache2.0-style
# license that can be found in the LICENSE file.
#
#  Created by luckydog on 2022/7/1.
#  Copyright © 2022年 luckydog. All rights reserved.
#

from xdbSearcher import XdbSearcher
import argparse
import time
import sys

def printHelp():
    print("python3 search_test.py [command options]")
    print("options: ")
    print(" --db string             ip2region binary xdb file path")
    print(" --cache-policy string   cache policy: file/vectorIndex/content")


def trim(string):
    if string[:1] != ' ' and string[-1:] != ' ':
        return string
    elif string[:1] == ' ':
        return trim(string[1:])
    else:
        return trim(string[:-1])


def start_search(dbFile="", cachePolicy="vectorIndex"):
    if cachePolicy == "file":
        try:
            searcher = XdbSearcher(dbfile=dbFile)
        except Exception as err:
            print(err)
            return
    elif cachePolicy == "vectorIndex":
        try:
            vi = XdbSearcher.loadVectorIndexFromFile(dbfile=dbFile)
            if vi is None:
                print(f"failed to load vector index from {dbFile}\n")
            searcher = XdbSearcher(dbfile=dbFile, vectorIndex=vi)

        except Exception as err:
            print(err)
            return
    else:
        try:
            cb = XdbSearcher.loadContentFromFile(dbfile=dbFile)
            if cb is None:
                print(f"failed to load xdb content from {dbFile}\n")
            searcher = XdbSearcher(contentBuff=cb)

        except Exception as err:
            print(err)
            return

    # 开始的提示
    print(f"ip2region xdb searcher test program, cachePolicy: {cachePolicy}\ntype 'quit' to exit")
    while True:
        line = trim(input("ip2region>> "))
        # print(f"{line}")

        if len(line) < 2:
            continue
        if line == "quit":
            break

        if not XdbSearcher.isip(None, ip=line):
            print("Error: invalid ip address")
            continue
        start = time.time()

        try:
            region_str = searcher.searchByIPStr(line)
        except Exception as error:
            print(error)
            return

        print(f"{{region: {region_str} , took: {round((time.time()-start)*1000.00, 4)} ms}}")
    # quit
    searcher.close()
    print("searcher test program exited, thanks for trying")


if __name__ == '__main__':
    if len(sys.argv) < 2:
        printHelp()
        exit(0)
    parse = argparse.ArgumentParser()
    parse.add_argument("--db", help="ip2region binary xdb file path")
    parse.add_argument("--cache-policy", choices=["file", "vectorIndex", "content"],
                       help="cache policy: file/vectorIndex/content")
    args = parse.parse_args()
    start_search(dbFile=args.db, cachePolicy=args.cache_policy)
