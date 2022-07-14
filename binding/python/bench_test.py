# Copyright 2022 The Ip2Region Authors. All rights reserved.
# Use of this source code is governed by a Apache2.0-style
# license that can be found in the LICENSE file.
#
#  Created by luckydog on 2022/7/1.
#  Copyright © 2022年 luckydog. All rights reserved.
#
# from ast import main
import io

from xdbSearcher import XdbSearcher
import argparse
import time
import sys


def printHelp():
    print("python bench_test.py [command options]")
    print("options: ")
    print(" --db string             ip2region binary xdb file path")
    print(" --src string            source ip text file path")
    print(" --cache-policy string   cache policy: file/vectorIndex/content")

def trim(string):
    if string[:1] != ' ' and string[-1:] != ' ':
        return string
    elif string[:1] == ' ':
        return trim(string[1:])
    else:
        return trim(string[:-1])

def start_bench(dbFile="", srcFile="", cachePolicy="vectorIndex"):
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
    # do the bench test

    try:
        count = 0
        costs = 0
        sTime = time.time()
        f = io.open(srcFile, "rb")
        while True:
            line = trim(f.readline(1024)).decode("utf-8").replace("\n", "")
            if len(line) < 1:
                break
            
            ps = line.split("|",2)
            if len(ps) != 3:
                print(f"invalid ip segment line :{line}")
                return
            sip = XdbSearcher.ip2long(None, ps[0])
            eip = XdbSearcher.ip2long(None, ps[1])
            
            if sip > eip:
                print(f"start ip({ps[0]}) should not be greater than end ip({ps[1]})")
                return
            
            mip = (sip + eip) >> 1
            
            for ip in [sip, (sip + mip) >> 1, mip, (mip + eip) >> 1, eip]:
                try:
                    cTime = time.time()
                    region = searcher.search(ip)
                    costs = costs + (time.time() - cTime)
                except Exception as error:
                    print(f"failed to search ip :{ip}")
                    return

                if region is None:
                    print(f"failed to search ip :{ip}")
                    return
                if region != ps[2]:
                    print(f"failed search({ip}) with ({region} != {ps[2]})")
                    return
                count = count + 1
                
        # close the searcher at last
        f.close()
        searcher.close()
        print(f"Bench finished, {{cachePolicy: {cachePolicy}, total: {count}, took: {round(time.time() - sTime, 2)} s, cost: {round(costs/count*1000, 4)} ms/op}}")
    except Exception as err:
        print(f"failed to open source text file :{err}")
        return


if __name__ == '__main__':
    if len(sys.argv) < 2:
        printHelp()
        exit(0)
    parse = argparse.ArgumentParser()
    parse.add_argument("--db", help="ip2region binary xdb file path")
    parse.add_argument("--src", help="source ip text file path")
    parse.add_argument("--cache-policy", choices=["file", "vectorIndex", "content"],
                       help="cache policy: file/vectorIndex/content")
    args = parse.parse_args()
    start_bench(dbFile=args.db, srcFile=args.src, cachePolicy=args.cache_policy)
