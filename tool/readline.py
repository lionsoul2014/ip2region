#!/usr/bin/python
# -*- coding: UTF-8 -*-

filepath = '../data/ip.merge.txt'
opath = "overseas.txt"
cnpath = "cn.txt"
inpath = "inner.txt"
fo = open(opath,"w")
fcn = open(cnpath,"w")
fin = open(inpath, "w")
with open(filepath) as fp:
    line = fp.readline()
    cnt = 1
    while line:
        #print("Line {}: {}".format(cnt, line))
        line = fp.readline()
        cnt += 1
        if "内网" in line:
           print("Line {}: {}".format(cnt, line))
           fin.write(line) 
        elif "中国" in line:
           fcn.write(line)
        else:
           fo.write(line)
    fp.close()
fo.close()
fcn.close()
fin.close()
