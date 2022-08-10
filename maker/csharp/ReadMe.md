# ip2region xdb csharp 生成实现

## 编译安装
编译环境：[dotnet6.0](https://dotnet.microsoft.com/zh-cn/download/dotnet/6.0)
```bash
# cd 到 maker/csharp/IP2RegionMaker目录
dotnet publish -o ./bin
```

然后会在当前目录的 bin 目录下得到一个 IP2RegionMaker.dll 的打包文件。

# 数据生成

通过 `dotnet IP2RegionMaker.dll` 来生成 ip2region.xdb 二进制文件：
```bash  
➜  csharp git:(master) ✗ dotnet IP2RegionMaker.dll
ip2region xdb maker
dotnet IP2RegionMaker.dll [command options]
--src string    source ip text file path
--dst string    destination binary xdb file path
```

例如，通过默认的 data/ip.merge.txt 原数据，在当前目录生成一个 ip2region.xdb 二进制文件：
```bash
➜  csharp git:(master) ✗ dotnet ./IP2RegionMaker/bin/IP2RegionMaker.dll --src=../../data/ip.merge.txt --dst=./ip2region.xdb
# 会看到一堆输出，最终会看到如下输出表示运行成功
...
...
...
write done, dataBlocks: 13804, indexBlocks: (683591, 720221), indexPtr: (982904, 11065984)
Done, elapsed:2.1966620833333335m
```

# 数据 查询/bench 测试

已经完成开发的 [binding](../../binding/) 都有查询和 bench 测试程序以及使用文档，你可以使用你熟悉的语言的 searcher 进行查询测试或者bench测试，来确认数据的正确性和完整性。