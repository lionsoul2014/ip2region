:globe_with_meridians: [中文简体](README_zh.md) | [English](README.md)

# ip2region xdb csharp generation implementation

# Compilation and Installation

Compilation environment: [dotnet6.0](https://dotnet.microsoft.com/zh-cn/download/dotnet/6.0)

```bash
# cd to maker/csharp/IP2RegionMaker directory
dotnet publish -o ./bin
```

Then you will get a packaged file named IP2RegionMaker.dll in the bin directory of the current folder.

# `xdb` Data Generation

Generate the xdb binary file via `dotnet IP2RegionMaker.dll`:

```bash
➜  csharp git:(master) ✗ dotnet IP2RegionMaker.dll
ip2region xdb maker
dotnet IP2RegionMaker.dll [command options]
--src string    source ip text file path
--dst string    destination binary xdb file path
```

For example, using the default data/ipv4_source.txt source data to generate an ip2region_v4.xdb binary file in the current directory:

```bash
➜  csharp git:(master) ✗ dotnet ./IP2RegionMaker/bin/IP2RegionMaker.dll --src=../../data/ipv4_source.txt --dst=./ip2region_v4.xdb
# You will see a lot of output; eventually, you will see the following output indicating the run was successful
...
...
...
write done, dataBlocks: 13804, indexBlocks: (683591, 720221), indexPtr: (982904, 11065984)
Done, elapsed:2.1966620833333335m
```

# Data Query / bench Test

All [bindings](../../binding/) come with query and bench test programs as well as usage documentation. You can use the searcher of your familiar language for query testing or bench testing to confirm the correctness and integrity of the data.
