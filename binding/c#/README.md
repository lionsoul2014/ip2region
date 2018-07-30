# IP2Region C# Client

## How To Use
### 1.Install from Nuget. Support .Net Framework 4.5+ And netstandard 2.0(.net core)
```powershell
Install-Package IP2Region
```
### 2.Init DbSearcher with Newest DBFile Downloaded into your project.(https://github.com/lionsoul2014/ip2region/blob/master/data/ip2region.db)
```c#
DbSearcher _search=new DbSearcher(Environment.CurrentDirectory + @"\DB\ip2region.db");
```
### 3.Invoke Search Method.(MemorySearch,BtreeSearch,BinarySearch)
```c#
    _search.MemorySearch("183.192.62.65").Region;
    _search.MemorySearchAsync("183.192.62.65").Result.Region;
    _search.BinarySearch("183.192.62.65").Region;
    _search.BinarySearchAsync("183.192.62.65").Result.Region;
    _search.BtreeSearch("183.192.62.65").Region;
    _search.BtreeSearchAsync("183.192.62.65").Result.Region;
```

## Test Result(From /IP2Region.Test.Benchmark)
 Method |     Mean |     Error |    StdDev | Rank |
------------------- |---------: |----------: |----------: |-----: |
MemorySearch | 10.66 us | 0.1424 us | 0.1332 us |    1 |
MemorySearch_Async | 10.90 us | 0.2060 us | 0.2023 us |    2 |
BinarySearch | 52.22 us | 0.6403 us | 0.5347 us |    5 |
BinarySearch_Async | 53.03 us | 1.0271 us | 0.9608 us |    6 |
BtreeSearch | 19.05 us | 0.2464 us | 0.2305 us |    3 |
BtreeSearch_Async | 19.40 us | 0.3820 us | 0.6690 us |    4 |

## Contribute History
| Name | Github | Responsibility | Date | Remark |
| ------ | ------ | ------ | ------ | ------ |
| RocherKong | https://github.com/RocherKong | Creator | 20180209|
| Dongwei | https://github.com/Maledong | Contributor | 20180708 | 1.Async 2.NetFxBenchmark 3.Rename of some Methods
| RocherKong | https://github.com/RocherKong | Creator | 20180209| 1.CodeStandardized 2.Support Netfx4.5 3.TestStandardized

