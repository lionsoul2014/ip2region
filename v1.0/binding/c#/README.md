# IP2Region C# Client

## How To Use
### 1.Install from Nuget. Support .Net Framework (>=4.5) And netstandard 2.0(.net core)
```powershell
Install-Package IP2Region
```
### 2.Init DbSearcher with [the newest DBFile](https://github.com/lionsoul2014/ip2region/blob/master/data/ip2region.db) Downloaded into your project, and invoke your search methods as below:
```csharp
using (var _search = new DbSearcher(Environment.CurrentDirectory + @"\DB\ip2region.db"))
{
    _search.MemorySearch("183.192.62.65").Region;
    _search.BinarySearch("183.192.62.65").Region;
    _search.BtreeSearch("183.192.62.65").Region;
}
```
For async methods (Methods with the suffix "Async"), just put them with `async` and `await`.
If you don't need the `SynchronizedContext`, please use `ConfigureAwait(false)`.
For more about this, please read https://msdn.microsoft.com/en-us/magazine/jj991977.aspx.

```csharp
 public async Task SearchAsync_Test()
        {
            // We don't need the synchronizeContext, so just set to false. 
            // So as for BinarySearchAsync and BtreeSearchAsync
            var memResult = await _search.MemorySearchAsync("183.192.62.65").ConfigureAwait(false);
        }
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


Because the speed of the program lookup has been fast, the asynchronous method here is just the packaging of the synchronization method, the essence is the synchronization method, the asynchronous method is not recommended, and asynchronous will bring the performance loss, unless you need the asynchronous call format.

