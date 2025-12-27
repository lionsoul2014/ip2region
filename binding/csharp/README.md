# IP2Region.Net

.NET client library for IP2Region

## Installation

Install the package with [NuGet](https://www.nuget.org/packages/IP2Region.Net)

```bash
Install-Package IP2Region.Net
```

## Usage

```csharp
using IP2Region.Net.Abstractions;
using IP2Region.Net.XDB;

ISearcher searcher = new Searcher(CachePolicy , "your xdb file path");
```
### Cache Policy Description
| Cache Policy            | Description                                                                                                | Thread Safe |
|-------------------------|------------------------------------------------------------------------------------------------------------|-------------|
| CachePolicy.Content     | Cache the entire `xdb` data.                                                                               | Yes         |
| CachePolicy.VectorIndex | Cache `vecotorIndex` to speed up queries and reduce system io pressure by reducing one fixed IO operation. | Yes         |
| CachePolicy.File        | Completely file-based queries                                                                              | Yes         |

### XDB File Description
Generate using [maker](https://github.com/lionsoul2014/ip2region/tree/master/maker/csharp), or [download](https://github.com/lionsoul2014/ip2region/blob/master/data/ip2region.xdb) pre-generated xdb files

## ASP.NET Core Usage

```csharp
services.AddIP2RegionService("your xdb file path", cachePolicy: CachePolicy.Content);
```

NET6/7
```csharp
provider.GetRequiredService<ISearcher>()
```

NET8+ support keyed service
```csharp
provider.GetRequiredKeyedService<ISearcher>("IP2Region.Net");
```

## TargetFrameworks
netstandard2.0;netstandard2.1;net6.0;net7.0;net8.0;net9.0;net10.0

## Performance
// * Summary *

BenchmarkDotNet v0.15.6, Windows 11 (10.0.26200.7171)
13th Gen Intel Core i7-13700 2.10GHz, 1 CPU, 24 logical and 16 physical cores
.NET SDK 10.0.100
  [Host]     : .NET 10.0.0 (10.0.0, 10.0.25.52411), X64 RyuJIT x86-64-v3
  DefaultJob : .NET 10.0.0 (10.0.0, 10.0.25.52411), X64 RyuJIT x86-64-v3


| Method      | Mean         | Error      | StdDev     | Gen0   | Allocated |
|------------ |-------------:|-----------:|-----------:|-------:|----------:|
| ContentIPv4 |     53.70 ns |   0.296 ns |   0.277 ns | 0.0086 |     136 B |
| VectorIPv4  |  4,446.04 ns |  18.673 ns |  15.593 ns | 0.0076 |     232 B |
| FileIPv4    |  6,712.40 ns |  15.718 ns |  13.934 ns | 0.0153 |     264 B |
| ContentIPv6 |    145.53 ns |   0.331 ns |   0.277 ns | 0.0126 |     200 B |
| VectorIPv6  |  7,058.39 ns | 125.505 ns | 117.398 ns | 0.0381 |     712 B |
| FileIPv6    | 10,657.97 ns |  53.907 ns |  50.425 ns | 0.0458 |     744 B |

// * Hints *
Outliers
  Benchmarks.VectorIPv4: Default  -> 2 outliers were removed (4.55 us, 4.58 us)
  Benchmarks.FileIPv4: Default    -> 1 outlier  was  removed (6.79 us)
  Benchmarks.ContentIPv6: Default -> 2 outliers were removed (148.08 ns, 152.27 ns)

// * Legends *
  Mean      : Arithmetic mean of all measurements
  Error     : Half of 99.9% confidence interval
  StdDev    : Standard deviation of all measurements
  Gen0      : GC Generation 0 collects per 1000 operations
  Allocated : Allocated memory per single operation (managed only, inclusive, 1KB = 1024B)
  1 ns      : 1 Nanosecond (0.000000001 sec)

// * Diagnostic Output - MemoryDiagnoser *


// ***** BenchmarkRunner: End *****
Run time: 00:02:06 (126.09 sec), executed benchmarks: 6

Global total time: 00:02:13 (133.47 sec), executed benchmarks: 6
// * Artifacts cleanup *
Artifacts cleanup is finished

## Contributing
Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.

Please make sure to update tests as appropriate.

## License
[Apache License 2.0](https://github.com/lionsoul2014/ip2region/blob/master/LICENSE.md)
