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

| Method      | Mean        | Error     | StdDev      | Median      |
|------------ |------------:|----------:|------------:|------------:|
| ContentIPv4 |    101.5 ns |   2.04 ns |     4.57 ns |    103.1 ns |
| VectorIPv4  |  7,488.1 ns | 222.91 ns |   657.26 ns |  7,819.2 ns |
| FileIPv4    | 11,686.9 ns |  59.81 ns |    55.95 ns | 11,707.6 ns |
| ContentIPv6 |    296.1 ns |   1.84 ns |     1.72 ns |    296.2 ns |
| VectorIPv6  | 15,025.1 ns | 938.17 ns | 2,766.21 ns | 16,642.9 ns |
| FileIPv6    | 19,721.0 ns | 807.55 ns | 2,381.08 ns | 20,905.7 ns |

## Contributing
Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.

Please make sure to update tests as appropriate.

## License
[Apache License 2.0](https://github.com/lionsoul2014/ip2region/blob/master/LICENSE.md)
