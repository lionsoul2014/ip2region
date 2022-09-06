# IP2Region.Net

IP2Region c# xdb client

## Installation

Install the package with [NuGet](https://www.nuget.org/packages/IP2Region.Net)


```bash
Install-Package IP2Region.Net
```

## Usage

```csharp
using IP2Region.Net.XDB;

//use default db and cache whole xdb file
Searcher searcher = new Searcher();
searcher.Search("ipaddress value");

/*
 * custom cache policy and xdb file path
 * CachePolicy.Content default cache policy , cache whole xdb file , thread safe
 * CachePolicy.VectorIndex cache vector index , reduce the number of IO operations , not thread safe!
 * CachePolicy.File no cache , not thread safe!
 */
Searcher searcher = new Searcher(CachePolicy.File, "your xdb file path");

```

## ASP.NET Core Usage

```csharp
services.AddSingleton<ISearcher,Searcher>();
```

## Performance

``` ini

BenchmarkDotNet=v0.13.2, OS=Windows 11 (10.0.22000.856/21H2)
AMD Ryzen 5 3550H with Radeon Vega Mobile Gfx, 1 CPU, 8 logical and 4 physical cores
.NET SDK=6.0.400
  [Host]     : .NET 6.0.8 (6.0.822.36306), X64 RyuJIT AVX2
  DefaultJob : .NET 6.0.8 (6.0.822.36306), X64 RyuJIT AVX2


```
|                  Method |        Mean |     Error |    StdDev |
|------------------------ |------------:|----------:|----------:|
|     CachePolicy_Content |    224.6 ns |   4.44 ns |   7.41 ns |
| CachePolicy_VectorIndex | 11,648.4 ns | 231.98 ns | 457.91 ns |



## Contributing
Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.

Please make sure to update tests as appropriate.

## License
[Apache License 2.0](https://github.com/lionsoul2014/ip2region/blob/master/LICENSE.md)