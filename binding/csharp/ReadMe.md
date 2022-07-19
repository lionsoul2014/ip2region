# ip2region csharp 查询客户端实现

# 使用方式

### 引入文件
将 IP2Region\xdb\Header.cs 和 IP2Region\xdb\Searcher.cs 复制到项目文件中即可。

兼容框架：netstandard2.0、netcoreapp3.1、net5.0和net6.0。其余框架请自行测试。

### 完全基于文件的查询
```csharp
// 1、创建 searcher 对象
String dbPath = "ip2region.xdb file path";
Searcher searcher = null;
try
{
	searcher = Searcher.NewWithFileOnly(dbPath);
}
catch (IOException e)
{
	Console.WriteLine("failed to create searcher with `{0}`: {1}", dbPath, e);
	return;
}

// 2、查询
String ip = "1.2.3.4";
try
{
	String region = searcher.Search(ip);
	Console.WriteLine("{{region: {0}, ioCount: {1}}}\n", region, searcher.IOCount);
}
catch (Exception e)
{
	Console.WriteLine("failed to search({0}): {1}\n", ip, e);
}

// 3、备注：并发使用，每个线程需要创建一个独立的 searcher 对象单独使用。
```

### 缓存 `VectorIndex` 索引
```csharp
String dbPath = "ip2region.xdb file path";

// 1、从 dbPath 中预先加载 VectorIndex 缓存，并且把这个得到的数据作为全局变量，后续反复使用。
byte[] vIndex;
try
{
	vIndex = Searcher.LoadVectorIndexFromFile(dbPath);
}
catch (Exception e)
{
	Console.WriteLine("failed to load vector index from `{0}`: {1}", dbPath, e);
	return;
}

// 2、使用全局的 vIndex 创建带 VectorIndex 缓存的查询对象。
Searcher searcher;
try
{
	searcher = Searcher.NewWithVectorIndex(dbPath, vIndex);
}
catch (Exception e)
{
	Console.WriteLine("failed to create vectorIndex cached searcher with `{0}`: {1}", dbPath, e);
	return;
}

// 3、查询
String ip = "1.2.3.4";
try
{
	String region = searcher.Search(ip);
	Console.WriteLine("{{region: {0}, ioCount: {1}}}", region, searcher.IOCount);
}
catch (Exception e)
{
	Console.WriteLine("failed to search({0}): {1}", ip, e);
}

// 备注：每个线程需要单独创建一个独立的 Searcher 对象，但是都共享全局的制度 vIndex 缓存。
```

### 缓存整个 `xdb` 数据

我们也可以预先加载整个 ip2region.xdb 的数据到内存，然后基于这个数据创建查询对象来实现完全基于文件的查询，类似之前的 memory search。

```csharp
String dbPath = "ip2region.xdb file path";

// 1、从 dbPath 加载整个 xdb 到内存。
byte[] cBuff;
try
{
	cBuff = Searcher.LoadContentFromFile(dbPath);
}
catch (Exception e)
{
	Console.WriteLine("failed to load content from `{0}`: {1}", dbPath, e);
	return;
}

// 2、使用上述的 cBuff 创建一个完全基于内存的查询对象。
Searcher searcher;
try
{
	searcher = Searcher.NewWithBuffer(cBuff);
}
catch (Exception e)
{
	Console.WriteLine("failed to create content cached searcher: {0}", e);
	return;
}

// 3、查询
String ip = "1.2.3.4";
try {
	String region = searcher.Search(ip);
	Console.WriteLine("{{region: {0}, ioCount: {1}}}", region, searcher.IOCount);
}
catch (Exception e)
{
	Console.WriteLine("failed to search({0}): {1}", ip, e);
}

// 备注：并发使用，用整个 xdb 数据缓存创建的查询对象可以安全的用于并发，也就是你可以把这个 searcher 对象做成全局对象去跨线程访问。
```

# 编译测试程序
使用 Visual Stuido 打开 IP2Region 解决方案，按“F6”生成测试程序。然后会在 `IP2Region.SearchTest\bin\Debug` 目录下得到一个 `IP2Region.SearchTest.exe` 测试程序。

# 查询测试
可以通过 `IP2Region.SearchTest.exe search` 命令来测试查询：
```bash
IP2Region.SearchTest.exe search
IP2Region.SearchTest.exe search [command options]
options:
 --db string              ip2region binary xdb file path
 --cache-policy string    cache policy: file/vectorIndex/content
```

例如：使用默认的 data/ip2region.xdb 文件进行查询测试：
```bash
IP2Region.SearchTest.exe search --db=../../../../../data/ip2region.xdb
ip2region xdb searcher test program, cachePolicy: vectorIndex
type 'quit' to exit
ip2region>> 1.2.3.4
{region: 美国|0|华盛顿|0|谷歌, ioCount: 7, took: 0 ms}
ip2region>>
```

输入 ip 即可进行查询测试，也可以分别设置 `cache-policy` 为 file/vectorIndex/content 来测试三种不同缓存实现的查询效果。

# bench 测试

可以通过 `IP2Region.SearchTest.exe bench` 命令来进行 bench 测试，一方面确保 `xdb` 文件没有错误，一方面可以评估查询性能：
```bash
IP2Region.SearchTest.exe bench
IP2Region.SearchTest.exe bench [command options]
options:
 --db string              ip2region binary xdb file path
 --src string             source ip text file path
 --cache-policy string    cache policy: file/vectorIndex/content
```

例如：通过默认的 data/ip2region.xdb 和 data/ip.merge.txt 文件进行 bench 测试：
```bash
IP2Region.SearchTest.exe bench --db=../../../../../data/ip2region.xdb --src=../../../../../data/ip.merge.txt
Bench finished, {cachePolicy: vectorIndex, total: 3417955, took: 00:00:48.0082981, cost: 0 ms/op}
```

可以通过分别设置 `cache-policy` 为 file/vectorIndex/content 来测试三种不同缓存实现的效果。
@Note: 注意 bench 使用的 src 文件要是生成对应 xdb 文件相同的源文件。
