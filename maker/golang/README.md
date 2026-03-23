:globe_with_meridians: [中文简体](README_zh.md) | [English](README.md)

# ip2region xdb golang generation implementation

# Program Compilation

Compile to get the xdb_maker executable through the following method:

```
# cd to the golang maker root directory
make
```

After successful compilation, a xdb_maker executable file will be generated in the current directory.

# `xdb` Data Generation

Generate the ip2region.xdb binary file via the `xdb_maker gen` command:

```
./xdb_maker gen [command options]
options:
 --src string           source ip text file path
 --dst string           destination binary xdb file path
 --version string       IP version, options: ipv4/ipv6, specify this flag so you don't get confused 
 --field-list string    field index list imploded with ',' eg: 0,1,2,3-6,7
 --log-level string     set the log level, options: debug/info/warn/error
```

For example, generate the xdb file to the current directory using the default source data under the repository's data/ directory:

```bash
# ipv4 
./xdb_maker gen --src=../../data/ipv4_source.txt --dst=./ip2region_v4.xdb --version=ipv4
# ipv6
./xdb_maker gen --src=../../data/ipv6_source.txt --dst=./ip2region_v6.xdb --version=ipv6
```

For custom data fields during the generation process, please refer to [xdb-文件生成#自定义数据字段](https://ip2region.net/doc/data/xdb_make#field-list)

# `xdb` Data Search

Test the input IP via the `xdb_maker search` command:

```
➜  golang git:(v2.0_xdb) ✗ ./xdb_maker search
./xdb_maker search [command options]
options:
 --db string    ip2region binary xdb file path
```

For example, run a search test using the built-in xdb file:

```bash
# ipv4
./xdb_maker search --db=../../data/ip2region_v4.xdb
ip2region xdb search test program,
source xdb: ../../data/ip2region_v4.xdb (IPv4)
commands:
  loadIndex : load the vector index for search speedup.
  clearIndex: clear the vector index.
  quit      : exit the test program
ip2region>> 58.251.30.115
{region:中国|广东省|深圳市|联通|CN, iocount:2, took:27.893µs}
ip2region>> 1.2.3.4
{region:Australia|Queensland|Brisbane|0|AU, iocount:5, took:58.746µs}

# ipv6
./xdb_maker search --db=../../data/ip2region_v6.xdb
ip2region xdb search test program,
source xdb: ../../data/ip2region_v6.xdb (IPv6)
commands:
  loadIndex : load the vector index for search speedup.
  clearIndex: clear the vector index.
  quit      : exit the test program
ip2region>> 2604:bc80:8001:11a4:ffff:ffff:ffff:ffff
{region:United States|Florida|Miami|velia.net Internetdienste GmbH|US, iocount:15, took:140.942µs}
ip2region>> 240e:3b7:3273:51d0:13f9:bf0:3db1:aa3f
{region:中国|广东省|深圳市|电信|CN, iocount:9, took:67058µs}
```

# `xdb` Data Editing

Edit the raw IP data via the `xdb_maker edit` command:

```
./xdb_maker edit [command options]
options:
 --src string        source ip text file path
 --version string    IP version, options: ipv4/ipv6, specify this flag so you don't get confused
```

For example, opening `./data/ipv4_source.txt` with the editor will show the following operation panel:

```bash
./xdb_maker edit --src=../../data/ipv4_source.txt --version=ipv4
init the editor from source @ `../../data/ipv4_source.txt` ... 
all segments loaded, length: 683591, elapsed: 479.73743ms
command list: 
  put [segment]        : put the specifield $segment
  put_file [file]      : put all the segments from the specified $file
  list [offset] [size] : list the first $size segments start from $offset
  save                 : save all the changes to the destination source file
  quit                 : exit the program
  help                 : print this help menu
editor>>
```

Modify the location information of a specified IP segment using the `put` command, for example:

```bash
editor>> put 36.132.128.0|36.132.147.255|中国|黑龙江省|哈尔滨市|移动|CN
Put(36.132.128.0|36.132.147.255|中国|黑龙江省|哈尔滨市|移动|CN): Ok, with 1 deletes and 2 additions
*editor>> 
```

Batch load modifications from a file using the `put_file` command. The IP segments in the file do not need to be as strict as the data in `./data/ipvx_source.txt`; they do not need to be continuous, and it does not matter if different IP segments overlap. The editor will automatically analyze and process them, for example:

```bash
*editor>> put_file ../../data/sample/ip.test.txt
PutFile(../../data/sample/ip.test.txt): Ok, with 25 deletes and 25 additions
*editor>> 
```

Save modifications using the `save` command. After saving successfully, you can re-generate the xdb from the modified raw IP file using the commands mentioned above:

```bash
*editor>> save
all segments saved to ../../data/ipv4_source.txt
editor>> 
```

# bench Test

If you have generated the `xdb` file yourself, please ensure you run the following `xdb_maker bench` command to verify the correctness of the generated `xdb` file:

```
./xdb_maker bench [command options]
options:
 --db string            ip2region binary xdb file path
 --src string           source ip text file path
 --version string       IP version, options: ipv4/ipv6, specify this flag so you don't get confused 
 --log-level string     set the log level, options: debug/info/warn/error
 --ignore-error bool    keep going if bench failed
```

For example: use the source files under data to bench test the xdb files in data:

```bash
# ipv4
./xdb_maker bench --db=../../data/ip2region_v4.xdb --src=../../data/ipv4_source.txt --version=ipv4

#ipv6
./xdb_maker bench --db=../../data/ip2region_v6.xdb --src=../../data/ipv6_source.txt --version=ipv6
```

*Please note that the `src` file used for the bench test must be the same as the source file used to generate the xdb*.
If an error occurs during execution, it will stop immediately. You can also execute with the `--ignore-error=true` parameter to ignore errors and view the failed statistics at the end.


# Docker

```bash
# Build the image (run in the maker/golang directory).
cd ip2region/maker/golang
docker build -t ip2region-maker .

# Generate IPv4 xdb
docker run --rm -v $(pwd)/../../data:/app/data ip2region-maker \
    gen --src=/app/data/ipv4_source.txt \
        --dst=/app/data/ip2region_v4.xdb \
        --version=ipv4

# Generate IPv6 xdb
docker run --rm -v $(pwd)/../../data:/app/data ip2region-maker \
    gen --src=/app/data/ipv6_source.txt \
        --dst=/app/data/ip2region_v6.xdb \
        --version=ipv6

# Interactive IPv4 Query
docker run -it --rm -v $(pwd)/../../data:/app/data ip2region-maker \
    search --db=/app/data/ip2region_v4.xdb

# Bench test IPv4
docker run --rm -v $(pwd)/../../data:/app/data ip2region-maker \
    bench --db=/app/data/ip2region_v4.xdb \
          --src=/app/data/ipv4_source.txt \
          --version=ipv4
```
