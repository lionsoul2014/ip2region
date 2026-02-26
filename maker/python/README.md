:globe_with_meridians: [中文简体](README_zh.md) | [English](README.md)

# ip2region xdb python generation implementation

# Cli Command

```
# cd to the python maker root directory
> python main.py
ip2region xdb maker
main.py [command] [command options]
Command:
  gen      generate the binary db file
```

# `xdb` Data Generation

Generate the ip2region_v4.xdb binary file via the `python main.py gen` command:

```
➜  python git:(v2.0_xdb) ✗ python main.py gen
main.py gen [command options]
options:
 --src string    source ip text file path
 --dst string    destination binary xdb file path
```

For example, using the default data/ipv4_source.txt as the source data to generate an ip2region_v4.xdb in the current directory:

```
➜  python git:(v2.0_xdb) ✗ python main.py gen --src=../../data/ipv4_source.txt --dst=./ip2region_v4.xdb
# You will see a lot of output; eventually, you will see something like the following output indicating the end of the execution
...
2022-07-13 19:58:00,540-root-238-INFO - write done, dataBlocks: 13804, indexBlocks: (683591, 720221), indexPtr: (982904, 11065984)
2022-07-13 19:58:00,540-root-63-INFO - Done, elapsed: 3m3s
```

# `xdb` Data Query and bench Test

For query functions and testing based on the xdb format, see ip2region [bindings](../../binding)
