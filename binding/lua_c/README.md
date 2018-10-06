# Lua ip2region c module binding


### 一，如何安装
* 1, cd到ip2region/binding/lua_c/根目录
* 2, 运行如下命令
```
make
sudo make install
```

* 3, 关于Makefile你可能需要更改里面的VER(版本号)和LIB_DIR(lua的so lib目录)来适应你的系统
```shell
VER = 5.2
CC = gcc
LIBS = -I ../c/ -I /usr/include/lua$(VER)/
FFLAGS = -O2 -Wall -fPIC
SO_FILE = Ip2region.so
LIB_DIR = /usr/local/share/lua/$(VER)
```

### 二，如何测试
* 1, cd到ip2region/binding/lua_c/根目录
* 2, 运行testSearcher测试程序
```shell
lua testSearcher.lua ../../data/ip2region.db
```
* 3, 输入ip地址开始测试即可
```shell
initializing btree
+----------------------------------+
| ip2region test script            |
| Author: chenxin619315@gmail.com  |
| Type 'quit' to exit program      |
+----------------------------------+
ip2region>> 1.2.3.4
0|美国|0|华盛顿|0|0 in 0.100000 millseconds
ip2region>> 101.233.153.103
2163|中国|0|广东省|深圳市|鹏博士 in 0.045000 millseconds
ip2region>>
```


### 三，如何使用
* 1, 先参考第一步安装，把Ip2region.so安装到你系统lua的lib目录下
* 2, 通过如下流程在你的lua程序中使用
```lua
-- 包含模块
local Ip2region = require "Ip2region";

-- 创建查询对象
-- 设置ip2region.db的文件地址，dbFile表示ip2region.db数据库文件的地址
-- 注意new方法是通过“.”调用，而不是“:”
local ip2region = Ip2region.new("ip2region.db file path");

local data;

-- 查询，备注，尽量请使用“:”调用方法，使用“.”需要主动传递ip2region对象参数
-- 1，binary查询
data = ip2region:binarySearch("101.233.153.103");

-- 2，btree查询
data = ip2region:btreeSearch("101.233.153.103");

-- 3，memory查询
data = ip2region:memorySearch("101.233.153.103");

-- 返回结果如下
print("city_id=", data.city_id, "region=", data.region);
```

### 四，备注
* 1，c模块的Ip2region以来binding/c/的实现，请保持binding/c/存在，并且和lua_c模块同目录。
* 2，Ip2region的c模块拥有和c模块几乎等同的性能，生产建议使用lua_c模块代替纯lua模块的使用。
