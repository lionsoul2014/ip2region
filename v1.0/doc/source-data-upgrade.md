### 数据更新方式

### 数据更新视频录制:
```
part1: https://www.bilibili.com/video/BV1934y1E7Q5/
part2: https://www.bilibili.com/video/BV1pF411j7Aw/
```


### 1、选择一个数据源
```
01、纯真
02、geoip
03、ip.taobao.com
04、其他
```

### 2、标准化数据结构
```
现在的格式：国家|大区|城市|网络运营商
```

### 3、检测更新算法

##### 01、从指定的数据源获取标准的定位信息数据
```
var src_url = 'http://'
func std_data(src) string {
    // stdlize the data
    // eg: 国家|大区|城市|网络运营商
}


// 输入ip，返回标准化的定位信息
func get_remote_region(src_url, ip) string {
    var sd = http_get(src_url+"ip="+ip)
    var fd = std_data(sd)
    // fd 就是标准化后的数据
    // eg: 国家|大区|城市|网络运营商
}
```

##### 02、检测更新流程
```
var handle = fopen("原始基础ip数据")
for l := range handle.lines() {
    var line = {
        sip: 起始IP,
        mip: 中间IP,
        eip: 结束IP,
    }

    // 检测更新
    sRegion = get_remote_region(src_url, line.sip)
    eRegion = get_remote_region(src_url, line.eip)
    if sRegion == eRegion {
        // 整个IP段没有的数据都是一样的
        fwrite(target_file, line.sip+"|"+line.eip+"|"+sRegion)
        return
    }

    
    // 拆分
    line.mip = (line.sip + line.eip) >> 1
    mRegion = get_remote_region(src_url, line.mip)
    if mRegion == sRegion {
        // line.sip|line.mip
    } else if mRegion == eRegion {
        // line.mip + 1|line.eip
    } else {
        // line.mip
        // line.sip|line.mip-1
        // line.mip+1|line.eip
    }
}
```

##### 03、并行更新
```
原始的基础ip段数据拆分成多个文件
part1: 运行一个更新程序
part2: 运行一个更新程序
...


合并全部part为一个最终文件
```


### 4、数据预处理
```
01、连续IP段合并：连续的不同IP段归属同一个地域信息，直接合并，目的是减少整个数据的行数，便于减少db文件的索引数量，减少了IO访问次数，加速访问。
02、广东省|深圳市，去掉省和市关键字，补齐省和市等等，目的是方便后续的地域信息的使用
03、自定义信息预处理，增加其他信息，减少现在的信息
```

### 5、生成 db 文件
```
看README文件
```

### 6、测试 db 文件
```
for l := range source_file.lines() {
    sRegion = db_get(l.sip)
    eRegion = db_get(l.eip)
    if sRegion != eRegion {
        // 报错
    }

    // 正常
}
```
