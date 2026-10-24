:globe_with_meridians: [中文简体](README_zh.md) | [English](README.md)

# ip2region Logstash 过滤器

`logstash-filter-ip2region` 从事件字段读取 IPv4 或 IPv6 地址，在 ip2region 的 xdb 文件中查询所属地区，并把地区的各列写回事件。插件在 Logstash 内部使用 [Java binding](../java/README_zh.md) 的 `Ip2Region` 查询服务，查询完全离线，并且可以安全地在多个 pipeline worker 之间共享。

# 编译和安装

插件是一个运行在 JRuby 上的 Ruby gem，使用 Logstash 自带的 JRuby 执行以下命令：

```bash
export LOGSTASH_PATH=/usr/share/logstash   # Logstash 的安装目录
export LOGSTASH_SOURCE=1
cd binding/logstash

$LOGSTASH_PATH/bin/ruby -S bundle install
# 从 Maven Central 下载 ip2region-{version}.jar 到 vendor/ 并校验 SHA-1
$LOGSTASH_PATH/bin/ruby -S bundle exec rake vendor
$LOGSTASH_PATH/bin/ruby $LOGSTASH_PATH/vendor/jruby/bin/gem build logstash-filter-ip2region.gemspec

$LOGSTASH_PATH/bin/logstash-plugin install logstash-filter-ip2region-1.0.0.gem
```

jar 文件会被打包进 gem，没有外网的机器也可以直接用 `logstash-plugin install` 安装。

# 配置

```text
filter {
  ip2region {
    source => "client_ip"
    v4_database => "/etc/logstash/ip2region/ip2region_v4.xdb"
    v6_database => "/etc/logstash/ip2region/ip2region_v6.xdb"
  }
}
```

`"client_ip" => "1.2.3.4"` 的事件处理后变为：

```text
{
     "client_ip" => "1.2.3.4",
     "ip2region" => {
             "country" => "Australia",
            "province" => "Queensland",
                "city" => "Brisbane",
        "country_code" => "AU"
    }
}
```

| 选项                | 类型   | 默认值                                                   | 说明                                                                                 |
| ------------------- | ------ | -------------------------------------------------------- | ------------------------------------------------------------------------------------ |
| `source`          | string | 必填                                                     | 存放 IP 地址的字段。                                                                 |
| `target`          | string | `ip2region`                                              | 写入地区各列的字段，例如 `[client][geo]`。                                           |
| `v4_database`     | path   | 无                                                       | IPv4 xdb 文件。`v4_database` 和 `v6_database` 至少设置一个。                         |
| `v6_database`     | path   | 无                                                       | IPv6 xdb 文件。                                                                      |
| `v4_cache_policy` | string | `vectorIndex`                                            | `file`、`vectorIndex` 或 `content`，参考[查询模式](../../README_zh.md)。             |
| `v6_cache_policy` | string | `vectorIndex`                                            | 同 `v4_cache_policy`，作用于 IPv6 xdb 文件。                                         |
| `searchers`       | number | `20`                                                     | `file` 和 `vectorIndex` 策略下每个 xdb 文件的查询器数量，建议不小于 `pipeline.workers`。 |
| `fields`          | array  | `["country", "province", "city", "isp", "country_code"]` | 按位置给 `\|` 分隔的地区各列命名。                                                   |
| `tag_on_failure`  | array  | `["_ip2region_lookup_failure"]`                          | 地址无效或查不到地区时添加的标签。                                                   |

插件同样支持 `add_field`、`add_tag`、`remove_field` 等通用选项，它们只在查询成功后生效。

查询规则：
1. 值为 `0`（ip2region 数据中表示未知）或空的列不会写入事件。
2. 地区字符串的列数多于 `fields` 时，多出的列会被忽略。使用自定义数据生成的 xdb 文件时，请在 `fields` 中列出自己的列名。
3. `source` 必须是 IP 字面量。域名和格式错误的值会被打上 `tag_on_failure` 标签，不会触发 DNS 查询。
4. 没有配置 `v6_database` 时的 IPv6 地址，或者没有配置 `v4_database` 时的 IPv4 地址，会被打上 `tag_on_failure` 标签。`::ffff:1.2.3.4` 这类 IPv4 映射的 IPv6 地址会查询 IPv4 xdb 文件。
5. 没有 `source` 字段的事件原样通过。

`content` 缓存策略会把整个 xdb 文件加载到 Logstash 的堆内存中，对 IPv6 文件使用前请检查 `jvm.options` 里的 `-Xmx` 设置。

# 测试

```bash
cd binding/logstash
$LOGSTASH_PATH/bin/ruby -S bundle exec rspec
```

测试使用本仓库的 `data/ip2region_v4.xdb` 和 `data/ip2region_v6.xdb`。

Dockerfile 会在官方 Logstash 镜像中运行测试，用 `logstash-plugin` 安装 gem，并在 pipeline 中查询一个 IPv4 和一个 IPv6 地址。在仓库根目录构建，可以用 `LOGSTASH_VERSION` 指定其他 Logstash 版本：

```bash
docker build --file binding/logstash/Dockerfile --build-arg LOGSTASH_VERSION=8.19.23 .
```
