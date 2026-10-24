:globe_with_meridians: [中文简体](README_zh.md) | [English](README.md)

# ip2region Logstash filter

`logstash-filter-ip2region` reads an IPv4 or IPv6 address from an event field, looks up its region in the ip2region xdb files and writes the region columns into the event. It runs the `Ip2Region` service from the [Java binding](../java/README.md) inside Logstash, so lookups stay offline and are safe to share across pipeline workers.

# Build and install

The plugin is a Ruby gem that runs on JRuby. Use the JRuby bundled with Logstash:

```bash
export LOGSTASH_PATH=/usr/share/logstash   # your Logstash home
export LOGSTASH_SOURCE=1
cd binding/logstash

$LOGSTASH_PATH/bin/ruby -S bundle install
# download ip2region-{version}.jar from Maven Central into vendor/ and verify its SHA-1
$LOGSTASH_PATH/bin/ruby -S bundle exec rake vendor
$LOGSTASH_PATH/bin/ruby $LOGSTASH_PATH/vendor/jruby/bin/gem build logstash-filter-ip2region.gemspec

$LOGSTASH_PATH/bin/logstash-plugin install logstash-filter-ip2region-1.0.0.gem
```

The jar is packed into the gem, so `logstash-plugin install` works on hosts without network access.

# Configuration

```text
filter {
  ip2region {
    source => "client_ip"
    v4_database => "/etc/logstash/ip2region/ip2region_v4.xdb"
    v6_database => "/etc/logstash/ip2region/ip2region_v6.xdb"
  }
}
```

An event with `"client_ip" => "1.2.3.4"` becomes:

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

| Option            | Type   | Default                                                  | Description                                                                                  |
| ----------------- | ------ | -------------------------------------------------------- | -------------------------------------------------------------------------------------------- |
| `source`          | string | required                                                 | Field holding the IP address.                                                                |
| `target`          | string | `ip2region`                                              | Field the region columns are written into, for example `[client][geo]`.                      |
| `v4_database`     | path   | none                                                     | IPv4 xdb file. Set `v4_database`, `v6_database` or both.                                     |
| `v6_database`     | path   | none                                                     | IPv6 xdb file.                                                                               |
| `v4_cache_policy` | string | `vectorIndex`                                            | `file`, `vectorIndex` or `content`, see the [query modes](../../README.md).                  |
| `v6_cache_policy` | string | `vectorIndex`                                            | Same as `v4_cache_policy`, for the IPv6 xdb file.                                            |
| `searchers`       | number | `20`                                                     | Pooled searchers per xdb file for `file` and `vectorIndex`. Keep it at least `pipeline.workers`. |
| `fields`          | array  | `["country", "province", "city", "isp", "country_code"]` | Names for the `\|`-separated region columns, by position.                                    |
| `tag_on_failure`  | array  | `["_ip2region_lookup_failure"]`                          | Tags added when the address is invalid or has no region.                                     |

The common options such as `add_field`, `add_tag` and `remove_field` are also supported and apply only after a successful lookup.

Lookup rules:
1. Region columns with the value `0` (unknown in the ip2region data) or an empty value are not written.
2. When the region string has more columns than `fields`, the extra columns are ignored. For an xdb file built from your own data, list your column names in `fields`.
3. `source` must hold an IP literal. Hostnames and malformed values get `tag_on_failure` and never trigger DNS lookups.
4. An IPv6 address without `v6_database`, or an IPv4 address without `v4_database`, gets `tag_on_failure`. IPv4-mapped IPv6 addresses such as `::ffff:1.2.3.4` are looked up in the IPv4 xdb file.
5. Events without the `source` field pass through unchanged.

The `content` cache policy loads the whole xdb file into the Logstash heap. Check `-Xmx` in `jvm.options` before using it for the IPv6 file.

# Test

```bash
cd binding/logstash
$LOGSTASH_PATH/bin/ruby -S bundle exec rspec
```

The tests use `data/ip2region_v4.xdb` and `data/ip2region_v6.xdb` from this repository.

The Dockerfile runs the tests in the official Logstash image, installs the gem with `logstash-plugin` and looks up an IPv4 and an IPv6 address in a pipeline. Build it from the repository root, optionally with another Logstash version:

```bash
docker build --file binding/logstash/Dockerfile --build-arg LOGSTASH_VERSION=8.19.23 .
```
