# IP2Region 补丁覆盖工具

一个轻量级的 Python 工具，用于应用 ip2region 数据库补丁，**无需修改**原始的 xdb 数据库文件。该工具创建一个 JSON 缓存，作为覆盖层，在查询主数据库之前检查补丁。

## 为什么需要这个工具？

ip2region 官方项目提供了直接修改 xdb 数据库文件的制作工具（Java/Golang/C++）。但是，有时您可能希望：

- **无需重建**数据库即可应用补丁
- 将补丁与主数据库**分离**
- 在应用程序中将补丁用作**覆盖层**
- 在提交数据库更改之前**测试**补丁

此工具提供了一种非破坏性的补丁应用方法。

## 功能特性

- ✅ 从 `data/fix` 目录解析补丁文件
- ✅ 创建 JSON 缓存以支持快速二分查找
- ✅ UTF-8 编码支持（Windows 系统自动回退）
- ✅ 命令行界面，支持自定义路径
- ✅ IP 查找测试功能
- ✅ 无需外部依赖（仅使用 Python 标准库）

## 安装

### 要求

- Python 3.7 或更高版本
- 无需外部依赖（仅使用标准库）

### 快速开始

1. **下载补丁文件**（从 ip2region 仓库）：
   ```bash
   # 创建补丁目录
   mkdir patches
   
   # 从以下地址下载补丁文件：
   # https://github.com/lionsoul2014/ip2region/tree/master/data/fix
   # 将它们保存为 .fix 文件到 patches/ 目录
   ```

2. **运行工具**：
   ```bash
   python patch_override.py
   ```

3. **在应用程序中使用缓存文件**：
   ```python
   from patch_override import load_patch_cache, find_patch_for_ip
   
   cache = load_patch_cache(Path("patches_cache.json"))
   result = find_patch_for_ip("39.144.0.1", cache)
   if result:
       print(f"省份: {result['province']}, 城市: {result['city']}")
   ```

## 使用方法

### 基本用法

```bash
# 使用默认路径（patches/ 目录，patches_cache.json 输出）
python patch_override.py
```

### 自定义路径

```bash
# 指定自定义目录
python patch_override.py --patches-dir ./data/fix --output ./cache.json
```

### 测试 IP 查找

```bash
# 使用特定 IP 地址进行测试
python patch_override.py --test-ip 39.144.0.1 --test-ip 39.144.10.5
```

### 命令行选项

```
--patches-dir DIR    包含 .fix 补丁文件的目录（默认：patches）
--output FILE        输出 JSON 缓存文件路径（默认：patches_cache.json）
--test-ip IP         用于验证补丁查找的测试 IP 地址（可重复指定）
```

## 补丁文件格式

补丁文件应为以下格式：

```
start_ip|end_ip|Country|Province|City|ISP
```

示例：
```
39.144.0.0|39.144.0.255|中国|山东省|菏泽市|移动
39.144.1.0|39.144.9.255|中国|0|0|移动
```

## 集成示例

以下是如何将此工具集成到您的 IP 地理位置服务中：

```python
from pathlib import Path
from patch_override import load_patch_cache, find_patch_for_ip

class IPGeolocationService:
    def __init__(self):
        self.patch_cache = load_patch_cache(Path("patches_cache.json"))
        # 初始化您的主 ip2region 数据库...
    
    def get_location(self, ip: str):
        # 首先检查补丁（补丁优先）
        patch_result = find_patch_for_ip(ip, self.patch_cache)
        if patch_result:
            return patch_result
        
        # 回退到主数据库
        return self._lookup_main_database(ip)
```

## 缓存文件格式

生成的缓存文件是一个 JSON 文件，结构如下：

```json
{
  "patches": [
    {
      "start_ip": "39.144.0.0",
      "end_ip": "39.144.0.255",
      "start_int": 663748608,
      "end_int": 663748863,
      "country": "中国",
      "province": "山东省",
      "city": "菏泽市",
      "isp": "移动",
      "source": "github-issue-196.fix",
      "line": 1
    }
  ],
  "total_patches": 237,
  "last_updated": "2024-12-28T03:33:13.643784",
  "patch_files": [
    "github-issue-196.fix",
    "github-issue-200.fix",
    "github-issue-243.fix"
  ]
}
```

## 与制作工具对比

| 功能 | 制作工具 | 本工具 |
|------|---------|--------|
| 修改 xdb 文件 | ✅ 是 | ❌ 否 |
| 需要重建 | ✅ 是 | ❌ 否 |
| 非破坏性 | ❌ 否 | ✅ 是 |
| 覆盖层 | ❌ 否 | ✅ 是 |
| 易于更新 | ❌ 否 | ✅ 是 |
| Python 支持 | ❌ 否 | ✅ 是 |

## 更新补丁

当发布新补丁时：

1. 将新的 `.fix` 文件下载到您的 `patches/` 目录
2. 再次运行工具：`python patch_override.py`
3. 缓存将使用所有补丁重新生成
4. 重启您的应用程序以加载新缓存

## 编码支持

该工具支持多种编码：
- UTF-8（主要）
- GBK/GB2312（Windows 回退）
- UTF-8 with BOM

中文字符在缓存文件中正确保留。

## 贡献

此工具旨在贡献给 ip2region 项目。如果您发现错误或有改进建议：

1. Fork ip2region 仓库
2. 将此工具添加到 `maker/python/` 或 `tools/python/`
3. 提交拉取请求

## 许可证

此工具遵循与 ip2region 项目相同的许可证（Apache 2.0）。

## 参考资料

- [ip2region GitHub 仓库](https://github.com/lionsoul2014/ip2region)
- [补丁文件位置](https://github.com/lionsoul2014/ip2region/tree/master/data/fix)
- [制作工具文档](https://github.com/lionsoul2014/ip2region/tree/master/maker)

## 作者

社区贡献 - ip2region 补丁应用的 Python 实现。

## 致谢

基于 lionsoul2014 的 ip2region 项目的补丁格式和概念。

