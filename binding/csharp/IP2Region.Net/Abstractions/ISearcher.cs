// Copyright 2025 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.
// @Author Alan <lzh.shap@gmail.com>
// @Date   2023/07/25
// Updated by Argo Zhang <argo@live.ca> at 2025/11/21

using System.Net;

namespace IP2Region.Net.Abstractions;

/// <summary>
/// IP 转化为地理位置搜索器接口
/// </summary>
public interface ISearcher : IDisposable
{
    /// <summary>
    /// 搜索方法
    /// </summary>
    /// <param name="ipStr">IP 地址字符串 如 192.168.0.1</param>
    /// <returns></returns>
    string? Search(string ipStr);

    /// <summary>
    /// 搜索方法
    /// </summary>
    string? Search(IPAddress ipAddress);

    /// <summary>
    /// 搜索方法 仅限 IPv4 使用
    /// </summary>
    /// <param name="ipAddress">IPv4 地址字节数组小端读取 uint 数值</param>
    [Obsolete("已弃用，请改用其他方法；Deprecated; please use Search(string) or Search(IPAddress) method.")]
    string? Search(uint ipAddress);

    /// <summary>
    /// 获得 内部 IO 访问次数
    /// </summary>
    int IoCount { get; }
}
