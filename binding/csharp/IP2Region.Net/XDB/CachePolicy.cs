// Copyright 2025 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.
// @Author Alan <lzh.shap@gmail.com>
// @Date   2023/07/25
// Updated by Argo Zhang <argo@live.ca> at 2025/11/21

namespace IP2Region.Net.XDB;

/// <summary>
/// 缓存策略枚举
/// </summary>
public enum CachePolicy
{
    /// <summary>
    /// no cache 
    /// </summary>
    File,

    /// <summary>
    /// cache vector index , reduce the number of IO operations
    /// </summary>
    VectorIndex,

    /// <summary>
    /// default cache policy , cache whole xdb file
    /// </summary>
    Content
}
