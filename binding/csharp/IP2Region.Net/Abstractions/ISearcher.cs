// Copyright 2023 The Ip2Region Authors. All rights reserved.
// Use of this source code is governed by a Apache2.0-style
// license that can be found in the LICENSE file.
// @Author Alan <lzh.shap@gmail.com>
// @Date   2023/07/25

using System.Net;

namespace IP2Region.Net.Abstractions;

public interface ISearcher
{
    string? Search(string ipStr);

    string? Search(IPAddress ipAddress);

    string? Search(uint ipAddress);

    int IoCount { get; }
}