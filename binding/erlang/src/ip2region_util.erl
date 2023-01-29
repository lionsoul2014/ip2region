%%%-------------------------------------------------------------------
%% Copyright 2022 The Ip2Region Authors. All rights reserved.
%% Use of this source code is governed by a Apache2.0-style
%% license that can be found in the LICENSE file.
%% 
%% @doc 
%% ip2region utils
%% @end
%%%-------------------------------------------------------------------
-module(ip2region_util).
-export([ipv4_to_n/1]).


ipv4_to_n(IntIp) when is_integer(IntIp) -> IntIp;
ipv4_to_n({A, B, C, D}) ->
	<<N:32>> = <<A, B, C, D>>,
	N;
ipv4_to_n(Ip) when is_binary(Ip) ->
	ipv4_to_n(binary_to_list(Ip));
ipv4_to_n(Ip) when is_list(Ip) ->
    case inet_parse:address(Ip) of
        {ok, Addr} ->
            ipv4_to_n(Addr);
        _ ->
            {error, bad_ip_format}
    end.