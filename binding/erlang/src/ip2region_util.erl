%%%-------------------------------------------------------------------
%% Copyright 2022 The Ip2Region Authors. All rights reserved.
%% Use of this source code is governed by a Apache2.0-style
%% license that can be found in the LICENSE file.
%%
%% @doc
%% ip2region utils: IPv4/IPv6 parsing and version detection.
%% @end
%%%-------------------------------------------------------------------
-module(ip2region_util).

-export([ipv4_to_n/1, ip_version/1, ip_to_bytes/1]).

-spec ip_version(Ip :: tuple() | list() | binary() | integer()) ->
    ipv4 | ipv6 | {error, atom()}.
ip_version(Ip) when is_integer(Ip) -> ipv4;
ip_version({_, _, _, _}) -> ipv4;
ip_version({_, _, _, _, _, _, _, _}) -> ipv6;
ip_version(Ip) when is_binary(Ip) ->
    ip_version(binary_to_list(Ip));
ip_version(Ip) when is_list(Ip) ->
    case inet_parse:address(Ip) of
        {ok, {_, _, _, _}} -> ipv4;
        {ok, {_, _, _, _, _, _, _, _}} -> ipv6;
        _ -> {error, bad_ip_format}
    end;
ip_version(_) ->
    {error, bad_ip_format}.

-spec ip_to_bytes(Ip :: tuple() | list() | binary() | integer()) ->
    {ok, ipv4 | ipv6, binary()} | {error, atom()}.
ip_to_bytes(Ip) when is_integer(Ip) ->
    {ok, ipv4, <<Ip:32>>};
ip_to_bytes({A, B, C, D}) ->
    {ok, ipv4, <<A, B, C, D>>};
ip_to_bytes({A, B, C, D, E, F, G, H}) ->
    {ok, ipv6, <<A:16, B:16, C:16, D:16, E:16, F:16, G:16, H:16>>};
ip_to_bytes(Ip) when is_binary(Ip) ->
    ip_to_bytes(binary_to_list(Ip));
ip_to_bytes(Ip) when is_list(Ip) ->
    case inet_parse:address(Ip) of
        {ok, {A, B, C, D}} ->
            {ok, ipv4, <<A, B, C, D>>};
        {ok, {A, B, C, D, E, F, G, H}} ->
            {ok, ipv6, <<A:16, B:16, C:16, D:16, E:16, F:16, G:16, H:16>>};
        _ ->
            {error, bad_ip_format}
    end;
ip_to_bytes(_) ->
    {error, bad_ip_format}.

-spec ipv4_to_n(Ip :: tuple() | list() | binary() | integer()) ->
    non_neg_integer() | {error, atom()}.
ipv4_to_n(IntIp) when is_integer(IntIp) -> IntIp;
ipv4_to_n({A, B, C, D}) ->
    <<N:32>> = <<A, B, C, D>>,
    N;
ipv4_to_n(Ip) when is_binary(Ip) ->
    ipv4_to_n(binary_to_list(Ip));
ipv4_to_n(Ip) when is_list(Ip) ->
    case ip_to_bytes(Ip) of
        {ok, ipv4, <<N:32>>} -> N;
        _ -> {error, bad_ip_format}
    end;
ipv4_to_n(_) ->
    {error, bad_ip_format}.
