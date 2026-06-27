%%%-------------------------------------------------------------------
%% Copyright 2022 The Ip2Region Authors. All rights reserved.
%% Use of this source code is governed by a Apache2.0-style
%% license that can be found in the LICENSE file.
%%
%% @doc
%% ip2region xdb client search api with IPv4/IPv6 auto-routing.
%% @end
%%%-------------------------------------------------------------------
-module(xdb).
-include("ip2region.hrl").

-export([search/1]).

-spec search(Ip :: tuple() | list() | binary() | integer()) ->
    Result :: list() | {error, Reason::atom()}.
search(Ip) ->
    case ip2region_util:ip_to_bytes(Ip) of
        {ok, ipv4, IpBin} ->
            do_search(v4_pool(), ipv4, IpBin);
        {ok, ipv6, IpBin} ->
            do_search(?IP2REGION_POOL_V6, ipv6, IpBin);
        Ret ->
            Ret
    end.

do_search(PoolName, Version, IpBin) ->
    CacheTable = cache_table(Version),
    case ets:lookup(CacheTable, IpBin) of
        [{_, Region}] -> Region;
        _ ->
            Worker = poolboy:checkout(PoolName, true, infinity),
            try
                ip2region_worker:search(Worker, IpBin)
            after
                poolboy:checkin(PoolName, Worker)
            end
    end.

v4_pool() ->
    case whereis(?IP2REGION_POOL) of
        Pid when is_pid(Pid) -> ?IP2REGION_POOL;
        undefined -> ?IP2REGION_POOL_V4
    end.

cache_table(ipv4) -> ?IP2REGION_CACHE_V4;
cache_table(ipv6) -> ?IP2REGION_CACHE_V6.
