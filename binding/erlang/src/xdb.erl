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
    Result :: list() | {error, term()}.
search(Ip) ->
    case ip2region_util:ip_to_bytes(Ip) of
        {ok, ipv4, IpBin} ->
            do_search(?IP2REGION_POOL_V4, ipv4, IpBin);
        {ok, ipv6, IpBin} ->
            do_search(?IP2REGION_POOL_V6, ipv6, IpBin);
        Ret ->
            Ret
    end.

do_search(PoolName, Version, IpBin) ->
    CacheTable = ip2region_xdb:cache_table(Version),
    case ets:lookup(CacheTable, IpBin) of
        [{_, Region}] -> Region;
        _ ->
            Worker = poolboy:checkout(PoolName, true, infinity),
            try
                case ip2region_worker:search(Worker, IpBin) of
                    {error, _} = Err -> Err;
                    Region ->
                        ets:insert(CacheTable, {IpBin, Region}),
                        Region
                end
            after
                poolboy:checkin(PoolName, Worker)
            end
    end.
