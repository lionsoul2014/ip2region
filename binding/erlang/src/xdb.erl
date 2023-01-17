
%%%-------------------------------------------------------------------
%% Copyright 2022 The Ip2Region Authors. All rights reserved.
%% Use of this source code is governed by a Apache2.0-style
%% license that can be found in the LICENSE file.
%% 
%% @doc 
%% ip2region xdb client search api
%% @end
%%%-------------------------------------------------------------------
-module(xdb).
-include("ip2region.hrl").

-export([search/1]).

-spec search(Ip :: tuple() | list() | binary()) -> Result :: binary | {error, Reason::atom()}.
search(Ip) when is_integer(Ip); is_list(Ip); is_tuple(Ip); is_binary(Ip) ->
    case ip2region_util:ipv4_to_n(Ip) of
    IntIp when is_integer(IntIp) ->
        case ets:lookup(?IP2REGION_CACHE, IntIp) of
        [{_IntIp, Region}] -> Region;
        _ ->
            Worker = poolboy:checkout(?IP2REGION_POOL, true, infinity),
            try
                ip2region_worker:search(Worker, IntIp)
            after
                poolboy:checkin(?IP2REGION_POOL, Worker)
            end
        end;
    Ret ->
        Ret
    end.
