
%%%===============================================================
%%% @author leihua <leihua918@sina.com>
%%% @doc
%%% ip2region xdb 查询客户端
%%% Created: 2023-1-13 16:53
%%% @end
%%%===============================================================
-module(ip2region).
-include("ip2region.hrl").

-export([search/1]).

-spec search(Ip :: tuple() | list() | binary()) -> Result :: {error, Reason::atom()} | map().
search(Ip) ->
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
