-module(ip2region).
-include("ip2region.hrl").

-export([search/1]).

-spec search(Ip :: tuple() | list() | binary()) -> Result :: {error, Reason::atom()} | map().
search(Ip) ->
    case check_ip(Ip) of
        {false, Reason} -> {error, Reason};
    _ ->
        Worker = poolboy:checkout(?IP2REGION_POOL, true, infinity),
        try
            ip2region_worker:search(Worker, Ip)
        after
            poolboy:checkin(?IP2REGION_POOL, Worker)
        end
    end.

check_ip({_A, _B, _C, _D}) -> true;
check_ip(Ip) when is_list(Ip); is_binary(Ip) ->
    IpRegx = "^((\\d|[1-9]\\d|1\\d\\d|2[0-4]\\d|25[0-5])\.){3}(\\d|[1-9]\\d|1\\d\\d|2[0-4]\\d|25[0-5])$",
    case re:run(Ip, IpRegx) of
        {match, _Captured} ->
            ok;
        _ ->
            {false, bad_ip_format}
    end.