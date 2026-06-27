-module(ip2region_sup_test).
-include_lib("eunit/include/eunit.hrl").
-include("ip2region.hrl").

pools_started_test() ->
    try application:stop(ip2region) catch _:_ -> ok end,
    try application:unload(ip2region) catch _:_ -> ok end,
    ok = application:load(ip2region),
    %% NOTE: rebar3 compiles tests under _build/test/lib/ip2region via a
    %% symlink back to the source tree, so ?FILE resolves to the original
    %% binding/erlang/test/... path. If rebar3 is configured to copy
    %% instead of symlink, this path traversal must be adjusted.
    TestDir = filename:dirname(?FILE),
    ErlangDir = filename:dirname(TestDir),
    BindingDir = filename:dirname(ErlangDir),
    RepoRoot = filename:dirname(BindingDir),
    V6File = filename:join([RepoRoot, "data", "ip2region_v6.xdb"]),
    ok = application:set_env(ip2region, db, [
        {ipv4, "ip2region.xdb"},
        {ipv6, V6File}
    ]),
    {ok, _} = application:ensure_all_started(ip2region),
    ?assert(is_pid(whereis(?IP2REGION_POOL_V4))),
    ?assert(is_pid(whereis(?IP2REGION_POOL_V6))).
