-module(ip2region_test_util).

-export([
    repo_root/0,
    v6_xdb_path/0,
    default_db_config/0,
    reset_app/0,
    load_app/0,
    set_db_config/1,
    start_app/0,
    start_app/1,
    stop_app/0
]).

repo_root() ->
    TestDir = filename:dirname(?FILE),
    ErlangDir = filename:dirname(TestDir),
    BindingDir = filename:dirname(ErlangDir),
    filename:dirname(BindingDir).

v6_xdb_path() ->
    filename:join([repo_root(), "data", "ip2region_v6.xdb"]).

default_db_config() ->
    [
        {ipv4, "ip2region.xdb"},
        {ipv6, v6_xdb_path()}
    ].

reset_app() ->
    try application:stop(ip2region) catch _:_ -> ok end,
    try application:unload(ip2region) catch _:_ -> ok end,
    ok.

load_app() ->
    ok = application:load(ip2region).

set_db_config(Config) ->
    ok = application:set_env(ip2region, db, Config).

start_app() ->
    start_app(default_db_config()).

start_app(Config) ->
    reset_app(),
    load_app(),
    set_db_config(Config),
    application:ensure_all_started(ip2region).

stop_app() ->
    application:stop(ip2region).
