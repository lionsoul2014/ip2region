%%%-------------------------------------------------------------------
%% Copyright 2022 The Ip2Region Authors. All rights reserved.
%% Use of this source code is governed by a Apache2.0-style
%% license that can be found in the LICENSE file.
%%
%% @doc
%% ip2region xdb client benchmark test, which fails on any region that
%% differs from the source file:
%% erl -noshell -pa ... -run xdb_benchmark main ipv4 DbFile DataFile -s init stop
%% @end
%%%-------------------------------------------------------------------
-module(xdb_benchmark).
-export([main/1]).

main([Version, DbFile, DataFile]) ->
    %% Keep benchmark output clean while still surfacing real errors.
    _ = logger:set_handler_config(default, level, error),
    _ = logger:set_primary_config(level, error),
    ok = application:load(ip2region),
    ok = application:set_env(ip2region, db, [{list_to_atom(Version), filename:absname(DbFile)}]),
    {ok, _} = application:ensure_all_started(ip2region),
    show_hw_sw_info(),
    Segments = load_test_data(DataFile),
    run(Segments).

show_hw_sw_info() ->
    {Model, Clock, Cores, Threads} = cpu_info(),
    io:format("~nSystem:~n", []),
    io:format("  CPU    : ~s", [Model]),
    case Clock of
        "" -> io:format("~n", []);
        _ -> io:format(" @ ~s~n", [Clock])
    end,
    io:format("  Cores  : ~s cores / ~s threads~n", [Cores, Threads]),
    io:format("  Erlang : ~s~n", [string:trim(erlang:system_info(system_version))]),
    ok.

cpu_info() ->
    case os:type() of
        {unix, darwin} ->
            Model = sysctl("machdep.cpu.brand_string"),
            Clock = first_non_empty([
                format_clock(sysctl("hw.cpufrequency")),
                format_clock(sysctl("hw.perflevel0.frequency")),
                format_clock(sysctl("hw.perflevel1.frequency"))
            ]),
            Cores = sysctl("hw.physicalcpu"),
            Threads = sysctl("hw.logicalcpu"),
            {Model, Clock, Cores, Threads};
        {unix, linux} ->
            Model = linux_cpu_field("model name"),
            Clock = format_clock_mhz(linux_cpu_field("cpu MHz")),
            Cores = string:trim(os:cmd("grep -c '^processor' /proc/cpuinfo 2>/dev/null")),
            Threads = Cores,
            {Model, Clock, Cores, Threads};
        _ ->
            {"unknown", "", "?", "?"}
    end.

sysctl(Key) ->
    string:trim(os:cmd("sysctl -n " ++ Key ++ " 2>/dev/null")).

linux_cpu_field(Key) ->
    Cmd = "grep -m1 '^" ++ Key ++ "' /proc/cpuinfo 2>/dev/null | cut -d: -f2- | sed 's/^ *//'",
    string:trim(os:cmd(Cmd)).

first_non_empty(["" | Rest]) -> first_non_empty(Rest);
first_non_empty([Val | _]) -> Val;
first_non_empty([]) -> "".

format_clock(HzStr) ->
    case string:to_integer(string:trim(HzStr)) of
        {Hz, _} when is_integer(Hz), Hz > 1000000000 ->
            lists:flatten(io_lib:format("~.2f GHz", [Hz / 1000000000]));
        {Hz, _} when is_integer(Hz), Hz > 1000000 ->
            lists:flatten(io_lib:format("~.2f MHz", [Hz / 1000000]));
        _ ->
            ""
    end.

format_clock_mhz(MhzStr) ->
    case string:to_float(string:trim(MhzStr)) of
        {Mhz, _} when is_float(Mhz) ->
            lists:flatten(io_lib:format("~.3f GHz", [Mhz / 1000]));
        _ ->
            ""
    end.

load_test_data(DataFile) ->
    {ok, Fd} = file:open(DataFile, [read, binary]),
    T0 = os:timestamp(),
    Segments = load_test_data(Fd, []),
    T1 = os:timestamp(),
    Sec = timer:now_diff(T1, T0) / 1000000,
    io:format("  Loaded : ~p IPs in ~.3f s~n", [2 * length(Segments), Sec]),
    Segments.

load_test_data(Fd, Segments) ->
    case file:read_line(Fd) of
        {ok, Line} ->
            case string:trim(Line) of
                <<>> ->
                    load_test_data(Fd, Segments);
                Segment ->
                    [SIp, Rest] = string:split(Segment, "|"),
                    [EIp, Region] = string:split(Rest, "|"),
                    Expected = unicode:characters_to_nfc_list(Region),
                    load_test_data(Fd, [{SIp, EIp, Expected} | Segments])
            end;
        eof ->
            ok = file:close(Fd),
            lists:reverse(Segments)
    end.

run(Segments) ->
    garbage_collect(),
    io:format("~nBenchmarks:~n", []),
    run_test("cold", Segments),
    run_test("warm", Segments),
    io:format("~nDone.~n", []).

run_test(Label, Segments) ->
    T0 = os:timestamp(),
    run_test_aux(Segments),
    T1 = os:timestamp(),
    Sec = timer:now_diff(T1, T0) / 1000000,
    Count = 2 * length(Segments),
    Qps = Count / Sec,
    MsOp = Sec * 1000 / Count,
    UsOp = Sec * 1000000 / Count,
    io:format(
        "  ~-8s  total=~7.3fs  count=~7w  qps=~12.2f  avg=~9.6f ms/op (~6.3f us/op)~n",
        [Label, Sec, Count, Qps, MsOp, UsOp]
    ).

run_test_aux([]) ->
    ok;
run_test_aux([{SIp, EIp, Region} | Tail]) ->
    check_search(SIp, Region),
    check_search(EIp, Region),
    run_test_aux(Tail).

check_search(Ip, Region) ->
    case xdb:search(Ip) of
        Region -> ok;
        Other -> erlang:error({region_mismatch, Ip, Other, Region})
    end.
