%%%-------------------------------------------------------------------
%% Copyright 2022 The Ip2Region Authors. All rights reserved.
%% Use of this source code is governed by a Apache2.0-style
%% license that can be found in the LICENSE file.
%% 
%% @doc 
%% ip2region xdb client benchmark test
%% @end
%%%-------------------------------------------------------------------
-module(xdb_benchmark).
-export([main/1]).

main(DataFile) ->
    %% Keep benchmark output clean while still surfacing real errors.
    _ = logger:set_handler_config(default, level, error),
    _ = logger:set_primary_config(level, error),
    application:ensure_started(ip2region),
    show_hw_sw_info(),
    IpList = load_test_data(DataFile),
    run(IpList).

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
            Cores = trim(os:cmd("grep -c '^processor' /proc/cpuinfo 2>/dev/null")),
            Threads = Cores,
            {Model, Clock, Cores, Threads};
        _ ->
            {"unknown", "", "?", "?"}
    end.

sysctl(Key) ->
    trim(os:cmd("sysctl -n " ++ Key ++ " 2>/dev/null")).

linux_cpu_field(Key) ->
    Cmd = "grep -m1 '^" ++ Key ++ "' /proc/cpuinfo 2>/dev/null | cut -d: -f2- | sed 's/^ *//'",
    trim(os:cmd(Cmd)).

first_non_empty(["" | Rest]) -> first_non_empty(Rest);
first_non_empty([Val | _]) -> Val;
first_non_empty([]) -> "".

format_clock(HzStr) ->
    case string:to_integer(trim(HzStr)) of
        {ok, Hz, _} when Hz > 1000000000 ->
            lists:flatten(io_lib:format("~.2f GHz", [Hz / 1000000000]));
        {ok, Hz, _} when Hz > 1000000 ->
            lists:flatten(io_lib:format("~.2f GHz", [Hz / 1000000000]));
        _ ->
            ""
    end.

format_clock_mhz(MhzStr) ->
    case string:to_float(trim(MhzStr)) of
        {ok, Mhz, _} ->
            lists:flatten(io_lib:format("~.3f GHz", [Mhz / 1000]));
        _ ->
            ""
    end.

load_test_data(DataFile) ->
    {ok, Fd} = file:open(DataFile, [read]),
    T0 = os:timestamp(),
    IpList = load_test_data(Fd, []),
    T1 = os:timestamp(),
    Sec = timer:now_diff(T1, T0) / 1000000,
    io:format("  Loaded : ~p IPs in ~.3f s~n", [length(IpList), Sec]),
    IpList.

load_test_data(Fd, IpList) ->
    case file:read_line(Fd) of
        {ok, Ip} -> 
            case string:tokens(unicode:characters_to_list(Ip), "|") of
            [SIp | _Tail] ->
                load_test_data(Fd, [string:trim(SIp)| IpList]);
            _ ->
                load_test_data(Fd, IpList)
            end;
        _ ->
            file:close(Fd),
            IpList
    end.

run(IpList) ->
    garbage_collect(),
    io:format("~nBenchmarks:~n", []),
    run_test("file", IpList),
    run_test("cache", IpList),
    io:format("~nDone.~n", []).

run_test(Label, IpList) ->
    T0 = os:timestamp(),
    run_test_aux(IpList),
    T1 = os:timestamp(),
    Sec = timer:now_diff(T1, T0) / 1000000,
    Count = length(IpList),
    Qps = Count / Sec,
    MsOp = Sec * 1000 / Count,
    UsOp = Sec * 1000000 / Count,
    io:format("  ~-8s  total=~7.3fs  count=~7w  qps=~12.2f  avg=~9.6f ms/op (~6.3f us/op)~n",
              [Label, Sec, Count, Qps, MsOp, UsOp]).

run_test_aux([]) -> ok;
run_test_aux([Ip | Tail]) ->
    xdb:search(Ip),
    run_test_aux(Tail).

trim(Str) -> string:trim(Str).
