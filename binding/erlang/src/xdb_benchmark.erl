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
	application:ensure_started(ip2region),
    show_hw_sw_info(),
    IpList = load_test_data(DataFile),
    run(IpList).

show_hw_sw_info() ->
    io:format("CPU info:~n", []),
    io:format("~s", [os:cmd("egrep '^model name' /proc/cpuinfo | head -1")]),
    io:format("~s", [os:cmd("egrep '^cache' /proc/cpuinfo | head -1")]),
    io:format("~s", [os:cmd("egrep '^cpu MHz' /proc/cpuinfo | head -1")]),
    io:format("~s", [os:cmd("egrep '^bogomips' /proc/cpuinfo | head -1")]),
    io:format("cores/threads   : ~s~n", [os:cmd("egrep -c '^processor' /proc/cpuinfo")]),
    io:format("Erlang info:~n", []),
    io:format("system_version:~s", [erlang:system_info(system_version)]),
    ok.

load_test_data(DataFile) ->
    {ok, Fd} = file:open(DataFile, [read]),
    T0 = os:timestamp(),
    IpList = load_test_data(Fd, []),
    T1 = os:timestamp(),
    Sec = timer:now_diff(T1, T0) / 1000000,
    io:format("load test data use ~ps~n", [Sec]),
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
    io:format("~nstart run benchmark tests~n", []),
    io:format("~nsearch from file:~n", []),
    run_test(IpList),
    io:format("~nsearch from cache:~n", []),
    run_test(IpList),
    io:format("~nbenchmark test finish~n", []).

run_test(IpList) ->
    T0 = os:timestamp(),
    run_test_aux(IpList),
    T1 = os:timestamp(),
    Sec = timer:now_diff(T1, T0) / 1000000,
    IpCount = length(IpList),
    io:format("ip count:~p,~ntotal time: ~ps,~nsearch ~p times per second,~nuse ~p micro second per search~n", 
        [IpCount, Sec, IpCount / Sec, Sec * 1000000/IpCount]).

run_test_aux([]) -> ok;
run_test_aux([Ip | Tail]) ->
    xdb:search(Ip),
    run_test_aux(Tail).

