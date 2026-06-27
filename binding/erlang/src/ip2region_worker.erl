%%%-------------------------------------------------------------------
%% Copyright 2022 The Ip2Region Authors. All rights reserved.
%% Use of this source code is governed by a Apache2.0-style
%% license that can be found in the LICENSE file.
%%
%% @doc
%% ip2region xdb client worker, now version-aware (IPv4/IPv6).
%% @end
%%%-------------------------------------------------------------------
-module(ip2region_worker).
-behaviour(gen_server).
-include("ip2region.hrl").

%% API
-export([start/1, stop/1, start_link/1]).
-export([search/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    xdb_fd,
    version :: ipv4 | ipv6,
    segment_index_size :: pos_integer()
}).

%%==========================================
%% API
%% =========================================
start(Args) ->
    Opts = [{spawn_opt, [{min_heap_size, 6000}]}],
    gen_server:start(?MODULE, Args, Opts).

start_link(Args) ->
    Opts = [{spawn_opt, [{min_heap_size, 6000}]}],
    gen_server:start_link(?MODULE, Args, Opts).

stop(Pid) ->
    gen_server:call(Pid, stop).

search(Pid, Ip) ->
    gen_server:call(Pid, {search, Ip}).

%%==========================================
%% gen_server callbacks
%% =========================================
init(Args) ->
    process_flag(trap_exit, true),
    AppName =
        case application:get_application() of
            {ok, AName} -> AName;
            _ -> ?APP_NAME
        end,
    PrivDir = code:priv_dir(AppName),
    XdbFileName =
        case proplists:get_value(xdb_file, Args) of
            undefined -> filename:join([PrivDir, "ip2region.xdb"]);
            Path ->
                case filename:pathtype(Path) of
                    absolute -> Path;
                    _ -> filename:join([PrivDir, Path])
                end
        end,
    error_logger:info_report(io_lib:format("XdbFile:~s~n", [XdbFileName])),
    {ok, IoDevice} = file:open(XdbFileName, [read, binary]),
    {ok, HeaderBin} = file:read(IoDevice, ?XDB_HEADER_SIZE),
    {ok, Header} = ip2region_xdb:parse_header(HeaderBin),
    Version = resolve_version(Header),
    SegmentIndexSize = ip2region_xdb:segment_index_size(Version),
    load_vector_index(IoDevice, Version),
    {ok, #state{
        xdb_fd = IoDevice,
        version = Version,
        segment_index_size = SegmentIndexSize
    }}.

handle_call(Request, From, State) ->
    try
        do_call(Request, From, State)
    catch
        Class:Error:Stacktrace ->
            error_logger:error_report(io_lib:format("~p handle call error, Req:~p ~p, stacktrace:~p~n",
                [?MODULE, Request, {Class, Error}, Stacktrace])),
            {reply, {error, {Class, Error}}, State}
    end.

handle_cast(Msg, State) ->
    try
        do_cast(Msg, State)
    catch
        Class:Error:Stacktrace ->
            error_logger:error_report(io_lib:format("~p handle cast error, Msg:~p, ~p, stacktrace:~w~n",
                [?MODULE, Msg, {Class, Error}, Stacktrace])),
            {noreply, State}
    end.

handle_info(Info, State) ->
    try
        do_info(Info, State)
    catch
        Class:Error:Stacktrace ->
            error_logger:error_report(io_lib:format("~p handle info error, Info:~p, ~p, stacktrace:~p~n",
                [?MODULE, Info, {Class, Error}, Stacktrace])),
            {noreply, State}
    end.

terminate(_Reason, State) ->
    #state{xdb_fd = XdbFd} = State,
    case is_pid(XdbFd) of
        true -> file:close(XdbFd);
        _ -> skip
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%==========================================
%% Internal function
%% =========================================
do_call({search, Ip}, _From, #state{xdb_fd = IoDevice} = State) ->
    Reply = search_ip(IoDevice, Ip, State),
    {reply, Reply, State};

do_call(stop, _From, State) ->
    {stop, normal, stopped, State};

do_call(Request, From, State) ->
    error_logger:error_report(io_lib:format("unknown request: ~p, from:~p", [Request, From])),
    {noreply, State}.

do_cast(Msg, State) ->
    error_logger:error_report(io_lib:format("unknown msg: ~p", [Msg])),
    {noreply, State}.

do_info(Info, State) ->
    error_logger:error_report(io:format("unknown info: ~p", [Info])),
    {noreply, State}.

resolve_version(Header) ->
    case ip2region_xdb:header_version(Header) of
        2 -> ipv4;
        3 ->
            case ip2region_xdb:header_ip_version(Header) of
                ?IP_VERSION_4 -> ipv4;
                ?IP_VERSION_6 -> ipv6;
                _ -> ipv4
            end;
        _ -> ipv4
    end.

load_vector_index(IoDevice, Version) ->
    Table = vector_index_table(Version),
    case ets:info(Table, size) of
        undefined ->
            Opts = [named_table, set, public, {read_concurrency, true}, {keypos, 1}],
            ets:new(Table, Opts),
            load_vector_index_data(IoDevice, Table);
        0 ->
            load_vector_index_data(IoDevice, Table);
        _ ->
            ok
    end.

load_vector_index_data(IoDevice, Table) ->
    {ok, VectorIndexBin} =
        file:read(IoDevice, ?XDB_VECTOR_INDEX_COUNT * 8),
    load_vector_index_aux(VectorIndexBin, 0, Table).

load_vector_index_aux(<<>>, _Index, _Table) -> ok;
load_vector_index_aux(<<SPtr:32/little, EPtr:32/little, VectorIndexBin/binary>>, Index, Table) ->
    ets:insert(Table, {Index, SPtr, EPtr}),
    load_vector_index_aux(VectorIndexBin, Index + 1, Table).

search_ip(IoDevice, IpInt, State) when is_integer(IpInt) ->
    search_ip(IoDevice, <<IpInt:32>>, State);
search_ip(IoDevice, Ip, #state{version = Version, segment_index_size = SegSize}) ->
    CacheTable = cache_table(Version),
    VectorTable = vector_index_table(Version),
    SegmentTable = segment_index_table(Version),
    case ets:lookup(CacheTable, Ip) of
        [{_, RegionInfo}] ->
            RegionInfo;
        _ ->
            <<A:8, B:8, _/binary>> = Ip,
            VectorIdx = A * ?XDB_VECTOR_COLS + B,
            [{_, SPtr, EPtr}] = ets:lookup(VectorTable, VectorIdx),
            RegionInfo = search_ip(IoDevice, Ip, SPtr, EPtr, 0,
                                   (EPtr - SPtr) div SegSize, SegSize, Version, SegmentTable),
            ets:insert_new(CacheTable, {Ip, RegionInfo}),
            RegionInfo
    end.

search_ip(IoDevice, Ip, SPtr, EPtr, Low, High, SegSize, Version, SegmentTable) when Low =< High ->
    Middle = (Low + High) bsr 1,
    SPtr2 = SPtr + Middle * SegSize,
    {SIp, EIp, DataLen, DataPtr} = read_segment_index(IoDevice, SPtr2, SegSize, SegmentTable),
    case ip_in_range(Ip, SIp, EIp, Version) of
        below ->
            search_ip(IoDevice, Ip, SPtr, EPtr, Low, Middle - 1, SegSize, Version, SegmentTable);
        above ->
            search_ip(IoDevice, Ip, SPtr, EPtr, Middle + 1, High, SegSize, Version, SegmentTable);
        inside ->
            {ok, DataBin} = read_file(IoDevice, DataPtr, DataLen),
            unicode:characters_to_nfc_list(DataBin)
    end;
search_ip(_IoDevice, _Ip, _SPtr, _EPtr, _Low, _High, _SegSize, _Version, _SegmentTable) ->
    {error, unknown}.

ip_in_range(Ip, SIp, EIp, ipv4) ->
    <<InputInt:32>> = Ip,
    if
        InputInt < SIp -> below;
        InputInt > EIp -> above;
        true -> inside
    end;
ip_in_range(Ip, SIp, EIp, ipv6) ->
    if
        Ip < SIp -> below;
        Ip > EIp -> above;
        true -> inside
    end.

read_file(IoDevice, Position, DataLength) ->
    file:position(IoDevice, {bof, Position}),
    file:read(IoDevice, DataLength).

read_segment_index(IoDevice, SPtr, SegSize, SegmentTable) ->
    case ets:lookup(SegmentTable, SPtr) of
        [{_SPtr, SIp, EIp, DataLen, DataPtr}] ->
            {SIp, EIp, DataLen, DataPtr};
        _ ->
            {ok, Bin} = read_file(IoDevice, SPtr, SegSize),
            {SIp, EIp, DataLen, DataPtr} = decode_segment_index(Bin, SegSize),
            ets:insert_new(SegmentTable, {SPtr, SIp, EIp, DataLen, DataPtr}),
            {SIp, EIp, DataLen, DataPtr}
    end.

decode_segment_index(Bin, ?XDB_SEGMENT_INDEX_SIZE_V4) ->
    <<SIp:32/little, EIp:32/little, DataLen:16/little, DataPtr:32/little>> = Bin,
    {SIp, EIp, DataLen, DataPtr};
decode_segment_index(Bin, ?XDB_SEGMENT_INDEX_SIZE_V6) ->
    <<SIp:16/binary, EIp:16/binary, DataLen:16/little, DataPtr:32/little>> = Bin,
    {SIp, EIp, DataLen, DataPtr}.

vector_index_table(ipv4) -> ?XDB_VECTOR_INDEX_V4;
vector_index_table(ipv6) -> ?XDB_VECTOR_INDEX_V6.

segment_index_table(ipv4) -> ?XDB_SEGMENT_INDEX_V4;
segment_index_table(ipv6) -> ?XDB_SEGMENT_INDEX_V6.

cache_table(ipv4) -> ?IP2REGION_CACHE_V4;
cache_table(ipv6) -> ?IP2REGION_CACHE_V6.
