%%%-------------------------------------------------------------------
%% Copyright 2022 The Ip2Region Authors. All rights reserved.
%% Use of this source code is governed by a Apache2.0-style
%% license that can be found in the LICENSE file.
%% 
%% @doc 
%% ip2region xdb client worker
%% @end
%%%-------------------------------------------------------------------
-module(ip2region_worker).
-behaviour(gen_server).
-include("ip2region.hrl").

%% API
-export([start/1, stop/1, start_link/1]).
-export([search/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {xdb_fd}).

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
init(_Args) ->
    process_flag(trap_exit, true),
    AppName = 
        case application:get_application() of
            {ok, AName} -> AName;
            _ -> ?APP_NAME
        end,
    PrivDir = code:priv_dir(AppName),
    XdbFileName = filename:join([PrivDir, "ip2region.xdb"]),
    error_logger:info_report(io_lib:format("XdbFile:~s~n", [XdbFileName])),
    {ok, IoDevice} = file:open(XdbFileName, [read, binary]),
    load_vector_index(IoDevice),
    {ok, #state{xdb_fd = IoDevice}}.

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
        true ->
            file:close(XdbFd);
        _ ->
            skip
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%==========================================
%% Internal function
%% =========================================
do_call({search, Ip}, _From, #state{xdb_fd = IoDevice} = State) ->
    Reply = search_ip(IoDevice, Ip),
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
    

load_vector_index(IoDevice) ->
    Key = ip2region_header_loaded,
    case persistent_term:get(Key, false) of
        true -> ok;
        _ -> 
            {ok, <<_Header:?XDB_HEADER_SIZE/binary, VectorIndexBin/binary>> } = 
                file:read(IoDevice, ?XDB_HEADER_SIZE + ?XDB_VECTOR_INDEX_COUNT*8),
            load_vector_index_aux(VectorIndexBin, 0),
            persistent_term:put(Key, true)
    end.

load_vector_index_aux(<<>>, _Index) -> ok;
load_vector_index_aux(<<SPtr:32/little, EPtr:32/little, VectorIndexBin/binary>>, Index) ->
    Term = {Index, SPtr, EPtr},
    ets:insert(?XDB_VECTOR_INDEX, Term),
    load_vector_index_aux(VectorIndexBin, Index + 1).


search_ip(IoDevice, Ip) ->
    IntIp = ip2region_util:ipv4_to_n(Ip),
    case ets:lookup(?IP2REGION_CACHE, IntIp) of
        [{_IntIp, RegionInfo}] ->
            RegionInfo;
        _ ->
            <<A:8, B:8, _Rest/binary>> = <<IntIp:32>>,
            VectorIdx = A * ?XDB_VECTOR_COLS + B,
            [{_, SPtr, EPtr}] = ets:lookup(?XDB_VECTOR_INDEX, VectorIdx),
            RegionInfo = search_ip(IoDevice, IntIp, SPtr, EPtr, 0, (EPtr - SPtr) div ?XDB_SEGMENT_INDEX_SIZE),
            ets:insert_new(?IP2REGION_CACHE, {IntIp, RegionInfo}),
            RegionInfo
    end.

search_ip(IoDevice, IntIp, SPtr, EPtr, Low, High) when Low =< High ->
    Middle = (Low + High) bsr 1,
    SPtr2 = SPtr + Middle * ?XDB_SEGMENT_INDEX_SIZE,
    {SIp, EIp, DataLen, DataPtr} = read_segement_index(IoDevice, SPtr2),
    if 
        IntIp < SIp ->
            search_ip(IoDevice, IntIp, SPtr, EPtr, Low, Middle - 1);
        IntIp > EIp ->
            search_ip(IoDevice, IntIp, SPtr, EPtr, Middle + 1, High);
        true ->
            {ok, DataBin} = read_file(IoDevice, DataPtr, DataLen),
            unicode:characters_to_nfc_list(DataBin)
    end;
search_ip(_IoDevice, _IntIp, _SPtr, _EPtr, _Low, _High)  ->
    {error, unknown}.

read_file(IoDevice, Position, DataLength) ->
    file:position(IoDevice, {bof, Position}),
    file:read(IoDevice, DataLength).

read_segement_index(IoDevice, SPtr) ->
    case ets:lookup(?XDB_SEGMENT_INDEX, SPtr) of
        [{_SPtr, SIp, EIp, DataLen, DataPtr}] ->
            {SIp, EIp, DataLen, DataPtr};
        _ ->
            {ok, <<SIp:32/little, EIp:32/little, DataLen:16/little, DataPtr:32/little>>} = 
	            read_file(IoDevice, SPtr, ?XDB_SEGMENT_INDEX_SIZE),
            ets:insert_new(?XDB_SEGMENT_INDEX, {SPtr, SIp, EIp, DataLen, DataPtr}),
            {SIp, EIp, DataLen, DataPtr}
    end.