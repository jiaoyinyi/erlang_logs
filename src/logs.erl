%%%-------------------------------------------------------------------
%%% @author huangzaoyi
%%% @copyright (C) 2020, <COMPANY>
%%% @doc 日志处理
%%%
%%% @end
%%% Created : 08. 三月 2020 23:08
%%%-------------------------------------------------------------------
-module(logs).
-author("huangzaoyi").

-behaviour(gen_server).

%% API
-export([
    log/5,
    get_level/1
]).
-export([start_link/0, call/1, cast/1, info/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-record(state, {
    fd       %% 日志文件名
}).

-define(log_dir, "log"). %% 日志目录
-define(log_levels, [debug, info, warn, error]). %% 所有日志等级

%% @doc 写日志接口
-spec log(debug|info|warn|error, module(), atom(), pos_integer(), string()) -> ok.
log(Flag, Mod, Func, Line, Str) ->
    logs:info({log, Flag, Mod, Func, Line, Str}),
    ok.

call(Call) ->
    gen_server:call(?MODULE, Call).

cast(Cast) ->
    gen_server:cast(?MODULE, Cast).

info(Info) ->
    ?MODULE ! Info.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    Fd = get_log_file(),
    error_logger:add_report_handler(error_logger_handler),
    Diff = next_diff(),
    erlang:send_after(Diff * 1000, self(), zero_flush),
    {ok, #state{fd = Fd}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    case catch do_handle_info(_Info, State) of
        {noreply, NewState} ->
            {noreply, NewState};
        _Err ->
            {noreply, State}
    end.

terminate(_Reason, _State = #state{fd = Fd}) ->
    catch error_logger:delete_report_handler(error_logger_handler),
    case erase(log_list) of
        LogList = [_|_] ->
            catch do_log(lists:reverse(LogList), Fd);
        _ ->
            ok
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% 接收到写日志消息，延时写
do_handle_info({log, Flag, Mod, Func, Line, Str}, State) ->
    LogList = case get(log_list) of undefined -> []; LogList0 -> LogList0 end,
    put(log_list, [{Flag, Mod, Func, Line, Str, calendar:local_time(), node()} | LogList]),
    case get(log_timer) of
        Ref when is_reference(Ref) ->
            ok;
        _ ->
            Ref = erlang:send_after(300, self(), do_log),
            put(log_timer, Ref)
    end,
    {noreply, State};

%% 写日志
do_handle_info(do_log, State = #state{fd = Fd}) ->
    LogList = case get(log_list) of undefined -> []; LogList0 -> LogList0 end,
    List = lists:reverse(LogList),
    {Logs, NewList} = split(30, List),
    do_log(Logs, Fd),
    put(log_list, lists:reverse(NewList)),
    catch erlang:cancel_timer(erase(log_timer)),
    case NewList == [] of
        false -> %% 如果还有日志，延迟100ms写
            Ref = erlang:send_after(100, self(), do_log),
            put(log_timer, Ref);
        _ ->
            ok
    end,
    {noreply, State};

do_handle_info(zero_flush, State) ->
    Fd = get_log_file(),
    Diff = next_diff(),
    erlang:send_after(Diff * 1000, self(), zero_flush),
    NewState = State#state{fd = Fd},
    {noreply, NewState};

do_handle_info(_Info, State) ->
    {noreply, State}.

get_log_file() ->
    {ok, Cwd} = file:get_cwd(),
    Dir = filename:join(Cwd, ?log_dir),
    filelib:is_dir(Dir) orelse file:make_dir(Dir),
    {Y, M, D} = erlang:date(),
    DateStr = lists:flatten(io_lib:format("~w-~.2.0w-~.2.0w", [Y, M, D])),
    filename:join(Dir, DateStr ++ ".log").

%% 写日志
do_log([], _Fd) -> ok;
do_log([{Flag, Mod, Func, Line, Str, DateTime, Node} | List], Fd) ->
    catch write(Fd, Flag, DateTime, Node, Mod, Func, Line, Str),
    do_log(List, Fd).

%% 写日志
write(Fd, Level, {{Y, M, D}, {H, Min, S}}, Node, Mod, Func, Line, Str) ->
    LevelStr = get_str(Level),
    LogStr = io_lib:format("~s ~w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w ~p:~p:~p:~p ~ts~n", [LevelStr, Y, M, D, H, Min, S, Node, Mod, Func, Line, Str]),
    Bin = unicode:characters_to_binary(LogStr),
    do_write(Fd, Bin).

%% 写入文件
do_write(Fd, Bin) ->
    catch file:write_file(Fd, Bin, [append, delayed_write]),
    ok.

%% 距离下一天0点的秒数
next_diff() ->
    Date = {{Y, M, D}, _} = calendar:local_time(),
    NextDate = {{Y, M, D}, {24, 0, 0}},
    {_, Time} = calendar:time_difference(Date, NextDate),
    calendar:time_to_seconds(Time).

get_level(debug) -> 1;
get_level(info) -> 2;
get_level(warn) -> 3;
get_level(error) -> 4.

get_str(debug) -> "[D]";
get_str(info) -> "[I]";
get_str(warn) -> "[W]";
get_str(error) -> "[E]".

split(N, []) when is_integer(N) ->
    {[], []};
split(N, List) when is_integer(N) andalso is_list(List) ->
    split(N, List, []).
split(_N, [], Acc) ->
    {lists:reverse(Acc), []};
split(N, [I | List], Acc) when is_integer(N) andalso N > 0 ->
    split(N - 1, List, [I | Acc]);
split(_N, List, Acc) ->
    {lists:reverse(Acc), List}.