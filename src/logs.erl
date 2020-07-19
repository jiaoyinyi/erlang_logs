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
-export([start_link/0, call/1, cast/1, info/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-include("common.hrl").
-include("logs.hrl").

-record(state, {
    level     %% 等级
    , fd       %% 日志文件
}).

call(Call) ->
    ?scall(?MODULE, Call).

cast(Cast) ->
    gen_server:cast(?MODULE, Cast).

info(Info) ->
    ?MODULE ! Info.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ?info("开始启动:~w", [?MODULE]),
    process_flag(trap_exit, true),
    Level = config:get(log_level, error),
    Fd = get_log_file(),

    Diff = time_lib:next_diff({0, 0, 0}),
    misc_lib:set_timer(zero_flush_timer, Diff, zero_flush),
    error_logger:add_report_handler(error_logger_handler),
    ?info("启动成功:~w", [?MODULE]),
    {ok, #state{level = Level, fd = Fd}}.

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
    catch do_log(lists:reverse(misc_lib:get(log_list, [])), Fd, calendar:local_time(), node()),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


do_handle_info({log, Flag, Mod, Func, Line, Format, Args}, State = #state{level = Level}) when Flag == debug orelse Flag == info orelse Flag == warn orelse Flag == error ->
    case get_level(Flag) >= get_level(Level) of
        true ->
            put(log_list, [{Flag, Mod, Func, Line, Format, Args} | misc_lib:get(log_list, [])]),
            case misc_lib:has_timer(log_timer) of
                true ->
                    ok;
                _ ->
                    misc_lib:set_ms_timer(log_timer, 100, do_log)
            end;
        _ ->
            ok
    end,
    {noreply, State};
do_handle_info(do_log, State = #state{fd = Fd}) ->
    List = lists:reverse(misc_lib:get(log_list, [])),
    Logs = lists:sublist(List, 50),
    do_log(Logs, Fd, calendar:local_time(), node()),
    NewList = lists:reverse(List -- Logs),
    put(log_list, NewList),
    misc_lib:unset_timer(log_timer),
    NewList == [] orelse misc_lib:set_ms_timer(log_timer, 50, do_log),
    {noreply, State};

do_handle_info(zero_flush, State) ->
    Fd = get_log_file(),
    Diff = time_lib:next_diff({0, 0, 0}),
    misc_lib:set_timer(zero_flush_timer, Diff, zero_flush),
    NewState = State#state{fd = Fd},
    {noreply, NewState};

do_handle_info(_Info, State) ->
    {noreply, State}.

get_level(debug) -> 1;
get_level(info) -> 2;
get_level(warn) -> 3;
get_level(error) -> 4.

get_str(debug) -> "[D]";
get_str(info) -> "[I]";
get_str(warn) -> "[W]";
get_str(error) -> "[E]".

get_log_file() ->
    Dir = config:get(log_root),
    filelib:is_dir(Dir) orelse file:make_dir(Dir),
    {Y, M, D} = erlang:date(),
    DateStr = lists:flatten(io_lib:format("~w-~.2.0w-~.2.0w", [Y, M, D])),
    filename:join(Dir, DateStr ++ ".log").

%% 写日志
do_log([], _Fd, _DateTime, _Node) -> ok;
do_log([{Flag, Mod, Func, Line, Format, Args} | List], Fd, DateTime, Node) ->
    catch write(Fd, Flag, DateTime, Node, Mod, Func, Line, Format, Args),
    do_log(List, Fd, DateTime, Node).

write(Fd, Level, {{Y, M, D}, {H, Min, S}}, Node, Mod, Func, Line, [], Args) ->
    LevelStr = get_str(Level),
    LogStr =
        case string_p(lists:flatten(Args)) of
            true ->
                io_lib:format("~s ~w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w ~p:~p:~p:~p ~ts~n", [LevelStr, Y, M, D, H, Min, S, Node, Mod, Func, Line] ++ Args);
            false ->
                io_lib:format("~s ~w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w ~p:~p:~p:~p ~w~n", [LevelStr, Y, M, D, H, Min, S, Node, Mod, Func, Line] ++ Args)
        end,
    Bin = unicode:characters_to_binary(LogStr, utf8),
    file:write_file(Fd, Bin, [append, delayed_write]);
write(Fd, Level, {{Y, M, D}, {H, Min, S}}, Node, Mod, Func, Line, Format, Args) ->
    LevelStr = get_str(Level),
    LogStr = io_lib:format("~s ~w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w ~p:~p:~p:~p " ++ Format ++ "~n", [LevelStr, Y, M, D, H, Min, S, Node, Mod, Func, Line] ++ Args),
    Bin = unicode:characters_to_binary(LogStr, utf8),
    file:write_file(Fd, Bin, [append, delayed_write]).

string_p([]) ->
    false;
string_p(FlatList) ->
    io_lib:printable_list(FlatList).