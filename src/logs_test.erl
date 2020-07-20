%%%-------------------------------------------------------------------
%%% @author jiaoyinyi
%%% @copyright (C) 2020, <COMPANY>
%%% @doc 日志测试
%%%
%%% @end
%%% Created : 20. 7月 2020 16:48
%%%-------------------------------------------------------------------
-module(logs_test).
-author("jiaoyinyi").

-include("logs.hrl").

%% API
-export([test/0, test2/0]).

test() ->
    ?debug("test日志"),
    ?debug("~ts", ["test日志"]),
    ?info("test日志"),
    ?info("~ts", ["test日志"]),
    ?warn("test日志"),
    ?warn("~ts", ["test日志"]),
    ?error("test日志"),
    ?error("~ts", ["test日志"]).

test2() ->
    error_logger:info_msg("test日志"),
    error_logger:info_msg("~ts", ["test日志"]),
    error_logger:info_report("test日志"),

    error_logger:warning_msg("test日志"),
    error_logger:warning_msg("~ts", ["test日志"]),
    error_logger:warning_report("test日志"),

    error_logger:error_msg("test日志"),
    error_logger:error_msg("~ts", ["test日志"]),
    error_logger:error_report("test日志").