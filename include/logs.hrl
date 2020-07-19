%%%-------------------------------------------------------------------
%%% @author huangzaoyi
%%% @copyright (C) 2020, <COMPANY>
%%% @doc 日志模块头文件
%%%
%%% @end
%%% Created : 08. 三月 2020 22:44
%%%-------------------------------------------------------------------
-author("huangzaoyi").

-define(debug(Format, Args), logs_lib:log(debug, ?MODULE, ?FUNCTION_NAME, ?LINE, Format, Args)).
-define(debug(Str), logs_lib:log(debug, ?MODULE, ?FUNCTION_NAME, ?LINE, "~ts", [Str])).

-define(info(Format, Args), logs_lib:log(info, ?MODULE, ?FUNCTION_NAME, ?LINE, Format, Args)).
-define(info(Str), logs_lib:log(info, ?MODULE, ?FUNCTION_NAME, ?LINE, "~ts", [Str])).

-define(warn(Format, Args), logs_lib:log(warn, ?MODULE, ?FUNCTION_NAME, ?LINE, Format, Args)).
-define(warn(Str), logs_lib:log(warn, ?MODULE, ?FUNCTION_NAME, ?LINE, "~ts", [Str])).

-define(error(Format, Args), logs_lib:log(error, ?MODULE, ?FUNCTION_NAME, ?LINE, Format, Args)).
-define(error(Str), logs_lib:log(error, ?MODULE, ?FUNCTION_NAME, ?LINE, "~ts", [Str])).