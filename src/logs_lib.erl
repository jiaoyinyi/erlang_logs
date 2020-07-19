%%%-------------------------------------------------------------------
%%% @author huangzaoyi
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. 三月 2020 22:42
%%%-------------------------------------------------------------------
-module(logs_lib).
-author("huangzaoyi").

%% API
-export([log/6]).

log(Flag, Mod, Func, Line, Format, Args) ->
    logs:info({log, Flag, Mod, Func, Line, Format, Args}).
