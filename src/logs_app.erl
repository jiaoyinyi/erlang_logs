%%%-------------------------------------------------------------------
%% @doc logs public API
%% @end
%%%-------------------------------------------------------------------

-module(logs_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    logs_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
