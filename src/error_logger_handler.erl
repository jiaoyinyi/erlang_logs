%%%-------------------------------------------------------------------
%%% @author huangzaoyi
%%% @copyright (C) 2019, <COMPANY>
%%% @doc error_logger的处理器
%%%
%%% @end
%%% Created : 31. 7月 2019 下午9:42
%%%-------------------------------------------------------------------
-module(error_logger_handler).
-author("huangzaoyi").

-include("logs.hrl").

-behaviour(gen_event).

%% gen_event callbacks
-export([init/1,
    handle_event/2,
    handle_call/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-record(state, {}).

init([]) ->
    {ok, #state{}}.

handle_event(Event, State) ->
    write_event(Event),
    {ok, State}.

handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Arg, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

write_event(Event) ->
    case parse_event(Event) of
        {Flag, Head, _, FormatList} ->
            Header = get_prefix(Head),
            Body = format_body(FormatList),
            do_log(Flag, Header, Body);
        _ ->
            ok
    end.

parse_event({error, _GL, {Pid, Format, Args}}) ->
    {error, error, Pid, [{Format, Args}]};
parse_event({info_msg, _GL, {Pid, Format, Args}}) ->
    {info, info_msg, Pid, [{Format, Args}]};
parse_event({warning_msg, _GL, {Pid, Format, Args}}) ->
    {warn, warning_msg, Pid, [{Format, Args}]};
parse_event({error_report, _GL, {Pid, std_error, Args}}) ->
    {error, error_report, Pid, format_term(Args)};
parse_event({info_report, _GL, {Pid, std_info, Args}}) ->
    {info, info_report, Pid, format_term(Args)};
parse_event({info_report, _GL, {Pid, progress, Args}}) ->
    {info, info_report, Pid, format_term(Args)};
parse_event({warning_report, _GL, {Pid, std_warning, Args}}) ->
    {warn, warning_report, Pid, format_term(Args)};
parse_event(_) -> ignore.

format_term(Term) when is_list(Term) ->
    case string_p(lists:flatten(Term)) of
        true ->
            [{"~ts\n", [Term]}];
        false ->
            format_term_list(Term)
    end;
format_term(Term) ->
    [{"~tp\n", [Term]}].

format_term_list([{Tag, Data} | T]) ->
    [{"    ~tp: ~tp\n", [Tag, Data]} | format_term_list(T)];
format_term_list([Data | T]) ->
    [{"    ~tp\n", [Data]} | format_term_list(T)];
format_term_list([]) ->
    [].

string_p([]) ->
    false;
string_p(FlatList) ->
    io_lib:printable_list(FlatList).


format_body([{Format, Args} | T]) ->
    S = try format(Format, Args) of
            S0 ->
                S0
        catch
            _:_ ->
                format("ERROR: ~tp - ~tp\n", [Format, Args])
        end,
    [S | format_body(T)];
format_body([]) ->
    [].

format(Format, Args) ->
    io_lib:format(Format, Args).


do_log(debug, Header, Body) ->
    catch ?debug("~n~ts~n~ts", [Header, Body]);
do_log(info, Header, Body) ->
    catch ?info("~n~ts~n~ts", [Header, Body]);
do_log(warn, Header, Body) ->
    catch ?warn("~n~ts~n~ts", [Header, Body]);
do_log(error, Header, Body) ->
    catch ?error("~n~ts~n~ts", [Header, Body]).

get_prefix(info_msg) -> get_info_msg();
get_prefix(info_report) -> get_info_report();
get_prefix(warning_msg) -> get_warning_msg();
get_prefix(warning_report) -> get_warning_report();
get_prefix(error) -> get_error_msg();
get_prefix(error_report) -> get_error_report().

get_info_msg() -> "=== Info Message ===".
get_info_report() -> "=== Info Report ===".
get_warning_msg() -> "=== Warning Message ===".
get_warning_report() -> "=== Warning Report ===".
get_error_msg() -> "=== Error Message ===".
get_error_report() -> "=== Error Report ===".