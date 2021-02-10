%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. 1月 2021 16:55
%%%-------------------------------------------------------------------
-module(top3).
-author("Eric").

%% API
-export([test/0]).

%% 由于排行榜Top100不便于测试 改为Top3 原理相同.

q_sort([]) -> [];
q_sort([{H_name, H_score} | T]) ->
  q_sort([{Name, Score} || {Name, Score} <- T, Score =< H_score]) ++ [{H_name, H_score}] ++
    q_sort([{Name, Score} || {Name, Score} <- T, Score > H_score]).

top3([L]) ->
  lists:sublist(q_sort(L),3).

test() ->
  Input = [{"apple", 75}, {"orange", 85}, {"car", 12}, {"bob", 32}, {"tele", 11}, {"pojo", 65}, {"mock", 13}],
  [{"tele", 11}, {"car", 12} ,{"mock", 13}] = top3([Input]),
  io:fwrite("test_ok").