%%%-------------------------------------------------------------------
%%% @author Administrator
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. 1月 2021 23:38
%%%-------------------------------------------------------------------
-module(hit_judgment).
-author("Eric").

%% API
-export([test/0]).

%% 默认坐标顺序左下X1/Y1 右上X2/Y2 依此类推

is_hit({X1, Y1, X2, Y2}, {X3, Y3, X4, Y4}) ->
  not ((X1 > X4) or (X2 < X3) or (Y1 > Y4) or (Y2 < Y3)).

test() ->
  % 相交
  true = is_hit({0, 0, 4, 2}, {2, 1, 6, 3}),
  % 交叉
  true = is_hit({0, 1, 3, 2}, {1, 0, 2, 3}),
  % 技能包含Boss
  true = is_hit({0, 0, 4, 4}, {1, 1, 2, 2}),
  % Boss包含技能
  true = is_hit({1, 1, 2, 2}, {0, 0, 4, 4}),
  % 相离
  false = is_hit({0, 0, 1, 1}, {2, 2, 4, 4}),
  io:fwrite("test_ok").