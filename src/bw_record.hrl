%%%-------------------------------------------------------------------
%%% @author ryan
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. 12월 2015 오후 6:29
%%%-------------------------------------------------------------------
-author("ryan").

-record(users, {
  id,
  password,
  token,
  level=0,
  exp=0,
  point=0
}).