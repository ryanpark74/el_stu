%%%-------------------------------------------------------------------
%%% @author ryan
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. 12월 2015 오후 6:56
%%%-------------------------------------------------------------------
-module(bw_users).
-author("ryan").

-include("bw_record.hrl").

%% API
-export([join/2, login/2, point/3, loop/2, make_session_key/2, print_point/1]).

join(Id, Password) ->
  F = fun() ->
    case mnesia:read(users, Id) of
      [] ->
        Users = #users{id = Id, password = Password},
        ok = mnesia:write(Users);
      _ ->
        fail
    end
  end,
  mnesia:activity(transaction, F).

login(Id, Password) ->
  F = fun() ->
    case mnesia:read(users, Id) of
      [U = #users{password = Password}] ->
        SessionKey = new_session(Id),
        {ok, SessionKey};
      _ ->
        fail
    end
  end,
  mnesia:activity(transaction, F).

point(SessionKey, Point, Save) ->
  case ets:lookup(session_list, SessionKey) of
    [{SessionKey, Pid}] ->
      io:format("point() : ~p ~p ~p ~n", [SessionKey, Pid, Save] ),
      Ref = make_ref(),
      Pid ! {self(), Ref, Save, Point},
      receive
        {Ref, saved, Point2} ->
          {ok, Point2};
        {Ref, printed, Point2} ->
          {ok, Point2};
        _ ->
          fail
      after 3000 ->
        fail
      end;
    _ ->
      fail
  end
.

new_session(Id) ->
  Time = erlang:timestamp(),
  Pid = spawn(bw_users, loop, [Id, Time]), %% 새로운 세션을 만들때 루프 함수를 무한으로 돌림.
  io:format("Pid : ~p ~n", [Pid]),
  SessionKey = make_session_key(Id, Pid),
  erlang:send_after(5000, Pid, {check}),
  SessionKey.

loop(Id, Time) ->
  %% loop 함수가 명령으로 인하여 호출 될때는 시간을 현재 시간으로 업데이트 하고,
  %% check 명령일경우는 기존 시간을 가지고 다시 호출되어 10초가 지났는지 확인한다.
  Time1 =
    receive
      {Pid, Ref, <<"save_point">>, Point} ->
        io:format("loop() ~p ~p ~p ~p ~n", [Pid, Ref, save_point, Point]),
        Point2 = save_point(Id, Point),
        Pid ! {Ref, saved, Point2},
        erlang:timestamp();
      {Pid, Ref, <<"print_point">>, Point} ->
        io:format("loop() ~p ~p ~p ~n", [Pid, Ref, print_point]),
        Point2 = print_point(Id),
        Pid ! {Ref, printed, Point2},
        erlang:timestamp();
      {check} ->
        Diff = timer:now_diff(erlang:timestamp(), Time),
        if(Diff > 20000000) ->
          delete_session_key(self()); %% 10초가 지났으면 delete_session_key를 실행하고
          true -> erlang:send_after(1000, self(), {check}) %% 그렇지 않으면 1초에 한번씩 체크 하도록 loop 호출
        end,
        Time;
      _ ->
        Time
    end,
    loop(Id, Time1).

make_session_key(Id, Pid) ->
  %% 시드 초기화
  {A1, A2, A3} = erlang:timestamp(), %% now() 함수는 더이상 사용되지 않으므로, timestamp()함수로 변경
  %%io:format("~w ~w ~w ~n", [A1, A2, A3]),
  rand:seed(exs1024, {A1, A2, A3}),
  Num = rand:uniform(10000),
  %%io:format("~p ~n",[Num]),

  %%io:format("~w ~w ~w ~n",[erlang:phash2([node()]), erlang:monotonic_time(), erlang:unique_integer()]), %% 또다른 시드

  Hash = erlang:phash2(Id),

  %% 두개의 값을 16진수로 조합하여 session key 생성
  List = io_lib:format("~.16B~.16B", [Hash, Num]),
  SessionKey = list_to_binary(lists:append(List)),
  %%io:format("~.16B~16B", [Hash, Num]),
  %%io:format("~n ~w ~n", [List]),
  %%io:format("~n ~w ~n", [lists:append(List)]),
  %%io:format("~n ~w ~n", [list_to_binary(lists:append(List))]),
  ets:insert(session_list, {SessionKey, Pid}),
  io:format("Session key : ~s ~n", [SessionKey]),
  SessionKey.

save_point(Id, Point) ->
  F = fun() ->
    case mnesia:read(users, Id) of
      [U] ->
        Users = U#users{point = Point},
        ok = mnesia:write(Users),
        Point;
      _ ->
        fail
    end
  end,
  mnesia:activity(transaction, F),
  io:format("savepoint ~p ~n", mnesia:dirty_read(users, Id))
.

print_point(Id) ->
  [U] = mnesia:dirty_read(users, Id), %% TODO: 이거 안감싸줘서 개 피봄.
  U#users.point.

delete_session_key(Pid) ->
  [Obj] = ets:match_object(session_list, {'_', Pid}),
  ets:delete_object(session_list, Obj),
  exit(normal). %% 프로세스 종료