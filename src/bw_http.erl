%%%-------------------------------------------------------------------
%%% @author ryan
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. 12월 2015 오후 8:15
%%%-------------------------------------------------------------------
-module(bw_http).
-author("ryan").

%% API
-export([init/3, handle/2, terminate/3]).

init(_Type, Req, []) ->
  {ok, Req, no_state}
.

handle(Req, State) ->
  {Api, Req1} = cowboy_req:binding(api, Req),
  {What, Req2} = cowboy_req:binding(what, Req1),
  {Opt, Req3} = cowboy_req:binding(opt, Req2),
  {ok, Data, Req4} = cowboy_req:body_qs(Req3),

  io:format("api=~p, what=~p, opt=~p ~n", [Api, What, Opt]),

  Reply = handle(Api, What, Opt, Data),

  {ok, Req5} = cowboy_req:reply(200,
                                [{<<"content-type">>, <<"text/plain">>}],
                                Reply,
                                Req4
  ),
  {ok, Req5, State}
.

handle(<<"login">>, _, _, Data) ->
  Id = proplists:get_value(<<"id">>, Data),
  Password = proplists:get_value(<<"password">>, Data),

  case bw_users:login(Id, Password) of
    {ok, SessionKey} ->
      jsx:encode([{<<"result">>, <<"ok">>}
      ,{<<"session_key">>, SessionKey}]);
    _ ->
      jsx:encode([{<<"result">>, <<"fail">>}])
  end;


handle(<<"join">>, _, _, Data) ->
  Id = proplists:get_value(<<"id">>, Data),
  Password = proplists:get_value(<<"password">>, Data),
  case bw_users:join(Id, Password) of
    fail ->
      jsx:encode([{<<"result">>, <<"Dupelication">>}]);
    ok ->
      jsx:encode([{<<"result">>, <<"Join">>}])
  end;

handle(<<"users">>, <<"point">>, _, Data) ->
  SessionKey = proplists:get_value(<<"session_key">>, Data),
  Point1 = proplists:get_value(<<"point">>, Data),
  Point = binary_to_integer(Point1),
  Save = proplists:get_value(<<"save">>, Data),

  case bw_users:point(SessionKey, Point, Save) of
    {ok, Point2} ->
      jsx:encode([{<<"result">>, Point2}]);
    fail ->
      jsx:encode([{<<"result">>, <<"fail">>}])
  end;


handle(<<"hello">>, <<"world">>, _, _) ->
  jsx:encode([{<<"result">>, <<"Hello World!">>}]);

handle(_, _, _, _) ->
  jsx:encode([{<<"result">>, <<"error..">>}]).

terminate(_Reason, _Req, _State) ->
  ok
.

