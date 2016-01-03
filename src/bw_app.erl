%%%-------------------------------------------------------------------
%%% @author ryan
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. 12월 2015 오후 7:13
%%%-------------------------------------------------------------------
-module(bw_app).
-author("ryan").
-include("bw_record.hrl").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @end
%%--------------------------------------------------------------------
-spec(start(StartType :: normal | {takeover, node()} | {failover, node()},
    StartArgs :: term()) ->
  {ok, pid()} |
  {ok, pid(), State :: term()} |
  {error, Reason :: term()}).

start(_StartType, _StartArgs) ->

  %% 필요한 어플리케이션 실행
  ok = application:start(crypto),
  ok = application:start(cowlib),
  ok = application:start(ranch),
  ok = application:start(cowboy),
  ok = application:start(mnesia),

  %% cowboy 라우터 설정
  %% cowboy_router:compile(Routes) <- 인자값은 List 형식의 Routes 하나.
  %% Routes = [Host1, Host2, ... HostN]
  %% Host1 = {HostMatch, PathsList}
  %% PathList = [Path1, Path2, ... PathN] ..복잡하다.
  %% Path1 = {PathMatch, Handler, Opts}
  Dispatch = cowboy_router:compile([
    { '_', [
      %%{"/hello/world", mon_http, []},
      %%{"/join", mon_http, []},
      %%{"/login", mon_http, []}
      {"/:api/[:what/[:opt]]", bw_http, []} %%cowboy에서 제공하는 match syntax
    ]}
  ]),

  %% HTTP 서버 실행
  {ok, _} = cowboy:start_http(http,
                              100,
                              [{port, 6060}],
                              [{env, [{dispatch, Dispatch}]}]
  ),

  %% 코드 리로더 실행
  bw_reloader:start(),

  %% session을 저장하기 위한 ETS 테이블 생성
  ets:new(session_list, [public, named_table]),

  case bw_sup:start_link() of
    {ok, Pid} ->
      io:format("start ok~n"),
      {ok, Pid};
    Error ->
      Error
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec(stop(State :: term()) -> term()).
stop(_State) ->
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
