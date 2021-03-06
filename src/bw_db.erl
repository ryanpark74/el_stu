%%%-------------------------------------------------------------------
%%% @author ryan
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. 12월 2015 오후 6:46
%%%-------------------------------------------------------------------
-module(bw_db).
-author("ryan").

-include("bw_record.hrl").

%% API
-export([install/0, uninstall/0]).

install() ->
  ok = mnesia:create_schema([node()]),
  application:start(mnesia),
  mnesia:create_table(users, [
    {attributes, record_info(fields, users)},
    {disc_copies, [node()]}
  ]),
  application:stop(mnesia)
.

uninstall() ->
  application:stop(mnesia),
  mnesia:delete_schema([node()])
.
