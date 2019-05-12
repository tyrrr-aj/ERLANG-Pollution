%%%-------------------------------------------------------------------
%%% @author adams
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. maj 2019 09:54
%%%-------------------------------------------------------------------
-module(pollution_server_sup).
-author("adams").

%% API
-export([start/0]).
-export([suprevise/0]).

start() -> spawn(?MODULE, suprevise, []).

suprevise() ->
  process_flag(trap_exit, true),
  pollution_server:start(),
  receive
    {'EXIT', _, normal} -> io:format("Server finished work");
    {'EXIT', _, _} -> suprevise()
  end.
