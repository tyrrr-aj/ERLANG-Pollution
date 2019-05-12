%%%-------------------------------------------------------------------
%%% @author adams
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. maj 2019 23:52
%%%-------------------------------------------------------------------
-module(pollution_gen_supervisor).
-author("adams").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%% API functions

start_link() ->
  supervisor:start_link({local, pollution_gen_supervisor}, ?MODULE, [initValue]).

init(_) ->
  {ok, {
    {one_for_all, 2, 3},
    [ {pollution_gen_server,
      {pollution_gen_server, start_link, []},
      permanent, brutal_kill, worker, [pollution_gen_server]}
    ]}
  }.