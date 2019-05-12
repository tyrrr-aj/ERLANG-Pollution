%%%-------------------------------------------------------------------
%%% @author adams
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. maj 2019 10:34
%%%-------------------------------------------------------------------
-module(pollution_gen_server).
-author("adams").

-behaviour(gen_server).

%% API
-export([start_link/0, addStation/2, addValue/4]).
-export([init/1, handle_call/3]).

start_link() -> gen_server:start_link({local, pollution_gen_server}, ?MODULE, initial_value, []).

init(_) -> {ok, pollution:createMonitor()}.

% user interface
addStation(Coords, StationName) -> gen_server:call(pollution_gen_server, {addStation, {Coords, StationName}}).
addValue(CoordsOrName, Date, Type, Value) -> gen_server:call(pollution_gen_server, {addValue, {CoordsOrName, Date, Type, Value}}).

% callback
handle_call({addStation, {Coords, StationName}}, _From, Monitor) ->
  Result = pollution:addStation(Monitor, Coords, StationName),
  case Result of
    {error, _} -> {reply, Result, Monitor};
    _ -> {reply, ok, Result}
  end;
handle_call({addValue, {CoordsOrName, Date, Type, Value}}, _From, Monitor) ->
  Result = pollution:addValue(Monitor, CoordsOrName, Date, Type, Value),
  case Result of
    {error, _} -> {reply, Result, Monitor};
    _ -> {reply, ok, Result}
  end.