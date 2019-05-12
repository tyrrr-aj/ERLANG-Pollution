%%%-------------------------------------------------------------------
%%% @author adams
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. maj 2019 00:22
%%%-------------------------------------------------------------------
-module(pollution_server).
-author("adams").

%% API
-export([start/0, addStation/2, addValue/4, removeValue/3, getOneValue/3, getStationMean/2, getDailyMean/2, getAreaMean/3, stop/0, crash/0]).
-export([init/0]).

% API
start() -> register(poll_server, spawn_link(pollution_server, init, [])).
addStation(Coords, StationName) -> call(addStation, Coords, StationName).
addValue(CoordsOrName, Date, Type, Value) -> call(addValue, CoordsOrName, Date, Type, Value).
removeValue(CoordsOrName, Date, Type) -> call(removeValue, CoordsOrName, Date, Type).
getOneValue(CoordsOrName, Date, Type) -> call(getOneValue, CoordsOrName, Date, Type).
getStationMean(CoordsOrName, Type) -> call(getStationMean, CoordsOrName, Type).
getDailyMean(Day, Type) -> call(getDailyMean, Day, Type).
getAreaMean(Type, CoordsOrName, Radius) -> call(getAreaMean, Type, CoordsOrName, Radius).
crash() -> call(crash).
stop() -> call(stop).

% Implementation
init() -> loop(pollution:createMonitor()).

loop(Monitor) ->
  receive
    stop -> unregister(poll_server);
    crash -> not_existing_module:not_existing_function(imaginary_argument);
    {PID, addStation, Coords, StationName} ->
      confirm(PID, pollution:addStation(Monitor, Coords, StationName), Monitor);
    {PID, addValue, CoordsOrName, Date, Type, Value} ->
      confirm(PID,pollution:addValue(Monitor, CoordsOrName, Date, Type, Value), Monitor);
    {PID, removeValue, CoordsOrName, Date, Type} ->
      confirm(PID,pollution:removeValue(Monitor, CoordsOrName, Date, Type), Monitor);
    {PID, getOneValue, CoordsOrName, Date, Type} ->
      return(PID,pollution:getOneValue(Monitor, CoordsOrName, Date, Type), Monitor);
    {PID, getStationMean, CoordsOrName, Type} ->
      return(PID,pollution:getStationMean(Monitor, CoordsOrName, Type), Monitor);
    {PID, getDailyMean, Day, Type} ->
      return(PID,pollution:getDailyMean(Monitor, Day, Type), Monitor);
    {PID, getAreaMean, Type, CoordsOrName, Radius} ->
      return(PID,pollution:getAreaMean(Monitor, Type, CoordsOrName, Radius), Monitor)
  end.

confirm(PID, Result, Monitor) ->
  case Result of
    {error, _} ->
      PID ! Result,
      loop(Monitor);
    _ ->
      PID ! ok,
      loop(Result)
  end.

return(PID, Result, Monitor) ->
  PID ! Result,
  loop(Monitor).

call(Function) ->
  poll_server ! Function.

call(Function, Arg1, Arg2) ->
  poll_server ! {self(), Function, Arg1, Arg2},
  receive
    Response -> Response
  after 1000 -> timeout
  end.

call(Function, Arg1, Arg2, Arg3) ->
  poll_server ! {self(), Function, Arg1, Arg2, Arg3},
  receive
    Response -> Response
  after 1000 -> timeout
  end.

call(Function, Arg1, Arg2, Arg3, Arg4) ->
  poll_server ! {self(), Function, Arg1, Arg2, Arg3, Arg4},
  receive
    Response -> Response
  after 1000 -> timeout
  end.