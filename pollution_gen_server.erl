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
-export([start_link/0, addStation/2, addValue/4, removeValue/3, getOneValue/3, getStationMean/2, getDailyMean/2, getAreaMean/3, stop/0, crash/0]).
-export([init/1, handle_call/3, handle_cast/2]).

start_link() -> gen_server:start_link({local, pollution_gen_server}, ?MODULE, initial_value, []).

init(_) -> {ok, pollution:createMonitor()}.

% user interface
addStation(Coords, StationName) -> gen_server:call(pollution_gen_server, {addStation, {Coords, StationName}}).
addValue(CoordsOrName, Date, Type, Value) -> gen_server:call(pollution_gen_server, {addValue, {CoordsOrName, Date, Type, Value}}).
removeValue(CoordsOrName, Date, Type) -> gen_server:call(pollution_gen_server, {removeValue, {CoordsOrName, Date, Type}}).
getOneValue(CoordsOrName, Date, Type) -> gen_server:call(pollution_gen_server, {getOneValue, {CoordsOrName, Date, Type}}).
getStationMean(CoordsOrName, Type) -> gen_server:call(pollution_gen_server, {getStationMean, {CoordsOrName, Type}}).
getDailyMean(Day, Type) ->  gen_server:call(pollution_gen_server, {getDailyMean, {Day, Type}}).
getAreaMean(Type, CoordsOrName, Radius) -> gen_server:call(pollution_gen_server, {getAreaMean, {Type, CoordsOrName, Radius}}).
crash() -> gen_server:cast(pollution_gen_server, crash).
stop() -> gen_server:cast(pollution_gen_server, stop).

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
  end;
handle_call({removeValue, {CoordsOrName, Date, Type}}, _From, Monitor) ->
  Result = pollution:removeValue(Monitor, CoordsOrName, Date, Type),
  case Result of
    {error, _} -> {reply, Result, Monitor};
    _ -> {reply, ok, Result}
  end;
handle_call({getOneValue, {CoordsOrName, Date, Type}}, _From, Monitor) ->
  {reply, pollution:getOneValue(Monitor, CoordsOrName, Date, Type), Monitor};
handle_call({getStationMean, {CoordsOrName, Type}}, _From, Monitor) ->
  {reply, pollution:getStationMean(Monitor, CoordsOrName, Type), Monitor};
handle_call({getDailyMean, {Day, Type}}, _From, Monitor) ->
  {reply, pollution:getDailyMean(Monitor, Day, Type), Monitor};
handle_call({getAreaMean, {Type, CoordsOrName, Radius}}, _From, Monitor) ->
  {reply, pollution:getAreaMean(Monitor, Type, CoordsOrName, Radius), Monitor}.

handle_cast(crash, _) -> non_existing_module:non_existing_function();
handle_cast(stop, _) -> gen_server:stop(pollution_gen_server).