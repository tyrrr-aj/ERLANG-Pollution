%%%-------------------------------------------------------------------
%%% @author adams
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. kwi 2019 21:17
%%%-------------------------------------------------------------------
-module(pollution).
-author("adams").

%% API
-export([createMonitor/0, addStation/3, addValue/5, removeValue/4, getOneValue/4, getStationMean/3, getDailyMean/3, getAreaMean/4]).

-define(NOTEST, true).

%-ifdef(TEST).
-compile(export_all).
%-endif.

createMonitor() -> #{}.

% Monitor structure: #{{X, Y} => #{name => StationName, values => #{Type, Date} => Value}}}

addStation(Monitor, {X, Y}, StationName) when is_number(X) and is_number(Y) ->
  case maps:is_key({X, Y}, Monitor) of
    true -> {error, "Coordinates already used"};
    false -> case lists:member(StationName, [maps:get(name, Station) || Station <- maps:values(Monitor)]) of
               true -> {error, "Station name already used"};
               false -> Monitor#{{X, Y} => #{name => StationName, values => #{}}}
             end
  end.

addValue(Monitor, Coords, Date, Type, Value) when is_tuple(Coords) ->
  case maps:is_key(Coords, Monitor) of
    true -> saveValue(Monitor, Coords, Date, Type, Value);
    false -> {error, "Unknown coordinates"}
  end;
addValue(Monitor, StationName, Date, Type, Value) ->
  case getCoords(Monitor, StationName) of
    no_coords -> {error, "Unknown station name"};
    Coords -> saveValue(Monitor, Coords, Date, Type, Value)
  end.

removeValue(Monitor, Coords, Date, Type) when is_tuple(Coords) ->
  case maps:is_key(Coords, Monitor) of
    true -> deleteValue(Monitor, Coords, Date, Type);
    false -> {error, "Unknown coordinates"}
  end;
removeValue(Monitor, StationName, Date, Type) ->
  case getCoords(Monitor, StationName) of
    no_coords -> {error, "Unknown station name"};
    Coords -> deleteValue(Monitor, Coords, Date, Type)
  end.

getOneValue(Monitor, Coords, Date, Type) when is_tuple(Coords) ->
  case maps:is_key(Coords, Monitor) of
    true -> Values = maps:get(values, maps:get(Coords, Monitor)),
            case maps:is_key({Type, Date}, Values) of
                  true -> maps:get({Type, Date}, Values);
                  false -> {error, "No value for given station, date and type"}
            end;
    false -> {error, "Unknown coordinates"}
  end;
getOneValue(Monitor, StationName, Date, Type) ->
  case getCoords(Monitor, StationName) of
    no_coords -> {error, "Unknown station name"};
    Coords -> getOneValue(Monitor, Coords, Date, Type)
  end.

getStationMean(Monitor, Coords, Type) when is_tuple(Coords) ->
  case maps:is_key(Coords, Monitor) of
    true -> Values = maps:get(values, maps:get(Coords, Monitor)),
            {Sum, NumberOfValues} = maps:fold(fun(K, V, {TSum, TNum}) -> case K of {Type, _} -> {TSum + V, TNum + 1}; _ -> {TSum, TNum} end end, {0, 0}, Values),
            if
              NumberOfValues == 0 -> {error, "No measured values for given station and type"};
              true -> Sum / NumberOfValues
            end;
    false -> {error, "Unknown coordinates"}
  end;
getStationMean(Monitor, StationName, Type) ->
  case getCoords(Monitor, StationName) of
    no_coords -> {error, "Unknown station name"};
    Coords -> getStationMean(Monitor, Coords, Type)
  end.

getDailyMean(Monitor, Day, Type) ->
  AllValues = [maps:get(values, Station) || Station <- maps:values(Monitor)],
  SumMap = fun(Values, Acc) -> maps:fold(fun(K, V, {TSum, TNum}) -> case K of {Type, Day} -> {TSum + V, TNum + 1}; _ -> {TSum, TNum} end end, Acc, Values) end,
  {Sum, NumberOfValues} = lists:foldl(SumMap, {0, 0}, AllValues),
  if
    NumberOfValues == 0 -> {error, "No measured values for given day and type"};
    true -> Sum / NumberOfValues
  end.

getAreaMean(Monitor, Type, CoordsCent, Radius) when is_tuple(CoordsCent) ->
  case maps:is_key(CoordsCent, Monitor) of
    true -> StationsInArea = maps:filter(fun(Coords, _) -> calculateDistance(Coords, CoordsCent) =< Radius end, Monitor),
            AllValues = [maps:get(values, Station) || Station <- maps:values(StationsInArea)],
            SumMap = fun(Values, Acc) -> maps:fold(fun(K, V, {TSum, TNum}) -> case K of {Type, _} -> {TSum + V, TNum + 1}; _ -> {TSum, TNum} end end, Acc, Values) end,
            {Sum, NumberOfValues} = lists:foldl(SumMap, {0, 0}, AllValues),
            if
              NumberOfValues == 0 -> {error, "No measured values for given day and type"};
              true -> Sum / NumberOfValues
            end;
    false -> {error, "Unknown coordinates"}
  end;
getAreaMean(Monitor, Type, StationName, Radius) ->
  case getCoords(Monitor, StationName) of
    no_coords -> {error, "Unknown station name"};
    Coords -> getAreaMean(Monitor, Type, Coords, Radius)
  end.

%----- module internal functions -----------------------------------------------------------------

getCoords(Monitor, StationName) -> maps:fold(fun(K, V, Acc) -> case maps:get(name, V) of StationName -> K; _ -> Acc end end, no_coords, Monitor).

saveValue(Monitor, Coords, Date, Type, Value) ->
  Station = maps:get(Coords, Monitor),
  Values = maps:get(values, Station),
  case maps:is_key({Type, Date}, Values) of
    false -> Monitor#{Coords := Station#{values => Values#{{Type, Date} => Value}}};
    true -> {error, "Value for given station, type and date already exists"}
  end.

deleteValue(Monitor, Coords, Date, Type) ->
  Station = maps:get(Coords, Monitor),
  Values = maps:get(values, Station),
  Monitor#{Coords := Station#{values := maps:remove({Type, Date}, Values)}}.

calculateDistance({X1, Y1}, {X2, Y2}) -> math:sqrt(math:pow(X1 - X2, 2) + math:pow(Y1 - Y2, 2)).