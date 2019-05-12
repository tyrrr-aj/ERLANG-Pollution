%%%-------------------------------------------------------------------
%%% @author adams
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. maj 2019 02:07
%%%-------------------------------------------------------------------
-module(pollution_tests).
-author("adams").

-include_lib("eunit/include/eunit.hrl").

createMonitor_test() -> ?assertEqual(#{}, pollution:createMonitor()).

addStation_test() ->
  M1 = pollution:addStation(#{}, {0,0}, "stacja1"),
  M2 = pollution:addStation(M1, {0,1}, "stacja2"),
  ?assertEqual(maps:get({0, 0}, M2), #{name=>"stacja1", values=>#{}}),
  ?assertEqual(maps:get({0, 1}, M2), #{name=>"stacja2", values=>#{}}),
  ?assertMatch({error, _},pollution:addStation(M2, {0,0}, "stacja")),
  ?assertMatch({error, _},pollution:addStation(M2, {1,1}, "stacja1")),
  ?assertMatch({error, _},pollution:addStation(M2, {0,0}, "stacja1")).

addValue_test() ->
  M1 = pollution:addStation(#{}, {0,0}, "stacja1"),
  M2 = pollution:addValue(M1, {0, 0}, "11-12-1998", pm10, 100),
  ?assertMatch({error, _}, pollution:addValue(M2, {0, 1}, "12-12-1998", pm10, 100)),
  ?assertMatch({error, _}, pollution:addValue(M2, "stacja2", "12-12-1998", pm10, 100)),
  ?assertMatch({error, _}, pollution:addValue(M2, {0, 0}, "11-12-1998", pm10, 200)).

removeValue_test() ->
  M1 = pollution:addStation(#{}, {0,0}, "stacja1"),
  M2 = pollution:addValue(M1, {0, 0}, "11-12-1998", pm10, 100),
  ?assert(not maps:is_key({pm10, "11-12-1998"}, maps:get(values, maps:get({0,0}, pollution:removeValue(M2, "stacja1", "11-12-1998", pm10))))),
  ?assert(not maps:is_key({pm10, "11-12-1998"}, maps:get(values, maps:get({0,0}, pollution:removeValue(M2, {0, 0}, "11-12-1998", pm10))))),
  ?assertMatch({error, _}, pollution:removeValue(M2, {0, 1}, "11-12-1998", pm10)),
  ?assertMatch({error, _}, pollution:removeValue(M2, "stacja2", "11-12-1998", pm10)),
  ?assertMatch(#{}, pollution:removeValue(M2, "stacja1", "12-12-1998", pm10)),
  ?assertMatch(#{}, pollution:removeValue(M2, {0, 0}, "11-12-1998", pm5)).

getOneValue_test() ->
  M1 = pollution:addStation(#{}, {0,0}, "stacja1"),
  M2 = pollution:addValue(M1, {0, 0}, "11-12-1998", pm10, 100),
  ?assertEqual(100, pollution:getOneValue(M2, {0, 0}, "11-12-1998", pm10)),
  ?assertEqual(100, pollution:getOneValue(M2, "stacja1", "11-12-1998", pm10)),
  ?assertMatch({error, _}, pollution:getOneValue(M2, {0, 1}, "11-12-1998", pm10)),
  ?assertMatch({error, _}, pollution:getOneValue(M2, "stacja2", "11-12-1998", pm10)),
  ?assertMatch({error, _}, pollution:getOneValue(M2, {0, 0}, "12-12-1998", pm10)),
  ?assertMatch({error, _}, pollution:getOneValue(M2, {0, 0}, "11-12-1998", pm5)).

getStationMean_test() ->
  M1 = pollution:addStation(#{}, {0,0}, "stacja1"),
  M2 = pollution:addValue(M1, {0, 0}, "11-12-1998", pm10, 100),
  M3 = pollution:addValue(M2, {0, 0}, "12-12-1998", pm10, 200),
  ?assertEqual(150.0, pollution:getStationMean(M3, "stacja1", pm10)),
  ?assertMatch({error, _}, pollution:getStationMean(M3, {0, 1}, pm10)),
  ?assertMatch({error, _}, pollution:getStationMean(M3, "stacja2", pm10)),
  ?assertMatch({error, _}, pollution:getStationMean(M3, {0, 0}, pm5)).

getDailyMean_test() ->
  M1 = pollution:addStation(#{}, {0,0}, "stacja1"),
  M2 = pollution:addStation(M1, {0,1}, "stacja2"),
  M3 = pollution:addValue(M2, {0, 0}, "11-12-1998", pm10, 100),
  M4 = pollution:addValue(M3, {0, 1}, "11-12-1998", pm10, 200),
  M5 = pollution:addValue(M4, {0, 1}, "12-12-1998", pm10, 400),
  ?assertEqual(150.0, pollution:getDailyMean(M5, "11-12-1998", pm10)),
  ?assertMatch({error, _}, pollution:getDailyMean(M5, "10-12-1998", pm10)),
  ?assertMatch({error, _}, pollution:getDailyMean(M5, "11-12-1998", pm5)).

getAreaMean_test() ->
  M1 = pollution:addStation(#{}, {0,0}, "stacja1"),
  M2 = pollution:addStation(M1, {0,1}, "stacja2"),
  M3 = pollution:addValue(M2, {0, 0}, "11-12-1998", pm10, 100),
  M4 = pollution:addValue(M3, {0, 1}, "11-12-1998", pm10, 200),
  M5 = pollution:addValue(M4, {0, 1}, "12-12-1998", pm10, 600),
  ?assertEqual(300.0, pollution:getAreaMean(M5, pm10, {0, 0}, 2)),
  ?assertEqual(300.0, pollution:getAreaMean(M5, pm10, {0, 0}, 1)),
  ?assertEqual(300.0, pollution:getAreaMean(M5, pm10, "stacja1", 2)),
  ?assertEqual(100.0, pollution:getAreaMean(M5, pm10, "stacja1", 0)),
  ?assertMatch({error, _}, pollution:getAreaMean(M5, pm10, {1, 1}, 2)),
  ?assertMatch({error, _}, pollution:getAreaMean(M5, pm10, "stacja3", 2)),
  ?assertMatch({error, _}, pollution:getAreaMean(M5, pm5, {0, 0}, 2)),
  ?assertMatch({error, _}, pollution:getAreaMean(M5, pm5, {0, 0}, -1)).

getCoords_test() ->
  M1 = pollution:addStation(#{}, {0,0}, "stacja1"),
  ?assertEqual({0, 0}, pollution:getCoords(M1, "stacja1")),
  ?assertEqual(no_coords, pollution:getCoords(M1, "stacja")).

saveValue_test() ->
  M1 = pollution:addStation(#{}, {0,0}, "stacja1"),
  M2 = pollution:saveValue(M1, {0, 0}, "11-12-1998", pm10, 100),
  ?assertEqual(100, maps:get({pm10, "11-12-1998"}, maps:get(values, maps:get({0,0}, M2)))),
  ?assertMatch({error, _}, pollution:saveValue(M2, {0, 0}, "11-12-1998", pm10, 100)).

deleteValue_test() ->
  M1 = pollution:addStation(#{}, {0,0}, "stacja1"),
  M2 = pollution:addValue(M1, {0, 0}, "11-12-1998", pm10, 100),
  ?assert(not maps:is_key({pm10, "11-12-1998"}, maps:get(values, maps:get({0,0}, pollution:deleteValue(M2, {0, 0}, "11-12-1998", pm10))))).