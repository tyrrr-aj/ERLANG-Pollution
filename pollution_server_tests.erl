%%%-------------------------------------------------------------------
%%% @author adams
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. maj 2019 02:16
%%%-------------------------------------------------------------------
-module(pollution_server_tests).
-author("adams").

-include_lib("eunit/include/eunit.hrl").

start_test() ->
  pollution_server:start(),
  ?assert(lists:member(pollution_server, registered())).

addStation_test() ->
%  pollution_server:start(),
  ?assertEqual(ok,pollution_server:addStation({0,0}, "stacja1")),
  ?assertEqual(ok,pollution_server:addStation({0,1}, "stacja2")),
  ?assertMatch({error, _},pollution_server:addStation({0,0}, "stacja")),
  ?assertMatch({error, _},pollution_server:addStation({1,1}, "stacja1")),
  ?assertMatch({error, _},pollution_server:addStation({0,0}, "stacja1")).
%  pollution_server:stop().

removeValue_test() ->
%  pollution_server:start(),
  pollution_server:addStation({0,0}, "stacja1"),
  pollution_server:addValue({0, 0}, "11-12-1998", pm10, 100),
  ?assertEqual(ok, pollution_server:removeValue("stacja1", "11-12-1998", pm10)),
  ?assertEqual(ok, pollution_server:removeValue({0, 0}, "11-12-1998", pm10)),
  ?assertMatch({error, _}, pollution_server:removeValue({0, 1}, "11-12-1998", pm10)),
  ?assertMatch({error, _}, pollution_server:removeValue("stacja2", "11-12-1998", pm10)),
  ?assertMatch(ok, pollution_server:removeValue("stacja1", "12-12-1998", pm10)),
  ?assertMatch(ok, pollution_server:removeValue({0, 0}, "11-12-1998", pm5)).
 % pollution_server:stop().

getOneValue_test() ->
%  pollution_server:start(),
  pollution_server:addStation({0,0}, "stacja1"),
  pollution_server:addValue({0, 0}, "11-12-1998", pm10, 100),
  ?assertEqual(100, pollution_server:getOneValue({0, 0}, "11-12-1998", pm10)),
  ?assertEqual(100, pollution_server:getOneValue("stacja1", "11-12-1998", pm10)),
  ?assertMatch({error, _}, pollution_server:getOneValue({0, 1}, "11-12-1998", pm10)),
  ?assertMatch({error, _}, pollution_server:getOneValue("stacja2", "11-12-1998", pm10)),
  ?assertMatch({error, _}, pollution_server:getOneValue({0, 0}, "12-12-1998", pm10)),
  ?assertMatch({error, _}, pollution_server:getOneValue({0, 0}, "11-12-1998", pm5)).
%  pollution_server:stop().

getStationMean_test() ->
%  pollution_server:start(),
  pollution_server:addStation({0,0}, "stacja1"),
  pollution_server:addValue({0, 0}, "11-12-1998", pm10, 100),
  pollution_server:addValue({0, 0}, "12-12-1998", pm10, 200),
  ?assertEqual(150.0, pollution_server:getStationMean("stacja1", pm10)),
  ?assertMatch({error, _}, pollution_server:getStationMean({0, 1}, pm10)),
  ?assertMatch({error, _}, pollution_server:getStationMean("stacja2", pm10)),
  ?assertMatch({error, _}, pollution_server:getStationMean({0, 0}, pm5)).
%  pollution_server:stop().

getDailyMean_test() ->
%  pollution_server:start(),
  pollution_server:addStation({0,0}, "stacja1"),
  pollution_server:addStation({0,1}, "stacja2"),
  pollution_server:addValue({0, 0}, "11-12-1998", pm10, 100),
  pollution_server:addValue({0, 1}, "11-12-1998", pm10, 200),
  pollution_server:addValue({0, 1}, "12-12-1998", pm10, 400),
  ?assertEqual(150.0, pollution_server:getDailyMean("11-12-1998", pm10)),
  ?assertMatch({error, _}, pollution_server:getDailyMean("10-12-1998", pm10)),
  ?assertMatch({error, _}, pollution_server:getDailyMean("11-12-1998", pm5)).
%  pollution_server:stop().

getAreaMean_test() ->
%  pollution_server:start(),
  pollution_server:addStation({0,0}, "stacja1"),
  pollution_server:addStation({0,1}, "stacja2"),
  pollution_server:addValue({0, 0}, "11-12-1998", pm10, 100),
  pollution_server:addValue({0, 1}, "11-12-1998", pm10, 200),
  pollution_server:addValue({0, 1}, "12-12-1998", pm10, 600),
  ?assertEqual(300.0, pollution_server:getAreaMean(pm10, {0, 0}, 2)),
  ?assertEqual(300.0, pollution_server:getAreaMean(pm10, {0, 0}, 1)),
  ?assertEqual(300.0, pollution_server:getAreaMean(pm10, "stacja1", 2)),
  ?assertEqual(100.0, pollution_server:getAreaMean(pm10, "stacja1", 0)),
  ?assertMatch({error, _}, pollution_server:getAreaMean(pm10, {1, 1}, 2)),
  ?assertMatch({error, _}, pollution_server:getAreaMean(pm10, "stacja3", 2)),
  ?assertMatch({error, _}, pollution_server:getAreaMean(pm5, {0, 0}, 2)),
  ?assertMatch({error, _}, pollution_server:getAreaMean(pm5, {0, 0}, -1)).
%  pollution_server:stop().