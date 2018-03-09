%% Some routines for generating sample Stanag 4607 packets.
-module(gen_4607).

-export([sample_mission_seg/0, sample_mission_seg/1, gen_position_fun/4]).

-define(MISSION_PLAN, "Hawk Sim 1").
-define(FLIGHT_PLAN, "FP 1").
-define(PLAT_CONFIG, "Sim v1.00").

%% @doc Generate a mission segment (including segment header) with today's 
%% date.
sample_mission_seg() ->
    {Date, _} = calendar:universal_time(),
    sample_mission_seg(Date).

%% @doc Generate a mission segment (including segment header) with the 
%% specified date.
sample_mission_seg({Year, Month, Day}) ->
    MS = mission:new(?MISSION_PLAN, 
                     ?FLIGHT_PLAN, 
                     other, 
                     ?PLAT_CONFIG, 
                     Year, 
                     Month, 
                     Day),
    segment:new(mission, MS).

%% @doc Generate a function which can calcuate the position of a given target 
%% at a specified time. This is based on an initial position, a constant 
%% speed and bearing. The Haversine formula is used.
gen_position_fun(Lat, Lon, Bearing, Speed) ->
    fun(Time) ->
        Distance = Speed * Time,
        coord:destination({Lat, Lon}, Bearing, Distance)
    end.

