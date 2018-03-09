%% Some routines for generating sample Stanag 4607 packets.
-module(gen_4607).

-export([sample_mission_seg/0, sample_mission_seg/1]).

-define(MISSION_PLAN, "Hawk Sim 1").
-define(FlIGHT_PLAN, "FP 1").
-define(PLAT_CONFIG, "Sim v1.00").

%% @doc Generate a mission segment (including segment header) with today's 
%% date.
sample_mission_seg() ->
    {{Year,Month,Day}, _} = calendar:universal_time().

%% @doc Generate a mission segment (including segment header) with the 
%% specified date.
sample_mission_seg({Year, Month, Day}) ->
    MS = mission:new(?MISSION_PLAN, 
                     ?FLIGHT_PLAN, 
                     other, 
                     ?PLAT_CONFIG, 
                     year, 
                     month, 
                     day),
    segment:new(mission, MS).

