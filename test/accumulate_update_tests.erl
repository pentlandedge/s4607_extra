%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright 2018 Pentland Edge Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License"); you may not
%% use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
%% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
%% License for the specific language governing permissions and limitations
%% under the License.
%%

-module(accumulate_update_tests).

-include_lib("eunit/include/eunit.hrl").

%% Define a test generator function to run all the tests.
accumulate_update_test_() ->
    [empty_packet_list_checks(), single_loc_update_checks(), 
     mission_loc_update_checks(), mission_loc_dwell_update_checks(),
     mission_loc_three_dwell_update_checks()].

empty_packet_list_checks() ->
    PacketList = [],
    Ret = tgt_stats:accumulate_updates(PacketList),
    [?_assertEqual([], Ret)].

%% Test a single packet with a single platform location segment.
single_loc_update_checks() ->
    LocPkt = sample_loc_packet(),
    [Update] = tgt_stats:accumulate_updates([LocPkt]),
    {loc_update, LocUpdate} = Update,
    LMS = tgt_stats:get_last_mission(LocUpdate),
    PlatLoc = tgt_stats:get_loc_data(LocUpdate),
    Alt = platform_loc:get_alt(PlatLoc),
    Track = platform_loc:get_platform_track(PlatLoc),
    [?_assertEqual(none, LMS), ?_assertEqual(130, Alt), 
     ?_assertEqual(350, Track)].

%% Test a list with two packets: one containing a mission segment and the
%% other a platform location segment.
mission_loc_update_checks() ->
    MisPkt = sample_mission_packet(),
    LocPkt = sample_loc_packet(),
    [Update] = tgt_stats:accumulate_updates([MisPkt, LocPkt]),
    {loc_update, LocUpdate} = Update,
    LMS = tgt_stats:get_last_mission(LocUpdate),
    Date = mission:get_time(LMS),
    [?_assertEqual({2018, 9, 9}, Date)].

%% Test a list with three packets: a mission, location segment followed by a 
%% dwell segment with the last dwell of revisit set (to complete a scan).
%% We should then get back two updates, one containing the platform location
%% and the second containing a scan. Both updates should have the mission 
%% date set correctly.
mission_loc_dwell_update_checks() ->
    MisPkt = sample_mission_packet(),
    LocPkt = sample_loc_packet(),
    DwlPkt = minimal_dwell_packet(),
    [Update1, Update2] = tgt_stats:accumulate_updates([MisPkt, LocPkt, DwlPkt]),
    {loc_update, LocUpdate} = Update1,
    {scan, Scan} = Update2,
    LMS = tgt_stats:get_last_mission(LocUpdate),
    Date = mission:get_time(LMS),
    PlatLoc = tgt_stats:get_loc_data(LocUpdate),
    Track = platform_loc:get_platform_track(PlatLoc),
    [DS] = tgt_stats:get_grouped_dwells(Scan),
    SensorAlt = dwell:get_sensor_alt(DS),
    [?_assertEqual({2018, 9, 9}, Date), ?_assertEqual(350, Track),
     ?_assertEqual(-10000, SensorAlt)].

%% Test a list with five packets containing mission, location, and three 
%% dwell segments. The three dwells comprise a single revisit so should be
%% grouped as a single scan.
mission_loc_three_dwell_update_checks() ->
    _MisPkt = sample_mission_packet(),
    _LocPkt = sample_loc_packet(),
    Dwell1 = dwell1(),
    Dwell2 = dwell2(),
    Dwell3 = dwell3(),
    Segs = [segment:new(dwell, X) || X <- [Dwell1, Dwell2, Dwell3]],
    packet_wrap(Segs),
    [].

sample_mission_packet() ->
    MisSeg = sample_mission_seg(),
    Gen = sample_packet_generator(),
    Gen([MisSeg]).

sample_loc_packet() ->
    LocSeg = sample_loc_seg(),
    Gen = sample_packet_generator(),
    Gen([LocSeg]).

minimal_dwell_packet() ->
    DwellSeg = sample_dwell_seg(),
    Gen = sample_packet_generator(),
    Gen([DwellSeg]).

packet_wrap(SegList) ->
    Gen = sample_packet_generator(),
    Gen(SegList).

sample_mission_seg() ->
    SegData = sample_mission_seg_data(),
    segment:new(mission, SegData).

sample_loc_seg() ->
    SegData = sample_loc_seg_data(),
    segment:new(platform_loc, SegData).

sample_dwell_seg() ->
    SegData = minimal_dwell(),
    segment:new(dwell, SegData).

sample_mission_seg_data() ->
    mission:new("MISSION 1", "FP 123", fire_scout, "SW 1829", 2018, 9, 9).

sample_loc_seg_data() ->
    % Mons Meg.
    platform_loc:new(34200000, 55.94877, -3.20015, 130, 350, 0, 0).

%% Function to create a sample dwell segment with only the mandatory fields
% set.
minimal_dwell() ->
    % Create a list of fields for the existence mask.
    F = [existence_mask, revisit_index, dwell_index, last_dwell_of_revisit,
         target_report_count, dwell_time, sensor_lat, sensor_lon, 
         sensor_alt, dwell_center_lat, dwell_center_lon, 
         dwell_range_half_extent, dwell_angle_half_extent],

    % Create an existence mask.
    EM = exist_mask:new(F), 
   
    % Set the fields of the dwell segment.
    P = [{existence_mask, EM}, {revisit_index, 100}, {dwell_index, 20000}, 
         {last_dwell_of_revisit, no_additional_dwells}, {target_report_count, 0}, 
         {dwell_time, 1000000}, {sensor_lat, -45.0}, {sensor_lon, 350},
         {sensor_alt, -10000}, {dwell_center_lat, -45.2}, 
         {dwell_center_lon, 350.2}, {dwell_range_half_extent, 255.0}, 
         {dwell_angle_half_extent, 350}],

    % Use the parameters to construct a new dwell segment.
    dwell:new(P).

dwell1() ->
    EM = sample_existence_mask(),

    % Set the fields of the dwell segment.
    P = [{existence_mask, EM}, {revisit_index, 100}, {dwell_index, 20000}, 
         {last_dwell_of_revisit, additional_dwells}, {target_report_count, 0}, 
         {dwell_time, 1000000}, {sensor_lat, -45.0}, {sensor_lon, 350},
         {sensor_alt, 10000}, {dwell_center_lat, -45.2}, 
         {dwell_center_lon, 350.2}, {dwell_range_half_extent, 255.0}, 
         {dwell_angle_half_extent, 350}],

    % Use the parameters to construct a new dwell segment.
    dwell:new(P).

dwell2() ->
    EM = sample_existence_mask(),

    % Set the fields of the dwell segment.
    P = [{existence_mask, EM}, {revisit_index, 100}, {dwell_index, 20000}, 
         {last_dwell_of_revisit, additional_dwells}, {target_report_count, 0}, 
         {dwell_time, 1010000}, {sensor_lat, -45.0}, {sensor_lon, 350},
         {sensor_alt, 10000}, {dwell_center_lat, -45.3}, 
         {dwell_center_lon, 350.2}, {dwell_range_half_extent, 255.0}, 
         {dwell_angle_half_extent, 350}],

    % Use the parameters to construct a new dwell segment.
    dwell:new(P).

dwell3() ->
    EM = sample_existence_mask(),

    % Set the fields of the dwell segment.
    P = [{existence_mask, EM}, {revisit_index, 100}, {dwell_index, 20000}, 
         {last_dwell_of_revisit, no_additional_dwells}, {target_report_count, 0}, 
         {dwell_time, 1020000}, {sensor_lat, -45.0}, {sensor_lon, 350},
         {sensor_alt, 10000}, {dwell_center_lat, -45.4}, 
         {dwell_center_lon, 350.2}, {dwell_range_half_extent, 255.0}, 
         {dwell_angle_half_extent, 350}],

    % Use the parameters to construct a new dwell segment.
    dwell:new(P).

sample_existence_mask() ->
    Fields = dwell_fields(),
    exist_mask:new(Fields).

dwell_fields() ->
    % Create a list of fields for the existence mask.
    [existence_mask, revisit_index, dwell_index, last_dwell_of_revisit, 
     target_report_count, dwell_time, sensor_lat, sensor_lon, 
     sensor_alt, dwell_center_lat, dwell_center_lon, dwell_range_half_extent,
     dwell_angle_half_extent].

sample_packet_generator() ->
    PL = [{version, {3, 1}}, {nationality, "UK"},
         {classification, unclassified}, {class_system, "UK"},
         {packet_code, none}, {exercise_ind, exercise_real},
         {platform_id, "Plat1"}, {mission_id, 16#11223344},
         {job_id, 16#55667788}],

    s4607:packet_generator(PL).


