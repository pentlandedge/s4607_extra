-module(s4607_replay_tests).

-include_lib("eunit/include/eunit.hrl").

%% Define a test generator function to run all the tests. 
s4607_replay_test_() ->
%    [patch_mission_checks()].
    [patch_mission_seg_data_checks(), patch_dwell_seg_data_checks()].

%% Simple check that the mission segment data record can be updated.
patch_mission_seg_data_checks() ->
    MS = mission:new("Drifter 1", "A1234", other, "Build 1", 2016, 2, 5),
    NewDate = {2018, 2, 2},
    NewMS = s4607_replay:patch_mission_seg_data(MS, NewDate),
    ActualDate = mission:get_time(NewMS),
    [?_assertEqual(NewDate, ActualDate)].

%% Simple check that a dwell segment data record can be updated successfully.
%% Uses the test data in packet_list.erl
patch_dwell_seg_data_checks() ->
    % Create a dwell segment data record.
    DS = packet_list:one_target_dwell(), 
    % 11 a.m. in milliseconds.
    NewDwellTime = 39600000,
    NewDS = s4607_replay:patch_dwell_seg_data(DS, NewDwellTime),
    ActualDwellTime = dwell:get_dwell_time(NewDS),
    [?_assertEqual(NewDwellTime, ActualDwellTime)].

sample_mission_packet() ->
    MS = mission:new("Drifter 1", "A1234", other, "Build 1", 2016, 2, 5),
    Seg = segment:new(mission, MS),
    PL = [{version, {3, 1}}, {nationality, "UK"},
          {classification, unclassified}, {class_system, "UK"},
          {packet_code, none}, {exercise_ind, exercise_real},
          {platform_id, "Plat1"}, {mission_id, 16#11223344},
          {job_id, 16#55667788}],
    Gen = s4607:packet_generator(PL),
    Packet = Gen([Seg]),
    Packet.
