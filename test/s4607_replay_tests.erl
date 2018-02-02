-module(s4607_replay_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TIME_11AM_MS, 39600000).

%% Define a test generator function to run all the tests. 
s4607_replay_test_() ->
    [patch_mission_seg_data_checks(), patch_dwell_seg_data_checks(), 
     patch_mission_checks(), patch_dwell_checks()].

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
    NewDwellTime = ?TIME_11AM_MS,
    NewDS = s4607_replay:patch_dwell_seg_data(DS, NewDwellTime),
    ActualDwellTime = dwell:get_dwell_time(NewDS),
    [?_assertEqual(NewDwellTime, ActualDwellTime)].

%% Check the patching of a packet containing a single mission segment.
patch_mission_checks() ->
    Packet = sample_mission_packet(),
    Date = {2018, 1, 24},
    Replay = s4607_replay:init_replay_state(Date),
    {NewPacket, NewReplay} = s4607_replay:update_packet(Packet, Replay, 0),
    [MS] = s4607:get_segments([NewPacket]),
    SegData = segment:get_data(MS), 
    NewDate = mission:get_time(SegData),
    [?_assertEqual(Date, NewDate)].

%% Check the patching of a dwell packet.
patch_dwell_checks() ->
    [_,Packet] = packet_list:get_list1(),
    Date = {2018, 2, 2},
    Time = ?TIME_11AM_MS, 
    Replay = s4607_replay:init_replay_state(Date),
    {NewPacket, NewReplay} = s4607_replay:update_packet(Packet, Replay, Time),
    [DS] = s4607:get_segments([NewPacket]),
    SegData = segment:get_data(DS), 
    NewTime = dwell:get_dwell_time(SegData),
    [?_assertEqual(Time, NewTime)].

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
