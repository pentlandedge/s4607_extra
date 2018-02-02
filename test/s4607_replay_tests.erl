-module(s4607_replay_tests).

-include_lib("eunit/include/eunit.hrl").

%% Define a test generator function to run all the tests. 
s4607_replay_test_() ->
%    [patch_mission_checks()].
    [].

patch_mission_checks() ->
    MissionPacket = sample_mission_packet(),
    Date = {2018, 1, 24},
    {NewPacket, _} = s4607_replay:map_packet(MissionPacket, Date, 0),
    [MS] = s4607:get_segments([NewPacket]),
    SegData = segment:get_data(MS), 
    NewDate = mission:get_time(SegData),
    [?_assertEqual(Date, NewDate)].


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
