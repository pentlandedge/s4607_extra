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
    [empty_packet_list_checks(), single_loc_update_checks()].

empty_packet_list_checks() ->
    PacketList = [],
    Ret = tgt_stats:accumulate_updates(PacketList),
    [?_assertEqual([], Ret)].

%% Test a single packet with a single platform location segment.
single_loc_update_checks() ->
    LocPkt = sample_loc_packet(),
    [Update] = tgt_stats:accumulate_updates([LocPkt]),
    LMS = tgt_stats:get_last_mission(Update),
    [?_assertEqual(none, LMS)].

sample_loc_packet() ->
    LocSeg = sample_loc_seg(),
    Gen = sample_packet_generator(),
    Gen([LocSeg]).
    
sample_loc_seg() ->
    SegData = sample_loc_seg_data(),
    segment:new(platform_loc, SegData).

sample_loc_seg_data() ->
    % Mons Meg.
    platform_loc:new(34200000, 55.94877, -3.20015, 130, 350, 0, 0).

sample_packet_generator() ->
    PL = [{version, {3, 1}}, {nationality, "UK"},
         {classification, unclassified}, {class_system, "UK"},
         {packet_code, none}, {exercise_ind, exercise_real},
         {platform_id, "Plat1"}, {mission_id, 16#11223344},
         {job_id, 16#55667788}],

    s4607:packet_generator(PL).
