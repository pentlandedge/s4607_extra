%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright 2016 Pentland Edge Ltd.
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
%% @doc Module to provide packet list test data to use in other unit tests.

-module(packet_list).

-export([get_list1/0]).

%% Packet List 1. Consists of a single mission segment and a single 
%% dwell segment with a single target report.
get_list1() ->
    % List the parameters for the packet header (no need to set size).
    PL = [{version, {3, 1}}, {nationality, "UK"}, {classification, top_secret}, 
          {class_system, "UK"}, {platform_id, "Pico1"}, 
          {mission_id, 16#11223344}, 
          {job_id, 16#55667788}],

    % Create a generator function
    Gen = s4607:packet_generator(PL),

    % Create a mission segment.
    MS = mission:new("Drifter 1", "A1234", other, "Build 1", 2016, 7, 28),

    % Create a complete segment with the header and payload.
    MissionSeg = segment:new(mission, MS),

    % Create a packet containing the mission segment
    Pack = Gen([MissionSeg]),

    [].

