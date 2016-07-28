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
    MissionPacket = Gen([MissionSeg]),

    % Create a dwell segment to encode.
    DS = one_target_dwell(),

    % Create a complete segment with the header and payload.
    DwellSeg = segment:new(dwell, DS),

    % Create a packet with the new segment.
    DwellPacket = Gen([DwellSeg]),

    [].

%% Function to create a sample dwell with a single target report in it.
one_target_dwell() ->
    % Create a list of fields for the existence mask (excluding the target
    % report).
    F = [existence_mask, revisit_index, dwell_index, last_dwell_of_revisit,
         target_report_count, dwell_time, sensor_lat, sensor_lon, 
         sensor_alt, dwell_center_lat, dwell_center_lon, 
         dwell_range_half_extent, dwell_angle_half_extent, targets],
  
    % The fields of the target report.
    Params = [{mti_report_index, 34}, {target_hr_lat, -33.3}, 
              {target_hr_lon, 357.57}, {target_delta_lat, -45},
              {target_delta_lon, 46}, {geodetic_height, 5000},
              {target_vel_los, 32000}, {target_wrap_velocity, 40000},
              {target_snr, -128}, {target_classification, vehicle_live_target},
              {target_class_prob, 90}, {target_slant_range_unc, 1000},
              {target_cross_range_unc, 2000}, {target_height_unc, 200},
              {target_rad_vel_unc, 5000}, {truth_tag_app, 200},
              {truth_tag_entity, 10000}, {target_rcs, 10}],

    % Extract the list of fields in the target report.
    FieldList = [K || {K, _V} <- Params],

    % Splice together all the fields that make up the existence mask.
    Efields = F ++ FieldList,

    % Create the existence mask.
    EM = exist_mask:new(Efields), 
   
    % Create the target report.
    TgtRep = tgt_report:new(Params),

    % Set the fields of the dwell segment.
    P = [{existence_mask, EM}, {revisit_index, 100}, {dwell_index, 20000}, 
         {last_dwell_of_revisit, no_additional_dwells}, {target_report_count, 1}, 
         {dwell_time, 1000000}, {sensor_lat, -45.0}, {sensor_lon, 350},
         {sensor_alt, -10000}, {dwell_center_lat, -45.2}, 
         {dwell_center_lon, 350.2}, {dwell_range_half_extent, 255.0}, 
         {dwell_angle_half_extent, 350}, {targets, [TgtRep]}],

    % Create and return the dwell segment.
    dwell:new(P).
    
    

