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

-export([get_list1/0, get_list2/0, one_target_dwell/0]).

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

    % Return the two packets as a list.
    [MissionPacket, DwellPacket].

%% Packet List 2. Same as 1, but with a job definition segment prepended.
get_list2() ->
    % List the parameters for the packet header (no need to set size).
    PL = [{version, {3, 1}}, {nationality, "UK"}, {classification, top_secret}, 
          {class_system, "UK"}, {platform_id, "Pico1"}, 
          {mission_id, 16#11223344}, 
          {job_id, 16#55667788}],

    % Create a generator function
    Gen = s4607:packet_generator(PL),

    % Create a complete segment with the header and payload.
    JD = sample_job_def(),
    JDSeg = segment:new(job_definition, JD),

    % Create a packet containing the job definition segment
    JDPacket = Gen([JDSeg]),
   
    % Prepend to packet list 1.
    PL1 = get_list1(), 
    [JDPacket|PL1].  

%% Function to create a sample dwell with a single target report in it.
one_target_dwell() ->
    % Create a list of fields for the existence mask (excluding the target
    % report).
    F = [existence_mask, revisit_index, dwell_index, last_dwell_of_revisit,
         target_report_count, dwell_time, sensor_lat, sensor_lon, 
         sensor_alt, dwell_center_lat, dwell_center_lon, 
         dwell_range_half_extent, dwell_angle_half_extent, targets],
  
    % The fields of the target report.
    Params = [{mti_report_index, 34}, {target_hr_lat, 55.9987}, 
              {target_hr_lon, -2.71}, {geodetic_height, 100},
              {target_vel_los, 530}, {target_wrap_velocity, 40000},
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
         {dwell_time, 1000000}, {sensor_lat, 55.928613}, {sensor_lon, -2.66116},
         {sensor_alt, 100000}, {dwell_center_lat, 55.999591}, 
         {dwell_center_lon, -2.718204}, {dwell_range_half_extent, 2.5}, 
         {dwell_angle_half_extent, 22.5}, {targets, [TgtRep]}],

    % Create and return the dwell segment.
    dwell:new(P).
    
%% Create a sample job definition segment for testing.
sample_job_def() ->
    job_def:new(sample_job_def_params()).

sample_job_def_params() ->
    [{job_id, 100}, {sensor_id_type, rotary_wing_radar},
     {sensor_id_model, "Heli 1"}, {target_filt_flag, no_filtering}, {priority, 30},
     {bounding_a_lat, 33.3}, {bounding_a_lon, 3.45},
     {bounding_b_lat, 23.4}, {bounding_b_lon, 350},
     {bounding_c_lat, -45.0}, {bounding_c_lon, 2.45},
     {bounding_d_lat, -60.0}, {bounding_d_lon, 140},
     {radar_mode, {monopulse_calibration, asars_aip}}, {nom_rev_int, 65000},
     {ns_pos_unc_along_track, no_statement},
     {ns_pos_unc_cross_track, 5000}, {ns_pos_unc_alt, 20000},
     {ns_pos_unc_heading, 45}, {ns_pos_unc_sensor_speed, 65534},
     {ns_val_slant_range_std_dev, 100},
     {ns_val_cross_range_std_dev, no_statement},
     {ns_val_tgt_vel_los_std_dev, 4000}, {ns_val_mdv, no_statement},
     {ns_val_det_prob, 100}, {ns_val_false_alarm_density, 254},
     {terr_elev_model, dgm50}, {geoid_model, geo96}].

